#|
General game theory play

Main choice is whether to represent game as:
1. normal form matrix for n players
2. network of nodes, each of which is a single-agent decision
Option 1 doesn't seem to scale well to a full network or to be cognitively plausible
Option 2 scales better, is naturally jag-compatible (and MDP-compatible), and cognitively plausible even for simultaneous games (e.g. PRS)
Node is player (name), state and features (optional, i.e. a chunk), and list of possible actions with associated outcomes (i.e. IBL chunks)
Outcomes are (1) rewards for various players and/or (2) subsequent nodes (both optional)

Chunks learned are
player - who
state - final
action - take or push
utility - own

|#

(defstruct node
  "Game node"
  player state actions utilities outcomes)

(defun generate-centipede (&key (length 12) (players '(a b)) (node 1) (actions '(take push)) (pot 4) (ratio 0.75) (multiplier 2))
  "Generate centipede network for specific parameters."
  (when (plusp length)
    (cons (make-node
           :player (first players)
           :state node
           :actions actions
           :utilities (list (list (list (first players) (* pot ratio))
                                  (list (second players) (* pot (- 1.0 ratio))))
                            (when (= length 1) (list (list (second players) (* multiplier pot ratio))
                                                        (list (first players) (* multiplier pot (- 1.0 ratio))))))
           :outcomes (list nil (unless (= length 1) (1+ node))))
          (generate-centipede :length (1- length)
                              :players (list (second players) (first players))
                              :node (1+ node)
                              :actions actions
                              :pot (* multiplier pot)
                              :ratio ratio
                              :multiplier multiplier))))

(defparameter *game* (generate-centipede))

(defun run-centipede (games &key (a-player 'play-push) (b-player 'play-take) (game *game*) (init t) (trace t))
  "Runs games of centipede."
  (when init (init-model))
  (dotimes (index games)
    (when trace (format t "Game ~D~%" (1+ index)))
    (dolist (node game)
      (let ((distribution (case (node-player node)
                            (a (funcall a-player node :trace trace))
                            (b (funcall b-player node :trace trace))
                            (t (play-random node))))
            (roll (random 1.0))
            (action nil)
            (total 0.0))
        (dolist (option distribution)
          (incf total (second option))
          (when (< roll total)
            (setf action (first option))
            (return)))
        (when trace (format t "Player ~D Turn ~D Actions ~S Chosen ~S~%"
                            (node-player node) (node-state node) distribution action))
        (when (eq action 'take)
          (when (eq a-player 'play-centipede) (learn-centipede node :player 'a :trace trace))
          (when (eq b-player 'play-centipede) (learn-centipede node :player 'b :trace trace))
          (when trace (format t "Payoffs: ~S~%" (node-utilities node)))
          (return))))))

(defun log-similarity (a b)
  "Log ratio. From 0 to minus infinity so not limited to fixed interval."
  (log (/ (min a b) (max a b))))

(defun linear-similarity (pa pb)
  "Linear differnece in [0,1] interval."
  (- (min pa pb) (max pa pb)))

(defun number-similarity (x y)
  "Assumes payoffs are integers and probabilities are real values."
  (when (and (numberp x) (numberp y))
    (if (or (integerp x) (integerp y)) ;;; if at least one of the numbers is an integer assume they are intensity
        (log-similarity x y)
        (linear-similarity x y))))

(defun init-model (&optional (parameters nil))
  (init-memory)
  (init-similarities)
  (learn '((player a) (payoff 10000)))
  (learn '((player b) (payoff 10000)))
  (actr-time 1.0)
  (setf *similarity-hook-function* 'number-similarity)
  (similarity 'a 'b -10.0)
  (similarity 'b 'a -10.0)
  (dolist (parameter parameters)
    (parameter (first parameter) (second parameter))))

(init-model)

(defstruct agent
  "Agent structures"
  name
  parameters
  memory
  similarities)

(defvar *agents*
  "Represents agents in centipede game."
  nil)

(defun reset-centipede ()
  "Reset centipede game by clearing agents."
  (setf *agents* nil))

(defun softmax (options utilities)
  "Calibrated softmax of list of options with list of utilities."
  (let* ((max (apply #'max utilities))
         (scales (mapcar #'(lambda (utility) (exp (/ (- (log utility) (log max)) *temperature*))) utilities))
         (sum (reduce #'+ scales)))
    (sort (mapcar #'list options (mapcar #'(lambda (scale) (/ scale sum)) scales)) #'> :key #'second)))

(defun play-centipede (node &key (memory *memory*) (trace t))
  "Plays centipede by generating expected value of possible actions."
  (let* ((player (node-player node))
;         (agent nil)
         (n (length (node-actions node)))
         (evs nil))
    (actr-time 1.0)
    (dotimes (move n)
      (let* ((utilities (nth move (node-utilities node)))
             (utility (if utilities (second (assoc player utilities))
                          (blend `((player ,player)) 'payoff memory))))
        (when trace (format t "Expected Utility of ~S is ~6,3F~%" (nth move (node-actions node)) utility))
        (push utility evs)))
    (softmax (node-actions node) (reverse evs))))

(defun learn-centipede (node &key (player (node-player node)) (memory *memory*) (trace t))
  "Learns the outcome of a game assuming there is only one action."
  (let* ((utilities (first (node-utilities node)))
         (payoff (if (numberp utilities) utilities
                     (second (assoc player utilities)))))
    (when trace (format t "Learning Player ~S Payoff ~6,3F~%" player payoff))
    (learn (list (list 'player player)
                 (list 'payoff payoff))
           :memory memory)))

(defun play-take (node &key (trace t))
  "Always take."
  (declare (ignore node))
  (when trace (format t "Always Take~%"))
  '((take 1.0) (push 0.0)))

(defun play-push (node &key (trace t))
  "Always push."
  (declare (ignore node))
  (when trace (format t "Always Push~%"))
  '((push 1.0) (take 0.0)))

(defparameter *take-probability* 0.2)

(defun play-random (node &key (p *take-probability*) (trace t))
  "Take randomly with probability p."
  (declare (ignore node))
  (when trace (format t "Take with Probability ~6,1F~%" p))
  (sort (list (list 'take p) (list 'push (- 1.0 p))) #'> :key #'second))

#|
(defun generate-centipede (&key (length 12) (players '(a b)) (actions '(take push)) (pot 4) (ratio 0.75) (multiplier 2))
  "Generate centipede network for specific parameters."
  (let ((game nil))
    (dotimes (index length (reverse game))
      (let* ((name (1+ index))
             (node (make-node
                    :player (nth (mod index 2) players)
                    :state name
                    :actions actions
                    :utilities (list (list (list (first player) (* pot ratio) (* pot (- 1.0 ratio)))
                                     (when (= name length) (list (* multiplier pot (- 1.0 ratio)) (* multiplier pot ratio))))
                    :outcomes (list nil (unless (= name length) (1+ name))))))
        (push node game)
        (setf pot (* multiplier pot))))))
|#

#|
;;; more explicit representation of utilities
?  (pprint (setf *game* (generate-centipede)))

(#S(NODE :PLAYER A :STATE 1 :ACTIONS (TAKE PUSH) :UTILITIES (((A 3.0) (B 1.0)) NIL) :OUTCOMES (NIL 2))
 #S(NODE :PLAYER B :STATE 2 :ACTIONS (TAKE PUSH) :UTILITIES (((B 6.0) (A 2.0)) NIL) :OUTCOMES (NIL 3))
 #S(NODE :PLAYER A :STATE 3 :ACTIONS (TAKE PUSH) :UTILITIES (((A 12.0) (B 4.0)) NIL) :OUTCOMES (NIL 4))
 #S(NODE :PLAYER B :STATE 4 :ACTIONS (TAKE PUSH) :UTILITIES (((B 24.0) (A 8.0)) NIL) :OUTCOMES (NIL 5))
 #S(NODE :PLAYER A :STATE 5 :ACTIONS (TAKE PUSH) :UTILITIES (((A 48.0) (B 16.0)) NIL) :OUTCOMES (NIL 6))
 #S(NODE :PLAYER B :STATE 6 :ACTIONS (TAKE PUSH) :UTILITIES (((B 96.0) (A 32.0)) NIL) :OUTCOMES (NIL 7))
 #S(NODE :PLAYER A :STATE 7 :ACTIONS (TAKE PUSH) :UTILITIES (((A 192.0) (B 64.0)) NIL) :OUTCOMES (NIL 8))
 #S(NODE :PLAYER B :STATE 8 :ACTIONS (TAKE PUSH) :UTILITIES (((B 384.0) (A 128.0)) NIL) :OUTCOMES (NIL 9))
 #S(NODE :PLAYER A :STATE 9 :ACTIONS (TAKE PUSH) :UTILITIES (((A 768.0) (B 256.0)) NIL) :OUTCOMES (NIL 10))
 #S(NODE :PLAYER B :STATE 10 :ACTIONS (TAKE PUSH) :UTILITIES (((B 1536.0) (A 512.0)) NIL) :OUTCOMES (NIL 11))
 #S(NODE :PLAYER A :STATE 11 :ACTIONS (TAKE PUSH) :UTILITIES (((A 3072.0) (B 1024.0)) NIL) :OUTCOMES (NIL 12))
 #S(NODE :PLAYER B :STATE 12 :ACTIONS (TAKE PUSH) :UTILITIES (((B 6144.0) (A 2048.0)) ((A 12288.0) (B 4096.0))) :OUTCOMES (NIL NIL)))
;;; running model against push strategy
? (run-centipede 1 :a-player 'play-centipede :b-player 'play-push :trace t)
Game 1
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 373.523
Player A Turn 1 Actions ((PUSH 0.9920324) (TAKE 0.007967639)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 597.312
Player A Turn 3 Actions ((PUSH 0.9803057) (TAKE 0.019694358)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 241.086
Player A Turn 5 Actions ((PUSH 0.8339593) (TAKE 0.1660407)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 451.463
Player A Turn 7 Actions ((PUSH 0.7016144) (TAKE 0.29838562)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 581.961
Player A Turn 9 Actions ((TAKE 0.56890535) (PUSH 0.4310947)) Chosen TAKE
Learning Player A Payoff 768.000
Payoffs: (((A 768.0) (B 256.0)) NIL)
NIL
;;; running series of 10 games of model against push
? (run-centipede 10 :a-player 'play-centipede :b-player 'play-push :trace t)
Game 1
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 702.717
Player A Turn 1 Actions ((PUSH 0.995749) (TAKE 0.0042509986)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 677.056
Player A Turn 3 Actions ((PUSH 0.98258483) (TAKE 0.01741512)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 604.200
Player A Turn 5 Actions ((PUSH 0.9264029) (TAKE 0.07359712)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 599.485
Player A Turn 7 Actions ((PUSH 0.7574179) (TAKE 0.24258208)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 794.400
Player A Turn 9 Actions ((PUSH 0.5084486) (TAKE 0.49155134)) Chosen PUSH
Player B Turn 10 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 3072.000
Expected Utility of PUSH is 720.602
Player A Turn 11 Actions ((TAKE 0.8099978) (PUSH 0.19000219)) Chosen TAKE
Learning Player A Payoff 3072.000
Payoffs: (((A 3072.0) (B 1024.0)) NIL)
Game 2
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 2164.736
Player A Turn 1 Actions ((PUSH 0.9986161) (TAKE 0.0013839324)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 2001.846
Player A Turn 3 Actions ((PUSH 0.99404126) (TAKE 0.0059587467)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 1365.062
Player A Turn 5 Actions ((PUSH 0.9660312) (TAKE 0.033968803)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 1438.295
Player A Turn 7 Actions ((PUSH 0.8822299) (TAKE 0.11777011)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 1413.505
Player A Turn 9 Actions ((PUSH 0.6479495) (TAKE 0.35205045)) Chosen TAKE
Learning Player A Payoff 768.000
Payoffs: (((A 768.0) (B 256.0)) NIL)
Game 3
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1413.133
Player A Turn 1 Actions ((PUSH 0.9978815) (TAKE 0.0021184443)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1462.670
Player A Turn 3 Actions ((PUSH 0.99186254) (TAKE 0.008137415)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 1661.484
Player A Turn 5 Actions ((PUSH 0.9719213) (TAKE 0.028078651)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 1259.600
Player A Turn 7 Actions ((PUSH 0.86773217) (TAKE 0.13226786)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 1361.331
Player A Turn 9 Actions ((PUSH 0.6393233) (TAKE 0.36067674)) Chosen PUSH
Player B Turn 10 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 3072.000
Expected Utility of PUSH is 1092.120
Player A Turn 11 Actions ((TAKE 0.7377309) (PUSH 0.26226905)) Chosen TAKE
Learning Player A Payoff 3072.000
Payoffs: (((A 3072.0) (B 1024.0)) NIL)
Game 4
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 2023.098
Player A Turn 1 Actions ((PUSH 0.99851936) (TAKE 0.0014806787)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1430.288
Player A Turn 3 Actions ((PUSH 0.99167985) (TAKE 0.008320113)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 1262.707
Player A Turn 5 Actions ((PUSH 0.96337855) (TAKE 0.036621474)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 1346.936
Player A Turn 7 Actions ((PUSH 0.87523854) (TAKE 0.124761514)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 2044.795
Player A Turn 9 Actions ((PUSH 0.7269619) (TAKE 0.27303806)) Chosen PUSH
Player B Turn 10 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 3072.000
Expected Utility of PUSH is 1650.459
Player A Turn 11 Actions ((TAKE 0.6505084) (PUSH 0.34949154)) Chosen TAKE
Learning Player A Payoff 3072.000
Payoffs: (((A 3072.0) (B 1024.0)) NIL)
Game 5
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 2548.813
Player A Turn 1 Actions ((PUSH 0.9988243) (TAKE 0.0011756343)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1366.577
Player A Turn 3 Actions ((PUSH 0.99129534) (TAKE 0.00870463)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 1601.888
Player A Turn 5 Actions ((PUSH 0.9709071) (TAKE 0.029092884)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 1442.069
Player A Turn 7 Actions ((PUSH 0.88250184) (TAKE 0.117498145)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 1848.830
Player A Turn 9 Actions ((PUSH 0.70651513) (TAKE 0.29348484)) Chosen PUSH
Player B Turn 10 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 3072.000
Expected Utility of PUSH is 1466.141
Player A Turn 11 Actions ((TAKE 0.67692906) (PUSH 0.3230709)) Chosen PUSH
Player B Turn 12 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Game 6
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1192.653
Player A Turn 1 Actions ((PUSH 0.9974909) (TAKE 0.00250909)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1354.200
Player A Turn 3 Actions ((PUSH 0.99121654) (TAKE 0.008783483)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 2018.820
Player A Turn 5 Actions ((PUSH 0.9767759) (TAKE 0.02322408)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 1696.018
Player A Turn 7 Actions ((PUSH 0.898306) (TAKE 0.101693936)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 1476.604
Player A Turn 9 Actions ((PUSH 0.657846) (TAKE 0.342154)) Chosen PUSH
Player B Turn 10 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 3072.000
Expected Utility of PUSH is 1825.947
Player A Turn 11 Actions ((TAKE 0.6272014) (PUSH 0.3727986)) Chosen TAKE
Learning Player A Payoff 3072.000
Payoffs: (((A 3072.0) (B 1024.0)) NIL)
Game 7
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1566.866
Player A Turn 1 Actions ((PUSH 0.998089) (TAKE 0.0019109907)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 2049.961
Player A Turn 3 Actions ((PUSH 0.9941803) (TAKE 0.005819703)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 2026.534
Player A Turn 5 Actions ((PUSH 0.97686225) (TAKE 0.023137733)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 2194.815
Player A Turn 7 Actions ((PUSH 0.9195581) (TAKE 0.080441944)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 2547.263
Player A Turn 9 Actions ((PUSH 0.76834416) (TAKE 0.2316558)) Chosen PUSH
Player B Turn 10 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 3072.000
Expected Utility of PUSH is 1400.762
Player A Turn 11 Actions ((TAKE 0.6868238) (PUSH 0.3131762)) Chosen PUSH
Player B Turn 12 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Game 8
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 2378.432
Player A Turn 1 Actions ((PUSH 0.99874026) (TAKE 0.0012597458)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1637.676
Player A Turn 3 Actions ((PUSH 0.99272585) (TAKE 0.0072741597)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 2166.775
Player A Turn 5 Actions ((PUSH 0.97832733) (TAKE 0.021672623)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 2069.609
Player A Turn 7 Actions ((PUSH 0.9151047) (TAKE 0.0848953)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 1791.890
Player A Turn 9 Actions ((PUSH 0.6999871) (TAKE 0.30001286)) Chosen PUSH
Player B Turn 10 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 3072.000
Expected Utility of PUSH is 1955.153
Player A Turn 11 Actions ((TAKE 0.61108136) (PUSH 0.38891864)) Chosen TAKE
Learning Player A Payoff 3072.000
Payoffs: (((A 3072.0) (B 1024.0)) NIL)
Game 9
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 2504.348
Player A Turn 1 Actions ((PUSH 0.9988035) (TAKE 0.0011964833)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1616.027
Player A Turn 3 Actions ((PUSH 0.99262905) (TAKE 0.007370886)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 1807.328
Player A Turn 5 Actions ((PUSH 0.9741286) (TAKE 0.025871439)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 1930.248
Player A Turn 7 Actions ((PUSH 0.9095299) (TAKE 0.0904701)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 1719.968
Player A Turn 9 Actions ((PUSH 0.6913144) (TAKE 0.30868557)) Chosen TAKE
Learning Player A Payoff 768.000
Payoffs: (((A 768.0) (B 256.0)) NIL)
Game 10
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1550.054
Player A Turn 1 Actions ((PUSH 0.9980684) (TAKE 0.0019316782)) Chosen PUSH
Player B Turn 2 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1708.225
Player A Turn 3 Actions ((PUSH 0.9930241) (TAKE 0.0069758315)) Chosen PUSH
Player B Turn 4 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 2203.075
Player A Turn 5 Actions ((PUSH 0.9786768) (TAKE 0.021323146)) Chosen PUSH
Player B Turn 6 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 1956.964
Player A Turn 7 Actions ((PUSH 0.9106546) (TAKE 0.089345366)) Chosen PUSH
Player B Turn 8 Actions ((PUSH 1.0) (TAKE 0.0)) Chosen PUSH
Expected Utility of TAKE is 768.000
Expected Utility of PUSH is 1846.876
Player A Turn 9 Actions ((PUSH 0.7062959) (TAKE 0.29370412)) Chosen TAKE
Learning Player A Payoff 768.000
Payoffs: (((A 768.0) (B 256.0)) NIL)
NIL
;;; running series of 10 games of model against random
? (run-centipede 10 :a-player 'play-centipede :b-player 'play-random :trace t)
Game 1
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 3198.626
Player A Turn 1 Actions ((PUSH 0.99906296) (TAKE 9.3702396E-4)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 4123.907
Player A Turn 3 Actions ((PUSH 0.99709857) (TAKE 0.0029014188)) Chosen PUSH
Player B Turn 4 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  8.000
Payoffs: (((B 24.0) (A 8.0)) NIL)
Game 2
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1036.382
Player A Turn 1 Actions ((PUSH 0.9971137) (TAKE 0.0028863323)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  2.000
Payoffs: (((B 6.0) (A 2.0)) NIL)
Game 3
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1144.509
Player A Turn 1 Actions ((PUSH 0.9973857) (TAKE 0.0026143594)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1262.459
Player A Turn 3 Actions ((PUSH 0.99058425) (TAKE 0.009415761)) Chosen PUSH
Player B Turn 4 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  8.000
Payoffs: (((B 24.0) (A 8.0)) NIL)
Game 4
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1511.341
Player A Turn 1 Actions ((PUSH 0.998019) (TAKE 0.0019810598)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1095.964
Player A Turn 3 Actions ((PUSH 0.98916936) (TAKE 0.0108306715)) Chosen PUSH
Player B Turn 4 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen PUSH
Expected Utility of TAKE is 48.000
Expected Utility of PUSH is 1174.195
Player A Turn 5 Actions ((PUSH 0.9607263) (TAKE 0.03927361)) Chosen PUSH
Player B Turn 6 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen PUSH
Expected Utility of TAKE is 192.000
Expected Utility of PUSH is 1755.333
Player A Turn 7 Actions ((PUSH 0.9014036) (TAKE 0.09859641)) Chosen PUSH
Player B Turn 8 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff 128.000
Payoffs: (((B 384.0) (A 128.0)) NIL)
Game 5
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 872.662
Player A Turn 1 Actions ((PUSH 0.99657404) (TAKE 0.0034259812)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen PUSH
Expected Utility of TAKE is 12.000
Expected Utility of PUSH is 1562.193
Player A Turn 3 Actions ((PUSH 0.99237704) (TAKE 0.0076229526)) Chosen PUSH
Player B Turn 4 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  8.000
Payoffs: (((B 24.0) (A 8.0)) NIL)
Game 6
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1185.138
Player A Turn 1 Actions ((PUSH 0.99747497) (TAKE 0.0025249585)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  2.000
Payoffs: (((B 6.0) (A 2.0)) NIL)
Game 7
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 878.478
Player A Turn 1 Actions ((PUSH 0.99659663) (TAKE 0.0034033742)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  2.000
Payoffs: (((B 6.0) (A 2.0)) NIL)
Game 8
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 1354.894
Player A Turn 1 Actions ((PUSH 0.9977907) (TAKE 0.0022093041)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  2.000
Payoffs: (((B 6.0) (A 2.0)) NIL)
Game 9
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 673.856
Player A Turn 1 Actions ((PUSH 0.99556774) (TAKE 0.0044322563)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  2.000
Payoffs: (((B 6.0) (A 2.0)) NIL)
Game 10
Expected Utility of TAKE is  3.000
Expected Utility of PUSH is 627.930
Player A Turn 1 Actions ((PUSH 0.99524516) (TAKE 0.0047548865)) Chosen PUSH
Player B Turn 2 Actions ((TAKE 0.5) (PUSH 0.5)) Chosen TAKE
Learning Player A Payoff  2.000
Payoffs: (((B 6.0) (A 2.0)) NIL)
NIL
? (time (run-centipede 10 :a-player 'play-centipede :b-player 'play-random :trace nil))
(RUN-CENTIPEDE 10 :A-PLAYER 'PLAY-CENTIPEDE :B-PLAYER 'PLAY-RANDOM :TRACE NIL)
took 17,811 microseconds (0.017811 seconds) to run.
During that period, and with 10 available CPU cores,
     18,024 microseconds (0.018024 seconds) were spent in user mode
      9,671 microseconds (0.009671 seconds) were spent in system mode
 38,832 bytes of memory allocated.
 748 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (run-centipede 100 :a-player 'play-centipede :b-player 'play-random :trace nil))
(RUN-CENTIPEDE 100 :A-PLAYER 'PLAY-CENTIPEDE :B-PLAYER 'PLAY-RANDOM :TRACE NIL)
took 163,894 microseconds (0.163894 seconds) to run.
During that period, and with 10 available CPU cores,
      99,258 microseconds (0.099258 seconds) were spent in user mode
      96,259 microseconds (0.096259 seconds) were spent in system mode
 449,952 bytes of memory allocated.
 15,994 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (run-centipede 1000 :a-player 'play-centipede :b-player 'play-random :trace nil))
(RUN-CENTIPEDE 1000 :A-PLAYER 'PLAY-CENTIPEDE :B-PLAYER 'PLAY-RANDOM :TRACE NIL)
took 1,495,825 microseconds (1.495825 seconds) to run.
         1,557 microseconds (0.001557 seconds, 0.10%) of which was spent in GC.
During that period, and with 10 available CPU cores,
       722,468 microseconds (0.722468 seconds) were spent in user mode
       808,078 microseconds (0.808078 seconds) were spent in system mode
 5,061,792 bytes of memory allocated.
 148,108 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (run-centipede 10000 :a-player 'play-centipede :b-player 'play-random :trace nil))
(RUN-CENTIPEDE 10000 :A-PLAYER 'PLAY-CENTIPEDE :B-PLAYER 'PLAY-RANDOM :TRACE NIL)
took 15,445,648 microseconds (15.445648 seconds) to run.
         17,970 microseconds ( 0.017970 seconds, 0.12%) of which was spent in GC.
During that period, and with 10 available CPU cores,
      7,514,961 microseconds ( 7.514961 seconds) were spent in user mode
      8,301,496 microseconds ( 8.301496 seconds) were spent in system mode
 50,273,840 bytes of memory allocated.
 1,386,282 minor page faults, 191 major page faults, 0 swaps.
NIL

|#
  
  
