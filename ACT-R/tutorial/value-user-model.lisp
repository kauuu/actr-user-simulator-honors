(clear-all)

(define-model value-user-model
    
(sgp :v t :show-focus t)
(sgp :esc t :er t) ; enable the subsymbolic computation, and randomness to make it non-deterministic
(sgp :bll 0.5 :rt 0 :ans 2) ; threshold is set to 0, and noise parameter is 5 to have a lot of variance
(sgp :act t)
(sgp :time-master-start-increment 5 :time-mult 1) ; ticks with length 5

(chunk-type value-selection step current-target choice-side)
(chunk-type retrieved-value type value1 rank1 value2 rank2 choice)
(chunk-type preference-rank value-type rank)
(chunk-type ranks rank1 rank2) ; stores the rank of each screen side
(chunk-type task-information task-desc value-left value-right choice)
(chunk-type task side value)

;; Define a helper function to initialize the variable with the permuted list
(defun initialize-value-preference-list ()
  (permute-list '("SelfDirection" "Benevolence" "Achievement" "Conformity")))

;; Define the global variable *VALUE-PREFERENCE-LIST* and initialize it with a permuted list
(defvar *VALUE-PREFERENCE-LIST*
  (initialize-value-preference-list)
  "A global variable representing the permuted list of value preferences.")

(defun phrase-contained-p (phrase string)
  "Check if PHRASE is contained in STRING."
  (if (and (stringp phrase) (stringp string))
      (let ((phrase-length (length phrase))
            (string-length (length string))
            (index 0))
        (loop while (and (< index (- string-length phrase-length)) 
                         (not (string= (subseq string index (+ index phrase-length)) phrase)))
              do (setq index (1+ index)))
        (not (= index (- string-length phrase-length))))))

(defvar *s-prob* '(0.5704697986577181 0.6144519015659955 0.6173154362416108 0.6425503355704698 0.636510067114094 0.6613870246085011 0.6395078299776287 0.6368680089485459 0.6066666666666667 0.5887248322147651 0.5497091722595079 0.5335123042505593 0.5514988814317673 0.5790156599552573 0.5502013422818792 0.5721252796420582 0.5739597315436241 0.5252796420581656 0.552930648769575 0.5293064876957494 0.5396868008948545 0.5288590604026846 0.5631767337807606 0.5503803131991052))

(defvar *r-prob* '(0.0829082774049217 0.07789709172259508 0.08344519015659956 0.07485458612975392 0.07463087248322148 0.07342281879194631 0.06769574944071588 0.06514541387024608 0.06398210290827741 0.07230425055928412 0.07543624161073825 0.0719910514541387 0.06796420581655481 0.06519015659955257 0.06040268456375839 0.07583892617449664 0.08044742729306488 0.06863534675615213 0.06098434004474273 0.06805369127516779 0.0712751677852349 0.07422818791946309 0.07955257270693512 0.08098434004474273))
(defvar *o-prob* '(0.3466219239373602 0.3076510067114094 0.2992393736017897 0.2825950782997763 0.28885906040268455 0.2651901565995526 0.2927964205816555 0.2979865771812081 0.32935123042505593 0.3389709172259508 0.3748545861297539 0.394496644295302 0.38053691275167784 0.35579418344519015 0.3893959731543624 0.3520357941834452 0.34559284116331096 0.4060850111856823 0.38608501118568234 0.4026398210290828 0.3890380313199105 0.39691275167785234 0.3572706935123042 0.3686353467561521))

(defun give-target (stage)
  "Based on the time, it will give a target: sl, sr, r, o"
  (let* ((s (nth stage *s-prob*))
         (r (nth stage *r-prob*))
         (random-number (random 1.0)))
    (cond
     ((< random-number s) (if (>= (random 1.0) 0.5)
            "look-screen-left"
            "look-screen-right"))
     ((< random-number (+ s r)) "look-robot")
     (t "look-other"))))

(defun decision-to-shift-target-attention (stage current-target)
  (let* ((stay-prob (cond ((string= current-target "screen") (nth stage *s-prob*))
                          ((string= current-target "robot") (nth stage *r-prob*))
                          (t (nth stage *o-prob*))))
         (random-num (random 1.0))) ; Generates a random number between 0 and 1
    (if (< random-num stay-prob)
        "switch-target"
        "target-search")))

(defun pick-a-random-choice ()
  (let* ((random-num (random 1.0))) ; Generates a random number between 0 and 1
    (if (< random-num 0.5)
        "retrieve-left"
        "retrieve-right")))

; (defun decision-to-shift-target-attention (stage current-target)
;   (let* ((stay-prob (cond ((string= current-target "screen") 
;                            (nth stage *s-prob*))
;                           ((string= current-target "robot") 
;                            (nth stage *r-prob*))
;                           ((string= current-target "other") 
;                            (nth stage *o-prob*))
;                           (t 
;                            (progn
;                              (format t "Warning: Unknown current-target: ~a~%" current-target)
;                              0.0))))  ; Fallback probability
;          (random-num (random 1.0))  ; Generates a random number between 0 and 1
;          (result (if (< random-num stay-prob)
;                      "switch-target"
;                      "target-search")))
;     (format t "Stage: ~a, Current Target: ~a, Stay Probability: ~a, Random Number: ~a, Result: ~a~%"
;             stage current-target stay-prob random-num result)
;     result))

; (defun give-target (stage)
;   "Based on the time, it will give a target: sl, sr, r, o"
;   (print (list "s:" *s-prob* "r:" *r-prob* "stage: " stage)) ; Print the values
;   (let* ((s (nth stage *s-prob*))
;          (r (nth stage *r-prob*))
;          (random-number (random 1.0)))
;     (cond
;      ((< random-number s) (if (> (random 1.0) 0.5)
;                               "look-screen-left"
;                               "look-screen-right"))
;      ((< random-number (+ s r)) "look-robot")
;      (t "look-other"))))

(add-dm
 (start isa chunk)
 (focus-screen-left isa chunk) (focus-screen-right isa chunk)
 (encode-screen-left isa chunk) (encode-screen-right isa chunk)
 (move-screen-right isa chunk)
 (respond isa chunk) (done isa chunk) (post-interview isa chunk)
 (store-rank-left isa chunk) (store-rank-right isa chunk)
 (retrieve-rank-left isa chunk) (retrieve-rank-right isa chunk)
 (start-search isa chunk) (task-details isa chunk)
 (target-search isa chunk) (focus-robot isa chunk) (focus-other isa chunk) 
 (STORE-TASK-INFO-FOR-QUALITY isa chunk) (RETRIEVE-CHOICE isa chunk) (STORE-TASK-CHOICES-FOR-QUALITY isa chunk) (SPEAK-QUALITY isa chunk) (SWITCH-TARGET isa chunk)
 (done-interaction isa chunk)
 (PICK-A-RANDOM-CHOICE isa chunk)
 (STORE-LEFT isa chunk) (RESPOND-LEFT isa chunk)
 (STORE-RIGHT isa chunk) (RESPOND-RIGHT isa chunk) 
 (IDLE isa chunk)
 (STORE-TASK-LEFT-FOR-QUALITY isa chunk) (RETRIEVE-RIGHT-FOR-QUALITY isa chunk) (STORE-TASK-RIGHT-FOR-QUALITY isa chunk)
 (rank1 isa preference-rank value-type "Conformity" rank 1)
 (rank2 isa preference-rank value-type "Benevolence" rank 2)
 (rank3 isa preference-rank value-type "Achievement" rank 3)
 (rank4 isa preference-rank value-type "SelfDirection" rank 4) ; NOTE got to randomise the rankings
 (goal isa value-selection step start))

(P detected-sound "if there is a change in the audiotory environment, and you are not attending to another sound, then move attention to this auditory source"
    =aural-location>
    ?aural>
        state free
==> 
    +aural>
        event =aural-location
)

(P look-at-screen-cue "production that will be used for the screen cue -> change prob distr such that 90% look at screen"
    =goal>
    =aural>
        isa sound
        content "Look at screen"
    !bind! =target (if (>= (random 1.0) 0.5) "look-screen-left" "look-screen-right") ; picks a screen target at random
==> 
    =goal>
        isa value-selection
        step =target
)

(P stop-attending-at-end "When the robot says, thank you, stop attending the screen"
    =goal>
    =aural>
        isa sound
        content "Thank you for your time, and good bye!"
==>
    +visual>
        cmd clear ;his request will cause the model to stop attending to any visual items until a new request to move-attention is made
    =goal>
        isa value-selection
        step idle ;remain idle until quality check is made
)

(P check-quality-cue "if there is a change in the audiotory environment, and you are not attending to another sound, then move attention to this auditory source"
    =goal>
        isa value-selection
        step idle
    =aural>
        isa sound
        content "Quality"
==> 
    =goal>
        isa value-selection
        step post-interview
)

(P respond-with-choice "If the robot asks to respond, then do that!"
    =goal>
    =aural>
        isa sound
        content "What is your choice?"
==>
    =goal>
        isa value-selection
        step pick-a-random-choice
)

(P pick-a-random-choice "Randomly picks one of the two sides"
    =goal>
        isa value-selection
        step pick-a-random-choice
    !bind! =choice (pick-a-random-choice)
==>
    =goal>
        isa value-selection
        step =choice
)

(P retrieve-left-for-quality "you have moved attention, and encoded it, then store in dec memory"
    =goal>
        isa value-selection
        step post-interview
==>
    +retrieval>
        isa task
        side left
    =goal>
        isa value-selection
        step store-task-left-for-quality
    -imaginal> ; clear whatever is in imaginal
)

(P store-task-left-for-quality
    =goal>
        isa value-selection
        step store-task-left-for-quality
    =retrieval> ; something has been retrieved
        value =value-left
==>
    +imaginal>
        isa task-information
        value-left =value-left
    =goal>
        isa value-selection
        step retrieve-right-for-quality
)

(P retrieve-right-for-quality "you have moved attention, and encoded it, then store in dec memory"
    =goal>
        isa value-selection
        step retrieve-right-for-quality
==>
    +retrieval>
        isa task
        side right
    =goal>
        isa value-selection
        step store-task-right-for-quality
)

(P store-task-right-for-quality
    =goal>
        isa value-selection
        step store-task-right-for-quality
    =retrieval> ; something has been retrieved
        value =value-right
    =imaginal>
==>
    =imaginal>
        isa task-information
        value-right =value-right
    =goal>
        isa value-selection
        step retrieve-choice
)

(P retrieve-choice "retrieve choice"
    =goal>
        isa value-selection
        step retrieve-choice
        choice-side =choice-side
    ?retrieval>
        state free
==>
    +retrieval>
        side =choice-side ;retrieves the imaginal chunk that stored all value information
    =goal>
        isa value-selection
        step store-task-choices-for-quality
)

(P store-task-choices-for-quality
    =goal>
        isa value-selection
        step store-task-choices-for-quality
    =imaginal>
    =retrieval> ; something has been retrieved
        value =value-choice
==>
    =imaginal>
        choice =value-choice
    =goal>
        isa value-selection
        step speak-quality
)

(P respond-memory-of-task
    =goal>
        isa value-selection
        step speak-quality
    =imaginal>
        isa task-information
        value-left =value-left
        value-right =value-right 
        choice =choice
    ?vocal>
        state free
==>
    =goal>
        isa value-selection
        step done
    +vocal>
        isa speak
        cmd speak
        string =choice
)

(P start-task "This is the starting point!"
    =goal>
        isa value-selection
        step start
==>
    =goal>
        isa value-selection
        step "target-search"
    ; +imaginal>
    ;     isa retrieved-value
    ;     type task-details
    +temporal> ; starts the ticks
        ticks 0
)

(P choose-target "Pick a target to look at, and attend to"
    =goal>
        isa value-selection
        step "target-search"
    =temporal>
        ticks =stage
    !bind! =target (give-target =stage) ; picks a target
==>
    =goal>
        isa value-selection
        step =target ; assigns the next step to look at the just picked target
)

(P switch-target "Decides whether to switch target or not"
    =goal>
        isa value-selection
        step "switch-target"
        current-target =current-target
    =temporal>
        ticks =stage
    !bind! =stay (decision-to-shift-target-attention =stage =current-target) ; This says if I should stay on the same screen or switch attention
==> 
    =goal>
        isa value-selection
        step =stay
)

(P find-robot "Look at the robot"
    =goal>
        isa value-selection
        step "look-robot"
==>
    +visual-location>
        screen-y lowest ; gets the top most, which is robot
    =goal>
        isa value-selection
        step focus-robot
)

(P focus-robot "Move my focus to the robot"
    =goal>
        isa value-selection
        step focus-robot
    =visual-location> ; make sure there is a chunk in this buffer, that is some object has been found in the scene
    ?visual>
        state free ; make sure that the visual buffer is not busy with some other action
==>
    +visual>
        isa move-attention
        cmd move-attention
        screen-pos =visual-location ; move the location to whatever is stored in the visual-location buffer
    =goal>
        isa value-selection
        step "switch-target" ; after attending, you can pick another target (could be the same)
        current-target "robot"
)

(P find-other "Look at the other"
    =goal>
        isa value-selection
        step "look-other"
==>
    +visual-location>
        screen-y highest ; gets the bottom most, which is other
    =goal>
        isa value-selection
        step focus-other
)

(P focus-other "Move my focus to the other"
    =goal>
        isa value-selection
        step focus-other
    =visual-location> ; make sure there is a chunk in this buffer, that is some object has been found in the scene
    ?visual>
        state free ; make sure that the visual buffer is not busy with some other action
==>
    +visual>
        isa move-attention
        cmd move-attention
        screen-pos =visual-location ; move the location to whatever is stored in the visual-location buffer
    =goal>
        isa value-selection
        step "switch-target" ; go back to picking target after
        current-target "other"
)

 
(P find-screen-left "First looks at the left screen"
    =goal>
        isa value-selection
        step "look-screen-left"
==>
    +visual-location>
        screen-x lowest ; gets the left most, which is screen left
    =goal>
        isa value-selection
        step focus-screen-left
)

(P focus-screen-left "Move my focus to the left screen"
    =goal>
        isa value-selection
        step focus-screen-left
    =visual-location> ; make sure there is a chunk in this buffer, that is some object has been found in the scene
    ?visual>
        state free ; make sure that the visual buffer is not busy with some other action
==>
    +visual>
        isa move-attention
        cmd move-attention
        screen-pos =visual-location ; move the location to whatever is stored in the visual-location buffer
    =goal>
        isa value-selection
        step encode-screen-left
)

(P encode-screen-left "Encode/retrieve value associated with left screen"
    =goal>
        isa value-selection
        step encode-screen-left
    =visual>
        value =value-left
    ?imaginal>
        state free
==>
    =goal>
        isa value-selection
        step "switch-target" ; NEED TO CHANGE
        current-target "screen"
    +imaginal>
        isa task
        side left
        value =value-left ; store the current letter you have encoded
    -imaginal> ; clear it to improve activation in memory
)

(P move-screen-right 
    =goal>
        isa value-selection
        step "look-screen-right"
    ?visual-location>
        state free ; make sure the state is free!
==>
    +visual-location>
        screen-x highest; gets the right most, which is the right screen/value
    =goal>
        isa value-selection
        step focus-screen-right
)

(P focus-screen-right "Move my focus to the right screen"
    =goal>
        isa value-selection
        step focus-screen-right
    =visual-location> ; make sure there is a chunk in this buffer, that is some object has been found in the scene
    ?visual>
        state free ; make sure that the visual buffer is not busy with some other action
==>
    +visual>
        isa move-attention
        cmd move-attention
        screen-pos =visual-location ; move the location to whatever is stored in the visual-location buffer
    =goal>
        isa value-selection
        step encode-screen-right
)

(P encode-screen-right "Encode informaton of right screen"
    =goal>
        isa value-selection
        step encode-screen-right
    =visual>
        value =value-right
    ?imaginal>
        state free
==>
    =goal>
        isa value-selection
        step "switch-target" ; go and pick another target
        current-target "screen"
    +imaginal>
        isa task
        side right
        value =value-right ; store the right value you have encoded
    -imaginal> ; clear it to improve activation in memory
)

(P retrieve-left "Retrieves information about the left image from the declarative memory"
    =goal>
        isa value-selection
        step "retrieve-left"
    ; =imaginal>
    ;     isa retrieved-value
    ;     type task-left
    ;     value1 =value-left
==> 
    +retrieval>
        isa task ; retrieves the task on the left
        side left
    =goal>
        isa value-selection
        step store-left
    -imaginal>
        ; isa retrieved-value
        ; value1 =value-left
)

(P store-left "stores the ranking of the left value from the declarative memory"
    =goal>
        isa value-selection
        step store-left
    ; =imaginal>
    ;     isa retrieved-value
    ;     type task-details
    ;     value1 =value-left
    =retrieval>
        isa task
        value =value-left
        side left
==> 
    +imaginal>
        isa task ; stores the value on the left
        side left
        value =value-left
    =goal>
        isa value-selection
        step respond-left
)

(P retrieve-right "Retrieves the ranking of the right value from the declarative memory"
    =goal>
        isa value-selection
        step "retrieve-right"
    ; =imaginal>
    ;     isa retrieved-value
    ;     type task-details
    ;     value2 =value-right
==> 
    +retrieval>
        isa task ; retrieves the right value from memory
        side right
    =goal>
        isa value-selection
        step store-right
    -imaginal>
    ; =imaginal>
    ;     isa retrieved-value
    ;     value2 =value-right
)

(P store-right "stores the ranking of the right value from the declarative memory"
    =goal>
        isa value-selection
        step store-right
    ; =imaginal>
    ;     isa retrieved-value
    ;     type task-details
    ;     value2 =value-right
    =retrieval>
        isa task
        side right
        value =value-right
==> 
    +imaginal>
        isa task ; stores rank-left
        side right
        value =value-right
    =goal>
        isa value-selection
        step respond-right
)

(P respond-left "Finally respond with the value you prefer"
    =goal>
        isa value-selection
        step respond-left
    =imaginal>
        isa task
        side =left
        value =value-left
        ; rank1 =rank1
        ; value2 =value-right 
        ; >= rank2 =rank1 ;rank2 is more than or equal to rank1, return rank1 -> smaller rank better
    ?vocal>
        state free
==>
    =goal>
        isa value-selection
        step done-interaction
        choice-side left
    +vocal>
        isa speak
        cmd speak
        string =value-left
    ; =imaginal>
    ;     isa retrieved-value
    ;     choice =value-left
    -imaginal>
)

(P respond-right "Finally respond with the value you prefer"
    =goal>
        isa value-selection
        step respond-right
    =imaginal>
        isa task
        side right
        value =value-right
        ; isa retrieved-value
        ; type task-details
        ; value1 =value-left
        ; rank1 =rank1
        ; value2 =value-right 
        ; <= rank2 =rank1 ;rank2 is more than or equal to rank1, return rank1 -> smaller rank better
    ?vocal>
        state free
==>
    =goal>
        isa value-selection
        step done-interaction
        choice-side right
    +vocal>
        isa speak
        cmd speak
        string =value-right
    ; =imaginal>
    ;     isa retrieved-value
    ;     choice =value-right
    -imaginal>
)
 
(goal-focus goal)
)
