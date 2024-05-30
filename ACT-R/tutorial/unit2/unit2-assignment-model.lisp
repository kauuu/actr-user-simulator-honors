(clear-all)

(define-model unit2
    
(sgp :v t :show-focus t)


(chunk-type read-letters step)
(chunk-type array letter1 letter2 letter3)
(chunk-type preference value1 value2 value3 value4)

(add-dm 
 (start isa chunk)
 (focus-first-letter isa chunk) (focus-second-letter isa chunk) (focus-third-letter isa chunk)
 (encode-first-letter isa chunk) (encode-second-letter isa chunk) (encode-third-letter isa chunk)
 (move-second-letter isa chunk) (move-third-letter isa chunk)
 (respond isa chunk) (done isa chunk)
 (user-preference isa preference value1 "SelfDirection" value2 "Benevolence" value3 "Achievement" value4 "Conformity")
 (goal isa read-letters step start))
 
(P find-first-letter "Starts the task"
    =goal>
        isa read-letters
        step start
==>
    +visual-location>
        :attended nil ; Looks to an object that has not been attended in the past, the vision module stores that information
    =goal>
        isa read-letters
        step focus-first-letter
)

(P focus-first-letter "Move my focus to the first letter"
    =goal>
        isa read-letters
        step focus-first-letter
    =visual-location> ; make sure there is a chunk in this buffer, that is some object has been found in the scene
    ?visual>
        state free ; make sure that the visual buffer is not busy with some other action
==>
    +visual>
        isa move-attention
        cmd move-attention
        screen-pos =visual-location ; move the location to whatever is stored in the visual-location buffer
    =goal>
        isa read-letters
        step encode-first-letter
)

(P encode-first-letter "Encode informaton of first letter"
    =goal>
        isa read-letters
        step encode-first-letter
    =visual>
        value =letter1
    ?imaginal>
        state free
==>
    =goal>
        isa read-letters
        step move-second-letter ; NEED TO CHANGE
    +imaginal>
        isa array
        letter1 =letter1 ; store the current letter you have encoded
)

(P move-second-letter 
    =goal>
        isa read-letters
        step move-second-letter
    ?visual-location>
        state free ; make sure the state is free!
==>
    +visual-location>
        :attended nil ; Looks to an object that has not been attended in the past, the vision module stores that information. First has been attended, so it will move to the second one
    =goal>
        isa read-letters
        step focus-second-letter
)

(P focus-second-letter "Move my focus to the second letter"
    =goal>
        isa read-letters
        step focus-second-letter
    =visual-location> ; make sure there is a chunk in this buffer, that is some object has been found in the scene
    ?visual>
        state free ; make sure that the visual buffer is not busy with some other action
==>
    +visual>
        isa move-attention
        cmd move-attention
        screen-pos =visual-location ; move the location to whatever is stored in the visual-location buffer
    =goal>
        isa read-letters
        step encode-second-letter
)

(P encode-second-letter "Encode informaton of second letter"
    =goal>
        isa read-letters
        step encode-second-letter
    =visual>
        value =letter2
    ?imaginal>
        state free
    =imaginal>
==>
    =goal>
        isa read-letters
        step move-third-letter ; NEED TO CHANGE
    =imaginal>
        isa array
        letter2 =letter2 ; store the current letter you have encoded
)

(P move-third-letter 
    =goal>
        isa read-letters
        step move-third-letter
    ?visual-location>
        state free ; make sure the state is free!
==>
    +visual-location>
        :attended nil ; Looks to an object that has not been attended in the past, the vision module stores that information. First has been attended, so it will move to the second one
    =goal>
        isa read-letters
        step focus-third-letter
)

(P focus-third-letter "Move my focus to the third letter"
    =goal>
        isa read-letters
        step focus-third-letter
    =visual-location> ; make sure there is a chunk in this buffer, that is some object has been found in the scene
    ?visual>
        state free ; make sure that the visual buffer is not busy with some other action
==>
    +visual>
        isa move-attention
        cmd move-attention
        screen-pos =visual-location ; move the location to whatever is stored in the visual-location buffer
    =goal>
        isa read-letters
        step encode-third-letter
)

(P encode-third-letter "Encode informaton of third letter"
    =goal>
        isa read-letters
        step encode-third-letter
    =visual>
        value =letter3
    ?imaginal>
        state free
    =imaginal>
==>
    =goal>
        isa read-letters
        step respond ; NEED TO CHANGE
    =imaginal>
        isa array
        letter3 =letter3 ; store the current letter you have encoded
)

(P respond-letter-1 "Finally respond with the answer, if letter1 is different"
    =goal>
        isa read-letters
        step respond
    =imaginal>
        isa array
        letter1 =letter1
        letter2 =letter2 
        letter3 =letter2 ; Check if letter2 and letter3 are the same, if yes, then respond with letter1
    ?manual>
        state free
==>
    =goal>
        isa read-letters
        step done
    +manual>
        isa press-key
        cmd press-key
        key =letter1
)

(P respond-letter-2 "Finally respond with the answer, if letter2 is different"
    =goal>
        isa read-letters
        step respond
    =imaginal>
        isa array
        letter1 =letter1 
        letter3 =letter1 ; Check if letter1 and letter3 are the same, if yes, then respond with letter2
        letter2 =letter2
    ?manual>
        state free
==>
    =goal>
        isa read-letters
        step done
    +manual>
        isa press-key
        cmd press-key
        key =letter2
)

(P respond-letter-3 "Finally respond with the answer, if letter3 is different"
    =goal>
        isa read-letters
        step respond
    =imaginal>
        isa array
        letter1 =letter1 
        letter3 =letter3 ; Check if letter1 and letter2 are the same, if yes, then respond with letter3
        letter2 =letter1
    ?manual>
        state free
==>
    =goal>
        isa read-letters
        step done
    +manual>
        isa press-key
        cmd press-key
        key =letter3
)

 
(goal-focus goal)
)
