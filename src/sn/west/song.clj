(ns sn.west.song
  (:require
    [sn.melody :refer :all]
    [leipzig.melody :refer :all]
    [leipzig.live :refer :all]
    [leipzig.temperament :refer :all]
    [leipzig.scale :refer :all]
    [leipzig.chord :refer :all]
    [leipzig.canon :refer :all]
    [sn.instruments :refer :all]))

(def progression
  (->> [0 -3 -2 -5]
       (map (partial root seventh))))

; Accompaniment
(def backing
  (let [lefts [true false true false]
        render-chord (fn [[left? chord]] (->> (phrase [4] [chord]) (all :left? left?)))]
    (->>
      progression
      (map vector lefts)
      (mapthen render-chord)
      (part ::accompaniment))))

(def theme
  (let [ill-run-away (->>
                       (after -1/2
                              (phrase
                                [1/2 1/4 1/4 1/2 3]
                                [  3   4   3   4 nil]))
                       (vary (partial but 1/4 1/2 (phrase [1/4] [6]))))
        my-heart-will-go-west-with-the-sun (->> (phrase
                                                  [1/2 3/4 3/4 2/4 3/4 3/4 1/4 17/4]
                                                  [  3   4   3   2   4   3   2   -1])
                                                (after -1/2))]
    (->>
      ill-run-away
      (then my-heart-will-go-west-with-the-sun)
      (part ::lead))))

(def gym
  "Gymnopédie One"
  (->>
    (phrase (cycle [3/2 3/2 2/2]) [nil 4 6 5 4 1 0 1 2])
    (then (phrase (repeat 4) [-1 0 4 5 4]))
    (where :pitch lower)
    (part ::epilogue)))

; Response
(def a-parting-kiss
  (phrase
    [1/4 1/4 1/4 3/4 10/4]
    [  4   3   4   6    4]))

(def like-fairy-floss
  (with (after -1/4 (phrase [1/4] [3])) a-parting-kiss))

(def dissolves-on-the-tip-of-my-tongue
  (->>
    (phrase
      [1/4 3/4 13/4]
      [  4   6    4])
    (after -1/4)))

(def reply
 (->>
   a-parting-kiss
   (then like-fairy-floss)
   (then (times 2 dissolves-on-the-tip-of-my-tongue))
   (part ::response)))

; Break
(def consider-this
  (after -3/2
     (phrase
       [1/2 1/2 1/2 8/2]
       [  4   9   8   7])))

(def consider-that
  (->> consider-this
       (but 0 1/2 (phrase [8/2] [6]))))

(def consider-everything
  (->> consider-this
       (but 0 8
            (phrase
              [2/2 1/2 2/2 2/2 9/2]
              [  7   8   7   6   4]))))

(def break
 (->>
   consider-this
   (then consider-that)
   (then consider-everything)
   (part ::break)))

; Bass
(def light-bass
  (->> progression
       (map :i)
       (phrase (repeat 4))
       (where :pitch lower)
       (part ::bass)))

(def bassline
  (->> light-bass
       (canon
         (comp (simple 1)
               (interval 6)
               (partial where :duration dec)
               (partial all :left? true)))))

(def back-beat
  (->> (phrase (repeat 4 1) (cycle [nil 0]))
       (times 4)
       (all :drum :pop)
       (part :beat)))

(def beat
  (->> (phrase [6/4 4/4 6/4] (repeat -14))
       (times 4)
       (with (times 2 (phrase [1 2 2 2 1/2 1/2] [nil -10 -10 -10 -10 -13])))
       (all :drum :kick)
       (part :beat)))

(def flat-beat
  (->> (phrase (repeat 4 1) (repeat -14))
       (times 4)
       (part :beat)
       (all :drum :kick)))

(def beat2
  (->> (phrase [1 1 1/4 3/4 1 1/4 1/4 1/2 1/2 1/4 1/4 1 1/2] (cycle [-7 -3]))
       (with (after 4 (phrase [3/2 3/2 1] [-8 -10 -12])))
       (times 2)
       (with beat)
       (part :beat)
       (all :drum :kick)))

(def west-with-the-sun
  "I'll run away.
  I'll get away.
  But my heart will go west with the sun."
  (let [roots [0 -3 -2 -5]]
    (->>
      (phrase (repeat 4) roots)
      ;(where :pitch lower)
      #_(canon (fn [notes]
                 (->> notes
                      (where :pitch (partial + 6))
                      (all :left? true)
                      (where :duration dec)
                      (where :time inc))))
      (part ::bass)
      (with
        ;backing
        ;back-beat
        ;beat
        ;theme
        ;reply beat2
        ;break flat-beat
        )
      ;(times 2) (with gym)
      (where :pitch (comp equal A minor))
      (tempo (bpm 80)))))

(comment
  (do (stop) (->> west-with-the-sun var jam))
)

; Arrangement
(defmethod play-note ::bass
  [{freq :pitch left? :left?}]
  (let [[position low] (if left? [-1/3 0.3] [1/5 2])]
    (-> freq (groan :volume 0.5 :position position :wet 0.1 :low low :limit 3000))))

(defmethod play-note ::accompaniment
  [{freq :pitch left? :left?}]
  (-> freq (shudder :volume 1 :pan (if left? 1/2 -1/2) :wet 0.8 :limit 6000)))

(defmethod play-note ::lead
  [{freq :pitch}]
  (-> freq (sawish :pan -1/6 :vibrato 8/3 :wet 0.6 :volume 0.7)))

(defmethod play-note ::response
  [{freq :pitch seconds :duration}]
  (-> freq (organ seconds :vol 0.5 :pan -1/4 :wet 0.8))
  (-> freq (sing seconds :volume 2.0 :pan 1/4 :wet 0.9)))

(defmethod play-note ::epilogue
  [{freq :pitch seconds :duration}]
  (-> freq (corgan seconds :vol 0.8 :pan 1/2 :wet 0.5 :vibrato 80/60 :room 0.9)))

(defmethod play-note ::break
  [{freq :pitch}]
  (-> freq (/ 2) (bell :duration 7 :vol 0.5 :position -1/5 :wet 0.6))
  (-> freq (bell :duration 7 :vol 1.5 :position -1/6 :wet 0.6)))

(def percussion
  {:kick (fn [freq] (kick2 freq :amp 0.8 :sustain 1.2 :pan -0.3))
   :pop (fn [freq] (tip freq :volume 0.8 :wet 0.6 :room 0.8 :pan 0.3))})

(defmethod play-note :beat
  [{freq :pitch drum :drum}]
  ((drum percussion) freq))
