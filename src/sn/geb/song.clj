(ns sn.geb.song
  (:require [overtone.live :refer :all :exclude [stop sharp flat]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon interval]]
            [leipzig.scale :refer [high low raise lower sharp flat E B minor major from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [overtone.inst.drum :as drums]
            [sn.geb.coding :as coding]))

(def bar-lengths [7/2 7/2 14/2])
(def alt-bar-lengths [4 4 8])
(def progression [-2 -1 0 0])

(def alt
  (let [alt-bass (->> (phrase (repeat 4 4) (cycle [3 0]))
                      (canon (interval -7))
                      (where :pitch (comp lower lower)))
        rising (->> (phrase alt-bar-lengths progression)
                    (canon (interval -7))
                    (where :pitch (comp lower lower)))
        twiddle (phrase (repeat 64 1/4) (cycle [4 2 5 4 5 4 7 7]))
        bomp (phrase (repeat 32 1/2) (cycle [4 2 2 0 -1]))
        decoration (phrase (repeat 64 1/4) (cycle [7 8 9 11 7 6]))
        together (with alt-bass #_rising twiddle bomp)]
    (->> together
         (then (with together decoration)))))

(def ascii (partial where :pitch coding/ascii->midi))
(defn person [notes]
  (->> notes
       (where :pitch coding/midi->ascii)
       (all :part :sample)))

(def geb
  (let [bass (->> (phrase bar-lengths progression)
                  (canon (interval -7))
                  (where :pitch (comp lower lower)))
        riff (->> progression
                  (mapthen #(->> (phrase (repeat 7 1/2)
                                         (interleave [[0 2] [0 2] [0 3] [0 2]] (repeat -3)))
                                 (where :pitch (from %)))))
        whirl (->> (phrase [0.5 2 1] [9 7 4])
                   (where :pitch raise)
                   (times 4)
                   (with (->> (phrase (repeat 28 1/4) (cycle [2 3]))
                              (then (phrase (repeat 7 1) [2 2 2 2 2 1 1])))))
        hit (->> (phrase [2 0.5 0.5 0.5] [14 [7 11] [7 12] [7 11]])
                 (times 4))
        beat (->> (phrase [1 1 0.5 0.5 0.5] (repeat -21))
                  (having :part [:kick :snare :clap :kick :snare])
                  (times 4))
        single-canon (partial canon ascii)
        double-canon (partial canon #(->> % ascii (canon person)))
        steady (->> (phrase (repeat 28 1/2) (repeat 0))
                    (all :part :click))
        flat (->> (phrase (repeat 14 1) (repeat -21))
                  (having :part (repeat :kick)))]
    (->> []
         ;(with bass #_riff)
         ;(with whirl #_hit)
         ;(with #_beat steady #_flat)
         ;(times 2)
         ;(with alt)
         (where :pitch (comp B minor))
         (with
           (->> (map coding/char->ascii "GEB") ; Play ascii codes as though
                (phrase [7/2 7/2 14/2])        ; they are midi pitches
                ))
         (tempo (bpm 100)))))

(comment
  (volume 0.9)
  (do (stop) (-> geb var live/jam))
  (def geb nil))

; Instrumentation
(defonce x
  (defsynth walker [out-bus 0 freq 0.5]
    (out:kr out-bus (lf-noise1:kr freq))))
(defonce random-walk (audio-bus))
(defonce walk (walker random-walk :freq (* 1/7 0.75)))
(def resonance (mul-add (in:kr random-walk) 1500 3000))

(definst overchauffeur [freq 110 dur 1.0 top 1500 vol 0.25]
  (let [inst (-> (sin-osc freq)
                 (+ (* 1/8 (sin-osc (* 2.992 freq))))
                 (+ (* 1/7 (sin-osc (* 3.003 freq))))
                 (+ (* 1/5 (sin-osc (* 4.01 freq))))
                 (+ (* 1/2 (sin-osc (* 0.5 freq))))
                 (* 8)
                 (clip2 0.9)
                 ;(* (square 1.5))
                 (rlpf resonance 0.2)
                 ;(* 0.4) (+ (square freq))
                 (* (env-gen (adsr 0.03 0.2 0.5 0.1)
                             (line:kr 1 0 dur) :action FREE))
                 (* vol))
        delayed (delay-l inst 0.001)
        reverbed (free-verb delayed :damp 0.1 :mix 0.1 :room 0.2)
        dryverbed (free-verb inst :damp 0.3 :mix 0.1 :room 0.2)]
    (+ (-> dryverbed (pan2 -1 0.5)) (pan2 reverbed 1 0.5))))

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (overchauffeur seconds :vol 0.4)))

#_(defmethod live/play-note :default
    [{midi :pitch seconds :duration}]
    (-> midi
        (+ 0.00001) ; Avoid a bug where a C is played as a B due to rounding.
        (piano :sustain 0 :release 0 :decay 0)))

(defmethod live/play-note :kick
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (drums/kick2 :amp 2 :decay 0.1 :noise 0.01)))

(defmethod live/play-note :snare
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (drums/snare :amp 1 :crackle-amp 200)))

(defmethod live/play-note :clap
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (drums/haziti-clap :amp 3.0)))

(defmethod live/play-note :click
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (drums/haziti-clap :amp 0.8))
  (drums/open-hat :amp 0.7 :t 0.03))

(definst godel []
  (let [pan -1.0
        buffer (load-sample "samples/goedel.aiff")]
    (-> (play-buf 1 buffer :action FREE :rate 0.5)
        (pan2 pan)
        (* 5))))

(definst escher []
  (let [pan 1.0
        buffer (load-sample "samples/escher.aiff")]
    (-> (play-buf 1 buffer :action FREE :rate 0.5)
        (pan2 pan)
        (* 5))))

(definst bach []
  (let [pan 0.0
        buffer (load-sample "samples/bach.aiff")]
    (-> (play-buf 1 buffer :action FREE :rate 0.5)
        (pan2 pan)
        (* 5))))

(defn book [initial]
  (({coding/G godel
     coding/E escher
     coding/B bach
     coding/g godel
     coding/e escher
     coding/b bach}
    initial
    (constantly nil))
   :out-bus (rand-int 2)))

(defmethod live/play-note :sample
  [{initial :pitch}]
  (book initial))
