(ns sn.tuesday.song
  (:require [overtone.live :refer :all :exclude [stop sharp flat]]
            [leipzig.canon :refer [canon interval]]
            [leipzig.melody :refer :all]
            [leipzig.chord :refer :all]
            [leipzig.scale :refer [high low raise lower sharp flat A B C mixolydian from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [overtone.inst.drum :as drums]))

(def part (partial all :part))

(defn vary [f notes]
  (->> notes
       (then (f notes))))

(defn reps [& repetitions]
  (mapcat (partial apply repeat) repetitions))

(defn silence [duration]
  (phrase [duration] [nil]))

(def tuesday
  (let [bass (->> (phrase [
                           5/3 3/3 1/3 1 1
                           5/3 3/3 1/3 1 1
                           5/3 3/3 1/3 1 1
                           1 1       1 1 1]
                          [ 0 4 4 4 2
                           -1 4 4 4 2
                           -2 4 4 4 2
                           -4 -2.5 0 3 0.5])
                  (where :pitch (comp lower lower lower)))
        chords [triad
                (-> triad (update :i dec))
                (-> triad (update :i (comp dec dec)))
                (-> triad (root -4) (update :iii (partial - 0.5)))
                ]
        beat (->>
               (rhythm          [ 3/3   2/3  4/3    3/3   3/3])
                  (having :part [:kick :hat :snare :kick :snare])
                  (then
                    (->>
               (rhythm          [ 3/3   2/3  3/3    1/3   3/3   2/3    1/3])
                  (having :part [:kick :hat :snare :kick :kick :snare :hat])))
                  (with (->> (rhythm (repeat 10 1)) (all :part :click)))
                  (times 2))
        ]
    (->>
      bass
      (with (->> (phrase (repeat 5) chords) (all :part :chords)))
      (with (->> (phrase [15 2 1 2] [4 4.5 4 3]) (all :part :chords)))
      (where :pitch (comp A mixolydian))
      (with beat)
      (tempo (bpm 110)))))

(comment
  (volume 0.9)
  (do (stop) (-> tuesday var live/jam))
  (def tuesday nil))

; Instrumentation
(defonce x
  (defsynth walker [out-bus 0 freq 0.5]
    (out:kr out-bus (lf-noise1:kr freq))))
(defonce random-walk (audio-bus))
(defonce walk (walker random-walk :freq (* 1/7 0.75)))
(def resonance (mul-add (in:kr random-walk) 1200 3000))

(definst blob [freq 110 dur 1.0 boost 5 vol 0.25 pan 0.0]
  (let [inst (-> (* 3 (saw freq))
                 (+ (* 2 (square freq)))
                 (+ (* 1/2 (sin-osc (* 3 freq))))
                 (+ (* 1 (brown-noise)))
                 (* boost)
                 (clip2 0.4)
                 (* 8)
                 (rlpf (line:kr resonance 500 0.3) 0.4)
                 (pan2 pan)
                 (* (env-gen (adsr 0.01 0.2 0.3 0.1)
                             (line:kr 1 0 dur) :action FREE))
                 (* vol))
        delayed (delay-l inst 0.001)
        reverbed (free-verb delayed :damp 0.8 :mix 0.5 :room 0.9)
        dryverbed (free-verb inst :damp 0.3 :mix 0.1 :room 0.2)]
    (mix reverbed dryverbed)))

(definst sing [freq 110 dur 1.0 boost 5 vol 0.25 pan 0.0]
  (let [inst (-> (* 2 (saw freq))
                 (rlpf (line:kr 700 3000 4) 0.1)
                 (pan2 pan)
                 (* (env-gen (adsr 0.01 0.2 0.5 0.1)
                             (line:kr 1 0 dur) :action FREE))
                 (* vol))
        delayed (delay-l inst 0.001)
        reverbed (free-verb delayed :damp 0.4 :mix 0.1 :room 0.2)
        dryverbed (free-verb inst :damp 0.2 :mix 0.1 :room 0.2)]
    (mix reverbed dryverbed)))

(definst bop [freq 110 dur 1.0 vol 0.25 pan 0.0]
  (let [inst (-> (square freq)
                 (rlpf (line:kr 1000 50 dur) 0.4)
                 (* (sin-osc 2 1.4))
                 (pan2 pan)
                 (* (env-gen (perc 0.1 5) (line:kr 1 0 dur) :action FREE))
                 (* vol))
        delayed (delay-l inst 0.01)
        reverbed (free-verb delayed :damp 0.4 :mix 0.1 :room 0.2)
        dryverbed (free-verb inst :damp 0.2 :mix 0.1 :room 0.2)]
    (mix reverbed dryverbed)))

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (blob seconds :boost 5))
  (-> midi midi->hz (sing seconds :boost 1 :vol 0.3)))

(defmethod live/play-note :chords
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (bop seconds :vol 0.6 :pan -0.2 :boost 10)))

(defmethod live/play-note :harmony
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (sing seconds :vol 0.2 :pan 0.5 :boost 1)))

(defmethod live/play-note :kick
  [{midi :pitch seconds :duration}]
  (drums/kick2))

(defmethod live/play-note :hat
  [{midi :pitch seconds :duration}]
  (drums/hat3 :hi 9000 :t 0.01))

(defmethod live/play-note :snare
  [{midi :pitch seconds :duration}]
  (drums/hat3 :hi 6000 :low 5000 :t 0.2 :amp 0.2))

(defmethod live/play-note :click
  [{midi :pitch seconds :duration}]
  (drums/haziti-clap 880 :decay 0.7 :amp 0.3))
