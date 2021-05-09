(ns sn.im-not-worried.song
  (:require [overtone.live :refer :all :exclude [stop sharp flat]]
            [leipzig.canon :refer [canon interval]]
            [leipzig.melody :refer :all]
            [leipzig.chord :refer :all]
            [leipzig.scale :refer [high low raise lower sharp flat G mixolydian from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [overtone.inst.drum :as drums]
            [sn.im-not-worried.instruments :as inst]))

(def part (partial all :part))

(defn vary [f notes]
  (->> notes
       (then (f notes))))

(defn reps [& repetitions]
  (mapcat (partial apply repeat) repetitions))

(defn silence [duration]
  (phrase [duration] [nil]))

(def not-worried
  (let [
        bass (phrase (cycle [1/2 1/2 1/2 1/4 1/4 1/2 1/2 1/2 1/2]) 
                     [4 4 4 4 4 4 3 4 5 6 6 6 6 6 6 5 6 5 3 3 3 3 3 3 2 3 2 0 0 0 0 0 0 0 4 0])
        beat (->> (rhythm (cycle [1 1 1 3/4 1/4]))
                  (having :part (cycle [:kick :kick :kick :kick :kick]))
                  (take 20)
                  (with (->> (rhythm (concat (repeat 7 2) [1])) (after 1) (all :part :snare))))
        basic (->>
                (phrase (cycle [1 1.5 1.0 0.5])
                        [6.5 4 3 4  6 4 3 4  5 3 6 5  4 2 0 3])
                (part :riff))
        harmony (->> (phrase [12 3 1] [8 7 10]) (part :harmony))
        harmony2 (->> (phrase (repeat 16 1) (cycle [0 1 -3 0])) (part :harmony))
        alt-bass (phrase (repeat 1/2) (repeat 32 0))
        alt-chords (->> (phrase (repeat 4) [triad
                                            (-> triad (root 1) (update :iii (partial + 1/2)))
                                            (-> triad (root 1))
                                            triad])
                        (part :riff))
        alt-line (->> (phrase (repeat 4) [4 3.5 3 2])
                      (part :harmony))]
    (->>
      bass
      ;(all :pitch 0)
      (where :pitch (comp lower lower lower))
      ;(with basic #_harmony #_harmony2)
      ;(with alt-chords alt-line)
      (where :pitch (comp G mixolydian))
      (with beat)
      (tempo (bpm 130)))))

(comment
  (volume 0.9)
  (do (stop) (-> not-worried var live/jam))
  (def not-worried nil))

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

(defmethod live/play-note :riff
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (bop seconds :vol 0.6 :pan -0.2 :boost 10)))

(defmethod live/play-note :harmony
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (sing seconds :vol 0.2 :pan 0.5 :boost 1)))

(defmethod live/play-note :kick
  [{midi :pitch seconds :duration}]
  (drums/kick2))

(defmethod live/play-note :snare
  [{midi :pitch seconds :duration}]
  (drums/hat3 :hi 8000))
