(ns sn.in-the-mood.song
  (:use leipzig.scale, leipzig.melody, leipzig.chord, leipzig.live sn.in-the-mood.instruments))

; Gracias a Wingy Manone, Andy Razaf y Joe Garland.

(def in-the-mood
  (->> []))

(comment
  (do (stop) (-> in-the-mood var jam))
  (def in-the-mood nil)
  )
