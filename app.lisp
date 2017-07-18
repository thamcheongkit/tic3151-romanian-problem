(setq locations (list
  'arad
  'bucharest
  'cralova
  'dobreta
  'eforie
  'fagaras
  'glurglu
  'hirsova
  'iasi
  'lugoj
  'mehadia
  'neanut
  'oradea
  'pitesti
  'rimnieu-vilnea
  'sibiu
  'timisoara
  'urziceni
  'vaslui
  'zerind
))

(setq distances (list
  366
  0
  160
  242
  161
  178
  77
  151
  226
  244
  241
  234
  380
  98
  194
  253
  329
  80
  199
  371
))

(setq distancesFromBucharest (make-hash-table))
(dotimes (n (min
              (length locations)
              (length distances)))
   (setf (gethash (nth n locations) distancesFromBucharest) (nth n distances))
)

(setq distanceBetweenCities (make-hash-table))
(setf (gethash 'arad distanceBetweenCities) (list 75 140 118))
(setf (gethash 'bucharest distanceBetweenCities) (list 85 101 90))
(setf (gethash 'cralova distanceBetweenCities) (list 138 146 120))
(setf (gethash 'dobreta distanceBetweenCities) (list 75 120))
(setf (gethash 'eforie distanceBetweenCities) (list 86))
(setf (gethash 'fagaras distanceBetweenCities) (list 99 211))
(setf (gethash 'glurglu distanceBetweenCities) (list 90))
(setf (gethash 'hirsova distanceBetweenCities) (list 96 86))
(setf (gethash 'iasi distanceBetweenCities) (list 87 92))
(setf (gethash 'lugoj distanceBetweenCities) (list 111 70))
(setf (gethash 'mehadia distanceBetweenCities) (list 70 75))
(setf (gethash 'neanut distanceBetweenCities) (list 87))
(setf (gethash 'oradea distanceBetweenCities) (list 71 151))
(setf (gethash 'pitesti distanceBetweenCities) (list 97 101 138))
(setf (gethash 'rimnieu-vilnea distanceBetweenCities) (list 80 146 97))
(setf (gethash 'sibiu distanceBetweenCities) (list 99 80 140))
(setf (gethash 'timisoara distanceBetweenCities) (list 118 70))
(setf (gethash 'urziceni distanceBetweenCities) (list 85 98 142))
(setf (gethash 'vaslui distanceBetweenCities) (list 92 142))
(setf (gethash 'zerind distanceBetweenCities) (list 71 73))

;; prepare cityNearby
(setq cityNearby (make-hash-table))
(setf (gethash 'arad cityNearby) (list 'zerind 'sibiu 'timisoara))
(setf (gethash 'bucharest cityNearby) (list 'urziceni 'pitesti 'glurglu))
(setf (gethash 'cralova cityNearby) (list 'pitesti 'rimnieu-vilnea 'dobreta))
(setf (gethash 'dobreta cityNearby) (list 'mehadia 'cralova))
(setf (gethash 'eforie cityNearby) (list 'hirsova))
(setf (gethash 'fagaras cityNearby) (list 'sibiu 'bucharest))
(setf (gethash 'glurglu cityNearby) (list 'bucharest))
(setf (gethash 'hirsova cityNearby) (list 'urziceni 'eforie))
(setf (gethash 'iasi cityNearby) (list 'neanut 'vaslui))
(setf (gethash 'lugoj cityNearby) (list 'timisoara 'mehadia))
(setf (gethash 'mehadia cityNearby) (list 'lugoj 'dobreta))
(setf (gethash 'neanut cityNearby) (list 'iasi))
(setf (gethash 'oradea cityNearby) (list 'zerind 'sibiu))
(setf (gethash 'pitesti cityNearby) (list 'rimnieu-vilnea 'bucharest 'cralova))
(setf (gethash 'rimnieu-vilnea cityNearby) (list 'sibiu 'cralova 'pitesti))
(setf (gethash 'sibiu cityNearby) (list 'fagaras 'rimnieu-vilnea 'arad))
(setf (gethash 'timisoara cityNearby) (list 'arad 'lugoj))
(setf (gethash 'urziceni cityNearby) (list 'bucharest 'hirsova 'vaslui))
(setf (gethash 'vaslui cityNearby) (list 'iasi 'urziceni))
(setf (gethash 'zerind cityNearby) (list 'oradea 'arad))

;; (defun heuristic_traverse (location)
;;   (setq cityReachable (gethash location cityNearby))
;;   (print (gethash location distanceBetweenCities))
;;   (setq distances
;;     (loop for city in cityReachable do
;;       collect (gethash city distancesFromBucharest)
;;     )
;;   )
;;   (setq minDistance (apply 'min distances))
;;   (nth (position minDistance distances) cityReachable)
;; )

(defun aStarTraverse (location)
  (setq cityReachable (gethash location cityNearby))
  (setq f
    (loop
        for city in cityReachable
        for distance in (gethash location distanceBetweenCities) do ()
      collect (+ (gethash city distancesFromBucharest) distance)
    )
  )
  (setq minDistance (apply 'min f))
  (list (nth (position minDistance f) cityReachable) minDistance)
)

(defun pathToBucharest ()
  (setq currentCity 'arad)
  (write (list 'arad (gethash 'arad distancesFromBucharest)))
  (write-line " ->")
  (loop
    (setq resultOfTraverse (aStarTraverse currentCity))
    (setq currentCity (nth 0 resultOfTraverse))
    (write resultOfTraverse)
    (when (equal currentCity 'bucharest) (return NIL))
    (write-line " ->")
  )
)

(pathToBucharest)