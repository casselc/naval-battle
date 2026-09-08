(ns voxel.camera
  "The battle camera, and what it can see of the sea.

  Pure geometry, no raylib: the renderer takes its constants from here and
  the ocean sizes itself from `sea-half-extent`, so the water always runs
  past the edge of the frame instead of ending in a visible diamond. Change
  the vantage or the window and the sea follows - voxel.world derives its
  extent from these numbers rather than carrying a constant that has to be
  remembered separately.

  The projection is orthographic, so FOVY is a height in world units, not an
  angle, and the visible region of the sea plane is a parallelogram: the
  camera's right vector already lies in the plane, and its up vector is
  swept onto the plane along the view direction."
  )

(def WIDTH 960)
(def HEIGHT 540)

;; A high isometric RTS vantage - 45 degrees round, ~35 up - so both fleets
;; and the water between them read like a battle map.
(def POS [-60.0 62.0 -60.0])
(def TARGET [0.0 0.0 0.0])
(def FOVY 70.0)      ; orthographic: vertical world units in view
(def ORTHO 1)        ; raylib projection enum: 1 = CAMERA_ORTHOGRAPHIC

;; The sea is built this much larger than the frame needs. A little slack
;; covers window resizes and the swell riding above y = 0 at the horizon.
(def SEA-MARGIN 1.08)

(defn- v- [a b] (mapv - a b))
(defn- dot [a b] (reduce + (map * a b)))
(defn- norm [v] (let [l (Math/sqrt (dot v v))] (mapv #(/ % l) v)))

(defn- cross
  [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn basis
  "[forward right up] of the camera, right-handed with world up +y."
  ([] (basis POS TARGET))
  ([pos target]
   (let [f (norm (v- target pos))
         r (norm (cross f [0.0 1.0 0.0]))]
     [f r (cross r f)])))

(defn sea-half-extent
  "Half-width of the smallest origin-centred square on the sea plane (y = 0)
  that covers everything the frame shows.

  A screen offset of `a` along the camera's right vector and `b` along its
  up vector lands on the plane at `a*right + b*U`, where U is the up vector
  swept along the view direction until its height cancels:
  U = up - (up_y / fwd_y) * fwd. Right already lies in the plane. The
  extreme corner is then |a|*|right| + |b|*|U| per axis."
  ([] (sea-half-extent FOVY (/ (double WIDTH) HEIGHT) POS TARGET))
  ([fovy aspect pos target]
   (let [[f r u] (basis pos target)
         ;; sweep the up vector onto the plane
         uu (mapv - u (mapv #(* (/ (u 1) (f 1)) %) f))
         ;; where the central ray meets the plane
         c (mapv + pos (mapv #(* (- (/ (pos 1) (f 1))) %) f))
         hh (* 0.5 fovy)
         hw (* 0.5 fovy aspect)
         reach (fn [axis]
                 (+ (Math/abs (double (c axis)))
                    (* hw (Math/abs (double (r axis))))
                    (* hh (Math/abs (double (uu axis))))))]
     (max (reach 0) (reach 2)))))

(defn sea-extent
  "The extent voxel.world builds the ocean to: the visible footprint plus
  SEA-MARGIN, rounded up to a whole unit."
  []
  (Math/ceil (* SEA-MARGIN (sea-half-extent))))
