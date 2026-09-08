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
  swept onto the plane along the view direction.

  The camera follows the battle: it sits over the point between the fleets
  and zooms to hold them both. The vantage never rotates, only slides and
  zooms, so the ocean can be a fixed window of particles that scrolls under
  it - see voxel.ocean/recenter!."
  )

(def WIDTH 960)
(def HEIGHT 540)

;; A high isometric RTS vantage - 45 degrees round, ~35 up - so both fleets
;; and the water between them read like a battle map.
(def OFFSET [-60.0 62.0 -60.0])  ; where the camera sits, relative to its target
(def POS OFFSET)     ; the vantage when it is looking at the origin
(def TARGET [0.0 0.0 0.0])
(def ORTHO 1)        ; raylib projection enum: 1 = CAMERA_ORTHOGRAPHIC

;; Orthographic zoom, in vertical world units. The ocean sheet is built to
;; cover MAX-FOVY, so that is also as far out as the camera may go - past it
;; the water would end inside the frame.
(def MIN-FOVY 48.0)
(def MAX-FOVY 72.0)
(def FOVY MAX-FOVY)  ; the default, and what the sheet is sized for
;; how much clear water to leave around the fleets when framing them
(def FRAME-MARGIN 1.45)

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

(defn view-reach
  "Half-width of the smallest square CENTRED ON THE VIEW that covers
  everything the frame shows of the sea plane.

  A screen offset of `a` along the camera's right vector and `b` along its
  up vector lands on the plane at `a*right + b*U`, where U is the up vector
  swept along the view direction until its height cancels:
  U = up - (up_y / fwd_y) * fwd. Right already lies in the plane. The
  extreme corner is then |a|*|right| + |b|*|U| per axis."
  ([fovy] (view-reach fovy (/ (double WIDTH) HEIGHT) POS TARGET))
  ([fovy aspect pos target]
   (let [[f r u] (basis pos target)
         uu (mapv - u (mapv #(* (/ (u 1) (f 1)) %) f))
         hh (* 0.5 fovy)
         hw (* 0.5 fovy aspect)
         reach (fn [axis]
                 (+ (* hw (Math/abs (double (r axis))))
                    (* hh (Math/abs (double (uu axis))))))]
     (max (reach 0) (reach 2)))))

(defn frame
  "Where the camera should be this frame, given the world points it has to
  hold (the live ships): {:target :pos :fovy}.

  It slides over the midpoint of those points and zooms to the tightest
  FOVY that still contains them all with FRAME-MARGIN of clear water, within
  the zoom limits. Beyond MAX-FOVY it stops zooming out and simply centres -
  during the long approach the fleets are further apart than any affordable
  sheet of water."
  ([points] (frame points (/ (double WIDTH) HEIGHT)))
  ([points aspect]
   (let [pts (seq points)
         c (if pts
             (let [n (double (count pts))]
               [(/ (reduce + (map #(nth % 0) pts)) n)
                0.0
                (/ (reduce + (map #(nth % 2) pts)) n)])
             TARGET)
         ;; how far the furthest point reaches, measured the same way
         ;; view-reach measures the frame, so the two are comparable
         [f r u] (basis (mapv + c OFFSET) c)
         uu (mapv - u (mapv #(* (/ (u 1) (f 1)) %) f))
         ;; solve p - c = a*right + b*U on the plane
         det (- (* (r 0) (uu 2)) (* (r 2) (uu 0)))
         need (reduce
               (fn [m p]
                 (let [dx (- (nth p 0) (c 0))
                       dz (- (nth p 2) (c 2))
                       a (/ (- (* dx (uu 2)) (* dz (uu 0))) det)
                       b (/ (- (* (r 0) dz) (* (r 2) dx)) det)]
                   ;; a is in right-units (half-width fovy*aspect/2), b in
                   ;; up-units (half-height fovy/2)
                   (max m (* 2.0 (Math/abs b))
                        (/ (* 2.0 (Math/abs a)) aspect))))
               0.0 (or pts []))
         fovy (max MIN-FOVY (min MAX-FOVY (* FRAME-MARGIN need)))]
     {:target c :pos (mapv + c OFFSET) :fovy fovy})))

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
  "The extent voxel.world builds the ocean to: everything the widest frame
  can show around the camera's own centre, plus SEA-MARGIN, rounded up to a
  whole unit. The sheet scrolls with the camera, so it only ever has to
  cover the view, not the whole arena."
  []
  (Math/ceil (* SEA-MARGIN (view-reach MAX-FOVY))))
