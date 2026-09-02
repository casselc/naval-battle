(ns voxel.box3d-test
  "Headless coverage of the voxel_b3 shim entries the ocean-ship coupling
  needs: apply-force (buoyancy patches, blasts), damping (ships vs rubble),
  sleep control (ships must stay responsive), gravity scale (hulls). Bodies in
  one world are spawned apart — overlapping boxes generate contact forces that
  poison every assertion."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.box3d :as b3]))

(defn- box-body
  "1x1x1 dynamic box of density 1 (mass 1) at [x 0 0], awake."
  [world x]
  (let [b (b3/create-body world b3/DYNAMIC-BODY (double x) 0.0 0.0 0.0 0.0 0.0 1.0 1)]
    (b3/add-box! b 0.0 0.0 0.0 0.5 0.5 0.5 1.0 0.3 0.0)
    b))

(defn- quat-spread
  "How far a quaternion has moved off identity (max abs component delta)."
  [_ [qx qy qz qw]]
  (apply max (map #(Math/abs %) [qx qy qz (- qw 1.0)])))

(deftest apply-force-accelerates-along-the-force
  (let [w (b3/create-world 0.0 0.0 0.0 0)
        b (box-body w 0.0)]
    (b3/apply-force! b 1.0 0.0 0.0 0.0 0.0 0.0 true)
    (b3/step! w 0.1 4)
    (let [[vx vy vz] (b3/velocity b)]
      (is (pos? vx) "force along +x pushes the body along +x")
      (is (< (Math/abs vy) 1e-4) "no lateral drift")
      (is (< (Math/abs vz) 1e-4) "no lateral drift"))
    (b3/destroy-world! w)))

(deftest off-center-force-rotates
  "Same force at an offset point torques the body: the quaternion leaves
  identity while the centred one stays put."
  (let [w (b3/create-world 0.0 0.0 0.0 0)
        centred (box-body w 0.0)
        offset (box-body w 5.0)]
    (b3/apply-force! centred 1.0 0.0 0.0 0.0 0.0 0.0 true)
    (b3/apply-force! offset 1.0 0.0 0.0 5.0 0.0 1.0 true)
    (b3/step! w 0.2 4)
    (let [s1 (quat-spread nil (second (b3/transform centred)))
          s2 (quat-spread nil (second (b3/transform offset)))]
      (is (< s1 1e-3) "centred force does not rotate (integrator noise only)")
      (is (> s2 0.01) "force at an offset point rotates the body")
      (is (> s2 (* 10.0 s1)) "and far more than the noise floor"))
    (b3/destroy-world! w)))

(deftest damping-bleeds-velocity
  (let [w (b3/create-world 0.0 0.0 0.0 0)
        free (box-body w 0.0)
        damped (box-body w 5.0)]
    ;; body creation bakes rubble damping; zero it so "free" really is free
    (b3/set-damping! free 0.0 0.0)
    (b3/set-damping! damped 5.0 5.0)
    (b3/set-velocity! free 2.0 0.0 0.0)
    (b3/set-velocity! damped 2.0 0.0 0.0)
    (b3/step! w 1.0 16)
    (let [vf (first (b3/velocity free))
          vd (first (b3/velocity damped))]
      (is (> vf 1.9) "undamped body keeps coasting")
      (is (< vd (* 0.1 vf)) "damped body nearly stops"))
    (b3/destroy-world! w)))

(deftest sleep-control-gates-settling
  (let [w (b3/create-world 0.0 0.0 0.0 0)
        sleeper (box-body w 0.0)
        restless (box-body w 5.0)]
    ;; slow drift below a raised threshold settles; sleep disabled never does
    (b3/set-sleep-threshold! sleeper 10.0)
    (b3/set-velocity! sleeper 1.0 0.0 0.0)
    (b3/set-velocity! restless 1.0 0.0 0.0)
    (b3/enable-sleep! restless false)
    (b3/step! w 2.0 16)
    (is (not (b3/awake? sleeper)) "slow drift under a high threshold sleeps")
    (is (b3/awake? restless) "sleep-disabled body stays awake")
    (b3/destroy-world! w)))

(deftest gravity-scale-zero-floats
  (let [w (b3/create-world 0.0 -10.0 0.0 0)
        floater (box-body w 0.0)
        faller (box-body w 5.0)]
    (b3/set-gravity-scale! floater 0.0)
    (b3/step! w 0.5 8)
    (let [yf (second (b3/velocity floater))
          yd (second (b3/velocity faller))]
      (is (< (Math/abs yf) 1e-3) "gravity-scaled body floats in place")
      (is (< yd -2.0) "unscaled body falls")
      (is (not (b3/awake? floater)) "a motionless floater settles to sleep")
      ;; the wake flag is the ship-coupling contract: buoyancy forces must
      ;; rouse a settled hull every frame
      (b3/apply-force! floater 1.0 0.0 0.0 0.0 0.0 0.0 true)
      (is (b3/awake? floater) "apply-force! with wake=true wakes it again"))
    (b3/destroy-world! w)))
