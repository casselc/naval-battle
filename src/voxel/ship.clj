(ns voxel.ship
  "Deterministic voxel warship layouts as pure data. A dreadnought is a
  tapered solid hull (watertight by construction - the skin is just the
  surface of the cell set), a raised superstructure amidships, and turret
  blocks mounted on deck. The world layer poses these maps with
  voxel.buoyancy/body-point->world and hands the cells to voxel.physics,
  which floats them; damage dissoc's cells and the hull floods through the
  exposed faces, so the layout here decides how the ship sails, fights and
  sinks.

  Coordinates: [i j k] with k along the keel (bow at high k), i across the
  beam, j up. The anchor is the midships keel point - on the centreline, so
  that propulsion through the centre of mass does not also have to correct
  for a lopsided grid.

  The hull is described in WORLD units and voxelised at RES. Everything that
  cares about how big a ship is reads the -U constants; only the mesh, the
  damage grid and the floatation solve care about cells. Raising RES gives a
  finer ship of exactly the same size and shape - which is affordable
  because the divergence-theorem solve is linear in surface triangles and
  runs in C (voxel.hullc), so a hull can be made of as many voxels as it
  looks like it should be."
  )

;; voxels per world unit. The whole point of the volume algorithm is that
;; this can be raised without the floatation cost mattering.
(def RES 2)
(def VOXEL (/ 1.0 (double RES)))

;; the ship, in world units
(def LENGTH-U 26.0)
(def HALF-BEAM-U 3.25)
(def DEPTH-U 6.0)
(def BEAM-U (* 2.0 HALF-BEAM-U))

(defn- v
  "World units to a voxel count."
  [u]
  (int (Math/round (* RES (double u)))))

;; ...and in voxels
(def LENGTH (v LENGTH-U))
(def HALF-BEAM (v HALF-BEAM-U))
(def BEAM (inc (* 2 HALF-BEAM)))   ; odd, so there is a true centreline
(def DEPTH (v DEPTH-U))

(defn- half-width
  "Beam half-width in voxels at keel station k: full midships, transom
  stern, raked bow. The profile is in fractions of the length, so the same
  hull comes out at any resolution."
  [k]
  (let [f (/ (double k) (double (dec LENGTH)))]
    (int (Math/round (* HALF-BEAM
                        (cond (< f 0.06) 0.67
                              (< f 0.86) 1.0
                              (< f 0.94) 0.67
                              (< f 0.98) 0.33
                              :else 0.0))))))

(defn- hull-cells
  "The solid tapered hull: DEPTH layers, top layer tagged :deck."
  []
  (into {}
        (for [k (range LENGTH)
              :let [hw (half-width k)]
              i (range (- HALF-BEAM hw) (inc (+ HALF-BEAM hw)))
              j (range DEPTH)]
          [[i j k] (if (= j (dec DEPTH)) :deck :hull)])))

(defn- block
  "A solid rectangular block from world-unit bounds: x across the beam with
  0 on the centreline, y up from the keel, z along the keel from the stern."
  [x0 x1 y0 y1 z0 z1 mat]
  (into {}
        (for [i (range (+ HALF-BEAM (v x0)) (inc (+ HALF-BEAM (v x1))))
              j (range (v y0) (inc (v y1)))
              k (range (v z0) (inc (v z1)))]
          [[i j k] mat])))

(defn dreadnought
  "The standard warship: hull + bridge + funnel + fore and aft turrets."
  []
  (let [deck DEPTH-U
        cells (merge (hull-cells)
                     (block -1.5 1.5 deck (+ deck 1.5) 9.0 14.0 :super)
                     (block -0.5 0.5 deck (+ deck 2.5) 15.0 16.5 :super)
                     (block -1.5 1.5 deck (+ deck 1.0) 20.5 22.0 :gun)
                     (block -1.5 1.5 deck (+ deck 1.0) 4.0 5.5 :gun))]
    {:name "Dreadnought"
     :cells cells
     ;; muzzles, in voxel coordinates: centreline, on top of each turret
     :guns [[HALF-BEAM (v (+ deck 1.0)) (v 21.5)]
            [HALF-BEAM (v (+ deck 1.0)) (v 4.5)]]
     :voxel VOXEL
     ;; midships on the centreline, at the keel
     :anchor [(+ HALF-BEAM 0.5) 0.0 (/ LENGTH 2.0)]
     :bounds {:length LENGTH-U :beam BEAM-U :depth DEPTH-U}}))
