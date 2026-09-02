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
  beam, j up. The anchor is the midships keel cell.")
(def LENGTH 26)
(def BEAM 7)
(def DEPTH 4)

(defn- half-width
  "Beam half-width at column k: full midships, transom stern, raked bow."
  [k]
  (cond (<= k 1) 2
        (<= k 21) 3
        (<= k 23) 2
        (= k 24) 1
        :else 0))

(defn- hull-cells
  "The solid tapered hull: DEPTH layers, top layer tagged :deck."
  []
  (into {}
        (for [k (range LENGTH)
              :let [hw (half-width k)]
              i (range (- 3 hw) (inc (+ 3 hw)))
              j (range DEPTH)]
          [[i j k] (if (= j (dec DEPTH)) :deck :hull)])))

(defn- block
  "A solid rectangular block of one material."
  [i0 i1 j0 j1 k0 k1 mat]
  (into {}
        (for [i (range i0 (inc i1))
              j (range j0 (inc j1))
              k (range k0 (inc k1))]
          [[i j k] mat])))

(defn dreadnought
  "The standard warship: hull + bridge + funnel + fore and aft turrets."
  []
  (let [cells (merge (hull-cells)
                     (block 2 4 4 5 9 14 :super)   ;; bridge
                     (block 3 3 4 5 15 16 :super)  ;; funnel
                     (block 2 3 4 4 21 22 :gun)    ;; fore turret
                     (block 2 3 4 4 4 5 :gun))]    ;; aft turret
    {:name "Dreadnought"
     :cells cells
     :guns [[2 4 22] [3 4 4]]
     :anchor [3 0 13]
     :bounds {:length LENGTH :beam BEAM :depth DEPTH}}))
