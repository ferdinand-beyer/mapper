(ns com.fbeyer.mapper
  (:require [com.fbeyer.mapper.convert :refer [convert]]))

;;;; Compilers

(defprotocol Spec
  "A mapper specification that can be compiled into Clojure code."
  (-compile [this ctx]
    "Compiles the spec in the given context.  Should return an updated
     context overriding the keys :>> and :<< as required.  Those are
     functions taking a Clojure form and returning a transformed form,
     adding mapping code for the spec.
     
     Some specs add additional keys to the context, e.g. the current
     :key in a map that can be replaced with a different key."))

(defn- -compile-struct
  [{:keys [>> <<] :as ctx} spec-map]
  (let [compiled (for [[k v] spec-map]
                   (-compile v (assoc ctx :src-key k :key k
                                      :>> identity :<< identity)))
        vsym (gensym "v-")
        gen (fn [k k' f e]
              (let [clauses (mapcat (fn [c]
                                      ;; TODO: Skip nils? (when-some [x (f c)])
                                      [(k c) [(k' c) ((f c) vsym)]])
                                    compiled)]
                `(let [m# ~e]
                   (into (empty m#) (for [[k# ~vsym] m#]
                                      (case k# ~@clauses nil))))))]
    (assoc ctx
           :>> (fn [e] (gen :src-key :key :>> (>> e)))
           :<< (fn [e] (<< (gen :key :src-key :<< e))))))

(defn >struct
  "Maps a 'struct map'."
  [spec-map]
  (reify Spec
    (-compile [_ ctx]
      (-compile-struct ctx spec-map))))

(defn- -compile-key [ctx k]
  (assoc ctx :key k))

(defn >key
  "Maps the current key to `k`."
  [k]
  (reify Spec
    (-compile [_ ctx]
      (-compile-key ctx k))))

;; TODO: Default value if it does not exist?
(defn >get
  "Maps the current value (a map) to a contained value at key `k`,
   using clojure.core/get.  The inverse will wrap the value in a
   map with the same key."
  [k]
  (reify Spec
    (-compile [_ {:keys [>> <<] :as ctx}]
      (assoc ctx
             :>> (fn [e] `(get ~(>> e) ~k))
             :<< (fn [e] (<< {k e}))))))

(defn >convert
  "Converts the current value from type `s` to type `t`, and the other
   way around for the invnerse."
  [s t]
  (reify Spec
    (-compile [_ {:keys [>> <<] :as ctx}]
      (assoc ctx
             :>> (fn [e] `(convert ~(>> e) ~t))
             :<< (fn [e] (<< `(convert ~e ~s)))))))

(defn >each
  "Maps each element of the current value with the given spec."
  [spec]
  (let [vsym (gensym "v-")
        gen (fn [f e]
              `(mapv (fn [~vsym] ~(f vsym)) ~e))]
    (reify Spec
      (-compile [_ {:keys [>> <<] :as ctx}]
        (let [compiled (-compile spec (assoc ctx :>> identity :<< identity))]
          (assoc ctx
                 :>> (fn [e] (gen (:>> compiled) (>> e)))
                 :<< (fn [e] (<< (gen (:<< compiled) e)))))))))

(defn- -compile-compose [ctx specs]
  (reduce #(-compile %2 %1) ctx specs))

(defn >compose
  "Composes a sequence of specs into one."
  [& specs]
  (reify Spec
    (-compile [_ ctx]
      (-compile-compose ctx specs))))

(extend-protocol Spec
  clojure.lang.IPersistentMap
  (-compile [m ctx]
    (-compile-struct ctx m))

  clojure.lang.IPersistentVector
  (-compile [cs ctx]
    (-compile-compose ctx cs))

  clojure.lang.Keyword
  (-compile [k ctx]
    (-compile-key ctx k)))

(defprotocol Mapper
  "Bidirectional mapper x <=> y."
  (map> [this x])
  (map< [this y]))

(defn- compile-mapper
  "Produces a compiled Clojure form from a mapper spec."
  [spec]
  (let [{:keys [>> <<]} (-compile spec {:>> identity, :<< identity})
        src (gensym "src-")]
    `(reify Mapper
       (~'map> [~'_ ~src] ~(>> src))
       (~'map< [~'_ ~src] ~(<< src)))))

(defmacro mapper
  "Creates a mapper by compiling the given spec."
  [spec]
  (compile-mapper (eval spec)))
