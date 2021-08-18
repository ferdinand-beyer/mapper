(ns com.fbeyer.mapper.convert)

(set! *warn-on-reflection* true)

(defmulti convert
  "Converts x to the given type."
  (fn [x t]
    [(type x) t]))

(defmethod convert [Long String] [n _]
  (Long/toString n))

(defmethod convert [String Long] [s _]
  (try (Long/parseLong s)
       (catch NumberFormatException _)))

(defmethod convert [java.util.UUID String] [^java.util.UUID uuid _]
  (.toString uuid))

(defmethod convert [String java.util.UUID] [^String s _]
  (java.util.UUID/fromString s))
