(ns com.fbeyer.mapper.convert
  (:import [java.util Date UUID]
           [java.time Instant]
           [java.text DateFormat]))

(set! *warn-on-reflection* true)

(defmulti convert
  "Converts x to the given type."
  (fn [x t]
    [(type x) t]))

(defmethod convert [Object String] [^Object o _]
  (.toString o))

(defmethod convert [Long String] [n _]
  (Long/toString n))

(defmethod convert [String Long] [s _]
  (try (Long/parseLong s)
       (catch NumberFormatException _)))

(defmethod convert [UUID String] [^UUID uuid _]
  (.toString uuid))

(defmethod convert [String UUID] [^String s _]
  (UUID/fromString s))

(defmethod convert [Instant Long] [^Instant inst _]
  (.toEpochMilli inst))

(defmethod convert [Long Instant] [^Long n _]
  (Instant/ofEpochMilli n))

(defmethod convert [Instant String] [^Instant inst _]
  (.toString inst))

(defmethod convert [String Instant] [^String s _]
  (Instant/parse s))

(defmethod convert [Instant Date] [^Instant inst _]
  (Date/from inst))

(defmethod convert [Date Instant] [^Date d _]
  (.toInstant d))

(defmethod convert [Date Long] [^Date d _]
  (.getTime d))

(defmethod convert [Long Date] [^Long n _]
  (Date. n))

(defmethod convert [Date String] [^Date d _]
  (-> d .toInstant .toString))

(defmethod convert [String Date] [^String s _]
  (-> s Instant/parse Date/from))
