(ns com.fbeyer.mapper.convert-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [com.fbeyer.mapper.convert :refer [convert]])
  (:import [java.util Date UUID]
           [java.time Instant]))

(defspec test-convert-int-string
  (for-all [n gen/small-integer]
           (= n (-> n (convert String) (convert Long)))))

(defspec test-convert-uuid-string
  (for-all [u gen/uuid]
           (= u (-> u (convert String) (convert UUID)))))

(defspec test-convert-date-int
  (for-all [n gen/small-integer]
           (= n (-> n (convert Date) (convert Long)))))

(defspec test-convert-date-string
  (for-all [n gen/small-integer]
           (let [d (Date. n)]
             (= d (-> d (convert String) (convert Date))))))

(defspec test-convert-instant-int
  (for-all [n gen/small-integer]
           (= n (-> n (convert Instant) (convert Long)))))

(defspec test-convert-instant-date
  (for-all [n gen/small-integer]
           (let [inst (Instant/ofEpochMilli n)]
             (= inst (-> inst (convert Date) (convert Instant))))))

(defspec test-convert-instant-string
  (for-all [n gen/small-integer]
           (let [inst (Instant/ofEpochMilli n)]
             (= inst (-> inst (convert String) (convert Instant))))))
