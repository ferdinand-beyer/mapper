(ns com.fbeyer.mapper.convert-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [com.fbeyer.mapper.convert :refer [convert]]))

(defspec test-convert-int-string
  (for-all [u gen/small-integer]
           (= u (-> u (convert String) (convert Long)))))

(defspec test-convert-uuid-string
  (for-all [u gen/uuid]
           (= u (-> u (convert String) (convert java.util.UUID)))))
