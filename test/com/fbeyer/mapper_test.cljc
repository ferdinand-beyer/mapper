(ns com.fbeyer.mapper-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.fbeyer.mapper :as m]))

(deftest test-key-mapper
  (testing "explicit spec"
    (let [mapper (m/mapper {:foo (m/>key :bar)})]
      (is (= {:bar 2} (m/map> mapper {:foo 2})))
      (is (= {:foo 2} (m/map< mapper {:bar 2})))))
  
  (testing "implicit spec"
    (let [mapper (m/mapper {:foo :bar})]
      (is (= {:bar 2} (m/map> mapper {:foo 2})))
      (is (= {:foo 2} (m/map< mapper {:bar 2}))))))

(deftest test-get-mapper
  (let [mapper (m/mapper (m/>get :foo))]
    (is (= 2 (m/map> mapper {:foo 2})))
    (is (nil? (m/map> mapper {:bar 2})))
    (is (= {:foo 2} (m/map< mapper 2)))))

(deftest test-convert-mapper
  (let [mapper (m/mapper (m/>convert Long String))]
    (is (= "42" (m/map> mapper 42)))
    (is (= 42 (m/map< mapper "42")))))

(deftest test-struct-mapper
  (testing "explicit spec"
    (let [mapper (m/mapper (m/>struct {:a (m/>get :b)}))]
      (is (= {:a :c} (m/map> mapper {:a {:b :c}})))
      (is (= {:a {:b :c}} (m/map< mapper {:a :c})))))

  (testing "implicit spec"
    (let [mapper (m/mapper {:a (m/>convert Long String)})]
      (is (= {:a "7"} (m/map> mapper {:a 7})))
      (is (= {:a 13} (m/map< mapper {:a "13"})))))
  
  (testing "removes unspecified keys"
    (let [mapper (m/mapper {:a :b, :c :d})]
      (is (= {:b 1, :d 3} (m/map> mapper {:a 1, :b 2, :c 3}))))))

(deftest test-each-mapper
  (let [mapper (m/mapper (m/>each (m/>get :a)))]
    (is (= [1 2 3] (m/map> mapper [{:a 1} {:a 2} {:a 3}])))
    (is (= [{:a 1} {:a 2} {:a 3}] (m/map< mapper [1 2 3])))))

(deftest test-compose-mapper
  (testing "explicit spec"
    (let [mapper (m/mapper (m/>compose (m/>get :a)
                                       (m/>convert Long String)))]
      (is (= "3" (m/map> mapper {:a 3})))))
  
  (testing "implicit spec"
    (let [mapper (m/mapper [(m/>get :a)
                            (m/>convert Long String)])]
      (is (= "3" (m/map> mapper {:a 3}))))))

