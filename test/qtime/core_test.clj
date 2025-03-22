(ns qtime.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [qtime.core :as core
             :refer [parse-instant adjust with now until get-long]])
  (:import [java.time Instant Duration]
           [java.time.temporal ChronoUnit TemporalAdjuster]))

(defn cs
  "A clean function to getting seconds from an instant that doesn't rely on the library"
  [^Instant i]
  (.getEpochSecond i))

(defn cm
  "A clean function to getting milliseconds from an instant that doesn't rely on the library"
  [^Instant i]
  (.toEpochMilli i))

(def sec1 (Duration/of 1 ChronoUnit/SECONDS))

(deftest test-parse
  (testing "Parsing basic ISO formats for seconds"
    (is (= 1742656321 (cs (parse-instant "2025-03-22T10:12:01.861482-05:00"))))
    (is (= 1742656321 (cs (parse-instant "2025-03-22T15:12:01.861482Z"))))
    (is (= 1742656321 (cs (parse-instant "2025-03-22T17:12:01.861482+02:00"))))
    (is (= 1742656321 (cs (parse-instant "2025-03-23T01:42:01.861482+10:30"))))
    (is (= 1742656321 (cs (parse-instant "2025-03-22T15:12:01.861Z"))))
    (is (= 1742656321 (cs (parse-instant "2025-03-22T15:12:01Z"))))
    (testing "with resolution"
      (is (= 1742656320 (cs (parse-instant "2025-03-22T15:12:01.861482Z" :min))))
      (is (= 1742655600 (cs (parse-instant "2025-03-22T15:12:01.861482Z" :hr))))
      (is (= 1742601600 (cs (parse-instant "2025-03-22T15:12:01.861482Z" :days))))
      (is (= 1742601600 (cs (parse-instant "2025-03-22T15:12:01.861482Z" :day))))))
  (testing "Parsing basic ISO formats for nanoseconds"
    (is (= 861000000 (.getNano (parse-instant "2025-03-22T10:12:01.861482-05:00"))))
    (is (= 861000000 (.getNano (parse-instant "2025-03-22T15:12:01.861482Z"))))
    (is (= 861000000 (.getNano (parse-instant "2025-03-22T15:12:01.861Z"))))
    (is (= 0 (.getNano (parse-instant "2025-03-22T15:12:01Z"))))
    (is (= 861482000 (.getNano (parse-instant "2025-03-22T10:12:01.861482-05:00" :ns))))
    (is (= 861482000 (.getNano (parse-instant "2025-03-22T15:12:01.861482Z" :nanos))))
    (is (= 861000000 (.getNano (parse-instant "2025-03-22T15:12:01.861Z" "nanos"))))
    (is (= 0 (.getNano (parse-instant "2025-03-22T15:12:01Z" :ns))))
    (is (= 861482000 (.getNano (parse-instant "2025-03-22T10:12:01.861482-05:00" :us))))
    (is (= 861482000 (.getNano (parse-instant "2025-03-22T15:12:01.861482Z" :micros))))
    (is (= 861000000 (.getNano (parse-instant "2025-03-22T15:12:01.861Z" "us"))))
    (is (= 0 (.getNano (parse-instant "2025-03-22T15:12:01Z" :us))))
    (is (= 0 (.getNano (parse-instant "2025-03-22T10:12:01.861482-05:00" :s))))
    (is (= 0 (.getNano (parse-instant "2025-03-22T15:12:01.861482Z" :sec))))
    (is (= 0 (.getNano (parse-instant "2025-03-22T15:12:01.861Z" :seconds))))
    (is (= 0 (.getNano (parse-instant "2025-03-22T15:12:01Z" :s))))))

(deftest test-adjust
  (testing "temporal adjusters"
    (let [n (now)
          f (fn [i] (.plus i sec1))
          ta (reify TemporalAdjuster (adjustInto [_ t] (.plus t (.negated sec1))))]
      (is (= (inc (cs n)) (cs (adjust n f))))
      (is (= (dec (cs n)) (cs (adjust n ta))))))
  (testing "modifying fields"
    (let [tms 1742680406725
          t (Instant/ofEpochMilli tms)]
      (is (= tms (cm (with t :ns 725000027))))  ;; still with the 725ms
      (is (= (* 1000000 (mod tms 1000)) (.getNano t)))
      (is (= 27 (.getNano (with t :ns 27)))))))

(deftest test-conversions
  (testing "various conversions"
    (is (= 277 (until "2025-03-22T21:53:26Z" "2025-12-25T00:00:00Z" :days)))
    (is (= 22 (get-long "2025-03-22T21:53:26Z" :days)))
    (is (= 81 (get-long "2025-03-22T21:53:26Z" :day-of-year)))
    (let [n (now)]
      (is (= (mod (cm n) 1000) (get-long n :ms))))))
