(ns qtime.core
  (:require [qtime.util :refer [require-optional when-accessible]])
  (:import [clojure.lang Keyword ExceptionInfo]
           [java.util Date]
           [java.time Instant ZoneId Duration Temporal]
           [java.time.format DateTimeFormatter DateTimeFormatterBuilder]
           [java.time.temporal ChronoUnit ChronoField TemporalAmount]))

(require-optional 'clj-time.core)

(set! *warn-on-reflection* true)

(def ^ZoneId utc (ZoneId/of "UTC"))

;; allow permissive parsing, with microseconds
(def ^DateTimeFormatter iso-pattern (let [formatter (doto (DateTimeFormatterBuilder.)
                                                      (.appendPattern "yyyy-MM-dd'T'HH:mm:ss")
                                                      (.appendFraction ChronoField/MICRO_OF_SECOND 3 6 true)
                                                      (.appendLiteral \Z))]
                                      (.withZone (.toFormatter formatter) utc)))

(def ^DateTimeFormatter iso-writer (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'" utc)))

(defprotocol Chronological
  (to-chrono ^ChronoUnit [c] "Returns the provided value to a ChronoUnit"))

(defprotocol Instantable
  (to-instant ^Instant [i] "Converts a datatype to an instant"))

(defprotocol Temporalable
  (to-temporal ^Duration [t] "Converts a datatype to a temporal amount. This is always a Duration."))

(defprotocol ArithmeticTime
  (plus [t v] "Adds an amount of v to the time t")
  (minus [t v] "Subtracts an amount of v from the time t")
  (multiply ^Duration [d v] "Multiplies a temporal unit by a scalar")
  (divide [d v] "Divides a temporal unit by a scalar or by another temporal unit")
  (negate ^Duration [d] "Negates a temporal unit"))

(def chrono-constants
  "Mapping of keywords to ChronoUnit"
  {:ns ChronoUnit/NANOS
   :nanos ChronoUnit/NANOS
   :us ChronoUnit/MICROS
   :micros ChronoUnit/MICROS
   :ms ChronoUnit/MILLIS
   :millis ChronoUnit/MILLIS
   :s ChronoUnit/SECONDS
   :sec ChronoUnit/SECONDS
   :seconds ChronoUnit/SECONDS
   :min ChronoUnit/MINUTES
   :minutes ChronoUnit/MINUTES
   :hr ChronoUnit/HOURS
   :hours ChronoUnit/HOURS
   :half-days ChronoUnit/HALF_DAYS
   :days ChronoUnit/DAYS
   :weeks ChronoUnit/WEEKS
   :years ChronoUnit/YEARS
   :decades ChronoUnit/DECADES
   :centuries ChronoUnit/CENTURIES
   :millennia ChronoUnit/MILLENNIA
   :eras ChronoUnit/ERAS
   :forever ChronoUnit/FOREVER})

(def keyword-units
  "Back mapping of ChronoUnit constants to keywords"
  {ChronoUnit/NANOS :ns
   ChronoUnit/MICROS :us
   ChronoUnit/MILLIS :ms
   ChronoUnit/SECONDS :s
   ChronoUnit/MINUTES :min
   ChronoUnit/HOURS :hr
   ChronoUnit/HALF_DAYS :half-days 
   ChronoUnit/DAYS :days
   ChronoUnit/WEEKS :weeks
   ChronoUnit/YEARS :years
   ChronoUnit/DECADES :decades
   ChronoUnit/CENTURIES :centuries
   ChronoUnit/MILLENNIA :millennia
   ChronoUnit/ERAS :eras
   ChronoUnit/FOREVER :forever})

(defn parse-instant
  "Parse a string to an Instant. By default, this will truncate parsing to the millisecond.
  An optional time unit can be passed for different granularity, or nil for no truncation at all"
  ([^String s] (parse-instant s :ms))
  ([^String s time-unit]
   (let [instant (Instant/from (.parse iso-pattern s))]
     (if-let [c (to-chrono time-unit)]
       (.truncatedTo instant c)
       instant))))

(extend-protocol Chronological
  ChronoUnit
  (to-chrono [c] c)
  Keyword
  (to-chrono [c] (or (chrono-constants c)
                     (throw (ex-info (str "Unknown chrono unit:" (name c)) {:unit c}))))
  String
  (to-chrono [c] (to-chrono (keyword c)))
  Object
  (to-chrono [c] (throw (ex-info (str "Don't know how to convert " c " to a chonological unit") {:unknown c})))
  nil
  (to-chrono [_] ChronoUnit/FOREVER))

(extend-protocol Instantable
  Instant
  (to-instant [i] i)
  Date
  (to-instant [d] (.toInstant d))
  String
  (to-instant
    [s]
    (try
      (parse-instant s)
      (catch Exception _
        (try
          (Instant/ofEpochMilli (parse-long s))
          (catch Exception _
            (throw (ex-info (str "Unknown date/time format: " s) {:string s})))))))
  Long
  (to-instant [l] (Instant/ofEpochMilli l))
  Object
  (to-instant
    [o]
    (if (inst? o)  ;; this will include org.joda.time.base.BaseDateTime
      (Instant/ofEpochMilli (inst-ms o))
      (throw (ex-info (str "Don't know how to convert type " (type o) " to an instant")
                      {:object o :type (type o)}))))
  nil
  (to-instant [_] (Instant/now)))

(extend-protocol Temporalable
  Duration
  (to-temporal [x] x)
  TemporalAmount
  (to-temporal [x] (Duration/from x))
  Long
  (to-temporal [x] (if (zero? x) Duration/ZERO (Duration/ofMillis x)))
  nil
  (to-temporal [_] Duration/ZERO))

;; Extend to Joda Durations and Periods if these are loaded
(when-accessible
    org.joda.time.ReadableDuration
    (extend-protocol Temporalable
      org.joda.time.ReadableDuration
      (to-temporal [d] (Duration/ofMillis (.getMillis ^org.joda.time.ReadableDuration d)))
      org.joda.time.Period
      (to-temporal [p]
        (let [d (.toStandardDuration ^org.joda.time.Period p)]
          (Duration/ofMillis (.getMillis ^org.joda.time.ReadableDuration d))))))

(extend-protocol ArithmeticTime
  Instant
  (plus [i v] (.plus i (to-temporal v)))
  (minus [i v] (.minus i (to-temporal v)))
  (multiply [i _] (throw (ex-info (str "Cannot multiply an instant: " i) {:value i})))
  (divide [i _] (throw (ex-info (str "Cannot divide an instant: " i) {:value i})))
  (negate [i] (throw (ex-info (str "Cannot negate an instant: " i) {:value i})))
  Duration
  (plus [d v] (.plus d (to-temporal v)))
  (minus [d v] (.minus d (to-temporal v)))
  (multiply [i v]
    (if (instance? Temporal v)
      (.multipliedBy i ^Temporal v)
      (.multipliedBy i v)))
  (divide [i v]
    (if (instance? Temporal v)
      (.dividedBy i ^Temporal v)
      (.dividedBy i v)))
  (negate [i] (.negated i))
  Long
  (plus [l v] (.plus (to-temporal l) (to-temporal v)))
  (minus [l v] (.minus (to-temporal l) (to-temporal v)))
  (multiply [l v] (.multipliedBy (to-temporal l) v))
  (divide [l v]
    (let [t (to-temporal l)]
      (if (instance? Temporal v)
        (.dividedBy t ^Temporal v)
        (.dividedBy t v))))
  (negate [l] (.negated (to-temporal l)))
  Object
  (plus [t v]
    (let [summand (to-temporal v)]
      (try
        ;; try to treat the first argument as an instant
        (let [inst (to-instant t)]
          (plus inst summand))
        (catch ExceptionInfo _
          ;; no, so now try it as a duration
          (plus (to-temporal t) summand)))))
  (minus [t v]
    (let [subtrahend (to-temporal v)]
      (try
        ;; try to treat the first argument as an instant
        (let [inst (to-instant t)]
          (minus inst subtrahend))
        (catch ExceptionInfo _
          ;; no, so now try it as a duration
          (minus (to-temporal t) subtrahend)))))
  (multiply [t v]
    ;; try to treat the first argument as a duration
    (multiply (to-temporal t) v))
  (divide [t v]
    ;; try to treat the first argument as a duration
    (divide (to-temporal t) v))
  (negate [t]
    ;; try to treat the first argument as a duration
    (negate (to-temporal t)))
  nil
  (plus [_ v] (to-temporal v))
  (minus [_ v] (.negated (to-temporal v)))
  (multiply [_ _] Duration/ZERO)
  (divide [_ v] (if (zero? v) (.dividedBy Duration/Zero 0) Duration/ZERO)) ;; throw an exception for 0/0
  (negate [_] Duration/ZERO))

;; Duration wrappers
(defn abs-duration
  ^Duration [i]
  (.abs (to-temporal i)))

(defn between
  ^Duration [start-inclusive end-exclusive]
  (Duration/between (to-temporal start-inclusive) (to-temporal end-exclusive)))

(defn compare-duration
  [d1 d2]
  (.compareTo (to-temporal d1) (to-temporal d2)))

(defn get-by-unit
  [d unit]
  (.get (to-temporal d) (to-chrono unit)))

(defn get-nano
  [d]
  (.getNano (to-temporal d)))

(defn get-seconds
  [d]
  (.getSeconds (to-temporal d)))

(defn get-units
  [d]
  (keyword-units (.getUnits (to-temporal d))))

(defn negative?
  [d]
  (.isNegative (to-temporal d)))

(defn positive?
  [d]
  (.isPositive (to-temporal d)))

(defn zero-time?
  [d]
  (.isZero (to-temporal d)))

(defn minus-days
  [d days]
  (.minusDays (to-temporal d) days))

(defn minus-hours
  [d hours]
  (.minusHours (to-temporal d) hours))

(defn minus-millis
  [d millis]
  (.minusMillis (to-temporal d) millis))

(defn minus-minutes
  [d minutes]
  (.minusMinutes (to-temporal d) minutes))

(defn minus-nanos
  [d nanos]
  (.minusNanos (to-temporal d) nanos))

(defn minus-seconds
  [d seconds]
  (.minusSeconds (to-temporal d) seconds))

(defn multiplied-by
  [d multiplicand]
  (.multipliedBy (to-temporal d) multiplicand))

(defn of
  [amount unit]
  (Duration/of amount (to-chrono unit)))

(defn of-days
  [days]
  (Duration/ofDays days))

(defn of-hours
  [hours]
  (Duration/ofHours hours))

(defn of-millis
  [millis]
  (Duration/ofMillis millis))

(defn of-minutes
  [minutes]
  (Duration/ofMinutes minutes))

(defn of-nanos
  [nanos]
  (Duration/ofNanos nanos))

(defn of-seconds
  ([seconds]
   (Duration/ofSeconds seconds))
  ([seconds nano-adjustment]
   (Duration/ofSeconds seconds nano-adjustment)))

(defn parse-duration
  [text]
  (Duration/parse text))

(defn plus-days
  [days-to-add]
  (Duration/plusDays days-to-add))

(defn plus-hours
  [hours-to-add]
  (Duration/plusHours hours-to-add))

(defn plus-millis
  [millis-to-add]
  (Duration/plusMillis millis-to-add))

(defn plus-minutes
  [minutes-to-add]
  (Duration/plusMinutes minutes-to-add))

(defn plus-nanos
  [nanos-to-add]
  (Duration/plusNanos nanos-to-add))

(defn plus-seconds
  [seconds-to-add]
  (Duration/plusSeconds seconds-to-add))

(defn to-days
  [d]
  (.toDays (to-temporal d)))

(defn to-days-part
  [d]
  (.toDaysPart (to-temporal d)))

(defn to-hours
  [d]
  (.toHours (to-temporal d)))

(defn to-hours-part
  [d]
  (.toHoursPart (to-temporal d)))

(defn to-millis
  [d]
  (.toMillis (to-temporal d)))

(defn to-millis-part
  [d]
  (.toMillisPart (to-temporal d)))

(defn to-minutes
  [d]
  (.toMinutes (to-temporal d)))

(defn to-minutes-part
  [d]
  (.toMinutesPart (to-temporal d)))

(defn to-nanos
  [d]
  (.toNanos (to-temporal d)))

(defn to-nanos-part
  [d]
  (.toNanosPart (to-temporal d)))

(defn to-seconds
  [d]
  (.toSeconds (to-temporal d)))

(defn to-seconds-part
  [d]
  (.toSecondsPart (to-temporal d)))

(defn to-string
  [d]
  (str (to-temporal d)))

(defn truncated-to
  [d unit]
  (.truncatedTo (to-temporal d) (to-chrono unit)))

(defn with-nanos
  [d nano-of-second]
  (.withNanos (to-temporal d) nano-of-second))

(defn with-seconds
  [d seconds]
  (.withSeconds (to-temporal d) seconds))

