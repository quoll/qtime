(ns qtime.impl
  (:require [qtime.constants :as constants]
            [qtime.transform :as transform]
            [qtime.protocols :as protocols :refer [Chronological Temporalable TemporalAmountable Zoneable
                                                   Fieldable HasNanos ArithmeticTime TimezoneOffsettable
                                                   Timezoneable to-chrono to-instant to-duration
                                                   to-timezone divide minus-seconds plus-nanos
                                                   minus-nanos minus plus-millis plus-seconds
                                                   truncated-to minus-millis multiply negate plus
                                                   to-timezone-offset get-nano]]
            [qtime.util :refer [require-optional when-accessible]])
  (:import [clojure.lang Keyword]
           [java.util Date]
           [java.time Instant Duration ZoneId ZoneOffset OffsetDateTime ZonedDateTime
            LocalDate LocalDateTime OffsetTime Year YearMonth]
           [java.time.format DateTimeFormatter DateTimeFormatterBuilder]
           [java.time.temporal ChronoUnit ChronoField TemporalUnit TemporalField
            TemporalAmount TemporalAccessor ValueRange Temporal TemporalAdjuster
            TemporalQueries]
           [java.time.chrono HijrahDate JapaneseDate MinguoDate ThaiBuddhistDate]))

(require-optional 'clj-time.core)

(set! *warn-on-reflection* true)

;; allow permissive parsing, with microseconds
(def ^DateTimeFormatter iso-unzoned
  (let [formatterb (doto (DateTimeFormatterBuilder.)
                     (.appendPattern "yyyy-MM-dd'T'HH:mm:ss")
                     (.appendFraction ChronoField/MICRO_OF_SECOND 0 6 true)
                     (.appendOffsetId))]
    (.toFormatter ^DateTimeFormatterBuilder formatterb)))

(def ^DateTimeFormatter utc-pattern (let [formatterb (doto (DateTimeFormatterBuilder.)
                                                      (.appendPattern "yyyy-MM-dd'T'HH:mm:ss")
                                                      (.appendFraction ChronoField/MICRO_OF_SECOND 0 6 true)
                                                      (.appendLiteral \Z))]
                                      (.withZone (.toFormatter ^DateTimeFormatterBuilder formatterb) constants/utc)))

(defn parse-temporal
  "Parse a string to a Temporal."
  ^Temporal [^String s]
  (let [ta (.parse iso-unzoned s)]
    (if (.isSupported ta ChronoField/OFFSET_SECONDS)
      (ZonedDateTime/from ta)
      (Instant/from ta))))

(defn ^Instant parse-instant
  "Parse a string to an Instant. By default, this will truncate parsing to the millisecond.
  An optional time unit can be passed for different granularity, or nil for no truncation at all"
  ([^String s] (parse-instant s :ms))
  ([^String s time-unit]
   (let [instant (Instant/from (.parse iso-unzoned s))]
     (if-let [c (to-chrono time-unit)]
       (.truncatedTo instant c)
       instant))))

(extend-protocol Chronological
  ChronoUnit
  (to-chrono [c] c)
  Keyword
  (to-chrono [c] (or (constants/chrono-units c)
                     (throw (ex-info (str "Unknown chrono unit:" (name c)) {:unit c}))))
  String
  (to-chrono [c] (or (to-chrono (keyword c))
                     (ChronoUnit/valueOf c)))
  Object
  (to-chrono [c] (throw (ex-info (str "Don't know how to convert " c " to a chonological unit") {:unknown c})))
  nil
  (to-chrono [_] ChronoUnit/FOREVER))

(extend-protocol Temporalable
  Instant
  (to-temporal [i] i)
  (to-instant [i] i)
  Temporal
  (to-temporal [t] t)
  (to-instant [t] (transform/tx-to-instant t))
  TemporalAccessor
  (to-temporal [t]
    (if (.isSupported t ChronoField/OFFSET_SECONDS)
      (.atZone (transform/tx-to-instant t) constants/utc)
      (transform/tx-to-instant t)))
  (to-instant [t] (transform/tx-to-instant t))
  Date
  (to-temporal [d] (.toInstant d))
  (to-instant [d] (.toInstant d))
  String
  (to-temporal
    [s]
    (try
      (parse-temporal s)
      (catch Exception _
        (try
          (Instant/ofEpochMilli (parse-long s))
          (catch Exception _
            (throw (ex-info (str "Unknown date/time format: " s) {:string s})))))))
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
  (to-temporal [l] (Instant/ofEpochMilli l))
  (to-instant [l] (Instant/ofEpochMilli l))
  Object
  (to-temporal [o] (to-instant o))
  (to-instant
    [o]
    (if (inst? o)
      (Instant/ofEpochMilli (inst-ms o))
      (throw (ex-info (str "Don't know how to convert type " (type o) " to an instant")
                      {:object o :type (type o)}))))
  nil
  (to-temporal [_] (Instant/now))
  (to-instant [_] (Instant/now)))

(when-accessible
 org.joda.time.ReadableInstant
 (extend-protocol Temporalable
   org.joda.time.ReadableInstant
   (to-temporal [i] (Instant/ofEpochMilli (.getMillis ^org.joda.time.ReadableInstant i)))
   (to-instant [i] (Instant/ofEpochMilli (.getMillis ^org.joda.time.ReadableInstant i)))))

(extend-protocol TemporalAmountable
  Duration
  (to-duration [x] x)
  TemporalAmount
  (to-duration [x] (Duration/from x))
  Long
  (to-duration [x] (if (zero? x) Duration/ZERO (Duration/ofMillis x)))
  String
  (to-duration [s]
    (try
      (Duration/parse s)
      (catch Exception e
        (try
          (to-duration (parse-long s))
          (catch Exception _
            (throw e))))))
  nil
  (to-duration [_] Duration/ZERO))

(extend-protocol Zoneable
  Instant
  (to-zone
    ([t] (.atZone t constants/utc))
    ([t tz] (.atZone t (to-timezone tz))))
  (has-zone? [_] false)
  (zone [_] constants/utc)
  OffsetDateTime
  (to-zone
    ([t] t)
    ([t tz] (.atZoneSameInstant t (to-timezone tz))))
  (has-zone? [t] (boolean (.query t (TemporalQueries/zone))))
  (zone [t] (or (.query t (TemporalQueries/zone)) constants/utc))
  OffsetTime
  (to-zone
    ([t] t)
    ([t tz]
     (let [zone (.normalized (to-timezone tz))]
       (if (instance? ZoneOffset zone)
         (.withOffsetSameInstant t zone)
         (.atZoneSameInstant (.atDate t constants/epoch-day) zone)))))  ;; best guess
  (has-zone? [t] (boolean (.query t (TemporalQueries/zone))))
  (zone [t] (or (.query t (TemporalQueries/zone)) constants/utc))
  ZonedDateTime
  (to-zone
    ([t] t)
    ([t tz] (.withZoneSameInstant t (to-timezone tz))))
  (has-zone? [t] (boolean (.query t (TemporalQueries/zone))))
  (zone [t] (or (.query t (TemporalQueries/zone)) constants/utc))
  LocalDateTime
  (to-zone
    ([t] (.atZone t constants/utc))
    ([t tz] (.atZone t (to-timezone tz))))
  (has-zone? [_] false)
  (zone [_] constants/utc)
  Temporal
  (to-zone
    ([t] t)
    ([t tz] (throw (ex-info (str "Unabled to change the timezone for " (type t))
                            {:temporal t :timezone tz}))))
  (has-zone? [_] false)
  (zone [_] constants/utc))

(defn parse-time-object
  ^Instant [^String s]
  (try
    (to-instant s)
    (catch Exception _
      (to-duration s)
      (throw (ex-info (str "Don't know how to interpret '" s "' as a time type") {:string s})))))

(defn to-time-object
  [o]
  (try
    (to-instant o)
    (catch Exception _
      (to-duration o)
      (throw (ex-info (str "Don't know how to convert '" o "' to a time type") {:object o})))))

;; Extend to Joda Durations and Periods if these are loaded
(when-accessible
    org.joda.time.ReadableDuration
    (extend-protocol TemporalAmountable
      org.joda.time.ReadableDuration
      (to-duration [d] (Duration/ofMillis (.getMillis ^org.joda.time.ReadableDuration d)))
      org.joda.time.Period
      (to-duration [p]
        (let [d (.toStandardDuration ^org.joda.time.Period p)]
          (Duration/ofMillis (.getMillis ^org.joda.time.ReadableDuration d))))))

(extend-protocol ArithmeticTime
  Instant
  (plus [i v] (.plus i (to-duration v)))
  (plus-millis [i millis] (.plusMillis i millis))
  (plus-nanos [i nanos] (.plusNanos i nanos))
  (plus-seconds [i seconds] (.plusSeconds i seconds))
  (minus [i v] (.minus i (to-duration v)))
  (minus-millis [i millis] (.minusMillis i millis))
  (minus-nanos [i nanos] (.minusNanos i nanos))
  (minus-seconds [i seconds] (.minusSeconds i seconds))
  (multiply [i _] (throw (ex-info (str "Cannot multiply an instant: " i) {:value i})))
  (divide [i _] (throw (ex-info (str "Cannot divide an instant: " i) {:value i})))
  (negate [i] (throw (ex-info (str "Cannot negate an instant: " i) {:value i})))
  (truncated-to [i unit] (.truncatedTo i (to-chrono unit)))
  TemporalAccessor
  (plus [ta v]
    (let [[i itx] (transform/tx ta)]
      (itx (.plus ^Instant i (to-duration v)))))
  (plus-millis [ta millis]
    (let [[i itx] (transform/tx ta)]
      (itx (.plusMillis ^Instant i millis))))
  (plus-nanos [ta nanos]
    (let [[i itx] (transform/tx ta)]
      (itx (.plusNanos ^Instant i nanos))))
  (plus-seconds [ta seconds]
    (let [[i itx] (transform/tx ta)]
      (itx (.plusSeconds ^Instant i seconds))))
  (minus [ta v]
    (let [[i itx] (transform/tx ta)]
      (itx (.minus ^Instant i (to-duration v)))))
  (minus-millis [ta millis]
    (let [[i itx] (transform/tx ta)]
      (itx (.minusMillis ^Instant i millis))))
  (minus-nanos [ta nanos]
    (let [[i itx] (transform/tx ta)]
      (itx (.minusNanos ^Instant i nanos))))
  (minus-seconds [ta seconds]
    (let [[i itx] (transform/tx ta)]
      (itx (.minusSeconds ^Instant i seconds))))
  (multiply [ta _] (throw (ex-info (str "Cannot multiply temporal: " ta) {:value ta})))
  (divide [ta _] (throw (ex-info (str "Cannot divide temporal: " ta) {:value ta})))
  (negate [ta] (throw (ex-info (str "Cannot negate temporal: " ta) {:value ta})))
  (truncated-to [ta unit]
    (let [[i itx] (transform/tx ta)]
      (itx (.truncatedTo ^Instant i (to-chrono unit)))))
  Duration
  (plus [d v] (if (instance? Temporal v)
                (.addTo d ^Temporal v)
                (.plus d (to-duration v))))
  (plus-millis [i millis] (.plusMillis i millis))
  (plus-nanos [i nanos] (.plusNanos i nanos))
  (plus-seconds [i seconds] (.plusSeconds i seconds))
  (minus [d v] (.minus d (to-duration v)))
  (minus-millis [i millis] (.minusMillis i millis))
  (minus-nanos [i nanos] (.minusNanos i nanos))
  (minus-seconds [i seconds] (.minusSeconds i seconds))
  (multiply [i v] (.multipliedBy i v))
  (divide [i v]
    (if (instance? TemporalAmount v)
      (.dividedBy i (to-duration v))
      (.dividedBy i (long v))))
  (negate [i] (.negated i))
  (truncated-to [d unit] (.truncatedTo d (to-chrono unit)))
  String
  (plus [s v] (plus-millis (parse-time-object s) v))
  (plus-millis [s millis] (plus-millis (parse-time-object s) millis))
  (plus-nanos [s nanos] (plus-nanos (parse-time-object s) nanos))
  (plus-seconds [s seconds] (plus-seconds (parse-time-object s) seconds))
  (minus [s v] (minus (parse-time-object s) v))
  (minus-millis [s millis] (minus-millis (parse-time-object s) millis))
  (minus-nanos [s nanos] (minus-nanos (parse-time-object s) nanos))
  (minus-seconds [s seconds] (minus-seconds (parse-time-object s) seconds))
  (multiply [s v] (multiply (parse-time-object s) v))
  (divide [s v] (divide (parse-time-object s) v))
  (negate [s] (negate (parse-time-object s)))
  (truncated-to [s unit] (truncated-to (parse-time-object s) unit))
  Long
  (plus [l v] (.plus (to-duration l) (to-duration v)))
  (plus-millis [l millis] (plus-millis (to-instant l) millis))
  (plus-nanos [l nanos] (plus-nanos (to-instant l) nanos))
  (plus-seconds [l seconds] (plus-seconds (to-instant l) seconds))
  (minus [l v] (.minus (to-duration l) (to-duration v)))
  (minus-millis [l millis] (minus-millis (to-instant l) millis))
  (minus-nanos [l nanos] (minus-nanos (to-instant l) nanos))
  (minus-seconds [l seconds] (minus-seconds (to-instant l) seconds))
  (multiply [l v] (.multipliedBy (to-duration l) v))
  (divide [l v]
    (let [t (to-duration l)]
      (if (instance? TemporalAmount v)
        (.dividedBy t (to-duration v))
        (.dividedBy t (long v)))))
  (negate [l] (.negated (to-duration l)))
  (truncated-to [l unit] (truncated-to (to-instant l) unit))
  Object
  (plus [t v] (plus (to-time-object t) (to-duration v)))
  (plus-millis [t millis] (plus-millis (to-time-object t) millis))
  (plus-nanos [t nanos] (plus-nanos (to-time-object t) nanos))
  (plus-seconds [t seconds] (plus-seconds (to-time-object t) seconds))
  (minus [t v] (minus (to-time-object t) (to-duration v)))
  (minus-millis [t millis] (minus-millis (to-time-object t) millis))
  (minus-nanos [t nanos] (minus-nanos (to-time-object t) nanos))
  (minus-seconds [t seconds] (minus-seconds (to-time-object t) seconds))
  (multiply [t v]
    ;; try to treat the first argument as a duration
    (multiply (to-duration t) v))
  (divide [t v]
    ;; try to treat the first argument as a duration
    (divide (to-duration t) v))
  (negate [t]
    ;; try to treat the argument as a duration
    (negate (to-duration t)))
  nil
  (plus [_ v] (to-duration v))
  (plus-millis [_ millis] (Duration/ofMillis millis))
  (plus-nanos [_ nanos] (Duration/ofNanos nanos))
  (plus-seconds [_ seconds] (Duration/ofSeconds seconds))
  (minus [_ v] (.negated (to-duration v)))
  (minus-millis [_ millis] (.negated (Duration/ofMillis millis)))
  (minus-nanos [_ nanos] (.negated (Duration/ofNanos nanos)))
  (minus-seconds [_ seconds] (.negated (Duration/ofSeconds seconds)))
  (multiply [_ _] Duration/ZERO)
  (divide [_ v] (if (zero? v) (.dividedBy Duration/ZERO 0) Duration/ZERO)) ;; throw an exception for 0/0
  (negate [_] Duration/ZERO)
  (truncated-to [_ _] Duration/ZERO))

(extend-protocol TimezoneOffsettable
  ZoneOffset
  (to-timezone-offset [o] o)
  String
  (to-timezone-offset [s] (ZoneOffset/of s))
  Keyword
  (to-timezone-offset [k] (ZoneOffset/of (name k)))
  TemporalAmount
  (to-timezone-offset [t] (ZoneOffset/ofTotalSeconds (.getSeconds (to-duration t))))
  TemporalAccessor
  (to-timezone-offset [a] (ZoneOffset/from a))
  Long
  (to-timezone-offset [hours] (ZoneOffset/ofHours hours))
  Double
  (to-timezone-offset [hours] (ZoneOffset/ofTotalSeconds (* hours 60 60))))

(extend-protocol Timezoneable
  ZoneId
  (to-timezone [z] z)
  String
  (to-timezone [s] (ZoneId/of s))
  Keyword
  (to-timezone [k] (ZoneId/of (name k)))
  TemporalAmount
  TemporalAccessor
  Long
  Double
  (to-timezone [o] (ZoneId/ofOffset "" (to-timezone-offset o))))

(extend-protocol Fieldable
  TemporalField
  (to-field [f] f)
  Keyword
  (to-field [f] (constants/temporal-fields f))
  String
  (to-field [f] (or (constants/temporal-fields (name f))
                    (ChronoField/valueOf f))))

(extend-protocol HasNanos
  TemporalAccessor
  (get-nano [t] (.getLong t ChronoField/NANO_OF_SECOND))
  TemporalAmount
  (get-nano [t] (.get t ChronoField/NANO_OF_SECOND))
  String
  (get-nano [s] (get-nano (parse-time-object s)))
  Long
  (get-nano [l] (.getNano (to-instant l)))
  nil
  (get-nano [_] 0))
