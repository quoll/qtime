(ns qtime.core
  "Clojure wrappers around common Java Time functionality.
  These wrappers will attempt to convert objects into `Instant` or `Duration` whenever possible.
  If the source of a conversion may be ambiguous (e.g. `String`) then conversion will preference
  `Instant` whenever possible, falling back to `Duration` when this is unsuccessful.

  Compatible types will always be converted appropriately into `Instant` or `Duration`. These
  include `java.util.Date` and objects from Joda Time.

  Instants are all compatible with ISO-8601 strings. They can be emitted as strings in UTC,
  or with a timezone, using the `iso-str` function.

  The `Instant` and `Duration` functions are all wrapped, with automatic conversions.
  References to time units may be made with strings, keywords, or Java objects.
  e.g. \"sec\" for seconds, :ms for milliseconds, java.time.temporal.ChronoUnit/NANOS for nanoseconds.

  This allows function calls like:
  (until \"2025-03-22T21:53:26Z\" \"2025-12-25T00:00:00Z\" :days) ;; whole days until Christmas
  (get-long (now) :day-of-year) ;; what day of the year is it?"
  (:require [qtime.constants :as constants]
            [qtime.transform :as transform]
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

(defprotocol Chronological
  (to-chrono ^ChronoUnit [c] "Returns the provided value to a ChronoUnit"))

(defprotocol Temporalable
  (to-temporal ^Temporal [t] "Converts a datatype to a temporal type. This will maintain a timezone if available.")
  (to-instant ^Instant [i] "Converts a datatype to the very specific Instant type.
                            This has no timezone and is essentially UTC."))

(defprotocol TemporalAmountable
  (to-duration ^Duration [t] "Converts a datatype to a Duration, as the default temporal type."))

(defprotocol ArithmeticTime
  (plus [t v] "Adds an amount of v to the time t")
  (plus-millis [obj millis] "Adds the specified duration in millis")
  (plus-nanos [obj nanos] "Adds the specified duration in nanos")
  (plus-seconds [obj seconds] "Adds the specified duration in seconds")
  (minus [t v] "Subtracts an amount of v from the time t")
  (minus-millis [obj millis] "Subtracts the specified duration in millis")
  (minus-nanos [obj nanos] "Subtracts the specified duration in nanos")
  (minus-seconds [obj seconds] "Subtracts the specified duration in seconds")
  (multiply ^Duration [d v] "Multiplies a temporal unit by a scalar")
  (divide ^Duration [d v] "Divides a temporal unit by a scalar or by another temporal unit")
  (negate ^Duration [d] "Negates a temporal unit")
  (truncated-to [obj unit] "Truncates the time object to the appropriate units"))

(defprotocol TimezoneOffsettable
  (to-timezone-offset ^ZoneOffset [t] "Converts the argument to a timezone offset"))

(defprotocol Timezoneable
  (to-timezone ^ZoneId [t] "Converts the argument to a timezone"))

(defprotocol Zoneable
  (to-zone ^Temporal [t] [t tz] "Converts an unzoned temporal into one with a timezone, UTC if none is available")
  (has-zone? [t] "Indicates if this object has a Timezone associated with it")
  (zone [t] "Returns the Timezone of this temporal, or UTC if none is available"))

(defprotocol Fieldable
  (to-field ^TemporalField [f] "Converts the argument to a temporal field"))

;; This is more general that operations shared between Durations and Instants
(defprotocol HasNanos
  (get-nano ^long [n] "Returns the nanoseconds of the object"))

(defn parse-temporal
  "Parse a string to a Temporal."
  ^Temporal [^String s]
  (let [ta (.parse iso-unzoned s)]
    (if (.isSupported ta ChronoField/OFFSET_SECONDS)
      (ZonedDateTime/from ta)
      (Instant/from ta))))

(defn parse-instant
  "Parse a string to an Instant. By default, this will truncate parsing to the millisecond.
  An optional time unit can be passed for different granularity, or nil for no truncation at all"
  (^Instant [^String s] (parse-instant s :ms))
  (^Instant [^String s time-unit]
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


(defn iso-str
  ([^TemporalAccessor t] (.format utc-pattern t))
  ([^TemporalAccessor t tz]
   (let [f (.withZone iso-unzoned (to-timezone tz))]
     (.format f t))))

;; Duration wrappers
(defn abs-duration
  "Returns a duration equivalent to the parameter, with a positive length."
  ^Duration [i]
  (.abs (to-duration i)))

(defn between
  "Obtains a Duration representing the duration between two temporal values."
  ^Duration [start-inclusive end-exclusive]
  (Duration/between (to-instant start-inclusive) (to-instant end-exclusive)))

(defn compare-duration
  "Compares 2 durations, returning <0 when d1<d2, >0 when d1>d2 and 0 when d1=d2"
  [d1 d2]
  (.compareTo (to-duration d1) (to-duration d2)))

(defn get-by-unit
  "Get the value of the requested unit, eg. :ms for milliseconds"
  [d unit]
  (.get (to-duration d) (to-chrono unit)))

(defn get-seconds
  "Gets the number of seconds in the duration"
  [d]
  (.getSeconds (to-duration d)))

(defn get-units
  "Gets the set of units supported by this duration"
  [d]
  (->> (to-duration d)
       (.getUnits)
       (map constants/keyword-units)))

(defn negative?
  "Checks if this duration is negative, excluding zero"
  [d]
  (.isNegative (to-duration d)))

(defn positive?
  "Checks if this duration is positive, excluding zero"
  [d]
  (.isPositive (to-duration d)))

(defn zero-time?
  "Checks if this duration is zero length"
  [d]
  (.isZero (to-duration d)))

(defn minus-days
  "Subtracts the specified duration in days"
  ^Duration [d days]
  (.minusDays (to-duration d) days))

(defn minus-hours
  "Subtracts the specified duration in hours"
  ^Duration [d hours]
  (.minusHours (to-duration d) hours))

(defn minus-minutes
  "Subtracts the specified duration in minutes"
  ^Duration [d minutes]
  (.minusMinutes (to-duration d) minutes))

(defn duration-of
  "Obtains a Duration representing an amount in the specified unit"
  ^Duration [amount unit]
  (Duration/of amount (to-chrono unit)))

(defn of-days
  "Obtains a Duration of a number of days"
  ^Duration [days]
  (Duration/ofDays days))

(defn of-hours
  "Obtains a Duration of a number of hours"
  ^Duration [hours]
  (Duration/ofHours hours))

(defn of-millis
  "Obtains a Duration of a number of milliseconds"
  ^Duration [millis]
  (Duration/ofMillis millis))

(defn of-minutes
  "Obtains a Duration of a number of minutes"
  ^Duration [minutes]
  (Duration/ofMinutes minutes))

(defn of-nanos
  "Obtains a Duration of a number of nanoseconds"
  ^Duration [nanos]
  (Duration/ofNanos nanos))

(defn of-seconds
  "Obtains a Duration of a number of seconds"
  (^Duration [seconds]
   (Duration/ofSeconds seconds))
  (^Duration [seconds nano-adjustment]
   (Duration/ofSeconds seconds nano-adjustment)))

(defn parse-duration
  "Parses a text string into a Duration with format such as P1DT2H3M4.5S"
  ^Duration [text]
  (Duration/parse text))

(defn plus-days
  "Adds the specified duration in days"
  ^Duration [d days-to-add]
  (.plusDays (to-duration d) days-to-add))

(defn plus-hours
  "Adds the specified duration in hours"
  ^Duration [d hours-to-add]
  (.plusHours (to-duration d) hours-to-add))

(defn plus-minutes
  "Adds the specified duration in minutes"
  ^Duration [d minutes-to-add]
  (.plusMinutes (to-duration d) minutes-to-add))

(defn to-days
  "The number of days in a duration"
  [d]
  (.toDays (to-duration d)))

(defn to-days-part
  "The part of a duration that is a complete number of days. The same as `to-days`."
  [d]
  (.toDaysPart (to-duration d)))

(defn to-hours
  "Number of hours in a duration"
  [d]
  (.toHours (to-duration d)))

(defn to-hours-part
  "Number of hours in a duration after removing the complete days"
  [d]
  (.toHoursPart (to-duration d)))

(defn to-millis
  "Number of milliseconds in a duration"
  [d]
  (.toMillis (to-duration d)))

(defn to-millis-part
  "Number of milliseconds in a duration after removing the whole minutes"
  [d]
  (.toMillisPart (to-duration d)))

(defn to-minutes
  "Number of minutes in a duration"
  [d]
  (.toMinutes (to-duration d)))

(defn to-minutes-part
  "Number of minutes in a duration after removing the whole hours"
  [d]
  (.toMinutesPart (to-duration d)))

(defn to-nanos
  "Number of nanoseconds in a duration. If this is too large then an exception is thrown."
  [d]
  (.toNanos (to-duration d)))

(defn to-nanos-part
  "Number of nanoseconds in a duration after removing the whole seconds"
  [d]
  (.toNanosPart (to-duration d)))

(defn to-seconds
  "Number of seconds in a duration"
  [d]
  (.toSeconds (to-duration d)))

(defn to-seconds-part
  "Number of seconds in a duration after removing the whole minutes"
  [d]
  (.toSecondsPart (to-duration d)))

(defn duration-str
  "Converts an object that can be treated as a duration into a duration string"
  [d]
  (str (to-duration d)))

(defn with-nanos
  "Creates a copy of a duration with the nanoseconds set to the provided value"
  ^Duration [d nano-of-second]
  (.withNanos (to-duration d) nano-of-second))

(defn with-seconds
  "Creates a copy of a duration with the seconds set to the provided value"
  ^Duration [d seconds]
  (.withSeconds (to-duration d) seconds))


;; Instant/temporal wrappers

(defn at-offset
  "Combines this instant with an offset to create an OffsetDateTime."
  ^OffsetDateTime [i offset]
  (.atOffset (to-instant i) (to-timezone-offset offset)))

(defn at-zone
  "Combines this instant with a time-zone to create a ZonedDateTime."
  ^ZonedDateTime [i zone]
  (.atZone (to-instant i) (to-timezone zone)))

(defn compare-to
  "Compares this instant to the specified instant."
  ^long [i other]
  (long (.compareTo (to-instant i) (to-instant other))))

(defn get-epoch-second
  "Gets the number of seconds from the Java epoch of 1970-01-01T00:00:00Z."
  ^long [i]
  (.getEpochSecond (to-instant i)))

(defn get-field
  "Gets the value of the specified field from this instant as a long. Returns nil if not supported."
  ^long [i field]
  (let [temp (to-temporal i)
        f (to-field field)]
    (if (.isSupported temp f)
      (.getLong temp f)
      (let [zi (to-zone (to-instant temp))]
        (when (.isSupported zi f)
          (.getLong zi f))))))

(defn after?
  "Checks if this instant is after the specified instant."
  [i other]
  (.isAfter (to-instant i) (to-instant other)))

(defn before?
  "Checks if this instant is before the specified instant."
  [i other]
  (.isBefore (to-instant i) (to-instant other)))

(defn field-supported?
  "Checks if the specified field is supported."
  [i field]
  (.isSupported (to-instant i) (to-field field)))

(defn unit-supported?
  "Checks if the specified unit is supported."
  [i unit]
  (.isSupported (to-instant i) (to-chrono unit)))

(defn supported?
  [i unit-or-field]
  (if (instance? TemporalField unit-or-field)
    (.isSupported (to-instant i) ^TemporalField unit-or-field)
    (.isSupported (to-instant i) ^TemporalUnit unit-or-field)))

(defn value-range
  "Gets the range of valid values for the specified field as a map."
  [i field]
  (let [r ^ValueRange (.range (to-instant i) (to-field field))]
    {:min (.getMinimum r)
     :max (.getMaximum r)
     :largest-min (.getLargestMinimum r)
     :smallest-max (.getSmallestMaximum r)}))

(defn value-range-object
  "Gets the range of valid values for the specified field."
  ^ValueRange [i field]
  (.range (to-instant i) (to-field field)))

(defn to-epoch-milli
  "Converts this instant to the number of milliseconds from the epoch of 1970-01-01T00:00:00Z."
  [i]
  (.toEpochMilli (to-instant i)))

(defn until
  "Calculates the amount of time until another instant in terms of the specified unit."
  [i end-exclusive unit]
  (.until (to-instant i) (to-instant end-exclusive) (to-chrono unit)))

(defn adjust
  "Returns an adjusted copy of this instant."
  ^Instant [i adjuster]
  (let [inst (to-instant i)]
    (cond
      (instance? TemporalAdjuster adjuster) (.with inst ^TemporalAdjuster adjuster)
      (fn? adjuster) (.with inst ^TemporalAdjuster (reify TemporalAdjuster
                                                     (adjustInto [_ t]
                                                       (adjuster t))))
      :else (throw (ex-info (str "Unable to use as an adjuster: " adjuster) {:object adjuster})))))

(defn with
  "Returns a copy of this instant with the specified field set to a new value."
  ^Instant [i field new-value]
  (.with (to-instant i) (to-field field) (long new-value)))

;; ZonedDateTime wrappers

;; TODO


(defn now
  "Convenience to returns the instant for the current system time."
  []
  (Instant/now))

(defn convert
  "Converts an instant to another type"
  (^Temporal [i t] (convert i t constants/utc))
  (^Temporal [i t z]
   (let [inst (to-zone i z)]
     (case t
       (:hijrah :hijrah-date) (HijrahDate/from inst)
       (:jp :japan :japanese :japanese-date) (JapaneseDate/from inst)
       :local-date (LocalDate/from inst)
       (:local :local-dt :local-date-time) (LocalDateTime/from inst)
       (:minguo :minguo-date) (MinguoDate/from inst)
       (:offset :offset-time) (OffsetTime/from inst)
       (:thai :thai-buddist :thai-buddist-date) (ThaiBuddhistDate/from inst)
       (:year :yr :y)  (Year/from inst)
       (:year-month :ym) (YearMonth/from inst)
       (:zoned-date-time :zoned-dt :zoned :zone) (ZonedDateTime/from inst)))))
