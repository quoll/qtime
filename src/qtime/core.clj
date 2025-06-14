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
            [qtime.protocols :as protocols
                             :refer [Chronological Temporalable TemporalAmountable Zoneable
                                     Fieldable HasNanos ArithmeticTime TimezoneOffsettable
                                     Timezoneable]]
            [qtime.impl :as impl])
  (:import [clojure.lang Keyword]
           [java.util Date]
           [java.time Instant Duration ZoneId ZoneOffset OffsetDateTime ZonedDateTime
            LocalDate LocalDateTime OffsetTime Year YearMonth]
           [java.time.format DateTimeFormatter DateTimeFormatterBuilder]
           [java.time.temporal ChronoUnit ChronoField TemporalUnit TemporalField
            TemporalAmount TemporalAccessor ValueRange Temporal TemporalAdjuster
            TemporalQueries]
           [java.time.chrono HijrahDate JapaneseDate MinguoDate ThaiBuddhistDate]))

(set! *warn-on-reflection* true)

;; Chronological
(def ^{:arglists (quote (^ChronoUnit [c]))} to-chrono protocols/to-chrono)
;; Temporalable
(def ^{:arglists (quote (^Temporal [t]))} to-temporal protocols/to-temporal)
(def ^{:arglists (quote (^Instant [i]))} to-instant protocols/to-instant)
;; TemporalAmountable
(def ^{:arglists (quote (^Duration [t]))} to-duration protocols/to-duration)
;; ArithmeticTime
(def plus protocols/plus)
(def plus-millis protocols/plus-millis)
(def plus-nanos protocols/plus-nanos)
(def plus-seconds protocols/plus-seconds)
(def minus protocols/minus)
(def minus-millis protocols/minus-millis)
(def minus-nanos protocols/minus-nanos)
(def minus-seconds protocols/minus-seconds)
(def ^{:arglists (quote (^Duration [d v]))} multiply protocols/multiply)
(def ^{:arglists (quote (^Duration [d v]))} divide protocols/divide)
(def ^{:arglists (quote (^Duration [d]))} negate protocols/negate)
(def truncated-to protocols/truncated-to)
;; TimezoneOffsetable
(def ^{:arglists (quote (^ZoneOffset [t]))} to-timezone-offset protocols/to-timezone-offset)
;; Timezoneable
(def ^{:arglists (quote (^ZoneId [t]))} to-timezone protocols/to-timezone)
;; Zoneable
(def ^{:tag Temporal :arglists (quote ([t] [t tz]))} to-zone protocols/to-zone)
(def has-zone? protocols/has-zone?)
(def zone protocols/zone)
;; Fieldable
(def ^{:arglists (quote (^TemporalField [f]))} to-field protocols/to-field)
;; HasNanos
(def ^{:arglists (quote (^long [i]))} get-nano protocols/get-nano)

(def ^{:tag Instant :arglists (quote ([^String s] [^String s time-unit]))} parse-instant impl/parse-instant)

(defn ^String iso-str
  ([^TemporalAccessor t] (.format impl/utc-pattern t))
  ([^TemporalAccessor t tz]
   (let [f (.withZone impl/iso-unzoned (to-timezone tz))]
     (.format f t))))

;; Duration wrappers
(defn abs-duration
  "Returns a duration equivalent to the parameter, with a positive length."
  ^Duration [i]
  (.abs (to-duration i)))

(defn between
  "Obtains a Duration representing the duration between two temporal values."
  ^Duration [start-inclusive end-exclusive]
  (let [t1 (to-temporal start-inclusive)
        t2 (to-temporal end-exclusive)]
    (cond
      (instance? Instant t1) (Duration/between t1 (to-instant t2))
      (instance? Instant t2) (Duration/between t2 (to-instant t1))
      :else (try
              (Duration/between t1 t2)
              (catch Exception _
                (try
                  (Duration/between t2 t1)
                  (catch Exception _
                    (Duration/between (to-instant t1) (to-instant t2)))))))))

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

(defn days
  "Obtains a Duration of a number of days"
  ^Duration [days]
  (Duration/ofDays days))

(defn hours
  "Obtains a Duration of a number of hours"
  ^Duration [hours]
  (Duration/ofHours hours))

(defn millis
  "Obtains a Duration of a number of milliseconds"
  ^Duration [millis]
  (Duration/ofMillis millis))

(defn minutes
  "Obtains a Duration of a number of minutes"
  ^Duration [minutes]
  (Duration/ofMinutes minutes))

(defn nanos
  "Obtains a Duration of a number of nanoseconds"
  ^Duration [nanos]
  (Duration/ofNanos nanos))

(defn seconds
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
