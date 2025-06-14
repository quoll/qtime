(ns qtime.protocols
  "Protocols for QTime, defining the core functionalities needed for time management and manipulation."
  (:require [qtime.util :refer [require-optional when-accessible]])
  (:import [clojure.lang Keyword]
           [java.util Date]
           [java.time Instant Duration ZoneId ZoneOffset OffsetDateTime ZonedDateTime
            LocalDate LocalDateTime OffsetTime Year YearMonth]
           [java.time.temporal ChronoUnit ChronoField TemporalUnit TemporalField
            TemporalAmount TemporalAccessor ValueRange Temporal TemporalAdjuster
            TemporalQueries]
           [java.time.chrono HijrahDate JapaneseDate MinguoDate ThaiBuddhistDate]))


(require-optional 'clj-time.core)


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

