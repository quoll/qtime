(ns qtime.transform
  "Provides a mechanism for transforming various temporal objects into an instant for manupulation.
  and then transforming them back into the original object. This makes presumptions that any temporal without
  a timezone is in UTC."
  (:require [qtime.constants :as constants :refer [utc]])
  (:import [java.time Instant ZoneId ZoneOffset ZonedDateTime LocalDateTime
            LocalDate LocalTime OffsetDateTime OffsetTime Year YearMonth DateTimeException]
           [java.time.temporal Temporal TemporalAccessor ChronoField TemporalQueries]
           [java.time.chrono ChronoLocalDate HijrahDate JapaneseDate MinguoDate ThaiBuddhistDate]))

(defprotocol InstantTransform
  (tx-to-instant ^Instant[^TemporalAccessor ta]
    "Converts a TemporalAccessor to an instant, specific to each type of temporal object"))

(defprotocol TimeTransform
  (tx [^TemporalAccessor t]
    "Transforms a time into a pair formed of an Instant and an inverse transform for time manipulations"))

(defn find-zone
  "Robust mechanism for determinine the zone or a TemporalAccessor, if one exists."
  [^TemporalAccessor ta]
  (.query ta (TemporalQueries/zone)))

(defn chrono-date-to-instant
  [^ChronoLocalDate d]
  (Instant/from (.atStartOfDay (LocalDate/ofEpochDay (.toEpochDay d)) utc)))

;; opinion: if you don't have a zone, then you are UTC. This is more stable than the system zone
(extend-protocol InstantTransform
  Instant
  (tx-to-instant [i] i)
  ZonedDateTime
  (tx-to-instant [zdt] (Instant/from zdt))
  LocalDateTime
  (tx-to-instant [ldt] (Instant/from (.atZone ldt utc)))
  LocalDate
  (tx-to-instant [ld] (Instant/from (.atStartOfDay ld utc)))
  LocalTime
  (tx-to-instant [lt] (Instant/from (.atZone (.atDate lt constants/epoch-day) utc)))
  OffsetDateTime
  (tx-to-instant [odt] (Instant/from odt))
  OffsetTime
  (tx-to-instant [ot] (Instant/from (.atDate ot constants/epoch-day)))
  Year
  (tx-to-instant [y] (Instant/from (.atStartOfDay (.atDay y 1) utc)))
  YearMonth
  (tx-to-instant [ym] (Instant/from (.atStartOfDay (.atDay ym 1) utc)))
  ChronoLocalDate
  (tx-to-instant [d] (chrono-date-to-instant d))
  TemporalAccessor
  (tx-to-instant [o]
    (if (.isSuported o ChronoField/INSTANT_SECONDS)
      (Instant/ofEpochMilli (.getLong o ChronoField/INSTANT_SECONDS))
      (Instant/from o))))

(extend-protocol TimeTransform
  Instant
  (tx [i] [i identity])
  ZonedDateTime
  (tx [zdt]
    (let [z (.getZone zdt)
          inv (fn [^Instant i] (.atZone i z))]
      [(tx-to-instant zdt) inv]))
  LocalDateTime
  (tx [ldt] [(tx-to-instant ldt) (fn [^Instant i] (LocalDateTime/ofInstant i utc))])
  LocalDate
  (tx [ld] [(tx-to-instant ld) (fn [^Instant i] (LocalDate/ofInstant i utc))])
  LocalTime
  (tx [lt]
    [(tx-to-instant lt) (fn [^Instant i] (LocalTime/ofInstant i utc))])
  OffsetDateTime
  (tx [odt]
    (let [zone (find-zone odt)]
      [(tx-to-instant odt) (fn [^Instant i] (OffsetDateTime/ofInstant i zone))]))
  OffsetTime
  (tx [ot]
    (let [zone (find-zone ot)]
      [(tx-to-instant ot) (fn [^Instant i] (OffsetTime/ofInstant i zone))]))
  Year
  (tx [y]
    [(tx-to-instant y) (fn [^Instant i] (Year/from (.atZone i utc)))])
  YearMonth
  (tx [ym]
    [(tx-to-instant ym) (fn [^Instant i]
                          (let [zi ^ZonedDateTime (.atZone i utc)]
                            (YearMonth/of (.getYear zi) (.getMonth zi))))])
  HijrahDate
  (tx [d] [(chrono-date-to-instant d) (fn [^Instant i] (HijrahDate/from (.atZone i utc)))])
  JapaneseDate
  (tx [d] [(chrono-date-to-instant d) (fn [^Instant i] (JapaneseDate/from (.atZone i utc)))])
  MinguoDate
  (tx [d] [(chrono-date-to-instant d) (fn [^Instant i] (MinguoDate/from (.atZone i utc)))])
  ThaiBuddhistDate
  (tx [d] [(chrono-date-to-instant d) (fn [^Instant i] (ThaiBuddhistDate/from (.atZone i utc)))])
  Object
  (tx [o] (throw (ex-info (str "Unknown Temporal object. No transform available: " o) {:temporal o}))))
