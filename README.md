# QTime
## A java.time wrapper!
<img src="https://github.com/user-attachments/assets/a6c53fc2-77d7-4213-b32d-c7af51beb46c" alt="The Persistence of Memory" width="375" height="285" align="right"></img>


This is a small library for time operations, based on [`java.time`](https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/time/package-summary.html).

It works with timestamps in UTC and durations, converting appropriately whenever possible. It is aware of other time objects, such as Joda Time, `java.util.Date`, string representations, and milliseconds since the Epoch, and will convert them to `java.time` objects whenever possible.

It also provides wrappers for each of the member functions around timestamps and durations, allowing for easier interoperability with the rest of the `java.time.*` packages.

The original Java time objects, such as `java.util.Date` and `java.util.Calendar`, were minimal and proved to be inadequate, leading to the open-source [Joda Time](https://www.joda.org/joda-time/) library. This was the foundation of [JSR-310](https://jcp.org/en/jsr/detail?id=310), which brought a fully capable time library into Java.

Consequently, these other libraries are deprecated, but are still in common use. QTime provides a simple way to use the new `java.time` library while still being able to work with these older libraries. QTime will convert other time formats into `java.time` objects though exporting to these legacy objects is not handled. This should typically be done by exporting milliseconds since the Epoch, which can be handled by all of the time libraries.

## Timestamps
Moments in time are described by the Java interface `java.time.Temporal`. Timestamps are handled by the `java.util.Instant` class, which implements `Temporal`, and are the main temporal object used by QTime.

Most common timestamps can be used in any function that expects a `Temporal` object, and QTime will convert it to an `Instant` whenever possible.
- `java.util.Date`
- `org.joda.time.ReadableDateTime`
- `String` - in ISO-8601 format
- `long` - milliseconds since the Unix Epoch (1970-01-01T00:00:00Z)

## Durations
Periods of time are described by the Java interface `java.time.TemporalAmount`. These are managed in QTime by the `java.time.Duration` class.

The main other implementation of `TemporalAmount` is `java.time.Period`. This is very similar to `Duration`, but automatically handles infrequent adjustments, such as leap years. When using QTime, `Periods` are converted into `Duration`. Long values are converted into `Duration` as milliseconds.

## License

Copyright © 2023-2025 Paula Gearon

Distributed under the Eclipse Public License version 2.0.
