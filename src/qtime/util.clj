(ns qtime.util
  (:import [java.io FileNotFoundException]))

(defn require-optional
  [req-spec]
  (try
    (require req-spec)
    (catch FileNotFoundException _)))

(defmacro when-accessible
  "If sym is an available symbol, then include the body.
  Used to define elements that depend on the availability of classes that are not direct dependencies."
  [sym & body]
  (when (resolve sym)
    `(do ~@body)))
