(ns piplin.protocols)

(defprotocol ITyped
  "Things with types in piplin implement this."
  (typeof [this] "Return type obj for this obj.")
  (value [this] "Return the value of this")
  (pipinst? [this] "Returns true if this is an
                   instance of the type (as opposed
                   to a symbolic representation)"))

