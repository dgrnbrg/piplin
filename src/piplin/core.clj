(ns piplin.core)

(comment
  The basis of building the type system is to have a type lattice.
  Using a fn that takes an obj and a target type, it must be able to cast the obj to the target type or throw an exception (or be a predicate)
  This lattice could be either a logic program or a contract set of fns.
  A logic program could enumerate the most specific types of all the nodes.
  Must consider how to flow types through functions.
  )

(comment
  The AST is represented as a tree of computation, the only exception being
  structural blocks. Structural blocks create a context for forward decls,
  and (connect) links the forward decl to a fragment.)

(comment
  Type inference has 2 phases--flowing through trees, and resolving structures

  The tree-flowing occurs with AST--consider the 8 bit up-counter:

  (defn up-counter []
    (:value (structural [value (make-type (uintm 8) 0)]
      (connect value (inc value)))))

  This will give an AST fragment like:

  {:type :structural
   :decls {:value {:type [:uintm 8] :value 0}}
   :connections {:value {:type :adder
                           :lhs {:type [:forward-decl :value]}
                           :rhs {:type :int :value 1}}}
   :value {:type :extractor :structure ... :port :value}

  Here we can see the structural block is entirely made out of forward decls
  and connections. It also exposes its ports separately.)

(comment
  It's hard to see how one can actually typecheck this thing.
  Ultimately, our goal is to find the most specific type for all connections
  between functions, or determine that there is a conflict, or determine
  that it's not synthesizable and don't worry about it.

  One issue is that if we rewrite the types into the tree during the walk
  and the same subtree appears in multiple places, those subtrees will become
  different instances of those nodes. This seems to imply that the types must
  be a separate table keyed by the fragments.

  An alternative solution says that each fragment is inferred separately, and
  generates separate code/sim, and that the synth tool must merge them. This
  is probably not a good solution because it doesn't match what's most logical

  )


