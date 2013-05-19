---
title: "Piplin values and deferred evaluation"
layout: article
---
## Introduction/Glossary
The goal of this guide is to help you understand how so-called "synthesizable" values returned from Piplin functions work.

- pipinst: these represent the actual binary values flowing through the wires
- AST: these represent deferred computations that need to know the values of registers or inputs before they can be computed
- module: a container that organizes Piplin values and registers. These are not discussed much in this guide; see the [modules tutorial](/articles/intro.html) for more information

## pipinsts

Let's look at some examples of pipinsts:

- `((uintm 8) 3)` is an **u**nsigned **int** with **m**odulo on overflow, 8 bits wide, with the value 3 (binary `#b0000_0011`)
- `(cast (sfxpts 7 3) 5.23)` is a **s**igned **f**i**x**ed-**p**oin**t** number that **s**atures on overflow (tops or bottoms out), with 7 bits for the integer part, and 3 bits for the fractional part, with the closest representable value to 5.23 (binary `#b0000101_001`)
- `true` is a boolean (which is 1 bit wide) (binary `#b1`)

These cover some of the primitive types. Other numeric types include `sints`, which is a **s**igned **int** with **s**aturation on overflow, and `complex`, which takes another numeric type and adjoins a complex component, so that you can use `+`, `-`, and `*` on complex numbers in hardware.

For more parameterizable types, we usually use a `def` to make the type easier to reuse:

{% highlight clojure %}
(def an-enum-type
  (enum #{:foo :bar :baz}))

(an-enum-type :foo)
;; => AST(PiplinEnum[...], :foo)
(cast an-enum-type :bar)
;; => AST(PiplinEnum[...], :bar)
(an-enum-type :baz)
;; => AST(PiplinEnum[...], :baz)
(an-enum-type :quux)
;; => Exception: "Cannot promote :quux to PiplinEnum[...]"
{% endhighlight %}

As we can see above, `enum`s are defined by providing a set of values. Then, we can promote Clojure keywords to Piplin `enum`s by invoking the `enum` like a function, or `cast`ing the keyword to the enum type.

{% highlight clojure %}
(def a-bundle-type
  (bundle {:a (anontype :boolean)
           :b an-enum-type}))

(cast a-bundle-type {:a true :b :baz})
;; => AST(Bundle[...], {:a true, :b AST(PiplinEnum[...], :baz)})
(a-bundle-type {:a true :b :baz}
;; => Exception: "{:a true :b baz} does not match schema {:a AnonType[:boolean], :b PiplinEnum[...]}"
{% endhighlight %}

In the `bundle`'s definition, you may have noticed the `(anontype :boolean)`. This is how to get the type of boolean values, i.e. `true` and `false`.

`bundle`s are constructing by `cast`ing a map to the correct bundle type. Due to an issue with aggregate types, `bundle`s must always be constructed via `cast`.

### Warning about Aggregate types

All aggregate types (`union`, `bundle`, and `array`) have a bug where you must `cast` values, instead of being able to use the type as a constructor. This will be addressed in a future version.

## Deferred Values
First, let's see how adding `uintm`s works:

{% highlight clojure %}
(assert (= (+ ((uintm 8) 3)
              ((uintm 8) 4))
           ((uintm 8) 7)))
{% endhighlight %}

### Uninst

Now, let's see what happens if we add `((uintm 8) 3)` to a register. As a debugging aid, there is a function `piplin.types/uninst` that takes a pipinst and returns a "deferred" version of that value. This will cause a typed fragment of AST to be constructed:

{% highlight clojure %}
(+ ((uintm 8) 3)
   (piplin.types/uninst ((uintm 8) 4)))
;; => AST(type: UIntM[8]
;; =>     data:
;; =>     {:args
;; =>      {:rhs
;; =>       AST(type: UIntM[8]
;; =>           data:
;; =>           {:args {:expr AST(type: UIntM[8] data: 4 )}, :op :noop} ),
;; =>       :lhs AST(type: UIntM[8] data: 3 )},
;; =>      :op :+} )
{% endhighlight %}

We can see that we end up with an AST fragment whose topmost node has type `UIntM[8]`. `data:` marks the section that describes what sort of AST this is. In this case, it's a deferred function evaluation, because it has the `:op` key, which determines the function, `+`. The `:args` key contains all the sub-fragments this one depends on. Arguments (in this case, `:lhs` and `:rhs`) have meaning determined by the `:op`.

The `uninst`'s AST representation has the `:op` set to `:noop` and one argument, `:expr`, which is `((uintm 8) 4)`.

### Registers and Inputs

Here are examples of what module registers and inputs look like when printed:

{% highlight clojure %}
;; a register named :foo
AST(UIntM[8], {:port-type :register, :port :foo, :args {}, :op :port})

;; an input named :foo
AST(UIntM[8], {:port-type :input, :port :foo, :args {}, :op :port})
{% endhighlight %}

## Tying it all together

When you use a function like `+`, `assoc`, or `nth`, the value returned will vary depending on the arguments. If the arguments were Clojure values, the return values will be as usual in Clojure; however, if the arguments are Piplin values, the return value could be either a pipinst or a deferred AST fragment. If all of the arguments are pipinsts, then they will immediately be evaluated. If one or more of the arguments is a deferred AST fragment, then a new deferred AST fragment will be returned. This records what computation to do once the deferred values are available.

This design is meant to make it easier to write typed Piplin code using regular Clojure control flow, in order to make it easier to port algorithms that are initially designed and written as software, i.e. as plain Clojure. As you develop your design, you will transition Clojure values into Piplin values. Then, instead of interacting with your functions on the REPL or in unit tests, you can wire them into modules, so that they can be further tested and simulated, then synthesized.

Another benefit of evaluating pipinsts immediately is that it reduces the amount of computation that is done at runtime, since everything that can be computed before simulation or synthesis will be precomputed.

Now you should know how Piplin is able to run at the REPL, in simulations, and in Verilog. Hopefully, this will help you better understand, interpret, and debug your designs.
