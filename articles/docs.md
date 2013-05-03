---
title: Documentation
layout: article
---
## Types
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/protocols.clj#L3'>piplin.protocols/typeof</a>

<code>[this]</code>

Return type obj for this obj.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types.clj#L45'>piplin.core/kindof</a>

<code>[a]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types.clj#L305'>piplin.core/anontype</a>

<code>[kind]</code>

Returns an anonymous type of
  the given kind. Useful to promote
  to jvm types.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types.clj#L404'>piplin.core/cast</a>

<code>[type expr]</code>

Converts the given expr to the given type.
  If the expr is immediate, this returns the
  same thing as (promote type expr). If the
  expr is a runtime value, this returns an
  astfrag.

## Modules
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/modules.clj#L86'>piplin.core/modulize</a>

<code>[computation state] [module-name computation state]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/modules.clj#L172'>piplin.core/compile-root</a>

<code>[module &nbsp; inputs]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/modules.clj#L196'>piplin.core/input</a>

<code>[name type]</code>



## Simulation
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/modules.clj#L254'>piplin.core/sim</a>

<code>[compiled-module cycles]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/vcd.clj#L85'>piplin.core/spit-trace</a>

<code>[path trace]</code>

Writes the trace to the file with the
  given name (path is a String)


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/vcd.clj#L91'>piplin.core/trace->gtkwave</a>

<code>[trace]</code>

Opens the given trace in GTKWave

## Verilog
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/verilog.clj#L952'>piplin.core/->verilog</a>

<code>[compiled-module outputs]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/verilog.clj#L1068'>piplin.core/verify</a>

<code>[module cycles]</code>



## Bits
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/bits.clj#L23'>piplin.core/bits</a>

<code>[n]</code>

Make a new bits type object.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/bits.clj#L56'>piplin.core/bit-width-of</a>


Takes a type and returns the number
  of bits needed to represent that type


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/bits.clj#L79'>piplin.core/serialize</a>

<code>[expr]</code>

Gets the bits representation of its argument. Supports AST frags.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/bits.clj#L90'>piplin.core/deserialize</a>

<code>[type bits]</code>

Takes a type and bits, and converts the bits
  to the given type.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/bits.clj#L138'>piplin.core/bit-cat</a>

<code>[] [bs] [b1 b2] [b1 b2 &nbsp; more]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/bits.clj#L124'>piplin.core/bit-slice</a>

<code>[expr low high]</code>

Takes an expr of type bits and returns a subrange
  of the bits.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L73'>piplin.core/bit-and</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L74'>piplin.core/bit-or</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L75'>piplin.core/bit-xor</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L77'>piplin.core/bit-not</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L103'>piplin.core/bit-shift-left</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L104'>piplin.core/bit-shift-right</a>




## Boolean Logic/Equality
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/binops.clj#L99'>piplin.core/=</a>


= is a very common function. It must be
  implemented explicitly (rather than using
  the def-binary-binop function) in order to
  explicitly check whether they're both not
  ASTNodes, and if so to delegate to Clojure's
  =. This is because not every object subject to
  = participates in the piplin typesystem,
  whereas all numbers do participate, so this
  isn't an issue for >, &lt;, &lt;=, >=, etc.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/boolean.clj#L26'>piplin.core/not=</a>

<code>[x] [x y] [x y &nbsp; more]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/boolean.clj#L31'>piplin.core/and</a>

<code>[] [x] [x y] [x y &nbsp; more]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/boolean.clj#L45'>piplin.core/or</a>

<code>[] [x] [x y] [x y &nbsp; more]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/boolean.clj#L17'>piplin.core/not</a>

<code>[x]</code>

not is important to implement ;-)

## Conditionals
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/mux.clj#L25'>piplin.core/mux2</a>

<code>[sel v1 v2]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/mux.clj#L52'>piplin.core/cond</a>

<code>[&nbsp; more]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/mux.clj#L62'>piplin.core/condp</a>

<code>[pred expr &nbsp; clauses]</code>



## Math Ops
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L70'>piplin.core/+</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L71'>piplin.core/-</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L72'>piplin.core/*</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L87'>piplin.core/inc</a>

<code>[x]</code>

Increments x


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L92'>piplin.core/dec</a>

<code>[x]</code>

Decrements x


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L100'>piplin.core/&lt;</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L102'>piplin.core/&lt;=</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L99'>piplin.core/></a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L101'>piplin.core/>=</a>





### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L106'>piplin.core/zero?</a>

<code>[x]</code>

Returns true if x is 0


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L111'>piplin.core/pos?</a>

<code>[x]</code>

Returns true if x is positive


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/core_impl.clj#L116'>piplin.core/neg?</a>

<code>[x]</code>

Returns true if x is negative

## Numeric Types
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/uintm.clj#L13'>piplin.core/uintm</a>

<code>[n]</code>

Makes a new uintm type object with the
  given number of bits.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/sints.clj#L14'>piplin.core/sints</a>

<code>[n]</code>

Makes a new sints type object with the
  given number of bits.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/sints.clj#L41'>piplin.core/sign-extend</a>

<code>[width' num]</code>

Takes an sints and a longer width and sign-extends
  the sints.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/sfxpts.clj#L10'>piplin.core/sfxpts</a>

<code>[i f]</code>

Makes a new sfxpts type object
  with `i` integer bits and `f`
  fractional bits.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/complex.clj#L10'>piplin.core/complex</a>

<code>[real imag]</code>

Makes a new complex number type
  with the real part having type `real`
  and the imaginary part having type `imag`.
  `real` and `imag` must support addition,
  subtraction, and multiplication.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/complex.clj#L20'>piplin.core/real-part</a>

<code>[complex]</code>

Returns the real part of a complex number.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/complex.clj#L30'>piplin.core/imag-part</a>

<code>[complex]</code>

Returns the real part of a complex number.

## Arrays
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/array.clj#L10'>piplin.core/array</a>

<code>[type length]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/array.clj#L165'>piplin.core/store</a>

<code>[array write-enable index v] [a we i v &nbsp; more]</code>




### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/clojure/core.clj#L177'>clojure.core/assoc</a>

<code>[map key val] [map key val &nbsp; kvs]</code>

assoc[iate]. When applied to a map, returns a new map of the
    same (hashed/sorted) type, that contains the mapping of key(s) to
    val(s). When applied to a vector, returns a new vector that
    contains val at index. Note - index must be &lt;= (count vector).


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/clojure/core.clj#L1388'>clojure.core/get</a>

<code>[map key] [map key not-found]</code>

Returns the value mapped to key, not-found or nil if key not present.

## Bundles
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/bundle.clj#L43'>piplin.core/bundle</a>

<code>[schema]</code>

Takes a map of keys to types and returns
  a bundle type with that schema.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/clojure/core.clj#L177'>clojure.core/assoc</a>

<code>[map key val] [map key val &nbsp; kvs]</code>

assoc[iate]. When applied to a map, returns a new map of the
    same (hashed/sorted) type, that contains the mapping of key(s) to
    val(s). When applied to a vector, returns a new vector that
    contains val at index. Note - index must be &lt;= (count vector).


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/clojure/core.clj#L5461'>clojure.core/update-in</a>

<code>[m [k &nbsp; ks] f &nbsp; args]</code>

'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/clojure/core.clj#L1388'>clojure.core/get</a>

<code>[map key] [map key not-found]</code>

Returns the value mapped to key, not-found or nil if key not present.

## Enums and Unions
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/enum.clj#L25'>piplin.core/enum</a>

<code>[coll &nbsp; more]</code>

Takes a collection of keywords or a map of
  keywords to bits and returns it as an enum
  type.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/union.clj#L12'>piplin.core/union</a>

<code>[schema &nbsp; backing-enum]</code>

Takes a map of keywords to types and an optional enum
  and returns a tagged union type whose keys are elements
  of the given enum or the default enum of the map's keys.


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types/union.clj#L148'>piplin.core/union-match</a>

<code>[u &nbsp; clauses]</code>

Takes a union u and a clause for each
  key in the schema. Clauses are of the form:

      (:key binding ...)

  where :key is the keyword, binding is the form
  that the value is bound to, and ... is an
  implicit do within a cond-like form.

  You can use any destructuring syntax as the
  binding form.

## Other
### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types.clj#L384'>piplin.core/log2</a>

<code>[v]</code>

Log base 2


### <a href='https://github.com/dgrnbrg/piplin/blob/316978a17f6bf0dd4fbcf44632147d2ff5227af9/src/piplin/types.clj#L365'>piplin.types/uninst</a>

<code>[pipinst]</code>

Takes a pipinst and makes it into an AST frag

