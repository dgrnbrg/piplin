---
title: "Modules"
layout: article
---
## Why modules?

State machines are everywhere in hardware. Piplin modules are a way to encapsulate and compose state machines. They can also be used to encapsulate purely functional logic.

### Modules as functions

Let's take a simple Piplin function, an incrementer, and see how it can be used as a module:

{% highlight clojure %}
(defn inc-function [x] (+ 1 x))

(def inc-module (modulize :inc-module {:output (fnk [input] (+ 1 input))} {}))

(assert (= (inc-function 1) 2))
(assert (= (:output (inc-module :input 1)) 2))
{% endhighlight %}

Modules are created using the `modulize` function. They take an optional keyword name and two maps.

The first map is the connectivity graph. Its keys are the names of signals, and its values are `fnk`s that compute the signals. The `fnk`s will automatically be invoked by passing the value returned by the `fnk` with that name to all of the `fnk`s that have that value in their argument lists. Any `fnk` that returns a Piplin type will automatically be accesible as a simulatable and synthesizable wire.

The second map declares the registers. Since combinational paths are forbidden, we must declare which of the nodes in the connectivity graph are state elements. Every cycle in the connectivity graph must have at least one register to avoid infinite/unstable loops.

The `modulize` function returns a map whose keys are all of the keys of `fnk`s that returned Piplin types, and whose values are those types. For example, you can see in the example about that invoking `inc-module` returns a map `{:output 2}`.

### Modules as state machines

Now that we've see simple combinational modules, let's look at a module that has state. We'll make a simple counter:

{% highlight clojure %}
(def counter-module
  (modulize :counter
    {:output (fnk [output] (inc output))}
    {:output ((uintm 8) 0)))

(assert (= (->> (sim (compile-root counter-module) 9)
                (map #(get % [:counter :output])))
           (map (uintm 8) (range 10))))
{% endhighlight %}

`counter-module` demonstrates a very simple use of state - we can insert a register at the `:output` node so that we can actually compute a feedback cycle. The register is declared in the second map, by specifying the node to be registered as the key, and its initial value as the val. If we tried to make the counter module without specifying that `:output` is a register, then we would get an exception about cyclic graphs! Remember: in Piplin, all logic must be computed, and then all registers get updated. This repetition is the model of computation.

`compile-root` is an important function. Try invoking `(counter-module)`. You will get a result, but it only contains the register output of the `counter-module`. Where did the `inc` go? `compile-root` is the function that you should use to invoke the root module of your design, because it is able to capture all of the uses of registers and memories in your design, so that they can be properly simulated or synthesized to Verilog. `compile-root` does this by binding a special thread-local variable that all of the module instantiations store their logic into as a side effect of being invoked.

We saw `sim` in the introduction, but I want to point out one more important aspect of it here: the keys from each cycle of the simulation results are vectors, whose elements represent the module hierarchy and register name of the signal. We can see here how every cycle's results are stored as `[:counter :output]`, since the module's name is `:counter` and its register is `:output`. We will see nested modules in the next section.

### Composing modules

Although modules can have names generated for them, if you do this, you will not easily be able to refer to particular signals within the module. We will make a module that outputs the sum 1, 1+2, 1+2+3, 1+2+3+4, etc:

{% highlight clojure %}
(def summer
  (modulize :summer
    {:n (fnk [n x] (+ n x))
     :sub-counter (fnk []
                    (counter-module))
     :x (fnk [sub-counter]
          (:output sub-counter))}
    {:n ((uintm 8) 0)}))

(require 'clojure.pprint)
(clojure.pprint/pprint
  (sim (compile-root summer) 10))
{% endhighlight %}

Here we can see how submodules are handled. Submodules are always instantiated within the `fnk`s of their parent module. This is necessary so that their proper place in the hierarchy can be inferred, and so that their state elements can be captured by `compile-root`. You can see that the `:sub-module` key in the `summer` module is not included in the module's output map (just try `(summer)` and see how it has only 2 keys: `:n` and `:x`).

When you look at the output of the simulation, you can see the traces of the signals in `summer` and the trace of the output of `counter-module`. Now, you can see why it is useful to name modules: it makes it easy to dig into the simulation results to see what each module in the hierarchy is doing.

### Simulating and testing modules

Now that you have seen how to construct modules, build hierarchies, and simulate them, you have all the tools you need to interact with modules at the REPL and in tests. Once you have got the behavior of your module, you should write some tests that assert the state of the signals during several cycles, so that if you change something later on, your test will tell you that your module is no longer behaving the way it used to.
