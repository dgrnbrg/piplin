---
title: "Modules"
layout: article
---
## Why modules?

State machines are everywhere in hardware. Piplin modules are a way to encapsulate and compose state machines. They can also be used to encapsulate purely functional logic.

## Modules as functions

Let's take a simple Piplin function, an incrementer, and see how it can be used as a module:

{% highlight clojure %}
(defn inc-function [x] (+ 1 x))

(def inc-module (modulize :inc-module
                  {:output (fnk [input]
                             (+ 1 input))}
                    {}))

(assert (= (inc-function 1) 2))
(assert (= (:output (inc-module :input 1)) 2))
{% endhighlight %}

Modules are created using the `modulize` function. They take an optional keyword to be their name and two maps.

The first map is the connectivity graph (in this case, it has one key, `:output`). Its keys are the names of signals, and its values are `fnk`s that compute the signals. The `fnk`s will be invoked in topological order, starting from the inputs and register values from the previous cycle, proceeding through the signals as their inputs become available. Any `fnk` that returns a Piplin type will automatically be accesible as a simulatable and synthesizable signal.

The second map declares the registers. Since combinational paths are forbidden, we must declare which of the nodes in the connectivity graph are state elements. Every cycle in the connectivity graph must have at least one register to avoid loop, which cannot be invoked in the topological order. For example, if `:a` depended on `:b` and `:b` depended on `:a`, as in `{:a (fnk [b] b), :b (fnk [a] a)}`, then at least one of `:a` or `:b` would have to be a register, so that the graph can be invoked as described above.

The `modulize` function returns a map whose keys are all of the keys of `fnk`s that returned Piplin types, and whose vals are the results of those `fnk`s. For example, you can see in the example above that invoking `inc-module` returns a map `{:output 2}`.

## Modules as state machines

Now that we've seen simple combinational modules, let's look at a module that has state. We'll make a simple counter:

{% highlight clojure %}
(def counter-module
  (modulize :counter
    {:output (fnk [output]
               (inc output))}
    {:output ((uintm 8) 0)}))

(assert (= (->> (sim (compile-root counter-module) 9)
                (map #(get % [:counter :output])))
           (map (uintm 8) (range 10))))
{% endhighlight %}

`counter-module` demonstrates a very simple use of state - we insert a register at the `:output` node so that we can compute a feedback cycle. The register is declared in the second map, by specifying the node to be registered as the key, and its initial value as the val. If we tried to make the counter module without specifying that `:output` is a register, then we would get an exception about cyclic graphs!

`compile-root` is an important function. Try invoking `(counter-module)`. You will get a result, but it only contains the register output of the `counter-module`. Where did the `inc` go? `compile-root` is the function that you should use to invoke the root module of your design, because it is able to capture all of the uses of registers and memories in your design, so that they can be properly simulated or synthesized to Verilog. `compile-root` does this by binding a special thread-local variable that all of the module instantiations store their logic into as a side effect of being invoked.

We saw `sim` in the [introduction](/articles/intro.html), but I want to point out one more important aspect of it here: the keys from each cycle of the simulation results are vectors, whose elements represent the module hierarchy and register name of the signal. We can see here how every cycle's results are stored as `[:counter :output]`, since the module's name is `:counter` and its register is `:output`. We will see nested modules in a later section.

## Defining inputs

Modules take every undefined signal as an input. We will look at a basic arithmetic logic unit (ALU) as an example.

{% highlight clojure %}
(def alu-module
  (modulize :alu
    {:output (fnk [op x y]
               (condp = op
                 0 (+ x y)
                 1 (- x y)
                 2 (* x y)))}
    {}))

(assert (= (alu-module :op 0 :x 2 :y 3) 5))
(assert (= (alu-module :op 1 :x 2 :y 3) -1))
(assert (= (alu-module :op 2 :x 2 :y 3) 6))
{% endhighlight %}

As you can see, inputs are passed to modules as key-value arguments to module function.

## Composing modules

Although modules can have names generated for them, if you do this, you will not easily be able to refer to particular signals within the module. We will make a module that outputs the sum `1`, `1 + 2`, `1 + 2 + 3`, `1 + 2 + 3 + 4`, etc:

{% highlight clojure %}
(def summer
  (modulize :summer
    {:n (fnk [n x]
          (:output (alu-module :op 0 :x n :y x)))
     :sub-counter (fnk []
                    (counter-module))
     :x (fnk [sub-counter]
          (:output sub-counter))}
    {:n ((uintm 8) 0)}))

(require 'clojure.pprint)
(clojure.pprint/pprint
  (sim (compile-root summer) 10))
{% endhighlight %}

Here we can see how submodules are handled. Submodules are always instantiated within the `fnk`s of their parent module. This is necessary so that their proper place in the hierarchy can be inferred, and so that their state elements can be captured by `compile-root`. You can see that the `:sub-counter` key in the `summer` module is not included in the module's output map because it is not a possible type for a signal (just try `(summer)` and see how it has only 2 keys: `:n` and `:x`).

When you look at the output of the simulation, you can see the traces of the signals in `summer`, `counter-module`, and `alu-module`. Now, you can see why it is useful to name modules: it makes it easy to dig into the simulation results to see what each module in the hierarchy is doing.

## Simulating and testing modules

Now that you have seen how to construct modules, build hierarchies, and simulate them, you have all the tools you need to interact with modules at the REPL and in tests. Once you have got the behavior of your module, you should write some tests that assert the state of the signals during several cycles, so that if you change something later on, your test will tell you that your module is no longer behaving the way it used to.
