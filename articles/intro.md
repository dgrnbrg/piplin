---
title: "Getting Started"
layout: article
---
## What is Piplin?

Piplin is a way to use Clojure to create hardware. Using Piplin, you can describe and simulate logic using the full power of Clojure. These descriptions can then be compiled to put them onto an FPGA. Currently, Piplin is in beta and as such APIs may be subject to change.

### For Software Developers

Piplin is a great way for you to use your knowledge of programming, algorithms, and JVM libraries to describe the structure of hardware. It is useful to have some knowledge of computer architecture or digital logic circuits with registers. [Here is a short introduction on digital logic](http://www.swarthmore.edu/NatSci/echeeve1/Ref/Digital/DigitalIntro.html). I recommend skipping over the parts explaining Karnaugh maps, as Piplin does that automatically.

Piplin doesn't do everything yet, though. You'll need to provide the file layer of integration with the FPGA, although this document links to a sample one. Also, Piplin will allow you to generate hardware that runs too slowly, so you may need to [manually pipeline your designs](http://www.cs.iastate.edu/~prabhu/Tutorial/PIPELINE/pipe_title.html).

### For Hardware Developers

Piplin is a pleasant improvement on Verilog and VHDL, with future potential to gain features of BlueSpec, OpenCL, and other HLS tools. It uses structural logic semantics to describe circuits using only flip-flops, memories, and combinational logic. Its primary benefits are:

1. Piplin's powerful and extensible type system makes it impossible to write many common errors, such as connecting busses of different widths or busses of the same width carrying mismatched data formats.
2. Clojure offers many powerful tools for organizing logic and generating complex structures. These let you use one language for design, high level composition, and testing, and offer a far richer syntax for describing hardware than existing HDLs.
3. Piplin's representation of logic is simple and manipulable. This allows powerful optimizations, such as phase-shifted time-slicing of circular pipelines and processor stage bypassing, to be automatically generated in designs.

If you don't know Clojure, I recommend going to the [Clojure Doc Site](http://clojure-doc.org) to learn about the language, its library, installation, and the associated tools. An excellent quick reference of the core APIs with examples and source code can be found at [Clojuredocs](http://clojuredocs.org/quickref/Clojure%20Core).

## A Brief Introduction to Piplin

You can get a working project with all of the code in this tutorial from [Piplin Starter](https://github.com/dgrnbrg/piplin-starter), which is a preconfigured sample project for Piplin on Github. If you'd like to use Piplin yourself, just add `[piplin "0.1.0"]` as a `:dependency` in your project.clj.

### Example ns form

First, we'll set up the `ns` form for this piplin project. You can copy the sample below, changing the namespace to yours:

{% highlight clojure %}
(ns piplin.my-first-project
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-left bit-shift-right pos? neg? zero?])
  (:use piplin.core plumbing.core))
{% endhighlight %}

### Piplin code

Let's look at some real Piplin code, now:

{% highlight clojure %}
(def johnson-counter
  ;We can convert maps of data to simulatable/synthesizable objects by "modulizing" them
  (modulize :johnson ;:johnson is the name of this module
    ;This map specifies all of the logic. fnk is like fn, but it takes only a single
    ;argument, which is a map. It then extracts keys from the maps in the argument
    ;position. Because we have lots of names wires, fnk is how we specify logic, since
    ;it allows us to succinctly write complex dataflows.
    {:q (fnk [q direction]
             (mux2 direction ;mux2 is like if, but synthesizable
               (bit-cat
                 (bit-slice q 1 4)
                 (bit-not (bit-slice q 0 1)))
               (bit-cat
                 (bit-not (bit-slice q 3 4))
                 (bit-slice q 0 3))))}
    ;This map specifies registers. You cannot have cycles in your logic, unless they're
    ;split by registers.
    {:q #b0000}))
{% endhighlight %}

Note: if you're unfamiliar with `fnk`, check out [Prismatic's documentation here](https://github.com/Prismatic/plumbing#bring-on-defnk).

The fundamental concept of Piplin's modules is this: every cycle, all the inputs are registers are read for their values. Then, all the logic runs to determine the values of the registers for the next cycle. This repeats forever (in hardware) or for a fixed number of cycles (in simulation). Logic is specified as a graph in the first map, and registers, along with their default values, are specified in the second map.

Here we are introduced to the `bits` type in Piplin. `#b00_00` is the syntax to declare a bit literal. Underscores have no meaning, and are just used as convenient visual separators. In this example, we'd need to provide an input, `direction`, which is a boolean, in order to synthesize it. Let's make another module to provide a direction.

{% highlight clojure %}
(def johnson-director
  (modulize :root
    ;This wire has no inputs; it just constructs a submodule
    {:output (fnk []
                  (:q (johnson-counter :direction true)))}
    ;No additional registers defined
    {}))
{% endhighlight %}

To use a module as a submodule, you simply invoke it like a function, using keyword arguments to specify all the inputs. This returns a map of all the wires and registers defined in that module for you to read from. In this case, we immediatly grab the `:q` key to return as the output of our `johnson-director`.

It's important to notice that the submodule is created inside the `fnk`! This is necessary so that Piplin can infer the hierarchy of modules, so that you can get easier-to-understand simulation and synthesis results. If you try to invoke a module outside of a `fnk` that's in another module, you'll get an error.

Let's try simulating this!

### Simulation

To simulate your project, use the following code:

{% highlight clojure %}
(def compiled-module (compile-root johnson-director))

(sim compiled-module 100)
{% endhighlight %}

This runs the simulation for 100 cycles, and returns the trace of the simulation. A trace is a seq of maps, where each map is a cycle, and the keys of the map correspond to the registers in the design. The keys of the maps are vectors with the names of all the containing modules in hierarchical order, with the name of the register or wire in the leftmost key.

To visualize the trace, you can use the free software GTKWave. If you have GTKWave installed, you can use `(trace->gtkwave the-trace)` to open the trace in GTKWave, or you can save the trace using `spit-trace`. Usually, the GTKWave will not display any signals by default. To add signals, click on "logic" in the upper-left pane, then drag signals from the lower-left pane into the "signals" box. You should then see the signals appear in the black and green signal box.

### Verification

Now that you're satisfied that the module is behaving as you expect, you can verify it. Verification allows you to simultaneously simulate the module in Clojure and Verilog, to ensure that the behavior in both languages is identical. Let's try that now:

{% highlight clojure %}
;Verify takes an uncompiled module that requires no inputs and a number of cycles
;It returns the Verilog source code to verify that the results are identical
(spit "tmp" (verify johnson-director 100))

;We'll just shell out with clojure
(require 'clojure.java.shell)
(clojure.java.shell/sh "iverilog" "tmp")
;You should see "tests passed" if the simulations matched
(clojure.java.shell/sh "./a.out")
{% endhighlight %}

iverilog is the Icarus Verilog Simulator's compiler. By default, it writes its compiled output to `a.out`. When you run `a.out`, you should see it print `tests passed`, or else you'll see information on which cycle the simualtion mismatched, and why.

### Synthesis

After you're satisfied with the results, you'll convert the module to Verilog. We're going to convert the `johnson-counter` to Verilog so that the design that uses this can select the direction at runtime:

{% highlight clojure %}
(->verilog (compile-root johnson-counter
                         :direction (input "dir" (anontype :boolean)))
           {[:johnson :q] "out"})
{% endhighlight %}

Here's the first example of the `input` function. This function declares a non-simulatable input, that will be specified by some external Verilog code. `input` requires the name of the input port in Verilog, and the type of the input. Most types in Piplin have first-class constructors, except for booleans. For booleans, simply use the `anontype` function with argument `:boolean` to declare a boolean type.

`->verilog` is the function that converts a compiled module to Verilog code. Remember how earlier I mentioned that modules names form a hierarchy to identify registers? You can use those hierarchical names to pull wires and registers to be outputs of the Verilog module. In this case, we're declaring the `:q` output of the `:johnson` module to be attached to an output port called `out` in Verilog.

This generated Verilog can then be used with normal HDL design tools to synthesize hardware!

## What's next?

Now that you've had a tour of how to use Piplin, you can explore more APIs in Piplin!

### Other scalar types

Piplin has synthesizable enums, multiple kinds of integers with different behaviors on overflow/underflow, fixed point numbers, and complex numbers.

### Aggregate types

Piplin has bundles (like structs) and unions, which form the basis of a powerful, flexible system for specifying interfaces between interesting functions/modules in your design.

### Synthesizable functions

Piplin has synthesizable versions of many, many Clojure functions. For numerics, it has `+`, `-`, `*`, `pos?`, `neg?`, `zero?`, `bit-shift-left`, `bit-shift-right`, `<`, `<=`, `>`, and `>=`. For control flow and miscellaneous logic, it has `mux2`, `cond`, `condp`, `=`, `not=`, and `not`. `cast` can convert Clojure types to Piplin types (i.e. keywords to enums, longs to `uintm`s, and floats to fixed points).

You can (and should!) use regular Clojure functions, maps, and namespaces to organize your hardware code.
