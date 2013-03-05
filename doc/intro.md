# What is Piplin?

Piplin is a way to use Clojure to create hardware. Using Piplin, you can describe and simulate logic using the full power of Clojure. These descriptions can then be compiled to put them onto an FPGA. Currently, Piplin is in beta and as such APIs may be subject to change.

## For Software Developers

Piplin is a great way for you to use your knowledge of programming, algorithms, and JVM libraries to describe the structure of hardware. It is useful to have some knowledge of computer architecture or digital logic circuits with registers. [Here is a short introduction on digital logic](http://www.swarthmore.edu/NatSci/echeeve1/Ref/Digital/DigitalIntro.html). I recommend skipping over the parts explaining Karnaugh maps, as Piplin does that automatically.

Piplin doesn't do everything yet, though. You'll need to provide the file layer of integration with the FPGA, although this document links to a sample one. Also, Piplin will allow you to generate hardware that runs too slowly, so you may need to [manually pipeline your designs](http://www.cs.iastate.edu/~prabhu/Tutorial/PIPELINE/pipe_title.html).

## For Hardware Developers

Piplin is a pleasant improvement on Verilog and VHDL, with future potential to gain features of BlueSpec, OpenCL, and other HLS tools. It uses structural logic semantics to describe circuits using only flip-flops, memories, and combinational logic. It's primary benefits are:

1. Piplin's powerful and extensible type system makes it impossible to write many common errors, such as connecting busses of different widths or busses of the same width carrying mismatched data formats.
2. Clojure offers many powerful tools for organizing logic and generating complex structures. These let you use one language for design, high level composition, and testing, and offer a far richer syntax for describing hardware than existing HDLs.
3. Piplin's representation of logic is simple and manipulable. This allows powerful optimizations, such as phase-shifted time-slicing of circular pipelines and processor stage bypassing, to be automatically generated in designs.

If you don't know Clojure, I recommend going to the [Clojure Doc Site](http://clojure-doc.org) to learn about the language, its library, installation, and the associated tools. An excellent quick reference of the core APIs with examples and source code can be found at [Clojuredocs](http://clojuredocs.org/quickref/Clojure%20Core).

# Getting Started

I have chosen the [Digilent Nexys 3](http://www.digilentinc.com/Products/Detail.cfm?NavPath=2,400,897&Prod=NEXYS3&CFID=277304&CFTOKEN=75901224) as the initial standard platform for experimenting with Piplin. It has a big FPGA with lots of capacity, plenty of RAM and non-volatile memory, video output, buttons and LEDs, USB, Ethernet, and expansion ports with a host of add-ons.

The Nexys 3 uses a Xilinx FPGA that can be programmed using the free [Xilinx ISE WebPACK](http://www.xilinx.com/products/design-tools/ise-design-suite/ise-webpack.htm). This software works for Windows and Linux. To install it on Ubuntu, you'll need to Google for directions--there are many blog posts that explain the extra packages required and scripts. [Here is one example post](http://blog.2gn.com/electronics/xilinx-ise-14-2-on-ubuntu-12-04/).

Piplin generates Verilog code, which is then fed into the ISE WebPACK to produce the actual bitstream that can be programmed onto the FPGA. I recommend the open source Verilog simulator IVerilog for checking that your generated hardware matches your simulation. You can install this with `brew install icarus-verilog` on Mac, or `apt-get install iverilog` on Ubuntu.

In order to copy the bitstream onto the Nexys 3, you'll need to install [Digilent Adept](http://www.digilentinc.com/Products/Detail.cfm?NavPath=2,66,828&Prod=ADEPT2), which is the tool that lets you interact with the Digilent board over USB from Windows and Linux.

Once you've got that, you can clone [this sample project](https://github.com/dgrnbrg/nexys6-sample) I've created for the Nexys 3. This sample project provides access to the VGA port at XVGA resolution, and provides access to the buttons, switches, LEDs, and 7 segment display. These are the easiest components to control--once you've figured them out, you'll be able to work on using the memories and Ethernet port (or I'll release a followup project).

# High level workflow

## Piplin code

First, we'll set up the `ns` form for this piplin project. You can copy the sample below, changing the namespace to yours:

```clojure
(ns piplin.my-first-project
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-left bit-shift-right])
  (:use piplin.core)
  (:require piplin.seven-segment-decoder))
```

Next, you'll define a module with these inputs and outputs, like so:

```clojure
(defmodule my-first-project []
  [:outputs [led #b0000_0000 ;8 bit quantity for controlling the 8 LEDs
             seven_seg_cathode #b0000_000_0
             seven_seg_anode #b0000
             vgaRed #b000
             vgaGreen #b000
             vgaBlue #b00
             Hsync false
             Vsync false]
   :inputs [sw (bits 8)]]
  ;; Your code here
  )
```

Here we are introduced to the `bits` type in Piplin. Inputs must have only their type declared, since their value is provided to the module. Outputs must have default values given, and those values determine the type of the output. `#b00_00` is the syntax to declare a bit literal. Underscores have no meaning, and are just used as convenient visual separators. Also, we can see here that booleans are actually synthesizable (`Hsync` and `Vsync`).

Next, you'll write some Piplin code (detailed elsewhere in the document).

## Simulation

To simulate your project, use the following code:

```clojure
(trace-module (my-first-project) 100)
```

This runs the simulation for 100 cycles, and returns the trace of the simulation. A trace is a seq of maps, where each map is a cycle, and the keys of the map correspond to the registers in the design. To visualize the trace, you can use the free software GTKWave. If you have GTKWave installed, you can use `(trace->gtkwave the-trace)` to open the trace in GTKWave, or you can save the trace using `spit-trace`.

## Synthesis

After you're satisfied with the results, you'll convert the module to verilog. You'll use `modules->all-in-one` to convert the module and all submodules to verilog. This function returns a string that you can write to a file. You'll copy that file into the same directory as the `nexys6-sample` project, and you'll tweak the module name in `toplevel.v` or your own file to match up. Then, you can select `toplevel.v` in WebPACK, double click `Generate Programming File`, and you'll end up with a .bit file. This file can then be loaded on to your FPGA using the Adept software.

## Nexys 6 Caveat and Documentation

[See here for Nexys 6 documentation](http://www.digilentinc.com/Data/Products/NEXYS3/Nexys3_rm.pdf).

The seven segment display in the Nexys 6 must be time-multiplexed to display a counter. The remainder of this document will go over how to program that.

# Programming the Nexys 6 with Piplin

## First thing's first

Since we have several peripherials we're going to control, we're going to need to provide dummy connections to many of the output ports of our module so that the Xilinx tools don't complain. Here's how we do that:

```clojure
(defmodule my-first-project []
  [:outputs [led #b0000_0000 ;8 bit quantity for controlling the 8 LEDs
             seven_seg_cathode #b0000_000_0
             seven_seg_anode #b0000
             vgaRed #b000
             vgaGreen #b000
             vgaBlue #b00
             Hsync false
             Vsync false]
   :inputs [sw (bits 8)]]
  (connect led #b0000_0000)
  (connect seven_seg_cathode #b0000_000_0)
  (connect seven_seg_anode #b0000)
  (connect vgaRed #b000)
  (connect vgaGreen #b000)
  (connect vgaBlue #b00)
  (connect Hsync false)
  (connect Vsync false))
```

`connect` is a function that we use within modules. It takes 2 arguments: a register and a value. Every cycle, that register is updated to contain the given value.

Try looking at `(trace->gtkwave (trace-module (my-first-project) 10))` to see the values held constant.

## Turning on some LEDs

Now that we have something simple, let's try controlling the LEDs with the switches. Since there are 8 LEDs and 8 switches, we can just connect them up!

```clojure
(defmodule my-first-project []
  [:outputs [led #b0000_0000 ;8 bit quantity for controlling the 8 LEDs
             seven_seg_cathode #b0000_000_0
             seven_seg_anode #b0000
             vgaRed #b000
             vgaGreen #b000
             vgaBlue #b00
             Hsync false
             Vsync false]
   :inputs [sw (bits 8)]]
  (connect led sw) ;; Connect up the switches!
  (connect seven_seg_cathode #b0000_000_0)
  (connect seven_seg_anode #b0000)
  (connect vgaRed #b000)
  (connect vgaGreen #b000)
  (connect vgaBlue #b00)
  (connect Hsync false)
  (connect Vsync false))
```

Try running that on the FPGA, and see how flipping the switches turns on and off the LEDs.

## Turning on some LEDs, round II

This time, we're going to invert every other switch, so that the even switches turn LEDs on when the switches are up, and the odd switchs turn LEDs on when the switches are down.

```clojure
(defn invert-every-other
  [binary]
  (let [the-bits (map #(bit-slice binary % (inc %)) (reverse (range 8)))
        inverting (map (fn [bit invert?]
                         (if invert?
                           (bit-not bit)
                           bit))
                       the-bits
                       (cycle [true false]))]
    (apply bit-cat inverting)))

(defmodule my-first-project []
  [:outputs [led #b0000_0000 ;8 bit quantity for controlling the 8 LEDs
             seven_seg_cathode #b0000_000_0
             seven_seg_anode #b0000
             vgaRed #b000
             vgaGreen #b000
             vgaBlue #b00
             Hsync false
             Vsync false]
   :inputs [sw (bits 8)]]
  (connect led (invert-every-other sw))
  (connect seven_seg_cathode #b0000_000_0)
  (connect seven_seg_anode #b0000)
  (connect vgaRed #b000)
  (connect vgaGreen #b000)
  (connect vgaBlue #b00)
  (connect Hsync false)
  (connect Vsync false))
```

You can test `invert-every-other` on the REPL. Note that we've hardcoded it to take an 8 bit value--if you want to use some other bit width, you'll need to change the `8` to the appropriate value.

Why do we need to `reverse` the `(range 8)`? The least significant bit (index 0) should go on the right side of `the-bits`, so that we can apply `bit-cat` to it; however, if we don't reverse the range, we'll end up with the least significant bit in the most significant bit position, which would be wrong.

Try running that on the FPGA now!

## Seven segment display and submodules

At this point, we've succeeded in playing around with the LEDs a bit. Now, we'd like to actually try to use the seven segment display. For this, we'll use a submodule. Piplin modules can be instantiated and used as components of other modules. We'll use the module `piplin.seven-segment-decoder/decoder` to display a simple counter.

Since our FPGA is running at 65MHz (in the provided sample project), we need to have a counter that is running slower--slow enough that we can actually see it counting. To do this, we'll create a counter that runs at 65MHz, but we'll divide it by 2^25 (33 million) so that it counts up about twice per second.

We can create local registers that aren't exposed as ports by using the `:feedback` section, and submodules using the `:modules` section.

```clojure
(defn invert-every-other
  [binary]
  (let [the-bits (map #(bit-slice binary % (inc %)) (reverse (range 8)))
        inverting (map (fn [bit invert?]
                         (if invert?
                           (bit-not bit)
                           bit))
                       the-bits
                       (cycle [true false]))]
    (apply bit-cat inverting)))

;; Pin mapping for Nexys 6 Seven Segment Displays
(def seven-seg-map
  {:top 0
   :upper-left 5
   :lower-left 4
   :bottom 3
   :lower-right 2
   :upper-right 1
   :middle 6})

(defmodule my-first-project []
  [:outputs [led #b0000_0000 ;8 bit quantity for controlling the 8 LEDs
             seven_seg_cathode #b0000_000_0
             seven_seg_anode #b0000
             vgaRed #b000
             vgaGreen #b000
             vgaBlue #b00
             Hsync false
             Vsync false]
   :inputs [sw (bits 8)]
   :feedback [counter ((uintm 29) 0)]
   :modules [deco (piplin.seven-segment-decoder/decoder 4 seven-seg-map)]]

  (connect counter (inc counter)) ;uintm is modulo
  (connect deco$in (bit-slice (serialize counter) 25 29))
  (connect seven_seg_cathode (bit-cat #b1 (bit-not deco$out)))
  (connect seven_seg_anode #b0000)

  (connect led (invert-every-other sw))
  (connect vgaRed #b000)
  (connect vgaGreen #b000)
  (connect vgaBlue #b00)
  (connect Hsync false)
  (connect Vsync false))
```

Creating a decoder module takes 2 arguments: the number of bits to decode, and the mapping from digits to wire indices.

Submodules' ports are specified with `$`, so that the ports `in` and `out` of the module named `deco` are referred to as `deco$in` and `deco$out`.

We cannot `bit-slice` the `counter` since it's a `uintm`, which is a number, not a `bits`. So, we use the function `serialize` to convert it to its bit representation, and then we can slice and concatenate with other bits to our heart's content. There is a corresponding function `deserialize` that we can use to convert bits to more meaningful types, as long as we know what type we want to deserialize the bits into.

The seven segment displays on the Nexys 6 are active low, so we `bit-not` the output of the decoder before putting it on the cathode. Also, we must include the extra `#b1` to turn off the decimal point, since the display requires 8 bits to drive--7 for the digit, and one for the decimal.

At this point, you should see all 4 digits displaying the same value and counting up together!

## Displaying different digits

You may have noticed from the last section that there's only one digit of output on the Nexys 6 board. How do we control the other digits!?

The seven segment display is controlled through a technique called **timeslicing**, **time multiplexing**, or **scanning**. This means that you light up one character at a time, and switch which character you're displaying every couple milliseconds. Since the human eye can't perceive changes that fast, the display appears to have a different digit on each character display. We'll use the clock divided by 2^12 to switch which charater we're displaying around 16k times per second. You can try adjusting the period faster and slower to see what happens.

```clojure
(defn invert-every-other
  [binary]
  (let [the-bits (map #(bit-slice binary % (inc %)) (reverse (range 8)))
        inverting (map (fn [bit invert?]
                         (if invert?
                           (bit-not bit)
                           bit))
                       the-bits
                       (cycle [true false]))]
    (apply bit-cat inverting)))

;; Pin mapping for Nexys 6 Seven Segment Displays
(def seven-seg-map
  {:top 0
   :upper-left 5
   :lower-left 4
   :bottom 3
   :lower-right 2
   :upper-right 1
   :middle 6})

(defmodule my-first-project []
  [:outputs [led #b0000_0000 ;8 bit quantity for controlling the 8 LEDs
             seven_seg_cathode #b0000_000_0
             seven_seg_anode #b0000
             vgaRed #b000
             vgaGreen #b000
             vgaBlue #b00
             Hsync false
             Vsync false]
   :inputs [sw (bits 8)]
   :feedback [counter ((uintm 41) 0)]
   :modules [deco (piplin.seven-segment-decoder/decoder 16 seven-seg-map)]]

  (connect counter (inc counter)) ;uintm is modulo
  (connect deco$in (bit-slice (serialize counter) 25 41)) ;Full 16 bits in now
  (let [;Must multiplex in the millisecond regime 
        current-state (bit-slice
                        (serialize counter) 12 14)
        anode (condp = current-state
                #b00 #b1110
                #b01 #b1101
                #b10 #b1011
                ;#b11
                #b0111)
        cathode (condp = current-state
                  #b00 (bit-slice
                         deco$out 0 7)
                  #b01 (bit-slice
                         deco$out 7 14)
                  #b10 (bit-slice
                         deco$out 14 21)
                 ; #b11
                  (bit-slice
                         deco$out 21 28))]
    (connect seven_seg_anode anode)
    (connect seven_seg_cathode (bit-cat #b1 (bit-not cathode))))

  (connect led (invert-every-other sw))
  (connect vgaRed #b000)
  (connect vgaGreen #b000)
  (connect vgaBlue #b00)
  (connect Hsync false)
  (connect Vsync false))
```

For more information on why the 2 connections to the display are called *anode* and *cathode*, see [this Wikipedia article on diodes](http://en.wikipedia.org/wiki/Diode). The short version is that the anode and cathode are the 2 wires that come out of the LED, and their must be a positive voltage difference between the anode and the cathode for the LED to light up.
