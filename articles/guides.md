---
title: "Piplin: all documentation guides"
layout: article
---

## Guide list

[Piplin documentation](http://piplin.org) is organized as a number of guides, covering all kinds of topics.

All of these guides assume that your namespace looks like this:

{% highlight clojure %}
(ns piplin.guide-contents
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-left bit-shift-right pos? neg? zero?])
  (:use piplin.core plumbing.core))
{% endhighlight %}


We recommend that you read these guides, if possible, in this order:

###  [Getting started](/articles/intro.html)

An overview of Piplin with a quick tutorial that helps you to get started with it. It should take about
30 minutes to read and study the provided code examples

###  [Piplin values and deferred evaluation](/articles/deferred-evaluation.html)

This guide explains how you can use the same functions and types at the REPL, in simulation, and in the generated Verilog. It goes over the simple and aggregate types and how they are constructed and interact.

###  [Modules](/articles/modules.html)

An explanation of the Piplin module system. Goes over how to declare modules, how to incorpate state and feedback loops in your designs, and how to compose modules together to make more complex designs.

## Tell Us What You Think!

Please take a moment to tell us what you think about this guide on Twitter or the [Piplin mailing list](https://groups.google.com/forum/#!forum/piplin)

Let us know what was unclear or what has not been covered. Maybe you do not like the guide style or grammar or discover spelling mistakes. Reader feedback is key to making the documentation better.
