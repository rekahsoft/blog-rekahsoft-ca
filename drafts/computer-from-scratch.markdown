---
title: Computer From Scratch
author: Collin J. Doering
date: Jun 19, 2015
description: Building a computer from scratch using VHDL
tags: general, programming, hardware, nand-to-tetris
---

Recently I have had the pleasure of completing the course *Nand to Tetris* on
[Coursea](http://coursera.org). Firstly, I'd like to highly recommend it to anyone! I never
thought it would be possible for me to understand computers all the way down to the hardware
level (past assembly that is); *Nand to Tetris* has shown me otherwise! It has also inspired me
to pursue learning a real world hardware description language and attempt to implement the
*Hack* system on an FPGA. In this article I will describe how the process is coming and the
what remains to be completed.

<!--more-->

After implementing the *Hack Computer System* in the hardware description language described by
the course, I went on to implement everything in [VHDL](https://en.wikipedia.org/wiki/VHDL),
with the end goal of being able to realize the design on a
[FPGA](https://en.wikipedia.org/wiki/Field-programmable_gate_array) (Field Programmable Gate
Array). I also needed a set of test benches to verify correctness of the design through
simulation. Initially I set out to find a completely open source solution. To my dismay, I
found that FPGA's are incredibly proprietary. Namely, the process of *synthesis* cannot be done
using open source tools, though there exists a few projects which are reverse engineering the
bit file format of various FPGA's in order to develop a open solution to synthesis, they are
not yet ready for use. So, synthesis needs to be done with proprietary tools.


which is
taking the hardware description (either VHDL or Verilog) and convert it to a bit file (a format
the FPGA can use).
