# VeriScala

[![Build Status](https://travis-ci.org/ScalaHDL/VeriScala.svg?branch=master)](https://travis-ci.org/ScalaHDL/VeriScala)

Field Programmable Gate Arrays (FPGAs) have gained momentum in the past decade, when the
improvements of high end CPUs started to level-off in terms of clock frequencies. Since FPGAs can provide
more flexible and more efficient solutions in most occasions, manufacturers gradually adopt FPGA chips
in their products such as CPUs and mobile devices. In the foreseeing future, FPGAs will be accessible computing
resources and software developers will build applications interacting with FPGAs. However, the efficiency of
development for applications based on FPGAs is severely constrained by the traditional languages
and tools, due to their deficiency in expressibility, extendability, limited libraries and semantic gap
between software and hardware descriptions. This project proposes a new open-source Domain-Specific
Language (DSL) based framework called VeriScala that supports highly abstracted object-oriented hardware
defining, programmatical testing, and interactive on-chip debugging. By adopting DSL embedded in Scala,
we introduce modern software developing concepts into hardware designing including object-oriented
programming, parameterized types, type safety, test automation, etc. VeriScala enables designers to describe
their hardware designs in Scala, generate Verilog code automatically and debug and test hardware design
interactively in real FPGA environment. 

This GitHub project provides implementation of the VeriScala DSL. We build the project using SBT, and any IDE
with scala and SBT supports can easily load it (Intellij IDEA is recommanded).



* Use less time write code on FPGA
* Available to develop on FPGA for people who don't have much hardware knowledge
* Generated code has been optimized, use less resource and have better performance
* Use high-level custom type, like Complex Number


