# VeriScala


### Introduction
___
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

Detailed explanation is in the [paper](https://github.com/VeriScala/VeriScala/blob/master/VeriScala_Intro.pdf)


### File Organization
___
This GitHub project provides implementation of the VeriScala DSL. We build the project using SBT, and any IDE
with scala and SBT supports can easily load it (Intellij IDEA is recommanded).

There are mainly two directory in the project
* `examples`
* `newhdl`

`examples` gives the demo implementation and corresponding test in VeriScala DSL 

`newhdl` provides the core implementation of the DSL

`VeriScala/newhdl/src/main/scala/` holds
* `Core`
* `Exceptions`
* `Simulation`

The relation amomg the files in these three directories is show in the figure below
![](http://pic3.178.com/3857/38579363/month_1702/70704dce2122da8425363cfe40a618f1.jpg "VeriScala DSL Architecture")

`VeriScala/examples/src/main/` gives part of the demo implementations

`VeriScala/examples/src/test/scala/` gives software test bench of the demo implementations

`VeriScala/examples/compile_result/` gives generation results of the demo implementations


### Usability Test

___
The test is conducted on ten experienced high-level language
programmers with close age and years of programming
career. The test includes the following steps:

* Divide ten people into two groups. One is called VeriScala group, and the other Verilog group.
* Read the prepared specification booklet of VeriScala or Verilog corresponding to their groups, 
  implement a FlashLED module after reading, and record the total time costed.
* Swap the group, and do the jobs in last step again.
* Count the code lines of each implementation and check the correctness.
* Analyse the collected data.

Without or with little modification on syntax, every piece
of code passes our test, which indicates that people find no
trouble in using VeriScala to describe correct hardware design,
and VeriScala is effective to learn. From the data, we firstly
find that nearly every one use less code to do the job when
using VeriScala, which can be seen in the figure below. 

![](http://pic3.178.com/3857/38579363/month_1702/659f9cd380de8e5a78835be34f4635ff.jpg "VeriScala DSL Architecture")

This result is different from the result shown in V-B1 where code lines of
VeriScala will be a little more than that of Verilog. By reading
the code, we find that people do not always use the same logic
to implement the FlashLED, they will change their designs
according to language features, such as high-order functions
in Scala, which will obviously decrease the code lines.

![](http://pic3.178.com/3857/38579363/month_1702/5934a9a4d8dfa53ffc82bc2a7ee3f589.jpg "VeriScala DSL Architecture")

Another discovery is that, shown in the figure above, the total
implementation time tends to be shorter, if they first learn
VeriScala other than Verilog. Interviews indicate the reason,
which is that Verilog is hard for beginners and VeriScala
serves as a good warm-up. Final discovery is demonstrated
by Figure 9. This figure shows that Scala programmer will
have advantages in learning VeriScala for they use less time
both in total and VeriScala, which is a reasonable result.



