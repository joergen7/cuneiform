# Cuneiform [![Build Status](https://travis-ci.org/joergen7/cuneiform.svg?branch=master)](https://travis-ci.org/joergen7/cuneiform)

A Functional Workflow Language

Cuneiform is a workflow specification language which makes it easy to integrate heterogeneous tools and libraries and exploit data parallelism. Users do not have to create heavy-weight wrappers for establised tools or to reimplement them. Instead, they apply their existing software to partitioned data. Using the [Hi-WAY](https://github.com/marcbux/Hi-WAY) application master Cuneiform can be executed on Hadoop YARN which makes it suitable for large scale data analysis.

Cuneiform comes in the form of a functional programming language with a Foreign Function Interface (FFI) that lets users create functions in any suitable scripting language and apply these functions in a uniform way.

Data paralelism is expressed by applying map, cross-product, dot-product, or combinations of the aforementioned algorithmic skeletons to collections of black-box data.

Tutorials, a publication list, and example workflows including [Variant calling](https://github.com/joergen7/variant-call) and [Methylation](https://github.com/joergen7/methylation) can be found under [saasfee.io](http://www.saasfee.io).

For a detailed description of the Cuneiform workflow language please refer to our publication [Cuneiform: A Functional Language for Large Scale Scientific Data Analysis](http://ceur-ws.org/Vol-1330/paper-03.pdf).

