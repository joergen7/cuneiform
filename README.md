# Cuneiform

A Functional Workflow Language

Cuneiform is a workflow specification language which makes it easy to integrate heterogeneous tools and libraries and exploit data parallelism. Users do not have to create heavy-weight wrappers for establised tools or to reimplement them. Instead, they apply their existing software to partitioned data. Cuneiform can be executed on Hadoop YARN which makes it suitable for large scale data analysis.

Cuneiform comes in the form of a functional programming language with a Foreign Function Interface (FFI) that lets users create functions in any suitable scripting language and apply these functions in a uniform way.

Data paralelism is expressed by applying map, cross-product, dot-product or combinations of the aforementioned algorithmic skeletons to lists of files containing black-box data.


