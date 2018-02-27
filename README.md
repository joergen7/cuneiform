![Cuneiform: Data analysis open and general](priv/cuneiform_title.jpg)

[![hex.pm](https://img.shields.io/hexpm/v/cuneiform.svg?style=flat-square)](https://hex.pm/packages/cuneiform) [![Build Status](https://travis-ci.org/joergen7/cuneiform.svg?branch=master)](https://travis-ci.org/joergen7/cuneiform)

Cuneiform is a functional language for large-scale data analysis.

Cuneiform is a minimal workflow specification language with immutable state,
lazy evaluation, lists, and second order functions operating on lists. In
this, it borrows from Functional Programming languages. Cuneiform
deliberately constrains users to specify workflows in a parallelizable way.
Its execution environment is designed for clusters and clouds. In addition,
functions (tasks) can be defined in any given scripting language, e.g.,
Bash, R, or Python. This way users can not only supplement features absent
in native Cuneiform but can reuse any tool or library no matter what API it
requires. For further information, please refer to the [Cuneiform paper](http://ceur-ws.org/Vol-1330/paper-03.pdf)

## Usage

### Compiling

Having rebar3 available on your system, compile the project by entering

    rebar3 escriptize

### Starting Cuneiform

Compiling the Cuneiform client using `escriptize` creates an Erlang script file `cf_worker` which allows starting the Cuneiform client via the command line.

To display a help text enter

    ./cuneiform --help


This will show the command line synopsis, which looks like the following:

    Usage: cf_worker [-v] [-h] [-n <n_wrk>] [-w <wrk_dir>] [-r <repo_dir>]
                     [-d <data_dir>]

      -v, --version   Show cf_worker version.
      -h, --help      Show command line options.
      -n, --n_wrk     Number of worker processes to start. 0 means auto-detect 
                      available processors.
      -w, --wrk_dir   Working directory in which workers store temporary files.
      -r, --repo_dir  Repository directory for intermediate and output data.
      -d, --data_dir  Data directory where input data is located.


#### Starting an Interactive Shell

You can start a shell and program Cuneiform interactively by starting it without any command line parameters like so:

    ./cuneiform

This will open a shell giving the following initial output, along with a number of status messages:

               @@WB      Cuneiform
              @@E_____
         _g@@@@@WWWWWWL  Type help for usage info
       g@@#*`3@B              quit to exit shell
      @@P    3@B
      @N____ 3@B         http://www.cuneiform-lang.org
      "W@@@WF3@B

    1>

#### Running a Cuneiform Script

Alternatively, Cuneiform can be started by giving it a source file which will only output the final result of the computation. If your Cuneiform script is stored in `my_script.cfl` start it by entering

    ./cuneiform my_script.cfl


## Resources

- [cuneiform-lang.org](https://www.cuneiform-lang.org/). Official website of the Cuneiform programming language.
- [joergen7/cuneiform-examples](https://github.com/joergen7/cuneiform-examples). Collection of small, self-contained Cuneiform code examples.
- [joergen7/cre](https://github.com/joergen7/cre). A common runtime environment (CRE) for distributed workflow languages.
- [joergen7/cf_client](https://github.com/joergen7/cf_client). A Cuneiform client implementation.
- [joergen7/cf_worker](https://github.com/joergen7/cf_worker). A Cuneiform worker implementation.

## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)