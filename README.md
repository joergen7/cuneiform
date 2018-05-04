![Cuneiform: Data analysis open and general](priv/cuneiform_title.jpg)

[![hex.pm](https://img.shields.io/hexpm/v/cuneiform.svg?style=flat-square)](https://hex.pm/packages/cuneiform) [![Build Status](https://travis-ci.org/joergen7/cuneiform.svg?branch=master)](https://travis-ci.org/joergen7/cuneiform)

Cuneiform is a large-scale data analysis functional programming language. It is *open* because it easily integrates foreign tools and libraries, e.g., Python libraries or command line tools. It is *general* because it has the expressive power of a functional programming language while using the independence of sub-expressions to automatically parallelize programs. Cuneiform uses distributed Erlang to scalably run in cluster and cloud environments.

[cuneiform-lang.org](https://www.cuneiform-lang.org/)

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
      "W@@@WF3@B         Jorgen Brandt

    1>

#### Running a Cuneiform Script

Alternatively, Cuneiform can be started by giving it a source file which will only output the final result of the computation. If your Cuneiform script is stored in `my_script.cfl` start it by entering

    ./cuneiform my_script.cfl

## Examples

A collection of self-contained Cuneiform examples is available under [joergen7/cuneiform-examples](https://github.com/joergen7/cuneiform-examples).

### Variable assignment

You can assign a value to a variable and retrieve a variable's content like so:

    let x : Str =
      "foo";

    x;

In the first line we assign the value `"foo"` to a variable named `x` declaring its type to be `Str`. In the last line we query the variable `x`.

### Booleans and Conditions

We can branch execution based on conditions using conditional statements. Conditionals are expressions.

    let x : Str =
      if true
      then
        "bla"
      else
        "blub"
      end;

    x;

The above command the conditional binding the string `"bla"` to the variable `x`. Then, we query the variable.

### Lists

We can construct list literals by enumerating their elements in square brackets and declaring the type of the list elements.

    let xs : [Bool] =
      [true, false, true, true : Bool];

    xs;

Here, we define the list `xs` whose elements are of type `Bool` giving four Boolean values of which only the second is `false`.

### Records and Pattern Matching

A record is a collection of fields that can be accessed via their labels. Literal records can be constructed like so:

    let r : <a : Str, b : Bool> =
      <a = "blub", b = false>;

    ( r|a );

We define a record `r` with two fields `a` and `b`, of types `Str` and `Bool` respectively. The field associated with `a` gets the value `"blub"` while the field associated with `b` gets the value `false`. In the last line we access the `a` field of the record `r`.

Alternatively, we can access record fields via pattern matching:

    let <a = z : Str> = r;
    z;

In the first line we associate the variable `z` with the field `a` of record `r`. In the second line we query the content of `z`.

### Native Function Definition

Defining native functions in Cuneiform is done by giving the function name, its signature, and a body expression in curly braces:

    def identity( x : Str ) -> Str {
      x
    }

    identity( x = "bar" );

In the first line we define the function `identity` which consumes an argument `x` of type `Str` and produces a return value of type `Str`. In the second line, the body expression is just the argument `x`. In the last line we call the function binding the argument `x` to the value `"bar"`.

### Foreign Function Definition

Defining foreign functions is done by giving the function name, its signature, the foreign language name, and the function body in mickey-mouse-eared curly braces.

    def greet( person : Str ) -> <out : Str> in Bash *{
      out="Hello $person"
    }*

    greet( person = "Peter" );

The first line defines a foreign function `greet` taking one argument `person` of type `Str` and returning a tuple with a single field `out` of type `Str`. The foreign function body is given in Bash code. In the last line we call the foreign function, binding the argument `person` to the string value `"Peter"`.

### Iterating over Lists using For

To perform an operation on each element of a list, one can iterate using for:

    let xs : [Bool] =
      [true, false, true, true : Bool];

    for x <- xs do
      not x
      : Bool
    end;

Here, we define a list of four Booleans and negate each element.

### Aggregating Lists using Fold

We can aggregate over lists using fold:

    def add( a : Str, b : Str ) -> <c : Str> in Python *{
      c = int( a )+int( b )
    }*

    let xs : [Str] = [1, 2, 3 : Str];

    let sum : Str =
      fold acc = 0, x <- xs do
        ( add( a = acc, b = x )|c )
      end;

    sum;

Here, we first define the function `add` which lets us add two numbers in Python and then the string list `xs` containing the numbers from one to three. We aggregate the sum of the numbers in `xs` and store it the result in the variable `sum`. Lastly, we query the `sum` variable.

## System Requirements

- [Erlang](https://www.erlang.org) OTP 19.0 or higher
- [Rebar3](https://www.rebar3.org) 3.0.0 or higher

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