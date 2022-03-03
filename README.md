# jCall

_jCall_ is a framework for simplifying the access to Julia code from fully compiled
languages like C, C++ or Fortran. Accessing Julia code from such a language 
is a bit tedious, since most non-primitive types (such as, e.g., Vectors) do
not map easily from and to C. Rather, one has to create structures that mirror
a Julia type and deserialize C data from/to them. All of this has to be done in wrapper
functions or structures and carried out recursively.

In order to facilitate this process, we created _jCall_: From a list of function 
signatures, jCall automatically generates wrapper functions and de/serialization.
Pointers for complex return values are allocated outside of Julia's garbage 
collection using Libc.

## Prerequisites
So far, we only tested jCall on a linux system (Ubuntu 20.04 LTS). You'll need:
- a working Julia installation
- GCC $\ge$ 8.0
- make

## Example & Instruction for use

We provide a small artificial example on how to use jCall in the folder
`app/`. For your own experiments, we would advise to clone this folder rather than
starting from scratch.

Let's first assume you have some Julia code that you need to access from C/C++;
for our example, let's assume we've written the code in `app/code.jl` (don't expect
it to make sense, it's meant purely for explanatory purposes) and want to
call all these functions from C, e.g. 

```
[...]
mutable struct ex3s
    b::Vector{Float64}
end

function ex3(a::ex3s) :: Int
    println("b in ex3:")
    for i in 1:size(a.b, 1)
        println("a[", i, "] = ", a.b[i])
    end

    return Int(1.0)
end
[...]
```

Function `ex3` consumes an instance of structure type `ex3s` and returns an
integer (64 bit by default). While returning the integer is trivial in C,
the structure input argument is not. Therefore, jCall uses an alternative approach,
genering a C `struct` that mirrors `ex3s`, as we will see later.

Next, we need to tell jCall which functions we actually need to include,
see `app/recipe.jl`:
```
ex_methods = [
    [..]
    ("ex3", example.ex3, [example.ex3s], Int),
    [...]
]
```
The entries of this list follows the pattern
`(shortname for function, fully qualified name, list of types of parameters, 
parameter of return type or nothing)` where the short name is a name you may choose.
In C, your function will be accessible as `long long int jl_ex3(...)`. As the
input es a structure, jCall will generate an analogue in C, letting you
pass a pointer to this struct instead.

That's it! Now, before we can use `make` to _build_ our library, we need to 
collect some paths to feed to `make`:
- `JULIA_LIB_DIR`: folder that contains the `libjulia.so`
- `JULIA_INCLUDE_DIR`: folder that contains the `julia.h`
- `JULIA_INTERNAL_LIB_DIR`: : folder that contains the `libjulia-internal.so`

Now, finally, we `cd` into `app/` and run:
`make JULIA_LIB_DIR=?? JULIA_INCLUDE_DIR=?? JULIA_INTERNAL_LIB_DIR=??`. This
will not only create the library (`libjcall.so`), but also a header file
for your c applications and a simple C binary (`app`) that demonstrates the use
of your newly generated library.

The relevant parts of the generated header file `jcall.h` looks as follows:
```
typedef struct {
	double * vals;
	unsigned long long dim1;
} jl_implicit1;

typedef struct {
	jl_implicit1 * b;
} jl_Main_example_ex3s;

long long jl_ex3(jl_Main_example_ex3s * a);
```

Here, we clearly see the analogue struct for `ex3s` (`jl_Main_example_ex3s`) as well
as a proxy for `Vector{Float64}` (`jl_implicit1`). In our `app/app.c`, we also see
how these functions may be accessed:
```
#include "jcall.h"

int main(int argc, char * argv[])
{
    jcall_init(argc, argv);

    [...]
    /* printing a vector yet again - but work using a structure */
    printf("=== ex3:\n");

    jl_implicit1 jl1;
    jl1.dim1 = 9;
    jl1.vals = (double *) malloc(jl1.dim1 * sizeof(double));
    for(int i = 0; i < jl1.dim1; ++i)
        jl1.vals[i] = (double) i;

    jl_Main_example_ex3s jl2;
    jl2.b = &jl1;

    int ret_ex3 = jl_ex3(&jl2);
    printf("Ex3 ret: %d\n", ret_ex3);
    [...]

    jcall_end();
    return EXIT_SUCCESS;
}
```
Note the `jcall_init` and `jcall_end` - their use is mandatory! In between, we
fill a `jl_implicit1` structure. Through our automatically generated wrapper functions
in Julia, these `struct`s are automatically converted into Julia objects.

However, the generated library still requires access to some Julia infrastructure,
so in order to build `app`, we need to build `app.c` with the following
parameters:
`[...] libjcall.so -L$(JULIA_LIB_DIR) -ljulia -L$(JULIA_INTERNAL_LIB_DIR) -ljulia-internal -lm -luv [...]`
inserting the correct paths that we gathered above - as the `make` script will 
undoubtedly tell you.

## Limitations
We currently support, besides primitive types, only vectors / matrices / arrays
of primitive types and structs as well as tuples thereof. Structs may contain
other structs.

Unfortunately, we did not find a way to vreate a static library, hence users still
need to link against `libjulia` and `libjulia-internal`.