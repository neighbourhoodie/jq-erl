# calling `jq` from erlang

This is an attempt to embed `jq` in an Erlang program using NIFs. You may need
to change the paths in `Makefile` to get things to compile.

First I figured out how to use the `jq` API in a standalone C program, `myjq.c`.
If you run `make myjq && ./myjq` you should see:

    ---- value type: object
    {
      "foo": [
        16,
        32,
        64
      ]
    }
    ---- result type: number
    17
    ---- result type: number
    33
    ---- result type: number
    65

This checks that our basic usage of the API at least runs and does something
useful.

Next I tried to get this same logic working by passing the `jq` program and JSON
document from Erlang. This is done in these files:

- `jq.erl`: Erlang program that makes a CouchDB-style JSON value, serialises it,
  and tries to use the NIF `jq/2` to evaluate a `jq` expression on it
- `jq_nif.c`: contains NIF definitions that turn the Erlang strings into C
  strings and try to run `jq` on them

The C code is a work in progress that sketches out what's needed so don't worry
too much about the details. I'm just trying to get this working by sending the
doc as a string. A better solution would require writing C code to convert
Erlang values into `jq` JSON values and back again, but that doesn't seem worth
it until I can get a basic example running.

This code wasn't working because `jq_compile()` segfaults, even though it works
fine in a standalone program. Getting the data in from Erlang and sending values
back seems to work fine, but `jq_compile()` segfaults even if you give it a
hard-coded string.

To demonstrate this I've also defined a NIF `jq_simple/0` which just creates a
`jq` instance and tries to compile a hard-coded pattern in it before returning
an Erlang int. This function also segfaults.

If you run `make clean test` you should see:

    ---- init result: [ok]
    ---- value type: object
    {
      "foo": [
        16
        32
        64
      ]
    }
    make: *** [test] Segmentation fault: 11

If you comment out the `jq_compile()` call the code runs fine and you see the
C value returned to the Erlang program:

    ---- init result: [ok]
    ---- value type: object
    {
      "foo": [
        16
        32
        64
      ]
    }
    ---- result: 42
