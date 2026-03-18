# DO

## Design
Oyster is a Scheme interpreter written as a true CISC architecture.

There are two opcodes: DO, and DONE.
Everything else is an argument to DO or DONE, and there may be any number of arguments to these instructions.

### Goals
- Minimize the number of opcodes
- Compilation performance
- Support currying (even though Scheme does not do this)

### Non-goals
- Runtime performance

## How it works

### Parsing
Parsing is implemented as a simple state machine which builds an AST of scopes, symbols, and constants.
Because there is no recursion and -
outside of switch statements, which will probably be compiled to array lookups, and number validation -
there is *no branching*.

Compilation performance: ✅

#### Future work
- Comments

### Compilation
The compiler of oyster does almost nothing:

1. Bindings are associated with a number (known if it's a native function keyword, otherwise unknown until compile time)
2. The beginning of the file is a DO
3. The end of the file is a DONE
4. Opening parentheses become DOs
5. Closing parentheses become DONEs

Compilation performance: ✅

Also, some metadata are associated with the DOs and DONEs, but that's solely for performance and is neither here nor there.

#### Future work
I plan to implement more compiler passes to speed up some of the things that the runtime currently has to manage.
- DONEs should be marked with the number of DONEs following, to prevent unnecessary instruction lookahead in handling tail calls
- The remaining arity of a function call should be passed to the function, to prevent unnecessary instruction lookahead in handling variable-arity functions

### Runtime
Everything happens at runtime.

When we encounter a DO opcode, we push DO onto the stack.

When we encounter a value `a`, look at the top of the stack.
- If it's a DO... then DO `a`.
- If it's a DONE... then `Squash` and check the second-from-top stack element - do this exact same 3-case check on it
- If it's anything else, don't DO `a`. DO whatever's on the stack with `a` as an argument.

(*Squash:* a flowering gourd.

*`Squash`:* removes the second-from-top item from the stack.)

Bindings are evaluated by hashmap lookup of their compiler-assigned numbers.
They may be in the stack, heap, static memory, or they may be native or "hidden" function pointers.

*Everything happens at runtime.*

## Examples
`5`

Compiler output: `DO 5 DONE`.

- `DO`: push `DO` on the stack.
- `5`: Pop from the stack: it's a `DO`! `DO 5`. The next instruction is `DONE`, so push `DO` and then `5`.
- `DONE`: `Squash`. Done.

The value at the top of the stack is the return value: 5.

---
`(+ 9 5)`

Compiler output: `DO DO + 9 5 DONE DONE`.
I say `+`, but I actually mean whatever number is associated with the native function `+`; in this version, it's 0.

- `DO`: push `DO` on the stack.
- `DO`: push `DO` on the stack.
- `+`: Pop from the stack: it's a `DO`! `DO +`. Push `DO` and then `+`.
- `9`: Pop from the stack: it's not a `DO` :( It's `+`, so we look up the function for `+` and `DO` it on `9`.
  - Pop again. This value is the return value of the previous function call (`+` says `DO => 0`) and push the result of `0 + 9` (9).
  - Push the function `+` should perform next (it's `+`).
- `5`: Pop from the stack: it's not a `DO` :( It's `+`, so we look up the function for `+` and `DO` it on `9`.
  - Pop again. This value is the return value of the previous function call (9).
  - Push the result of `9 + 5` (16).
  - Push the function `+` should perform next (it's `+`).
  - Actually, since the next instruction is `DONE`, we reverse the order: `16` should be on top of the stack, with `+` under it. (the first example does this too, implicitly)
- `DONE`: `Squash`. Done.
- `DONE`: `Squash`. Done.

The value at the top of the stack is the return value: 16.

---
`(if 1 16 64)`

Compiler output: `DO DO if 1 16 64 DONE DONE`
As before, `if` refers to the associated index (4, in this version).

- `DO`: push `DO` on the stack.
- `DO`: push `DO` on the stack.
- `if`: Pop from the stack: it's a `DO`! `DO if`. Push `DO` and then `if`.
- `1`: Pop from the stack: it's not a `DO` :( It's `if`, so we look up the function for `if` and `DO` it on `1`.
  - Pop twice. The second value is the return value of the previous function call (discarded, in `if`). Push the result of `(if 1)` (`DO`, doesn't matter).
  - Push the function `if` should perform next (it's `ifdo`)
  - `ifdo` is a "hidden" function: it's not usable directly from the user scripting, but `if` needs it as a subsidiary function.
- `16`: Pop from the stack: it's not a `DO` :( It's `ifdo`, so we look up the function for `ifdo` and `DO` it on `16`.
  - Pop again. This value is discarded (according to what `ifdo` needs).
  - `ifdo` says: `DO` the next thing: push `DO 16` (as before)
  - Push the function `ifdo` should perform next (it's `ifdont`).
- `64`: Pop from the stack: it's not a `DO` :( It's `ifdont`, so we look up the function for `ifdont` and `DO` it on `64`.
  - Pop again. This is the return value from `ifdo`: `16`.
  - Push the result of `ifdont 16 64` (16). `ifdont` doesn't even try to evaluate 64 (this is necessary for nested statements).
  - Push the function `ifdont` should perform next (it doesn't matter. Just slap a `DO` on there)
  - Actually, since the next instruction is `DONE`, we reverse the order: `16` should be on top of the stack, with `DO` under it.
- `DONE`: `Squash`. Done.
- `DONE`: `Squash`. Done.

---
One last example: `(let ((a 5) (b 9)) b)`

Compiler output: `DO DO let DO DO a 5 DONE DO b 9 DONE DONE b DONE`.
As before, `let` refers to the associated index (8, in this version), and `a` and `b` refer to comptime-generated indices.
Out of consideration for the reader, I leave out the elucidation of every single opcode.

Essentially, this encounters a list of lists and passes it as an argument.
However, since the bindings are unbound, and the argument passed must be a single value,
this whole rigamarole is useless.
Instead, when we evaluate the `let`, we discard the passed-in argument and just parse the opcodes
as (binding, value) pairs.
Notably, this means that *an unbound binding is not an error until a function tries to use it*.

Bad runtime performance: ✅

Then:
- Push the values onto the stack, along with the binding pointer value of the bindings' previous value (nothing, for `a` and `b`).
- Push a raw int `2` onto the stack instead of a `DO`, so when `DONE` is evaluating, it knows to pop 2 extra things off the stack.

Also, tail calls:
When we encounter `b` at the very end, we see that there is a chain of two `DONE`s in the code afteward. Therefore, this is a tail call.
Then, we can preemptively pop two things off the stack before we return our `b` value:

The first is a raw int `2`, so we pop 2 extra things off (our bindings)

The second is a `DO`. Pop. Done.

# DONE

## Credits

Max Bernstein, for assigning this as a project to make a compiler project,
even though I ostensibly failed by making everything part of the runtime and nothing part of the compiler.

The [Ghuloum Paper](https://bernsteinbear.com/dartmouth-compilers/11-ghuloum.pdf),
for using RISC-like, separate, constant-sized opcodes for all native functions (like a normal computer would).
However, as we all know, CISC is cooler than RISC.
