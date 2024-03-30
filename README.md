# Wasm Instructions Cheat Sheet

This is (or pursues to be) a complete handbook of using the WebAssembly Text
Format. You can find a comprehensible set of information for each WebAssembly
instruction, including:

- Input values: What types and how many of the values are popped from the stack
  and are given by instant numbers;
- Output values: what types and how many of the values are pushed back to the
  stack;
- Semantics: what does the instruction do;
- Execution: how does the instruction achieve its purpose, represented by pseudo
  code written in TypeScript;
- (Optional) Opcode: the code of the instruction in the binary format.
- (Optional) Syntax: how should you write this instruction in the text format.

For example, the instruction `i32.const` is defined as:

```typescript
const instructions = {
  // ... other instructions
  i32: {
    // ... other instructions

    /** [0x41] Push an instant number. */
    const(x: u32): i32 { return x },

    // ... other instructions
  },
  // ... other instructions
}
```

The `instructions` variable contains the instruction definitions. From there we
know about the `i32.const` instruction from the path to the method
`instructions.i32.const`. This method has several significant parts:

- The documentation comments between `/**` and `*/`;
- The name, which is `const`;
- The parameter list: `(x: u32)`;
- The return type: `: i32`;
- The body: `{ return x }`.

The documentation comments provide the opcode of the instruction (the code of
the instruction in the binary format), i.e. `0x41`, and describe that the
instruction pushes an instant number to the stack.

> An instant number is a value directly provided with the instruction, instead
> of popped from the stack.

The parameter list declares all input values that the instruction requires. For
`i32.const`, an `u32` parameter `x` is declared. From its name, we know that it
is an instant number instead of stack value, because its name is not of a
certain form (we will cover that later). Because it is an instant number, we
must provide it with the instruction, e.g. `i32.const 0`. From the documentation
comments, we know that this is the number pushed to the stack.

The return type declares all output values that the instruction will produce. If
it is an array (like `[u32, u32]`), it means that the instruction produces
multiple outputs. In this case, the `i32.const` instruction only produces one
`u32` value, so after its execution, a new `u32` value will be pushed to the
stack.

The body contains pseudo code that indicates how this instruction runs. In this
case, it is simply returning the input value, which verifies the description in
the comments.

Let's see another example `i32.sub`. This time, it includes two stack values:

```typescript
const instructions = {
  // ... other instructions
  i32: {
    // ... other instructions

    /** [0x6b] Subtract `s1` from `s0`. */
    sub(s0: i32, s1: i32): i32 { return s0 - s1 },

    // ... other instructions
  },
  // ... other instructions
}
```

Again, we know the opcode of `i32.sub` is `0x6b`, and what it does is
subtracting the value `s1` from `s0`. What is different is that both the input
values are stack values.

The name of a stack input value is like `s0`, `s1`, `s2`... The values are
pushed in the same order as the parameters (`s0` is pushed first), thus popped
in the reversed order. WebAssembly defines the stack values of every instruction
in this order. So if we want to do `3 - 2`, the correct way is

`(i32.sub (i32.const 3) (i32.const 2))`

instead of

`(i32.sub (i32.const 2) (i32.const 3))`.

or the other equivalent way:

```
i32.const 2
i32.const 3
i32.sub
```

From the input types we know that the verification process of WebAssembly will
ensure that the two stack values are both of type `i32` before running into the
instruction.

The body `return s0 - s1` clearly shows that the result is by subtracting `s1`
from `s0`.

Let's look at a more complicated instruction, `local.set`:

```typescript
const instructions = {
  // ... other instructions
  local: {
    // ... other instructions

    /** [0x21] Syntax: `local.set $var`. Pop the stack and store the value
     * into the local variable. */
    set(s0: cellval, localidx: u32): void {
        let local = F().locals[localidx]
        assert(exists(local))
        local.value = s0
    },

    // ... other instructions
  },
  // ... other instructions
}
```

The documentation comment gives a new piece of information: syntax. It gives an
example of how to use the instruction when the input declaration is not clear
enough. In this case, `local.set $var` is the minimum syntax of this
instruction, and `$var` is a WebAssembly label that will be compiled to a local
variable index (defined by the input parameter `localidx: u32`).

`local.set` also requires a stack value (`s0`) of any numeric type (`cellval`).

`local.set` pushes nothing back to the stack, so the return type is `void`.

From the body, we know that this `local.set` first gets local variable from the
current WebAssembly Frame (`F()`). The Frame contains contextual information,
and `locals` is an array of the local variables.

The second line of the body uses two simulated behavior functions: `assert` and
`exists`. A simulated behavior represents something that the WebAssembly runtime
does, but its call position is not necessary where or when it happens. Here,
`assert` verifies that its argument is true. If it is not, `assert` will result
in `trap` (error). The `exists` function test whether a thing exists. The
combination `assert(exists(local))` means that the set behavior
`local.value = s0` happens only when a local variable at `F().locals[localidx]`
exists.
