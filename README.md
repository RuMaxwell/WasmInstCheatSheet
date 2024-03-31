# Wasm Instructions Cheat Sheet

This manual ([Wasm Instructions Cheat Sheet](./wasm-instructions-cheat-sheet.ts)
) aims to provide an exhaustive reference for understanding and using 
WebAssembly instructions in the text format.

You will find comprehensive information about each WebAssembly instruction,
covering:

- **Input Values**: The number and type(s) of values that are taken from the
  stack or given with the instruction as arguments.
- **Output Values**: The number and type(s) of values that are returned and
  pushed back to the stack.
- **Semantics**: A description of what the instruction does.
- **Execution**: Pseudo code written in TypeScript, illustrating how the
  instruction achieves its purpose.
- **(Optional) Opcode**: The code representing the instruction in the binary
  format.
- **(Optional) Syntax**: An example of how to write the instruction in the text
  format.

## Examples

Let's learn how to use this manual by examples.

The instruction `i32.const` is defined as:

```typescript
const instructions = {
  // ... other instructions
  i32: {
    // ... other instructions

    /** [0x41] Push an instant number. */
    const(x: i32): i32 { return x },

    // ... other instructions
  },
  // ... other instructions
}
```

The `instructions` object houses the definition for the `i32.const` instruction
at `instructions.i32.const`. It includes the following components:

- Documentation comments (between `/**` and `*/`)
- Name (`const`)
- Parameter list (`(x: i32)`)
- Return type (`: i32`)
- Body (pseudocode): `{ return x }`

The documentation comments provide the opcode (binary representation) of the
instruction (in this case, `0x41`) and explaining that it pushes an instant
value to the stack.

> Instant values are explicitly specified within the instruction rather than
> being taken from the stack.

The parameter list defines the single input required by the instruction, namely
an 32-bit integer (`i32`) `x` (which serves as an instant value).

The return type specifies the data type of the single value produced (a 32-bit
integer, `i32`). If the return type is an array, the values are pushed one by
one to the stack.

The body demonstrates the operation performed by the instruction, confirming the
commentary above.

Upon executing `i32.const`, the resulting `i32` value gets pushed onto the top
of the stack. Additionally, since the input value is defined as an instant
(rather than a stack value), users must include it when invoking the instruction
— for instance, `i32.const 0`.



Let's see another example `i32.sub`. This time, it requires two stack values:

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

Regarding the `i32.sub` instruction, its opcode is `0x6b`, and its purpose is
subtracting `s1` from `s0`. Both inputs are drawn from the stack. Stack input
names follow the pattern `s0`, `s1`, `s2`, etc. (exceptionally, `ss` for any
number of stack values), with push order matching the order of their appearance
in the function call. Consequently, popping occurs in reverse order. WebAssembly
sets the stack value ordering per instruction, meaning that for arithmetic
operations such as `3 - 2`, the proper syntax would be:

`(i32.sub (i32.const 3) (i32.const 2))`

Alternatively, another valid arrangement could be:

```
i32.const 2
i32.const 3
i32.sub
```

WebAssembly's validation mechanism guarantees that both stack inputs adhere to
the `i32` type prior to processing through the `i32.sub` instruction. As evident
in the pseudocode, the outcome results from subtracting `s1` from `s0`:

`{ return s0 - s1 }`



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

Documentation comments offer syntax examples to clarify usage when input
declarations are not enough for knowing how to use the instruction. Consider the
`local.set` instruction, whose minimal syntax involves `local.set $var`. The
symbol `$var` signifies a WebAssembly label that translates into a local
variable index during compilation (as determined by the `localidx: u32` input
parameter).

As indicated in the input requirements, `local.set` expects a single operand
(`s0`), applicable to any numeric type (designated hereafter as `cellval`). Note
that no value is returned, making the return type `void`.

Examining the body reveals that `local.set` extracts the local variable
associated with the current WebAssembly frame (denoted as `F()`). Within this
frame, `locals` refers to an array containing local variables.

In the body, `assert` and `exists` are two simulated behavioral methods. These
serve as placeholders for actions typically executed by the WebAssembly runtime,
without indicating precise placement or timing. Specifically, `assert` checks
whether its input evaluates as true. If false, `assert` triggers a `trap`
(error). Meanwhile, `exists` determines if a certain entity is present. Thus,
`assert(exists(local))` implies that setting the local variable (`local.value =
s0`) only happens when a corresponding local variable resides at
`F().locals[localidx]`.

## Contributions

This manual still needs a lot of contributions! To get started, consider helping
me with the following tasks:

- **Proofreading**: Spot typos, unclear phrases, or incorrect information in the
  README and documentation.
- **Expanding documentations**: Enhance our content by adding more documentations
  or submitting issues about what you find it hard to understand.
- **Completing missing documentations**: Fill in documentations where they are
  left off.
- **WebAssembly consistency**: Ensure instruction definitions (such as input and
  output types, opcodes) align with the WebAssembly specification.
- **Instruction accuracy**: Include more valid instructions in WebAssembly;
  remove any non-existent ones.

Your assistance is much appreciated! If you have additional ideas on how to
enhance the project, feel free to open an issue or pull request.
