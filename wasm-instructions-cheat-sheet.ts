type i8 = number
type i16 = number
type i32 = number
type bool = 0 | 1
type i64 = number
type i128 = number
type s32 = number
type s64 = number
type u8 = number
type u32 = number
type u64 = number
type f32 = number
type f64 = number
type v128 = number
type i8x16 = number
type i16x8 = number
type i32x4 = number
type i64x2 = number
type f32x4 = number
type f64x2 = number
type vec<t, len> = number[]
type addr = number
/** a stack value */
type val = cellval | FrameActivation
type cellval = num | v128 | ref
type num = i32 | i64 | f32 | f64
type ref = funcref | externref
type funcref = number
type externref = number
type anytype = typeidx | typeref
type typeidx = u32
type typeref = u32
type valtype = numtype | vectype | reftype
type numtype = typeof i32typecode | typeof i64typecode
    | typeof f32typecode | typeof f64typecode
type vectype = typeof v128typecode
type reftype = typeof funcreftypecode | typeof externreftypecode
type refnull<t> = null
type instruction_byte = number

type memarg = { offset: u32, align: u32 }

const i32typecode = 0x7f
const i64typecode = 0x7e
const f32typecode = 0x7d
const f64typecode = 0x7c
const v128typecode = 0x7b
const funcreftypecode = 0x70
const externreftypecode = 0x6f
const PAGE_SIZE = 65536

/// Simulated Behaviors of the Runtime ///

/** Stands for some operation that does not representable by TypeScript or not
 * learned from the specification. */
function _(...args: any[]): never { throw '' }
/** Stands for a preprocessor operation that does not do anything in the
 * runtime. */
function virtual(): void { }
/** Triggers an error. */
function trap(): never { throw 'trap' }
/** States that the operation requires a condition being true to run. */
function assert(statement: boolean): void { if (!statement) trap() }
/** Pushes a value to the evaluation stack. */
function push(value: cellval): void { _() }
/** Pushes a frame activation to the stack. */
function push_frame_activation(value: FrameActivation): void { _() }
/** Pushes a label to the stack with the current function as the continuation. */
function push_label(value: Label): void { _() }
function jump_to_start_of_instruction(): void { _() }
/** Pops a value from the evaluation stack. */
function pop(): cellval | undefined { _() }
function pop_label(): Label | undefined { _() }
/** Gets the type code of a value. */
function typeOf(value: cellval): valtype { _() }
/** Gets the runtime frame context. */
function F(): Frame { _() }
/** Gets the runtime store context. */
function S(): Store { _() }
/** Tests whether a cell, a global, a table, or a memory object exists. */
function exists(cell: Cell | Glob | Tab | Mem | Data | Func): boolean { return !!cell }
/** Grows a table by `n` bytes with initial value `initVal`. */
function growTab(tab: Tab, n: i32, initVal: ref): boolean { _() }
/** Grows a memory by `n` bytes. */
function growMem(mem: Mem, n: i32): boolean { _() }
/** Reinterprets 2 consecutive bytes into an i16. An i16 is represented by an i32
 * or i64 when being used. */
function bytesToI16(bytes: i8[]): i32 | i64 { _() }
/** Reinterprets 4 consecutive bytes into an i32. An i32 is represented by an i32
 * or i64 when being used. */
function bytesToI32(bytes: i8[]): i32 | i64 { _() }
/** Reinterprets 8 consecutive bytes into an i64. */
function bytesToI64(bytes: i8[]): i64 { _() }
/** Reinterprets 4 consecutive bytes into an f32. */
function bytesToF32(bytes: i8[]): f32 { _() }
/** Reinterprets 8 consecutive bytes into an f64. */
function bytesToF64(bytes: i8[]): f64 { _() }
/** Reinterprets 16 consecutive bytes into a v128. */
function bytesToV128(bytes: i8[]): v128 { _() }
/** Reinterprets an i16 represented in i32 or i64 into 2 consecutive bytes. */
function i16ToBytes(value: i32 | i64): i8[] { _() }
/** Reinterprets an i32 represented in i32 or i64 into 4 consecutive bytes. */
function i32ToBytes(value: i32 | i64): i8[] { _() }
/** Reinterprets an i64 into 8 consecutive bytes. */
function i64ToBytes(value: i64): i8[] { _() }
/** Reinterprets an f32 into 4 consecutive bytes. */
function f32ToBytes(value: f32): i8[] { _() }
/** Reinterprets an f64 into 8 consecutive bytes. */
function f64ToBytes(value: f64): i8[] { _() }
/** Reinterprets a v128 into 16 consecutive bytes. */
function v128ToBytes(value: v128): i8[] { _() }
/** Gets the return types of a function type. */
function return_types_of(type: anytype): anytype[] { _() }

/** Asserts that a table at `tableidx` exists and returns it. */
function assertGetTab(tableidx: u32): Tab {
    let tableaddr = F().module.tableaddrs[tableidx]
    assert(exists(tableaddr))
    let tab = S().tables[tableaddr.value]
    assert(exists(tab))
    return tab
}

/** Asserts that an element at `elemidx` exists in the memory and returns it. */
function assertGetElem(elemidx: u32): Elem {
    let elemaddr = F().module.elemaddrs[elemidx]
    assert(exists(elemaddr))
    let elem = S().elems[elemaddr.value]
    assert(exists(elem))
    return elem
}

/** Asserts that a memory at `memidx` exists and returns it. */
function assertGetMem(memidx: u32): Mem {
    let memaddr = F().module.memoryaddrs[memidx]
    assert(exists(memaddr))
    let mem = S().mems[memaddr.value]
    assert(exists(mem))
    return mem
}

function assertGetData(dataidx: u32): Data {
    let dataaddr = F().module.dataaddrs[dataidx]
    assert(exists(dataaddr))
    let data = S().datas[dataaddr.value]
    assert(exists(data))
    return data
}

/** Represents the runtime frame. */
interface Frame {
    locals: Cell[]
    module: {
        globaladdrs: Cell[]
        tableaddrs: Cell[]
        elemaddrs: Cell[]
        memoryaddrs: Cell[]
        dataaddrs: Cell[]
        funcaddrs: Cell[]
    }
}

/** Represents a cell. */
interface Cell {
    value: cellval
}

type FrameActivation = {
    frame: Frame
    arity: number
}

/** Represents a runtime label. */
type Label = {
    arity: number
    instruction_bytes: instruction_byte[]
}

/** Represents the runtime store. */
interface Store {
    globals: Glob[]
    tables: Tab[]
    elems: Elem[]
    mems: Mem[]
    datas: Data[]
    funcs: Func[]
}

/** Represents a global variable. */
interface Glob {
    mut: mut
    value: cellval
}

/** Represents the mutability of a global variable. */
enum mut {
    const = 0x00,
    var = 0x01,
}

/** Represents a table. */
interface Tab {
    elem: cellval[]
}

/** Represents an element. */
interface Elem {
    elem: cellval[]
}

/** Represents a memory instance. */
interface Mem {
    data: i8[]
    /** Size of `mem` is length of `mem.data` divided by page_size */
    get size(): i32
}

/** Represents a data instance. */
interface Data {
    data: i8[]
}
function emptyData(): Data {
    return { data: [] }
}

/** Represents a function instance. */
interface Func {
    type: anytype
    code: {
        locals: anytype[]
        body: instruction_byte[]
    }
    module: Frame['module']
}

/**
 * Example:
 *
 * `i32.const` represents the instruction `i32.const`.
 *
 * Input parameters `s0`, `s1`, ... represent the stack values. The values are
 * popped in the ascending order of the parameters, which means the actual order
 * (defined by the WebAssembly specification) of the instruction operands are in
 * the same direction. e.g. `(i32.gt_s (i32.const 2) (i32.const 1))` returns 1
 * (which means `2 > 1 == true`). The first operand `2` is pushed to the stack
 * before the second operand `1`. This also goes with separated instructions:
 *
 * ```
 * i32.const 2
 * i32.const 1
 * i32.gt_s
 * ```
 *
 * Input parameter `ss` represents any number of values popped from the
 * stack.
 *
 * Other parameters (like `x`, `y`, `funcidx`) represent the instants of the
 * instruction, in the same order of what they are defined by the WebAssembly
 * specification.
 *
 * The return type represents the result given back to the stack. If the return
 * type is an array, the values in the array are pushed one by one back to the
 * stack.
 */
const instructions = {
    /// Numeric Instructions ///

    i32: {
        /** [0x41] Push an instant number. */
        const(x: u32): i32 { return x },

        /** [0x67] Count the leading zeros of `s0`. */
        clz(s0: i32): i32 { _() },
        /** [0x68] Count the trailing zeros of `s0`. */
        ctz(s0: i32): i32 { _() },
        /** [0x69] Count the amount of ones in `s0`. */
        popcnt(s0: i32): i32 { _() },

        /** [0x6a] Add two numbers. */
        add(s0: i32, s1: i32): i32 { return s0 + s1 },
        /** [0x6b] Subtract `s1` from `s0`. */
        sub(s0: i32, s1: i32): i32 { return s0 - s1 },
        /** [0x6c] Multiply two numbers. */
        mul(s0: i32, s1: i32): i32 { return s0 * s1 },
        /** [0x6d] Divide `s0` by `s1`. */
        div_s(s0: s32, s1: s32): i32 { return s0 / s1 },
        /** [0x6e] Divide `s0` by `s1`. */
        div_u(s0: u32, s1: u32): i32 { return s0 / s1 },
        /** [0x6f] Get the remainder of dividing `s0` by `s1`. */
        rem_s(s0: s32, s1: s32): i32 { return s0 % s1 },
        /** [0x70] Get the remainder of dividing `s0` by `s1`. */
        rem_u(s0: u32, s1: u32): i32 { return s0 % s1 },

        /** [0x71] Get the bitwise AND of two numbers. */
        and(s0: i32, s1: i32): i32 { return s0 & s1 },
        /** [0x72] Get the bitwise OR of two numbers. */
        or(s0: i32, s1: i32): i32 { return s0 | s1 },
        /** [0x73] Get the bitwise XOR of two numbers. */
        xor(s0: i32, s1: i32): i32 { return s0 ^ s1 },
        /** [0x74] Perform bitwise left-shift of `s1` bits on `s0`. */
        shl(s0: i32, s1: i32): i32 { return s0 << s1 },
        /** [0x75] Perform signed bitwise right-shift of `s1` bits on `s0`. */
        shr_s(s0: s32, s1: i32): i32 { return s0 >>> s1 },
        /** [0x76] Perform unsigned bitwise right-shift of `s1` bits on `s0`. */
        shr_u(s0: u32, s1: i32): i32 { return s0 >>> s1 },
        /** [0x77] Perform bitwise left-rotate of `s1` bits on `s0`. */
        rotl(s0: i32, s1: i32): i32 { _() },
        /** [0x78] Perform bitwise right-rotate of `s1` bits on `s0`. */
        rotr(s0: i32, s1: i32): i32 { _() },

        /** [0x45] Test if `s0` equals to zero. */
        eqz(s0: i32): bool { return s0 ? 1 : 0 },
        /** [0x46] Test if two numbers equal. */
        eq(s0: i32, s1: i32): bool { return s0 === s1 ? 1 : 0 },
        /** [0x47] Test if two numbers do NOT equal. */
        ne(s0: i32, s1: i32): bool { return s0 !== s1 ? 1 : 0 },
        /** [0x48] Test if `s0` < `s1`. */
        lt_s(s0: s32, s1: s32): bool { return s0 < s1 ? 1 : 0 },
        /** [0x49] Test if `s0` < `s1`. */
        lt_u(s0: u32, s1: u32): bool { return s0 < s1 ? 1 : 0 },
        /** [0x4a] Test if `s0` > `s1`. */
        gt_s(s0: s32, s1: s32): bool { return s0 > s1 ? 1 : 0 },
        /** [0x4b] Test if `s0` > `s1`. */
        gt_u(s0: u32, s1: u32): bool { return s0 > s1 ? 1 : 0 },
        /** [0x4c] Test if `s0` <= `s1`. */
        le_s(s0: s32, s1: s32): bool { return s0 <= s1 ? 1 : 0 },
        /** [0x4d] Test if `s0` <= `s1`. */
        le_u(s0: u32, s1: u32): bool { return s0 <= s1 ? 1 : 0 },
        /** [0x4e] Test if `s0` >= `s1`. */
        ge_s(s0: s32, s1: s32): bool { return s0 >= s1 ? 1 : 0 },
        /** [0x4f] Test if `s0` >= `s1`. */
        ge_u(s0: u32, s1: u32): bool { return s0 >= s1 ? 1 : 0 },

        /** Take the lowest s8 from `s0`, and extend it back into s32. */
        extend8_s(s0: s32): s32 { return s0 & 0xff },
        /** Take the lowest s16 from `s0`, and extend it back into s32. */
        extend16_s(s0: s32): s32 { return s0 & 0xffff },

        /** [0xa7] Convert `s0` into i32 by discarding the higher 32 bits. */
        wrap_i64(s0: i64): i32 { return s0 % (1 << 32) },
        /** [0xa8] Convert `s0` into i32, where the integral part of `s0` must
         * fall into the signed i32 range. */
        trunc_f32_s(s0: f32): s32 { return Math.trunc(s0) },
        /** [0xa9] Convert `s0` into i32, where the integral part of `s0` must
         * fall into the unsigned i32 range. */
        trunc_f32_u(s0: f32): u32 { return Math.trunc(s0) },
        /** [0xaa] Convert `s0` into i32, where the integral part of `s0` must
         * fall into the signed i32 range. */
        trunc_f64_u(s0: f64): u32 { return Math.trunc(s0) },
        /** [0xab] Convert `s0` into i32, where the integral part of `s0` must
         * fall into the unsigned i32 range. */
        trunc_f64_s(s0: f64): s32 { return Math.trunc(s0) },
        trunc_sat_f32_u(s0: f32): u32 { _() },
        trunc_sat_f32_s(s0: f32): s32 { _() },
        trunc_sat_f64_u(s0: f64): u32 { _() },
        trunc_sat_f64_s(s0: f64): s32 { _() },
        /** [0xbc] Reinterpret bits of `s0` to i32. */
        reinterpret_f32(s0: f32): i32 { _() },

        /** [0x28] Load a number from memory at the offset `s0`. */
        load(s0: i32, memarg: memarg): i32 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 4)
            return bytesToI32(bytes)
        },
        /** [0x2c] Load a signed i8 from memory at the offset `s0` and extend it
         * into an i32. */
        load8_s(s0: i32, memarg: memarg): i32 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 1 > mem.data.length) { trap() }
            let byte = mem.data[offset]
            return byte
        },
        /** [0x2d] Load an unsigned i8 from memory at the offset `s0` and extend
         * it into an i32. */
        load8_u(s0: i32, memarg: memarg): i32 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 1 > mem.data.length) { trap() }
            let byte = mem.data[offset]
            return byte
        },
        /** [0x2e] Load a signed i16 from memory at the offset `s0` and extend
         * it into an i32. */
        load16_s(s0: i32, memarg: memarg): i32 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 2 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 2)
            return bytesToI16(bytes)
        },
        /** [0x2f] Load an unsigned i16 from memory at the offset `s0` and
         * extend it into an i32. */
        load16_u(s0: i32, memarg: memarg): i32 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 2 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 2)
            return bytesToI16(bytes)
        },
        /** [0x36] Store `s1` into memory at the offset `s0`. */
        store(s0: i32, s1: i32, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 4, ...i32ToBytes(s1))
        },
        /** [0x3a] Store an i8 `s1` represented in i32 into memory at the offset
         * `s0`. */
        store8(s0: i32, s1: i32, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 1 > mem.data.length) { trap() }
            mem.data[s0] = s1
        },
        /** [0x3b] Store an i16 `s1` represented in i32 into memory at the
         * offset `s0`. */
        store16(s0: i32, s1: i32, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 2 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 2, ...i16ToBytes(s1))
        },
    },
    i64: {
        /** [0x42] Push an instant number. */
        const(x: u64): i64 { return x },

        /** [0x79] Count the leading zeros of `s0`. */
        clz(s0: i64): i64 { _() },
        /** [0x7a] Count the trailing zeros of `s0`. */
        ctz(s0: i64): i64 { _() },
        /** [0x7b] Count the amount of ones in `s0`. */
        popcnt(s0: i64): i64 { _() },

        /** [0x7c] Add two numbers. */
        add(s0: i64, s1: i64): i64 { return s0 + s1 },
        /** [0x7d] Subtract `s1` from `s0`. */
        sub(s0: i64, s1: i64): i64 { return s0 - s1 },
        /** [0x7e] Multiply two numbers. */
        mul(s0: i64, s1: i64): i64 { return s0 * s1 },
        /** [0x7f] Divide `s0` by `s1`. */
        div_s(s0: s64, s1: s64): i64 { return s0 / s1 },
        /** [0x80] Divide `s0` by `s1`. */
        div_u(s0: u64, s1: u64): i64 { return s0 / s1 },
        /** [0x81] Get the remainder of dividing `s0` by `s1`. */
        rem_s(s0: s64, s1: s64): i64 { return s0 % s1 },
        /** [0x82] Get the remainder of dividing `s0` by `s1`. */
        rem_u(s0: u64, s1: u64): i64 { return s0 % s1 },

        /** [0x83] Get the bitwise AND of two numbers. */
        and(s0: i64, s1: i64): i64 { return s0 & s1 },
        /** [0x84] Get the bitwise OR of two numbers. */
        or(s0: i64, s1: i64): i64 { return s0 | s1 },
        /** [0x85] Get the bitwise XOR of two numbers. */
        xor(s0: i64, s1: i64): i64 { return s0 ^ s1 },
        /** [0x86] Perform bitwise left-shift of `s1` bits on `s0`. */
        shl(s0: i64, s1: i64): i64 { return s0 << s1 },
        /** [0x87] Perform signed bitwise right-shift of `s1` bits on `s0`. */
        shr_s(s0: s64, s1: i64): i64 { return s1 >>> s0 },
        /** [0x88] Perform unsigned bitwise right-shift of `s1` bits on `s0`. */
        shr_u(s0: u64, s1: i64): i64 { return s1 >>> s0 },
        /** [0x89] Perform bitwise left-rotate of `s1` bits on `s0`. */
        rotl(s0: i64, s1: i64): i64 { _() },
        /** [0x8a] Perform bitwise left-rotate of `s1` bits on `s0`. */
        rotr(s0: i64, s1: i64): i64 { _() },

        /** [0x50] Test if `s0` equals to 0. */
        eqz(s0: i64): bool { return s0 ? 1 : 0 },
        /** [0x51] Test if two numbers equal. */
        eq(s0: i64, s1: i64): bool { return s0 === s1 ? 1 : 0 },
        /** [0x52] Test if two numbers do NOT equal. */
        ne(s0: i64, s1: i64): bool { return s0 !== s1 ? 1 : 0 },
        /** [0x53] Test if `s0` < `s1`. */
        lt_s(s0: s64, s1: s64): bool { return s0 < s1 ? 1 : 0 },
        /** [0x54] Test if `s0` < `s1`. */
        lt_u(s0: u64, s1: u64): bool { return s0 < s1 ? 1 : 0 },
        /** [0x55] Test if `s0` > `s1`. */
        gt_s(s0: s64, s1: s64): bool { return s0 > s1 ? 1 : 0 },
        /** [0x56] Test if `s0` > `s1`. */
        gt_u(s0: u64, s1: u64): bool { return s0 > s1 ? 1 : 0 },
        /** [0x57] Test if `s0` <= `s1`. */
        le_s(s0: s64, s1: s64): bool { return s0 <= s1 ? 1 : 0 },
        /** [0x58] Test if `s0` <= `s1`. */
        le_u(s0: u64, s1: u64): bool { return s0 <= s1 ? 1 : 0 },
        /** [0x59] Test if `s0` >= `s1`. */
        ge_s(s0: s64, s1: s64): bool { return s0 >= s1 ? 1 : 0 },
        /** [0x5a] Test if `s0` >= `s1`. */
        ge_u(s0: u64, s1: u64): bool { return s0 >= s1 ? 1 : 0 },

        /** Take the lowest s8 of `s0`, and extend it back into s64. */
        extend8_s(s0: s64): i64 { return s0 & 0xff },
        /** Take the lowest s16 of `s0`, and extend it back into s64. */
        extend16_s(s0: s64): i64 { return s0 & 0xffff },
        /** Take the lowest s32 of `s0`, and extend it back into s64. */
        extend32_s(s0: s64): s64 { return s0 & 0xffffffff },

        /** [0xac] Extend `s0` to i64. */
        extend_i32_s(s0: s32): s64 { return s0 },
        /** [0xad] Extend `s0` to i64. */
        extend_i32_u(s0: u32): u64 { return s0 },

        /** [0xae] Convert `s0` into i64, where the integral part of `s0` must
         * fall into the signed i64 range. */
        trunc_f32_s(s0: f32): s64 { return Math.trunc(s0) },
        /** [0xaf] Convert `s0` into i64, where the integral part of `s0` must
         * fall into the unsigned i64 range. */
        trunc_f32_u(s0: f32): u64 { return Math.trunc(s0) },
        /** [0xb0] Convert `s0` into i64, where the integral part of `s0` must
         * fall into the signed i64 range. */
        trunc_f64_s(s0: f64): s64 { return Math.trunc(s0) },
        /** [0xb1] Convert `s0` into i64, where the integral part of `s0` must
         * fall into the unsigned i64 range. */
        trunc_f64_u(s0: f64): u64 { return Math.trunc(s0) },
        trunc_sat_f32_u(s0: f32): u64 { _() },
        trunc_sat_f32_s(s0: f32): s64 { _() },
        trunc_sat_f64_u(s0: f64): u64 { _() },
        trunc_sat_f64_s(s0: f64): s64 { _() },
        /** [0xbd] Reinterpret bits of `s0` to i64. */
        reinterpret_f64(s0: f64): i64 { _() },

        /** [0x29] Load a number from memory at the offset `s0`. */
        load(s0: i64, memarg: memarg): i64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 8 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 8)
            return bytesToI64(bytes)
        },
        /** [0x30] Load a signed i8 from memory at the offset `s0` and extend it
         * into an i64. */
        load8_s(s0: i32, memarg: memarg): i64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 1 > mem.data.length) { trap() }
            let byte = mem.data[offset]
            return byte
        },
        /** [0x31] Load an unsigned i8 from memory at the offset `s0` and extend
         * it into an i64. */
        load8_u(s0: i32, memarg: memarg): i64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 1 > mem.data.length) { trap() }
            let byte = mem.data[offset]
            return byte
        },
        /** [0x32] Load a signed i16 from memory at the offset `s0` and extend
         * it into an i64. */
        load16_s(s0: i32, memarg: memarg): i64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 2 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 2)
            return bytesToI16(bytes)
        },
        /** [0x33] Load an unsigned i16 from memory at the offset `s0` and
         * extend it into an i64. */
        load16_u(s0: i32, memarg: memarg): i64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 2 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 2)
            return bytesToI16(bytes)
        },
        /** [0x34] Load a signed i32 from memory at the offset `s0` and extend
         * it into an i64. */
        load32_s(s0: i32, memarg: memarg): i64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 4)
            return bytesToI32(bytes)
        },
        /** [0x35] Load an unsigned i32 from memory at the offset `s0` and
         * extend it into an i64. */
        load32_u(s0: i32, memarg: memarg): i64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 4)
            return bytesToI32(bytes)
        },
        /** [0x37] Store `s1` into memory at the offset `s0`. */
        store(s0: cellval, s1: i64, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 8 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 8, ...i64ToBytes(s1))
        },
        /** [0x3c] Store an i8 `s1` represented in i64 into memory at the offset
         * `s0`. */
        store8(s0: i32, s1: i64, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 1 > mem.data.length) { trap() }
            mem.data[s0] = s1
        },
        /** [0x3d] Store an i16 `s1` represented in i64 into memory at the
         * offset `s0`. */
        store16(s0: i32, s1: i64, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 2 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 2, ...i16ToBytes(s1))
        },
        /** [0x3e] Store an i32 `s1` represented in i64 into memory at the
         * offset `s0`. */
        store32(s0: i32, s1: i64, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 4, ...i32ToBytes(s1))
        },
    },
    f32: {
        /** [0x43] Push an instant number. */
        const(x: f32): f32 { return x },

        /** [0x8b] Get the absolute value of `s0`. */
        abs(s0: f32): f32 { return Math.abs(s0) },
        /** [0x8c] Get the negative value of `s0`. */
        neg(s0: f32): f32 { return -s0 },
        /** [0x8d] Round up `s0` to the next integer. */
        ceil(s0: f32): f32 { return Math.ceil(s0) },
        /** [0x8e] Round down `s0` to the next integer. */
        floor(s0: f32): f32 { return Math.floor(s0) },
        /** [0x8f] Truncate the fractional part of `s0`. */
        trunc(s0: f32): f32 { return Math.trunc(s0) },
        /** [0x90] Round `s0` to the nearest integer. */
        nearest(s0: f32): f32 { return Math.fround(s0) },
        /** [0x91] Get the square root of `s0`. */
        sqrt(s0: f32): f32 { return Math.sqrt(s0) },

        /** [0x92] Add two numbers. */
        add(s0: f32, s1: f32): f32 { return s0 + s1 },
        /** [0x93] Subtract `s1` from `s0`. */
        sub(s0: f32, s1: f32): f32 { return s0 - s1 },
        /** [0x94] Multiply two numbers. */
        mul(s0: f32, s1: f32): f32 { return s0 * s1 },
        /** [0x95] Divide `s0` by `s1`. */
        div(s0: f32, s1: f32): f32 { return s0 / s1 },

        /** [0x96] Get the minimum of two numbers. */
        min(s0: f32, s1: f32): f32 { return Math.min(s0, s1) },
        /** [0x97] Get the maximum of two numbers. */
        max(s0: f32, s1: f32): f32 { return Math.max(s0, s1) },
        /** [0x98] Copy just the sign bit of `s1` and apply it to `s0`. */
        copysign(s0: f32, s1: f32): f32 { return s0 * s1 / Math.abs(s1) },

        /** [0x5b] Test if two numbers equal. */
        eq(s0: f32, s1: f32): bool { return s0 === s1 ? 1 : 0 },
        /** [0x5c] Test if two numbers do NOT equal. */
        ne(s0: f32, s1: f32): bool { return s0 !== s1 ? 1 : 0 },
        /** [0x5d] Test if `s0` < `s1`. */
        lt(s0: f32, s1: f32): bool { return s0 < s1 ? 1 : 0 },
        /** [0x5e] Test if `s0` > `s1`. */
        gt(s0: f32, s1: f32): bool { return s0 > s1 ? 1 : 0 },
        /** [0x5f] Test if `s0` <= `s1`. */
        le(s0: f32, s1: f32): bool { return s0 <= s1 ? 1 : 0 },
        /** [0x60] Test if `s0` >= `s1`. */
        ge(s0: f32, s1: f32): bool { return s0 >= s1 ? 1 : 0 },

        /** [0xb6] Demote `s0` to f32. */
        demote_f64(s0: f64): f32 { _() },
        /** [0xb2] Convert `s0` to f32. */
        convert_i32_s(s0: s32): f32 { return s0 + 0.0 },
        /** [0xb3] Convert `s0` to f32. */
        convert_i32_u(s0: u32): f32 { return s0 + 0.0 },
        /** [0xb4] Convert `s0` to f32. */
        convert_i64_s(s0: s64): f32 { return s0 + 0.0 },
        /** [0xb5] Convert `s0` to f32. */
        convert_i64_u(s0: u64): f32 { return s0 + 0.0 },
        /** [0xbe] Reinterpret bits of `s0` to f32. */
        reinterpret_i32(s0: i32): f32 { _() },

        /** [0x2a] Load a number from memory at the offset `s0`. */
        load(s0: f32, memarg: memarg): f32 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 4)
            return bytesToF32(bytes)
        },
        /** [0x38] Store `s1` into memory at the offset `s0`. */
        store(s0: i32, s1: f32, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 4, ...f32ToBytes(s1))
        },
    },
    f64: {
        /** [0x44] Push an instant number. */
        const(x: f64): f64 { return x },

        /** [0x99] Get the absolute value of `s0`. */
        abs(s0: f64): f64 { return Math.abs(s0) },
        /** [0x9a] Get the negative value of `s0`. */
        neg(s0: f64): f64 { return -s0 },
        /** [0x9b] Round up `s0` to the next integer. */
        ceil(s0: f64): f64 { return Math.ceil(s0) },
        /** [0x9c] Round down `s0` to the next integer. */
        floor(s0: f64): f64 { return Math.floor(s0) },
        /** [0x9d] Truncate the fractional part of `s0`. */
        trunc(s0: f64): f64 { return Math.trunc(s0) },
        /** [0x9e] Round `s0` to the nearest integer. */
        nearest(s0: f64): f64 { return Math.round(s0) },
        /** [0x9f] Get the square root of `s0`. */
        sqrt(s0: f64): f64 { return Math.sqrt(s0) },

        /** [0xa0] Add two numbers. */
        add(s0: f64, s1: f64): f64 { return s0 + s1 },
        /** [0xa1] Subtract `s1` from `s0`. */
        sub(s0: f64, s1: f64): f64 { return s0 - s1 },
        /** [0xa2] Multiply two numbers. */
        mul(s0: f64, s1: f64): f64 { return s0 * s1 },
        /** [0xa3] Divide `s0` by `s1`. */
        div(s0: f64, s1: f64): f64 { return s0 / s1 },

        /** [0xa4] Get the minimum of two numbers. */
        min(s0: f64, s1: f64): f64 { return Math.min(s0, s1) },
        /** [0xa5] Get the maximum of two numbers. */
        max(s0: f64, s1: f64): f64 { return Math.max(s0, s1) },
        /** [0xa6] Copy just the sign bit of `s1` and apply it to `s0`. */
        copysign(s0: f64, s1: f64): f64 { return s0 * s1 / Math.abs(s1) },

        /** [0x61] Test if two numbers equal. */
        eq(s0: f64, s1: f64): bool { return s0 === s1 ? 1 : 0 },
        /** [0x62] Test if two numbers do NOT equal. */
        ne(s0: f64, s1: f64): bool { return s0 !== s1 ? 1 : 0 },
        /** [0x63] Test if `s0` < `s1`. */
        lt(s0: f64, s1: f64): bool { return s0 < s1 ? 1 : 0 },
        /** [0x64] Test if `s0` > `s1`. */
        gt(s0: f64, s1: f64): bool { return s0 > s1 ? 1 : 0 },
        /** [0x65] Test if `s0` <= `s1`. */
        le(s0: f64, s1: f64): bool { return s0 <= s1 ? 1 : 0 },
        /** [0x66] Test if `s0` >= `s1`. */
        ge(s0: f64, s1: f64): bool { return s0 >= s1 ? 1 : 0 },

        /** [0xbb] Promote `s0` to f64. */
        promote_f32(s0: f32): f64 { _() },
        /** [0xb7] Convert `s0` to f64. */
        convert_i32_u(s0: u32): f64 { return s0 + 0.0 },
        /** [0xb8] Convert `s0` to f64. */
        convert_i32_s(s0: s32): f64 { return s0 + 0.0 },
        /** [0xb9] Convert `s0` to f64. */
        convert_i64_u(s0: u64): f64 { return s0 + 0.0 },
        /** [0xba] Convert `s0` to f64. */
        convert_i64_s(s0: s64): f64 { return s0 + 0.0 },
        /** [0xbf] Reinterpret bits of `s0` to f64. */
        reinterpret_i64(s0: i64): f64 { _() },

        /** [0x2b] Load a number from memory at the offset `s0`. */
        load(s0: f64, memarg: memarg): f64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 8 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 8)
            return bytesToF64(bytes)
        },
        /** [0x39] Store `s1` into memory at the offset `s0`. */
        store(s0: i32, s1: f64, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 8 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 8, ...f64ToBytes(s1))
        },
    },

    /// Vector Instructions ///

    v128: {
        /** Push an instant i128.
         *
         * An i128 instant is in one of the following forms:
         * - `i8x16` i8_0 i8_1 ... i8_15
         * - `i16x8` i16_0 i16_1 ... i16_7
         * - `i32x4` i32_0 i32_1 i32_2 i32_3
         * - `i64x2` i64_0 i64_1
         *
         * The numbers are given in little-endian order, in other words, the
         * first number carries the least significant bytes, and the last number
         * carries the most significant bytes. For example, the same value
         * `0xffeeddccbbaa99887766554433221100` can be given `i8x16 0x00 0x11
         * 0x22 0x33 0x44 0x55 0x66 0x77 0x88 0x99 0xaa 0xbb 0xcc 0xdd 0xee
         * 0xff` or `i16x8 0x1100 0x3322 0x5544 0x7766 0x9988 0xbbaa 0xddcc
         * 0xffee`.
         */
        const(x: i128): v128 { return x },

        not(s0: v128): v128 { _() },

        and(s0: v128, s1: v128): v128 { _() },
        andnot(s0: v128, s1: v128): v128 { _() },
        or(s0: v128, s1: v128): v128 { _() },
        xor(s0: v128, s1: v128): v128 { _() },

        bitselect(s0: v128, s1: v128, s2: v128): v128 { _() },

        any_true(s0: v128): bool { _() },

        load(s0: v128, memarg: memarg): v128 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 16 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 16)
            return bytesToV128(bytes)
        },
        store(s0: cellval, s1: v128, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 16 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 16, ...v128ToBytes(s1))
        },

    },
    i8x16: {
        shuffle(x: vec<u8, 16>): i8x16 { _() },
        swizzle(s0: i8x16): i8x16 { _() },

        /** Duplicate an i8 (stored as i32) value 16 times to produce an i8x16.
         * */
        splat(s0: i32): i8x16 { return bytesToV128(new Array(16).fill(s0)) },

        /** Select the `x`th lane of `s0` (`x` < 16) and return the unpacked
         * value. */
        extract_lane_u(s0: i8x16, x: u8): u32 { return v128ToBytes(s0)[x] },
        /** Select the `x`th lane of `s0` (`x` < 16) and return the unpacked
         * value. */
        extract_lane_s(s0: i8x16, x: u8): s32 { return v128ToBytes(s0)[x] },
        /** Replace the `x`th lane of `s0` (`x` < 16) into `s1` and return the
         * replaced v128. */
        replace_lane(s0: i8x16, s1: i32, x: u8): i8x16 {
            let bytes = v128ToBytes(s0)
            bytes[x] = s1
            return bytesToV128(bytes)
        },

        eq(s0: i8x16, s1: i8x16): bool { _() },
        ne(s0: i8x16, s1: i8x16): bool { _() },
        lt_u(s0: i8x16, s1: i8x16): bool { _() },
        lt_s(s0: i8x16, s1: i8x16): bool { _() },
        gt_u(s0: i8x16, s1: i8x16): bool { _() },
        gt_s(s0: i8x16, s1: i8x16): bool { _() },
        le_u(s0: i8x16, s1: i8x16): bool { _() },
        le_s(s0: i8x16, s1: i8x16): bool { _() },
        ge_u(s0: i8x16, s1: i8x16): bool { _() },
        ge_s(s0: i8x16, s1: i8x16): bool { _() },

        abs(s0: i8x16): i8x16 { _() },
        neg(s0: i8x16): i8x16 { _() },
        popcnt(s0: i8x16): i8x16 { _() },

        all_true(s0: i8x16): bool { _() },

        bitmask(s0: i8x16): i32 { _() },

        narrow_i16x8_u(s0: i16x8): i8x16 { _() },
        narrow_i16x8_s(s0: i16x8): i8x16 { _() },

        shl(s0: i8x16, s1: i32): i8x16 { _() },
        shr_u(s0: i8x16, s1: i32): i8x16 { _() },
        shr_s(s0: i8x16, s1: i32): i8x16 { _() },

        add(s0: i8x16, s1: i8x16): i8x16 { _() },
        sub(s0: i8x16, s1: i8x16): i8x16 { _() },

        min_u(s0: i8x16, s1: i8x16): i8x16 { _() },
        min_s(s0: i8x16, s1: i8x16): i8x16 { _() },
        max_u(s0: i8x16, s1: i8x16): i8x16 { _() },
        max_s(s0: i8x16, s1: i8x16): i8x16 { _() },

        add_sat_u(s0: i8x16, s1: i8x16): i8x16 { _() },
        add_sat_s(s0: i8x16, s1: i8x16): i8x16 { _() },
        sub_sat_u(s0: i8x16, s1: i8x16): i8x16 { _() },
        sub_sat_s(s0: i8x16, s1: i8x16): i8x16 { _() },

        avgr_u(s0: i8x16, s1: i8x16): i8x16 { _() },
    },
    i16x8: {
        /** Duplicate an i16 (stored as i32) value 8 times to produce an i16x8.
         * */
        splat(s0: i32): i16x8 { _() },

        /** Select the `x`th lane of `s0` (`x` < 8) and return the unpacked
         * value. */
        extract_lane_u(s0: i16x8, x: u8): u32 { _() },
        /** Select the `x`th lane of `s0` (`x` < 8) and return the unpacked
         * value. */
        extract_lane_s(s0: i16x8, x: u8): s32 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 8) into `s1` and return the
         * replaced v128. */
        replace_lane(s0: i16x8, s1: i32, x: u8): i16x8 { _() },

        eq(s0: i16x8, s1: i16x8): bool { _() },
        ne(s0: i16x8, s1: i16x8): bool { _() },
        lt_u(s0: i16x8, s1: i16x8): bool { _() },
        lt_s(s0: i16x8, s1: i16x8): bool { _() },
        gt_u(s0: i16x8, s1: i16x8): bool { _() },
        gt_s(s0: i16x8, s1: i16x8): bool { _() },
        le_u(s0: i16x8, s1: i16x8): bool { _() },
        le_s(s0: i16x8, s1: i16x8): bool { _() },
        ge_u(s0: i16x8, s1: i16x8): bool { _() },
        ge_s(s0: i16x8, s1: i16x8): bool { _() },

        abs(s0: i16x8): i16x8 { _() },
        neg(s0: i16x8): i16x8 { _() },
        q15mulr_sat_s(s0: i16x8): i16x8 { _() },

        all_true(s0: i16x8): bool { _() },

        bitmask(s0: i16x8): i32 { _() },

        narrow_i32x4_u(s0: i32x4): i16x8 { _() },
        narrow_i32x4_s(s0: i32x4): i16x8 { _() },
        extend_low_i8x16_u(s0: i8x16): i16x8 { _() },
        extend_low_i8x16_s(s0: i8x16): i16x8 { _() },
        extend_high_i8x16_u(s0: i8x16): i16x8 { _() },
        extend_high_i8x16_s(s0: i8x16): i16x8 { _() },

        shl(s0: i16x8, s1: i32): i16x8 { _() },
        shr_u(s0: i16x8, s1: i32): i16x8 { _() },
        shr_s(s0: i16x8, s1: i32): i16x8 { _() },

        add(s0: i16x8, s1: i16x8): i16x8 { _() },
        sub(s0: i16x8, s1: i16x8): i16x8 { _() },

        min_u(s0: i16x8, s1: i16x8): i16x8 { _() },
        min_s(s0: i16x8, s1: i16x8): i16x8 { _() },
        max_u(s0: i16x8, s1: i16x8): i16x8 { _() },
        max_s(s0: i16x8, s1: i16x8): i16x8 { _() },

        add_sat_u(s0: i16x8, s1: i16x8): i16x8 { _() },
        add_sat_s(s0: i16x8, s1: i16x8): i16x8 { _() },
        sub_sat_u(s0: i16x8, s1: i16x8): i16x8 { _() },
        sub_sat_s(s0: i16x8, s1: i16x8): i16x8 { _() },

        mul(s0: i16x8, s1: i16x8): i16x8 { _() },

        avgr_u(s0: i16x8, s1: i16x8): i16x8 { _() },

        extmul_low_i8x16_u(s0: i8x16, s1: i8x16): i16x8 { _() },
        extmul_low_i8x16_s(s0: i8x16, s1: i8x16): i16x8 { _() },
        extmul_high_i8x16_u(s0: i8x16, s1: i8x16): i16x8 { _() },
        extmul_high_i8x16_s(s0: i8x16, s1: i8x16): i16x8 { _() },

        extadd_pairwise_i8x16_u(s0: i8x16, s1: i8x16): i16x8 { _() },
    },
    i32x4: {
        /** Duplicate an i32 value 4 times to produce an i32x4. */
        splat(s0: i32): i32x4 { _() },

        /** Select the `x`th lane of `s0` (`x` < 4) and return the unpacked
         * value. */
        extract_lane(s0: i32x4, x: u8): i32 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 4) into `s1` and return the
         * replaced v128. */
        replace_lane(s0: i32x4, s1: i32, x: u8): i32x4 { _() },

        eq(s0: i32x4, s1: i32x4): bool { _() },
        ne(s0: i32x4, s1: i32x4): bool { _() },
        lt_u(s0: i32x4, s1: i32x4): bool { _() },
        lt_s(s0: i32x4, s1: i32x4): bool { _() },
        gt_u(s0: i32x4, s1: i32x4): bool { _() },
        gt_s(s0: i32x4, s1: i32x4): bool { _() },
        le_u(s0: i32x4, s1: i32x4): bool { _() },
        le_s(s0: i32x4, s1: i32x4): bool { _() },
        ge_u(s0: i32x4, s1: i32x4): bool { _() },
        ge_s(s0: i32x4, s1: i32x4): bool { _() },

        abs(s0: i32x4): i32x4 { _() },
        neg(s0: i32x4): i32x4 { _() },
        dot_i16x8_s(s0: i32x4): i32x4 { _() },

        all_true(s0: i32x4): bool { _() },

        bitmask(s0: i32x4): i32 { _() },

        extend_low_i16x8_u(s0: i16x8): i32x4 { _() },
        extend_low_i16x8_s(s0: i16x8): i32x4 { _() },
        extend_high_i16x8_u(s0: i16x8): i32x4 { _() },
        extend_high_i16x8_s(s0: i16x8): i32x4 { _() },

        shl(s0: i32x4, s1: i32): i32x4 { _() },
        shr_u(s0: i32x4, s1: i32): i32x4 { _() },
        shr_s(s0: i32x4, s1: i32): i32x4 { _() },

        add(s0: i32x4, s1: i32x4): i32x4 { _() },
        sub(s0: i32x4, s1: i32x4): i32x4 { _() },

        min_u(s0: i32x4, s1: i32x4): i32x4 { _() },
        min_s(s0: i32x4, s1: i32x4): i32x4 { _() },
        max_u(s0: i32x4, s1: i32x4): i32x4 { _() },
        max_s(s0: i32x4, s1: i32x4): i32x4 { _() },

        mul(s0: i32x4, s1: i32x4): i32x4 { _() },

        extmul_low_i16x8_u(s0: i16x8, s1: i16x8): i32x4 { _() },
        extmul_low_i16x8_s(s0: i16x8, s1: i16x8): i32x4 { _() },
        extmul_high_i16x8_u(s0: i16x8, s1: i16x8): i32x4 { _() },
        extmul_high_i16x8_s(s0: i16x8, s1: i16x8): i32x4 { _() },

        extadd_pairwise_i16x8_u(s0: i16x8, s1: i16x8): i32x4 { _() },

        trunc_sat_f32x4_u(s0: f32x4): i32x4 { _() },
        trunc_sat_f32x4_s(s0: f32x4): i32x4 { _() },
        trunc_sat_f64x2_u_zero(s0: f64x2): i32x4 { _() },
        trunc_sat_f64x2_s_zero(s0: f64x2): i32x4 { _() },
    },
    i64x2: {
        /** Duplicate an i64 value to produce an i64x2. */
        splat(s0: i64): i64x2 { _() },

        /** Select the `x`th lane of `s0` (`x` < 2) and return the unpacked
         * value. */
        extract_lane(s0: i64x2, x: u8): i64 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 2) into `s1` and return the
         * replaced v128. */
        replace_lane(s0: i64x2, s1: i64, x: u8): i64x2 { _() },

        eq(s0: i64x2, s1: i64x2): bool { _() },
        ne(s0: i64x2, s1: i64x2): bool { _() },
        lt_s(s0: i64x2, s1: i64x2): bool { _() },
        gt_s(s0: i64x2, s1: i64x2): bool { _() },
        le_s(s0: i64x2, s1: i64x2): bool { _() },
        ge_s(s0: i64x2, s1: i64x2): bool { _() },

        abs(s0: i64x2): i64x2 { _() },
        neg(s0: i64x2): i64x2 { _() },

        all_true(s0: i64x2): bool { _() },

        bitmask(s0: i64x2): i32 { _() },

        extend_low_i32x4_u(s0: i32x4): i64x2 { _() },
        extend_low_i32x4_s(s0: i32x4): i64x2 { _() },
        extend_high_i32x4_u(s0: i32x4): i64x2 { _() },
        extend_high_i32x4_s(s0: i32x4): i64x2 { _() },

        shl(s0: i64x2, s1: i32): i64x2 { _() },
        shr_u(s0: i64x2, s1: i32): i64x2 { _() },
        shr_s(s0: i64x2, s1: i32): i64x2 { _() },

        add(s0: i64x2, s1: i64x2): i64x2 { _() },
        sub(s0: i64x2, s1: i64x2): i64x2 { _() },
        mul(s0: i64x2, s1: i64x2): i64x2 { _() },

        extmul_low_i32x4_u(s0: i32x4, s1: i32x4): i64x2 { _() },
        extmul_low_i32x4_s(s0: i32x4, s1: i32x4): i64x2 { _() },
        extmul_high_i32x4_u(s0: i32x4, s1: i32x4): i64x2 { _() },
        extmul_high_i32x4_s(s0: i32x4, s1: i32x4): i64x2 { _() },
    },
    f32x4: {
        /** Duplicate an f32 value 4 times to produce an f32x4. */
        splat(s0: f32): f32x4 { _() },

        /** Select the `x`th lane of `s0` (`x` < 4) and return the unpacked
         * value. */
        extract_lane(s0: f32x4, x: u8): f32 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 4) into `s1` and return the
         * replaced v128. */
        replace_lane(s0: f32x4, s1: f32, x: u8): f32x4 { _() },

        eq(s0: f32x4, s1: f32x4): bool { _() },
        ne(s0: f32x4, s1: f32x4): bool { _() },
        lt(s0: f32x4, s1: f32x4): bool { _() },
        gt(s0: f32x4, s1: f32x4): bool { _() },
        le(s0: f32x4, s1: f32x4): bool { _() },
        ge(s0: f32x4, s1: f32x4): bool { _() },

        abs(s0: f32x4): f32x4 { _() },
        neg(s0: f32x4): f32x4 { _() },
        sqrt(s0: f32x4): f32x4 { _() },
        ceil(s0: f32x4): f32x4 { _() },
        floor(s0: f32x4): f32x4 { _() },
        trunc(s0: f32x4): f32x4 { _() },
        nearest(s0: f32x4): f32x4 { _() },

        add(s0: f32x4, s1: f32x4): f32x4 { _() },
        sub(s0: f32x4, s1: f32x4): f32x4 { _() },
        mul(s0: f32x4, s1: f32x4): f32x4 { _() },
        div(s0: f32x4, s1: f32x4): f32x4 { _() },
        min(s0: f32x4, s1: f32x4): f32x4 { _() },
        max(s0: f32x4, s1: f32x4): f32x4 { _() },
        pmin(s0: f32x4, s1: f32x4): f32x4 { _() },
        pmax(s0: f32x4, s1: f32x4): f32x4 { _() },

        convert_i32x4_u(s0: i32x4): f32x4 { _() },
        convert_i32x4_s(s0: i32x4): f32x4 { _() },

        demote_f64x2_zero(s0: f64x2): f32x4 { _() },
    },
    f64x2: {
        /** Duplicate an f64 value to produce an f64x2. */
        splat(s0: f64): f64x2 { _() },

        /** Select the `x`th lane of `s0` (`x` < 2) and return the unpacked
         * value. */
        extract_lane(s0: f64x2, x: u8): f64 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 2) into `s1` and return the
         * replaced v128. */
        replace_lane(s0: f64x2, s1: f64, x: u8): f64x2 { _() },

        eq(s0: f64x2, s1: f64x2): bool { _() },
        ne(s0: f64x2, s1: f64x2): bool { _() },
        lt(s0: f64x2, s1: f64x2): bool { _() },
        gt(s0: f64x2, s1: f64x2): bool { _() },
        le(s0: f64x2, s1: f64x2): bool { _() },
        ge(s0: f64x2, s1: f64x2): bool { _() },

        abs(s0: f64x2): f64x2 { _() },
        neg(s0: f64x2): f64x2 { _() },
        sqrt(s0: f64x2): f64x2 { _() },
        ceil(s0: f64x2): f64x2 { _() },
        floor(s0: f64x2): f64x2 { _() },
        trunc(s0: f64x2): f64x2 { _() },
        nearest(s0: f64x2): f64x2 { _() },

        add(s0: f64x2, s1: f64x2): f64x2 { _() },
        sub(s0: f64x2, s1: f64x2): f64x2 { _() },
        mul(s0: f64x2, s1: f64x2): f64x2 { _() },
        div(s0: f64x2, s1: f64x2): f64x2 { _() },
        min(s0: f64x2, s1: f64x2): f64x2 { _() },
        max(s0: f64x2, s1: f64x2): f64x2 { _() },
        pmin(s0: f64x2, s1: f64x2): f64x2 { _() },
        pmax(s0: f64x2, s1: f64x2): f64x2 { _() },

        convert_low_i32x4_u(s0: i32x4): f64x2 { _() },
        convert_low_i32x4_s(s0: i32x4): f64x2 { _() },

        promote_low_f32x4(s0: f32x4): f64x2 { _() },
    },

    /// Reference Instructions ///

    ref: {
        null(x: reftype): refnull<typeof x> { return null },
        is_null(s0: ref): bool { return s0 === null ? 1 : 0 },
        func(funcidx: u32): addr {
            let address = F().module.funcaddrs[funcidx]
            assert(exists(address))
            return address.value
        },
    },

    /// Variable Instructions ///

    local: {
        /** Syntax: `local $var type`. Declare a local variable. This is a
         * preprocessor instruction to let you use an identifier instead of an
         * index number to reference a local variable. */
        _(): void { virtual() },
        /** [0x20] Syntax: `local.get $var`. Load the value of local variable
         * onto the stack. */
        get(localidx: u32): cellval {
            let local = F().locals[localidx]
            assert(exists(local))
            return local.value
        },
        /** [0x21] Syntax: `local.set $var`. Pop the stack and store the value
         * into the local variable. */
        set(s0: cellval, localidx: u32): void {
            let local = F().locals[localidx]
            assert(exists(local))
            local.value = s0
        },
        /** [0x22] Syntax: `local.tee $var`. Store the value at top of the stack
         * into the local variable. */
        tee(s0: cellval, localidx: u32): cellval {
            instructions.local.set(s0, localidx)
            return s0
        },
    },
    global: {
        /** Syntax: `global $var type`. Declare a global variable. This is a
         * preprocessor instruction to let you use an identifier instead of an
         * index number to reference a global variable. */
        _(): void { virtual() },
        /** [0x23] Syntax: `global.get $var`. Load the value of global variable
         * onto the stack. */
        get(globalidx: u32): cellval {
            let globaladdr = F().module.globaladdrs[globalidx]
            assert(exists(globaladdr))
            let glob = S().globals[globaladdr.value]
            assert(exists(glob))
            return glob.value
        },
        /** [0x24] Syntax: `global.set $var`. Pop the stack and store the value
         * into the global variable. */
        set(s0: cellval, globalidx: u32): void {
            let globaladdr = F().module.globaladdrs[globalidx]
            assert(exists(globaladdr))
            let glob = S().globals[globaladdr.value]
            assert(glob.mut === mut.var)
            assert(exists(glob))
            glob.value = s0
        },
    },

    /// Table Instructions ///

    table: {
        get(s0: i32, tableidx: u32): cellval {
            let tab = assertGetTab(tableidx)
            if (s0 >= tab.elem.length) { trap() }
            return tab.elem[s0]
        },
        set(s0: ref, s1: i32, tableidx: u32): void {
            let tab = assertGetTab(tableidx)
            if (s1 >= tab.elem.length) { trap() }
            tab.elem[s1] = s0
        },
        size(tableidx: u32): i32 {
            let tab = assertGetTab(tableidx)
            return tab.elem.length
        },
        grow(s0: i32, s1: ref, tableidx: u32): i32 {
            let tab = assertGetTab(tableidx)
            let sz = tab.elem.length
            let err = -1 as i32
            if (growTab(tab, s0, s1)) {
                return sz
            } else {
                return err
            }
            return err
        },
        fill(s0: i32, s1: ref, s2: i32, tableidx: u32): void {
            let tab = assertGetTab(tableidx)
            if (s2 + s0 > tab.elem.length) { trap() }
            if (s0 === 0) { return }
            instructions.table.set(s1, s2, tableidx)
            instructions.table.fill(s0 - 1, s1, s2 + 1, tableidx)
        },
        copy(s0: i32, s1: i32, s2: i32, tableidx0: u32, tableidx1: u32): void {
            let tab0 = assertGetTab(tableidx0)
            let tab1 = assertGetTab(tableidx1)
            if (s1 + s2 > tab1.elem.length || s0 + s2 > tab0.elem.length) {
                trap()
            }
            if (s0 === 0) { return }
            if (s2 <= s1) {
                let r0 = instructions.table.get(s1, tableidx1)
                instructions.table.set(r0, s2, tableidx0)
                push(s2 + 1)
                push(s1 + 1)
            } else {
                let r0 = instructions.table.get(s1 + s0 - 1, tableidx1)
                instructions.table.set(r0, s2 + s0 - 1, tableidx0)
                push(s2)
                push(s1)
            }
            instructions.table.copy(s0 - 1, pop()!, pop()!, tableidx0, tableidx1)
        },
        init(s0: i32, s1: i32, s2: i32, tableidx: u32, elemidx: u32): void {
            let tab = assertGetTab(tableidx)
            let elem = assertGetElem(elemidx)
            if (s1 + s2 > elem.elem.length || s0 + s2 > tab.elem.length) {
                trap()
            }
            if (s0 === 0) { return }
            let val = elem.elem[s1]
            instructions.table.set(val, s2, tableidx)
            instructions.table.init(s0 - 1, s1 + 1, s2 + 1, tableidx, elemidx)
        },
    },
    elem: {
        drop(elemidx: u32): void {
            let elem = assertGetElem(elemidx)
            elem.elem = []
        },
    },

    /// Memory Instructions ///

    memory: {
        /** [0x3f] Returns the amount of pages the memory instance currently
         * has, each page is sized 64KiB. */
        size(): i32 {
            let mem = assertGetMem(0)
            return mem.size
        },
        /** [0x40] Increases the size of the memory instance by a `s0` number of
         * pages, each page is sized 64KiB. Push the previous size, or -1 if
         * failed. */
        grow(s0: i32): i32 {
            let mem = assertGetMem(0)
            let size = mem.size
            let err = -1 as i32
            if (growMem(mem, s0)) {
                return size
            } else {
                return err
            }
        },
        /** [0xfc][0x0a] Copies data from one region of memory to another.
         * Source memory region starts from index `s1`. Destination memory
         * region starts from index `s0`. `s2` is the number of bytes to copy.
         * */
        copy(s0/*d*/: i32, s1/*s*/: i32, s2/*n*/: i32): void {
            let mem = assertGetMem(0)
            if (s0 + s2 > mem.data.length) { trap() }
            if (s1 + s2 > mem.data.length) { trap() }
            if (s2 === 0) { return }
            if (s0 <= s1) {
                let t = instructions.i32.load8_u(s1, { offset: 0, align: 0 })
                instructions.i32.store8(s0, t, { offset: 0, align: 0 })
                assert(s0 + 1 < (1 << 32))
                push(s0 + 1)
                assert(s1 + 1 < (1 << 32))
                push(s1 + 1)
            } else {
                assert(s0 + s2 - 1 < (1 << 32))
                assert(s1 + s2 - 1 < (1 << 32))
                let t = instructions.i32.load8_u(s1 + s2 - 1, { offset: 0, align: 0 })
                instructions.i32.store8(s0 + s2 - 1, t, { offset: 0, align: 0 })
                push(s0)
                push(s1)
            }
            let s = pop()!
            let d = pop()!
            instructions.memory.copy(d, s, s2 - 1)
        },
        /** [0xfc][0x0b] Sets all bytes in a memory region to byte `s1`. The
         * memory region starts from index `s0` and has a length of `s2` bytes.
         * */
        fill(s0/*d*/: i32, s1/*val*/: i32, s2/*n*/: i32): void {
            let mem = assertGetMem(0)
            if (s0 + s2 > mem.data.length) { trap() }
            if (s2 === 0) { return }
            instructions.i32.store8(s0, s1, { offset: 0, align: 0 })
            assert(s0 + 1 < (1 << 32))
            instructions.memory.fill(s0 + 1, s1, s2 - 1)
        },
        /** Syntax: `memory.init dataidx`. Initializes all bytes in a memory
         * region to a data region. The memory region starts from index `s0` and
         * has a length of `s2` bytes. The data region starts from index `s1`
         * and has a length of `s2` data instances, and is picked from the data
         * instance at the `dataidx`-th data address. */
        init(s0/*d*/: i32, s1/*s*/: i32, s2/*n*/: i32, dataidx: u32): void {
            let mem = assertGetMem(0)
            let data = assertGetData(dataidx)
            if (s0 + s2 > mem.data.length) { trap() }
            if (s1 + s2 > data.data.length) { trap() }
            if (s2 === 0) { return }
            let b = data.data[s1]
            instructions.i32.store8(s0, b, { offset: 0, align: 0 })
            assert(s0 + 1 < (1 << 32))
            assert(s1 + 1 < (1 << 32))
            instructions.memory.init(s0 + 1, s1 + 1, s2 - 1, dataidx)
        },
    },
    data: {
        /** Syntax: `data.drop dataidx`. Replace the data instance at the
         * `dataidx`-th data address with the empty data instance. */
        drop(dataidx: u32): void {
            let dataaddr = F().module.dataaddrs[dataidx]
            assert(exists(dataaddr))
            let data = S().datas[dataaddr.value]
            assert(exists(data))
            S().datas[dataaddr.value] = emptyData()
        },
    },

    /// Control Flow Instructions ///

    /** [0x00] Denotes a point in code that should not be reachable. */
    unreachable(): never { trap() },

    /** [0x01] Do nothing. */
    nop(): void { },

    /** [0x02] Syntax: `(block $label ...)`. Create a label that can be branched
     * to. Branching to the label takes the execution to the end of the block,
     * that is, out of the block. In high-level programming languages, this is
     * like a `do { } while (0)` block, and branching is like a break statement.
     * The label is not an actual parameter of this intruction, but rather a
     * preprocessor declaration. */
    block(): void { _() },
    /** [0x03] Syntax: `(loop $label ...)`. Create a label that can be branched
     * to. Branching to the label takes the execution to the start of the loop.
     * This instruction does not loop itself but requires branching to do so.
     * The label is not an actual parameter of this intruction, but rather a
     * preprocessor declaration. */
    loop(): void { _() },

    /** [0x04] Syntax: `(if (then ...))` or `(if (then ...) (else ...))`.
     * Execute a statement if the value popped from the stack is true
     * (non-zero), or optionally executes another statement if the value is
     * false (0). */
    if(s0: cellval): void { _() },

    /** [0x0b] Indicates the end of a `block`, `loop`, `if`, or `else`. */
    end(): void { _() },

    /** [0x0f] Returns from the function. If there are no values left on the
     * stack, it returns nothing/void. If there are the same amount of values
     * left on the stack as specified in the function's type signature, it
     * returns those values. If there are more values than that the function's
     * return type specifies, then the first N values are returned, and the
     * excess values are popped from the stack and discarded. */
    return(ss: cellval[]): void { _() },

    /** [0x0c] Syntax: `br $label`. Unconditionally branch to a `loop`, `block`,
     * or `if`. If its target is a `loop`, it jumps to the start of the loop. If
     * its target is a `block` or `if`, it jumps to the end of the block. It is
     * like a `continue` statement in some high-level programming languages.
     *
     * Note: A label index is how many layers of blocks and loops this
     * instruction is away with the target. For the block or loop this
     * instruction is directly in, the label index is `0`. */
    br(labelidx: u32): void { _() },
    /** [0x0d] Syntax: `br_if $label`. Branch to a `loop` or `block` if the
     * value popped from the stack is true (non-zero). Despite of the condition,
     * it is identical to the `br` instruction. */
    br_if(s0: bool, labelidx: u32): void { _() },
    /** [0x0e] Syntax: `br_table $label1 $label2 ...`. Branch to one of the
     * `loop`s and `block`s given by the labels according to the index `s0`
     * popped from the stack.
     * 
     * @example
     * (block $a
     *   (block $b
     *     (block $c (i32.const 1)
     *               ; This breaks the block $b and continues at (i32.const 42).
     *               (br_table $a $b $c)
     *     ))
     *   (i32.const 42)*/
    br_table(s0: u32, ...labelidxs: u32[]): void { _() },

    /** [0x10] Call a function. */
    call(ss: cellval[], funcidx: u32, functype: valtype) {
        let func = S().funcs[funcidx]
        assert(exists(func))
        assert(ss.length === func.code.locals.length)
        let frame: Frame = {
            module: func.module,
            locals: ss.map((value) => ({ value })),
        }
        let arity = return_types_of(func.type).length
        push_frame_activation({ frame, arity })
        push_label({ arity, instruction_bytes: func.code.body })
        jump_to_start_of_instruction()
    },
    /** [0x11] Call the function of index `s0` in a table. The table index is
     * optional if there is only one table. */
    call_indirect(s0: i32, tableidx: u32, funcidx: u32, functype: valtype) { _() },
    /** [0x12] The tail-call version of `call`. */
    return_call(funcidx: u32) { _() },
    /** [0x13] The tail-call version of `call_indirect`. */
    return_call_indirect(s0: i32, tableidx: u32, funcidx: u32) { _() },

    /** [0x1a] Pop a value from the stack and discard it. */
    drop(s0: val): void { },

    /** [0x1b] Syntax: `select` or [0x1c] `select (result T)`. Return `s0` is `s2` is
     * zero, or `s1` if not. This instruction does not short-circuit. The simple
     * `select` syntax only allows the values of WebAssembly MVP types (`i32`,
     * `i64`, `f32`, `f64`). While the alternative, `select (result T)`, allows
     * other types, such as two `externref` values. */
    select(s0: i32, s1: cellval, s2: cellval, valtype: valtype): cellval {
        assert(typeOf(s0) === valtype)
        assert(typeOf(s1) === valtype)
        return s2 ? s0 : s1;
    },
}
