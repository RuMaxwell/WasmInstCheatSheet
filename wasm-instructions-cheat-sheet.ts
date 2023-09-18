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
type val = num | v128 | ref
type num = i32 | i64 | f32 | f64
type ref = funcref | externref
type funcref = number
type externref = number
type anytype = typeidx | typeref
type typeidx = u32
type typeref = u32
type valtype = numtype | vectype | reftype
type numtype = typeof i32typecode | typeof i64typecode | typeof f32typecode | typeof f64typecode
type vectype = typeof v128typecode
type reftype = typeof funcreftypecode | typeof externreftypecode
type refnull<t> = null

type memarg = { offset: u32, align: u32 }

const i32typecode = 0x7f
const i64typecode = 0x7e
const f32typecode = 0x7d
const f64typecode = 0x7c
const v128typecode = 0x7b
const funcreftypecode = 0x70
const externreftypecode = 0x6f
const PAGE_SIZE = 65536

function _(): never { throw '' }
function trap(): never { throw 'trap' }
function assert(statement: boolean): void { }
function push(value: val): void { }
function pop(): val { _() }
function typeOf(value: val): valtype { _() }
function isNumType(valtype: valtype) { return (valtype & 0x7c) === 0x7c }
function F(): Frame { _() }
function S(): Store { _() }
function exists(cell: Cell | Glob | Tab | Mem): boolean { return !!cell }
function growTab(tab: Tab, n: i32, initVal: ref): boolean { _() }
function growMem(mem: Mem, n: i32): boolean { _() }
function bytesToI32(bytes: i8[]): i32 { _() }
function bytesToI64(bytes: i8[]): i64 { _() }
function bytesToF32(bytes: i8[]): f32 { _() }
function bytesToF64(bytes: i8[]): f64 { _() }
function bytesToV128(bytes: i8[]): v128 { _() }
function i32ToBytes(value: i32): i8[] { _() }
function i64ToBytes(value: i64): i8[] { _() }
function f32ToBytes(value: f32): i8[] { _() }
function f64ToBytes(value: f64): i8[] { _() }
function v128ToBytes(value: v128): i8[] { _() }

function assertGetTab(tableidx: u32): Tab {
    let tableaddr = F().module.tableaddrs[tableidx]
    assert(exists(tableaddr))
    let tab = S().tables[tableaddr.value]
    assert(exists(tab))
    return tab
}

function assertGetElem(elemidx: u32): Elem {
    let elemaddr = F().module.elemaddrs[elemidx]
    assert(exists(elemaddr))
    let elem = S().elems[elemaddr.value]
    assert(exists(elem))
    return elem
}

function assertGetMem(memidx: u32): Mem {
    let memaddr = F().module.memoryaddrs[memidx]
    assert(exists(memaddr))
    let mem = S().mems[memaddr.value]
    assert(exists(mem))
    return mem
}

interface Frame {
    locals: Cell[]
    module: {
        globaladdrs: Cell[]
        tableaddrs: Cell[]
        elemaddrs: Cell[]
        memoryaddrs: Cell[]
    }
}

interface Cell {
    value: val
}

interface Store {
    globals: Glob[]
    tables: Tab[]
    elems: Elem[]
    mems: Mem[]
}

interface Glob {
    mut: mut
    value: val
}

interface Tab {
    elem: val[]
}

interface Elem {
    elem: val[]
}

interface Mem {
    data: i8[]
    /** Size of `mem` is length of `mem.data` divided by page_size */
    get size(): i32
}

enum mut {
    const = 0x00,
    var = 0x01,
}

/**
 * Example:
 * 
 * `i32.const` represents the instruction `i32.const`.
 * 
 * Input parameters `s0`, `s1`, ... represent the stack values. The values are popped in the
 * ascending order of the parameters, which means the actual order (defined by the WASM
 * specification) of the instruction operands are in the opposite direction.
 * 
 * The doc comment of an instruction uses a signature syntax:
 * `instruction_name instant_names : input_params -> output_params`,
 * where `instruction_name` is the name of the instruction;
 *       `instant_names` are names of the instants (optional);
 *       `input_params` are names of the operands popped from the stack before the instruction body (optional);
 *       `output_params` are names of the values pushed to the stack after the instruction body (optional).
 * The `input_params` are always in the reversed order agianst the `sN` parameters declared by the
 * instruction function.
 * 
 * Other parameters (like `x`, `y`, `funcidx`) represent the instants of the instruction, in the
 * same order of what they are defined by the WASM specification.
 * 
 * The return type represents the result given back to the stack.
 */
const instructions = {
    /// Numeric Instructions ///

    i32: {
        /** Return an instant i32. */
        const(x: u32): i32 { _() },

        clz(s0: i32): i32 { _() },
        ctz(s0: i32): i32 { _() },
        popcnt(s0: i32): i32 { _() },

        add(s0: i32, s1: i32): i32 { _() },
        sub(s0: i32, s1: i32): i32 { _() },
        mul(s0: i32, s1: i32): i32 { _() },
        div_u(s0: u32, s1: u32): i32 { _() },
        div_s(s0: s32, s1: s32): i32 { _() },
        rem_u(s0: u32, s1: u32): i32 { _() },
        rem_s(s0: s32, s1: s32): i32 { _() },
        and(s0: i32, s1: i32): i32 { _() },
        or(s0: i32, s1: i32): i32 { _() },
        xor(s0: i32, s1: i32): i32 { _() },
        shl(s0: i32, s1: i32): i32 { _() },
        shr_u(s0: u32, s1: i32): i32 { _() },
        shr_s(s0: s32, s1: i32): i32 { _() },
        rotl(s0: i32, s1: i32): i32 { _() },
        rotr(s0: i32, s1: i32): i32 { _() },

        eqz(s0: i32): bool { _() },

        eq(s0: i32, s1: i32): bool { _() },
        ne(s0: i32, s1: i32): bool { _() },
        lt_u(s0: u32, s1: u32): bool { _() },
        lt_s(s0: s32, s1: s32): bool { _() },
        gt_u(s0: u32, s1: u32): bool { _() },
        gt_s(s0: s32, s1: s32): bool { _() },
        le_u(s0: u32, s1: u32): bool { _() },
        le_s(s0: s32, s1: s32): bool { _() },
        ge_u(s0: u32, s1: u32): bool { _() },
        ge_s(s0: s32, s1: s32): bool { _() },

        /** Consume an s32, cut it into s8, and extend it into s32. */
        extend8_s(s0: s32): s32 { _() },
        /** Consume an s32, cut it into s16, and extend it into s32. */
        extend16_s(s0: s32): s32 { _() },

        /** Convert an i64 into i32 by returning `s0` % (2 << 32). */
        wrap_i64(s0: i64): i32 { _() },
        trunc_f32_u(s0: f32): u32 { _() },
        trunc_f32_s(s0: f32): s32 { _() },
        trunc_f64_u(s0: f64): u32 { _() },
        trunc_f64_s(s0: f64): s32 { _() },
        trunc_sat_f32_u(s0: f32): u32 { _() },
        trunc_sat_f32_s(s0: f32): s32 { _() },
        trunc_sat_f64_u(s0: f64): u32 { _() },
        trunc_sat_f64_s(s0: f64): s32 { _() },
        reinterpret_f32(s0: f32): i32 { _() },

        /** i32.load memarg : index -> value */
        load(s0: i32, memarg: memarg): i32 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 4)
            return bytesToI32(bytes)
        },
        /** i32.load memarg : index value -> nil */
        store(s0: val, s1: i32, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 4, ...i32ToBytes(s1))
        },
        // TODO: Continue at inn.load8_sx
    },
    i64: {
        /** Return an instant i64. */
        const(x: u64): i64 { _() },

        clz(s0: i64): i64 { _() },
        ctz(s0: i64): i64 { _() },
        popcnt(s0: i64): i64 { _() },

        add(s0: i64, s1: i64): i64 { _() },
        sub(s0: i64, s1: i64): i64 { _() },
        mul(s0: i64, s1: i64): i64 { _() },
        div_u(s0: u64, s1: u64): i64 { _() },
        div_s(s0: s64, s1: s64): i64 { _() },
        rem_u(s0: u64, s1: u64): i64 { _() },
        rem_s(s0: s64, s1: s64): i64 { _() },
        and(s0: i64, s1: i64): i64 { _() },
        or(s0: i64, s1: i64): i64 { _() },
        xor(s0: i64, s1: i64): i64 { _() },
        shl(s0: i64, s1: i64): i64 { _() },
        shr_u(s0: u64, s1: i64): i64 { _() },
        shr_s(s0: s64, s1: i64): i64 { _() },
        rotl(s0: i64, s1: i64): i64 { _() },
        rotr(s0: i64, s1: i64): i64 { _() },

        eqz(s0: i64): bool { _() },

        eq(s0: i64, s1: i64): bool { _() },
        ne(s0: i64, s1: i64): bool { _() },
        lt_u(s0: u64, s1: u64): bool { _() },
        lt_s(s0: s64, s1: s64): bool { _() },
        gt_u(s0: u64, s1: u64): bool { _() },
        gt_s(s0: s64, s1: s64): bool { _() },
        le_u(s0: u64, s1: u64): bool { _() },
        le_s(s0: s64, s1: s64): bool { _() },
        ge_u(s0: u64, s1: u64): bool { _() },
        ge_s(s0: s64, s1: s64): bool { _() },

        /** Consume an s64, cut it into s8, and extend it into s64. */
        extend8_s(s0: s64): i64 { _() },
        /** Consume an s64, cut it into s16, and extend it into s64. */
        extend16_s(s0: s64): i64 { _() },
        /** Consume an s64, cut it into s32, and extend it into s64. */
        extend32_s(s0: s64): s64 { _() },
        /** Return the value as-is. */
        extend_i32_u(s0: u64): u64 { _() },
        /** Consume an s64, cut it into s32, and extend it into s64. */
        extend_i32_s(s0: s64): s64 { _() },

        trunc_f32_u(s0: f32): u64 { _() },
        trunc_f32_s(s0: f32): s64 { _() },
        trunc_f64_u(s0: f64): u64 { _() },
        trunc_f64_s(s0: f64): s64 { _() },
        trunc_sat_f32_u(s0: f32): u64 { _() },
        trunc_sat_f32_s(s0: f32): s64 { _() },
        trunc_sat_f64_u(s0: f64): u64 { _() },
        trunc_sat_f64_s(s0: f64): s64 { _() },
        reinterpret_f64(s0: f64): i64 { _() },

        /** i64.load memarg : index -> value */
        load(s0: i64, memarg: memarg): i64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 8 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 8)
            return bytesToI64(bytes)
        },
        /** i64.load memarg : index value -> nil */
        store(s0: val, s1: i64, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 8 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 8, ...i64ToBytes(s1))
        },
    },
    f32: {
        /** Return an instant f32. */
        const(x: f32): f32 { _() },

        abs(s0: f32): f32 { _() },
        neg(s0: f32): f32 { _() },
        sqrt(s0: f32): f32 { _() },
        ceil(s0: f32): f32 { _() },
        floor(s0: f32): f32 { _() },
        trunc(s0: f32): f32 { _() },
        nearest(s0: f32): f32 { _() },

        add(s0: f32, s1: f32): f32 { _() },
        sub(s0: f32, s1: f32): f32 { _() },
        mul(s0: f32, s1: f32): f32 { _() },
        div(s0: f32, s1: f32): f32 { _() },
        min(s0: f32, s1: f32): f32 { _() },
        max(s0: f32, s1: f32): f32 { _() },
        copysign(s0: f32, s1: f32): f32 { _() },

        eq(s0: f32, s1: f32): bool { _() },
        ne(s0: f32, s1: f32): bool { _() },
        lt(s0: f32, s1: f32): bool { _() },
        gt(s0: f32, s1: f32): bool { _() },
        le(s0: f32, s1: f32): bool { _() },
        ge(s0: f32, s1: f32): bool { _() },

        demote_f64(s0: f64): f32 { _() },
        convert_i32_u(s0: u32): f32 { _() },
        convert_i32_s(s0: s32): f32 { _() },
        convert_i64_u(s0: u64): f32 { _() },
        convert_i64_s(s0: s64): f32 { _() },
        reinterpret_i32(s0: i32): f32 { _() },

        /** f32.load memarg : index -> value */
        load(s0: f32, memarg: memarg): f32 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 4)
            return bytesToF32(bytes)
        },
        /** f32.load memarg : index value -> nil */
        store(s0: val, s1: f32, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 4 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 4, ...f32ToBytes(s1))
        },
    },
    f64: {
        /** Return an instant f64. */
        const(x: f64): f64 { _() },

        abs(s0: f64): f64 { _() },
        neg(s0: f64): f64 { _() },
        sqrt(s0: f64): f64 { _() },
        ceil(s0: f64): f64 { _() },
        floor(s0: f64): f64 { _() },
        trunc(s0: f64): f64 { _() },
        nearest(s0: f64): f64 { _() },

        add(s0: f64, s1: f64): f64 { _() },
        sub(s0: f64, s1: f64): f64 { _() },
        mul(s0: f64, s1: f64): f64 { _() },
        div(s0: f64, s1: f64): f64 { _() },
        min(s0: f64, s1: f64): f64 { _() },
        max(s0: f64, s1: f64): f64 { _() },
        copysign(s0: f64, s1: f64): f64 { _() },

        eq(s0: f64, s1: f64): bool { _() },
        ne(s0: f64, s1: f64): bool { _() },
        lt(s0: f64, s1: f64): bool { _() },
        gt(s0: f64, s1: f64): bool { _() },
        le(s0: f64, s1: f64): bool { _() },
        ge(s0: f64, s1: f64): bool { _() },

        promote_f32(s0: f32): f64 { _() },
        convert_i32_u(s0: u32): f64 { _() },
        convert_i32_s(s0: s32): f64 { _() },
        convert_i64_u(s0: u64): f64 { _() },
        convert_i64_s(s0: s64): f64 { _() },
        reinterpret_i64(s0: i64): f64 { _() },

        /** f64.load memarg : index -> value */
        load(s0: f64, memarg: memarg): f64 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 8 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 8)
            return bytesToF64(bytes)
        },
        /** f64.load memarg : index value -> nil */
        store(s0: val, s1: f64, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 8 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 8, ...f64ToBytes(s1))
        },
    },

    /// Vector Instructions ///

    v128: {
        const(x: i128): v128 { _() },

        not(s0: v128): v128 { _() },

        and(s0: v128, s1: v128): v128 { _() },
        andnot(s0: v128, s1: v128): v128 { _() },
        or(s0: v128, s1: v128): v128 { _() },
        xor(s0: v128, s1: v128): v128 { _() },

        bitselect(s0: v128, s1: v128, s2: v128): v128 { _() },

        any_true(s0: v128): bool { _() },

        /** v128.load memarg : index -> value */
        load(s0: v128, memarg: memarg): v128 {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 16 > mem.data.length) { trap() }
            let bytes = mem.data.slice(offset, offset + 16)
            return bytesToV128(bytes)
        },
        /** v128.load memarg : index value -> nil */
        store(s0: val, s1: v128, memarg: memarg): void {
            let mem = assertGetMem(0)
            let offset = s0 + memarg.offset
            if (offset + 16 > mem.data.length) { trap() }
            mem.data.splice(offset, offset + 16, ...v128ToBytes(s1))
        },

    },
    i8x16: {
        shuffle(x: vec<u8, 16>): i8x16 { _() },
        swizzle(s0: i8x16): i8x16 { _() },

        /** Duplicate an i8 (stored as i32) value 16 times to produce an i8x16. */
        splat(s0: i32): i8x16 { _() },

        /** Select the `x`th lane of `s0` (`x` < 16) and return the unpacked value. */
        extract_lane_u(s0: i8x16, x: u8): u32 { _() },
        /** Select the `x`th lane of `s0` (`x` < 16) and return the unpacked value. */
        extract_lane_s(s0: i8x16, x: u8): s32 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 16) into `s1` and return the replaced v128. */
        replace_lane(s0: i8x16, s1: i32, x: u8): i8x16 { _() },

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
        /** Duplicate an i16 (stored as i32) value 8 times to produce an i16x8. */
        splat(s0: i32): i16x8 { _() },

        /** Select the `x`th lane of `s0` (`x` < 8) and return the unpacked value. */
        extract_lane_u(s0: i16x8, x: u8): u32 { _() },
        /** Select the `x`th lane of `s0` (`x` < 8) and return the unpacked value. */
        extract_lane_s(s0: i16x8, x: u8): s32 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 8) into `s1` and return the replaced v128. */
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

        /** Select the `x`th lane of `s0` (`x` < 4) and return the unpacked value. */
        extract_lane(s0: i32x4, x: u8): i32 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 4) into `s1` and return the replaced v128. */
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

        /** Select the `x`th lane of `s0` (`x` < 2) and return the unpacked value. */
        extract_lane(s0: i64x2, x: u8): i64 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 2) into `s1` and return the replaced v128. */
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

        /** Select the `x`th lane of `s0` (`x` < 4) and return the unpacked value. */
        extract_lane(s0: f32x4, x: u8): f32 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 4) into `s1` and return the replaced v128. */
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

        /** Select the `x`th lane of `s0` (`x` < 2) and return the unpacked value. */
        extract_lane(s0: f64x2, x: u8): f64 { _() },
        /** Replace the `x`th lane of `s0` (`x` < 2) into `s1` and return the replaced v128. */
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
        null(x: reftype): refnull<typeof x> { _() },
        is_null(s0: reftype): bool { _() },
        func(funcidx: u32): addr { _() },
    },

    /// Parametric Instructions ///

    /** drop : value -> nil */
    drop(s0: val): void { _() },
    /** select valtype : value_x value_y condition */
    select(s0: i32, s1: val, s2: val, valtype: valtype): val {
        assert(typeOf(s1) === valtype)
        assert(typeOf(s2) === valtype)
        return s0 ? s2 : s1;
    },

    /// Variable Instructions ///

    local: {
        /** local.get x : -> value */
        get(localidx: u32): val {
            let local = F().locals[localidx]
            assert(exists(local))
            return local.value
        },
        /** local.set x : value -> nil */
        set(s0: val, localidx: u32): void {
            let local = F().locals[localidx]
            assert(exists(local))
            local.value = s0
        },
        /** local.tee x : value -> same_value */
        tee(s0: val, localidx: u32): val {
            instructions.local.set(s0, localidx)
            return s0
        },
    },
    global: {
        /** global.get x : -> value */
        get(globalidx: u32): val {
            let globaladdr = F().module.globaladdrs[globalidx]
            assert(exists(globaladdr))
            let glob = S().globals[globaladdr.value]
            assert(exists(glob))
            return glob.value
        },
        /** global.set x : -> value */
        set(s0: val, globalidx: u32): void {
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
        /** table.get x : offset -> value */
        get(s0: i32, tableidx: u32): val {
            let tab = assertGetTab(tableidx)
            if (s0 >= tab.elem.length) { trap() }
            return tab.elem[s0]
        },
        /** table.set x : offset value -> nil */
        set(s0: ref, s1: i32, tableidx: u32): void {
            let tab = assertGetTab(tableidx)
            if (s1 >= tab.elem.length) { trap() }
            tab.elem[s1] = s0
        },
        /** table.set x : offset value -> nil */
        size(tableidx: u32): i32 {
            let tab = assertGetTab(tableidx)
            return tab.elem.length
        },
        /** table.grow x : value length -> original_size */
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
        /** table.fill x : start value length -> nil */
        fill(s0: i32, s1: ref, s2: i32, tableidx: u32): void {
            let tab = assertGetTab(tableidx)
            if (s2 + s0 > tab.elem.length) { trap() }
            if (s0 === 0) { return }
            instructions.table.set(s1, s2, tableidx)
            instructions.table.fill(s0 - 1, s1, s2 + 1, tableidx)
        },
        /** table.copy x y : start_x start_y length -> nil */
        copy(s0: i32, s1: i32, s2: i32, tableidx0: u32, tableidx1: u32): void {
            let tab0 = assertGetTab(tableidx0)
            let tab1 = assertGetTab(tableidx1)
            if (s1 + s2 > tab1.elem.length || s0 + s2 > tab0.elem.length) { trap() }
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
            instructions.table.copy(s0 - 1, pop(), pop(), tableidx0, tableidx1)
        },
        /** table.init x elemidx : table_offset elem_offset length -> nil */
        init(s0: i32, s1: i32, s2: i32, tableidx: u32, elemidx: u32): void {
            let tab = assertGetTab(tableidx)
            let elem = assertGetElem(elemidx)
            if (s1 + s2 > elem.elem.length || s0 + s2 > tab.elem.length) { trap() }
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
        /** memory.size : -> size */
        size(): i32 {
            let mem = assertGetMem(0)
            return mem.size
        },
        /** memory.grow : new_page_count -> original_size */
        grow(s0: i32): i32 {
            let mem = assertGetMem(0)
            let err = -1 as i32
            if (growMem(mem, s0)) {
                return mem.size
            } else {
                return err
            }
            return err
        },
        /** memory.fill : start value length -> nil */
        fill(s0: i32, s1: i32, s2: i32): void {
            let mem = assertGetMem(0)
            if (s2 + s0 > mem.data.length) { trap() }
            if (s0 === 0) { return }
            push(s2)
            push(s1)
            // TODO: Continue at memory.fill 16.
        },
    },
}