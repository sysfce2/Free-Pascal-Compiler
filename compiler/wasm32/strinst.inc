{
    Copyright (c) 2016 by Karoly Balogh

    This include file contains the WebAssembly instruction string table

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

        '<none>',
        // control flow
        'block', 'loop', 'br', 'br_if', 'br_table', 'if', 'else', 'end' {block},
        'end' {loop}, 'end' {if}, '' {end function},
        'return', 'unreachable',
        // basic
        'nop', 'drop', 'i32.const', 'i64.const', 'f32.const', 'f64.const',
        'local.get', 'local.set', 'local.tee', 'global.get', 'global.set',
        'select', 'call', 'call_indirect',
        // integer
        'i32.add', 'i64.add', 'i32.sub', 'i64.sub', 'i32.mul', 'i64.mul',
        'i32.div_s', 'i64.div_s', 'i32.div_u', 'i64.div_u', 'i32.rem_s', 'i64.rem_s',
        'i32.rem_u', 'i64.rem_u', 'i32.and', 'i64.and', 'i32.or', 'i64.or',
        'i32.xor', 'i64.xor', 'i32.shl', 'i64.shl', 'i32.shr_s', 'i64.shr_s',
        'i32.shr_u', 'i64.shr_u', 'i32.rotl', 'i64.rotl', 'i32.rotr', 'i64.rotr',
        'i32.clz', 'i64.clz', 'i32.ctz', 'i64.ctz', 'i32.popcnt', 'i64.popcnt',
        'i32.eqz', 'i64.eqz',
        // floating point
        'f32.add', 'f64.add', 'f32.sub', 'f64.sub', 'f32.mul', 'f64.mul',
        'f32.div', 'f64.div', 'f32.sqrt', 'f64.sqrt', 'f32.min', 'f64.min',
        'f32.max', 'f64.max', 'f32.ceil', 'f64.ceil', 'f32.floor', 'f64.floor',
        'f32.trunc', 'f64.trunc', 'f32.nearest', 'f64.nearest', 'f32.abs', 'f64.abs',
        'f32.neg', 'f64.neg', 'f32.copysign', 'f64.copysign',
        // integer compare
        'i32.eq', 'i64.eq', 'i32.ne', 'i64.ne', 'i32.lt_s', 'i64.lt_s',
        'i32.lt_u', 'i64.lt_u', 'i32.le_s', 'i64.le_s', 'i32.le_u', 'i64.le_u',
        'i32.gt_s', 'i64.gt_s', 'i32.gt_u', 'i64.gt_u', 'i32.ge_s', 'i64.ge_s',
        'i32.ge_u', 'i64.ge_u',
        // floating point compare
        'f32.eq', 'f64.eq', 'f32.ne', 'f64.ne', 'f32.lt', 'f64.lt',
        'f32.le', 'f64.le', 'f32.gt', 'f64.gt', 'f32.ge', 'f64.gt',
        // conversion
        'i32.wrap/i64', 'i64.extend_s/i32', 'i64.extend_u/i32',
        'i32.extend_s/8','i32.extend_s/16','i64.extend_s/8','i64.extend_s/16','i64.extend_s/32',
        'i32.trunc_s/f32', 'i32.trunc_s/f64', 'i64.trunc_s/f32', 'i64.trunc_s/f64',
        'i32.trunc_u/f32', 'i32.trunc_u/f64', 'i64.trunc_u/f32', 'i64.trunc_u/f64',
        'f32.demote/f64', 'f64.promote/f32',
        'f32.convert_s/i32', 'f32.convert_s/i64', 'f64.convert_s/i32', 'f64.convert_s/i64',
        'f32.convert_u/i32', 'f32.convert_u/i64', 'f64.convert_u/i32', 'f64.convert_u/i64',
        'i32.reinterpret/f32', 'i64.reinterpret/f64', 'f32.reinterpret/i32', 'f64.reinterpret/i64',
        // load/store
        'i32.load', 'i64.load', 'f32.load', 'f64.load',
        'i32.store', 'i64.store', 'f32.store', 'f64.store',
        'i32.load8_s', 'i32.load16_s', 'i64.load8_s', 'i64.load16_s', 'i64.load32_s',
        'i32.load8_u', 'i32.load16_u', 'i64.load8_u', 'i64.load16_u', 'i64.load32_u',
        'i32.store8', 'i32.store16', 'i64.store8', 'i64.store16', 'i64.store32',
        // additional memory
        'grow_memory', 'current_memory',
        // bulk memory operations
        'memory.copy', 'memory.fill', 'memory.init', 'data.drop',
        // reference instructions
        'ref.null_func', 'ref.null_extern', 'ref.is_null', 'ref.func',
        // table instructions
        'table.get', 'table.set', 'table.size', 'table.grow', 'table.fill', 'table.copy', 'table.init', 'elem.drop',
        // saturating truncation instructions
        'i32.trunc_sat_f32_s',
        'i32.trunc_sat_f32_u',
        'i32.trunc_sat_f64_s',
        'i32.trunc_sat_f64_u',
        'i64.trunc_sat_f32_s',
        'i64.trunc_sat_f32_u',
        'i64.trunc_sat_f64_s',
        'i64.trunc_sat_f64_u',
        // exceptions
        'try','catch','catch_all','delegate','throw','rethrow','end' {try},
        // atomic memory accesses - load/store
        'i32.atomic.load8_u', 'i32.atomic.load16_u', 'i32.atomic.load',
        'i64.atomic.load8_u', 'i64.atomic.load16_u', 'i64.atomic.load32_u',
        'i64.atomic.load', 'i32.atomic.store8', 'i32.atomic.store16',
        'i32.atomic.store', 'i64.atomic.store8', 'i64.atomic.store16',
        'i64.atomic.store32', 'i64.atomic.store',
        // atomic memory accesses - read-modify-write
        'i32.atomic.rmw8.add_u', 'i32.atomic.rmw16.add_u', 'i32.atomic.rmw.add',
        'i64.atomic.rmw8.add_u', 'i64.atomic.rmw16.add_u', 'i64.atomic.rmw32.add_u',
        'i64.atomic.rmw.add', 'i32.atomic.rmw8.sub_u', 'i32.atomic.rmw16.sub_u',
        'i32.atomic.rmw.sub', 'i64.atomic.rmw8.sub_u', 'i64.atomic.rmw16.sub_u',
        'i64.atomic.rmw32.sub_u', 'i64.atomic.rmw.sub', 'i32.atomic.rmw8.and_u',
        'i32.atomic.rmw16.and_u', 'i32.atomic.rmw.and', 'i64.atomic.rmw8.and_u',
        'i64.atomic.rmw16.and_u', 'i64.atomic.rmw32.and_u', 'i64.atomic.rmw.and',
        'i32.atomic.rmw8.or_u', 'i32.atomic.rmw16.or_u', 'i32.atomic.rmw.or',
        'i64.atomic.rmw8.or_u', 'i64.atomic.rmw16.or_u', 'i64.atomic.rmw32.or_u',
        'i64.atomic.rmw.or', 'i32.atomic.rmw8.xor_u', 'i32.atomic.rmw16.xor_u',
        'i32.atomic.rmw.xor', 'i64.atomic.rmw8.xor_u', 'i64.atomic.rmw16.xor_u',
        'i64.atomic.rmw32.xor_u', 'i64.atomic.rmw.xor', 'i32.atomic.rmw8.xchg_u',
        'i32.atomic.rmw16.xchg_u', 'i32.atomic.rmw.xchg', 'i64.atomic.rmw8.xchg_u',
        'i64.atomic.rmw16.xchg_u', 'i64.atomic.rmw32.xchg_u', 'i64.atomic.rmw.xchg',
        // atomic memory accesses - compare exchange
        'i32.atomic.rmw8.cmpxchg_u', 'i32.atomic.rmw16.cmpxchg_u', 'i32.atomic.rmw.cmpxchg',
        'i64.atomic.rmw8.cmpxchg_u', 'i64.atomic.rmw16.cmpxchg_u', 'i64.atomic.rmw32.cmpxchg_u',
        'i64.atomic.rmw.cmpxchg',
        // atomic memory accesses - wait and notify operators
        'memory.atomic.wait32', 'memory.atomic.wait64', 'memory.atomic.notify', 'atomic.fence'

