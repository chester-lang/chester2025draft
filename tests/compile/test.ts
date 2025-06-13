const a: number = 42;
const a_1: number = a;
const a_2: number = (Math.floor(Math.random() * 10) + 1);
const a_3: number = a_2;
const c: (_0: number) => number = (function(x) { return x+1; });
const b: number = c(42);
const d: (_0: number, _1: number) => number = (function(x, y) { return x + y; });
const e: (_0: number, _1: number) => number = d;