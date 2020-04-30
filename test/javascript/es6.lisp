(in-package :javascript-test)

(in-suite :javascript-test)

(test es6/constant
  (it
    (test-parse-and-serialize "const PI = 3.141593;")))

(test es6/block-scoped-variable
  (it
    (test-parse-and-serialize "
for (let i = 0; i < a.length; i++) {
    let x = a[i];
}
")))

(test es6/block-scoped-function
  (it
    (test-parse-and-serialize "
{
    function foo () { return 1 }
    foo() === 1;
    {
        function foo () { return 2 }
        foo() === 2;
    }
    foo() === 1;
}
")))

(test es6/arrow-function
  (it
    (test-parse-and-serialize "
odds  = evens.map(v => v + 1);
pairs = evens.map(v => ({ even: v, odd: v + 1 }));
nums  = evens.map((v, i) => v + i);
")))

(test es6/default-parameter-value
  (it
    (test-parse-and-serialize "
function f (x, y = 7, z = 42) {
    return x + y + z;
}
")))

(test es6/rest-parameter
  (it
    (test-parse-and-serialize "
function f (x, y, ...a) {
    return (x + y) * a.length;
}
")))

(test es6/spread-operator
  (it
    (test-parse-and-serialize "
var other = [ 1, 2, ...params ];
f(1, 2, ...params) === 9;
")))

(test es6/string-interpolation
  (it
    (test-parse-and-serialize "
var message = `Hello ${customer.name},
want to buy ${card.amount} ${card.product} for
a total of ${card.amount * card.unitprice} bucks?`;
")))

;; (test es6/custom-interpolation
;;   (it
;;     (test-parse-and-serialize "
;; get`http://example.com/foo?bar=${bar + baz}&quux=${quux}`
;; ")))

(test es6/binary-literal
  (it
    (test-parse-and-serialize "
0b111110111 === 503;
")))

(test es6/octal-literal
  (it
    (test-parse-and-serialize "
0o767 === 503;
")))

(test es6/property-shorthand
  (it
    (test-parse-and-serialize "
var x = 0, y = 0;
var obj = { x, y };
")))

(test es6/computed-property-name
  (it
    (test-parse-and-serialize "
let obj = {
    foo: 'bar',
    [ 'baz' + quux() ]: 42
}
")))

(test es6/method-property
  (it
    (test-parse-and-serialize "
obj = {
    foo (a, b) {

    },
    bar (x, y) {

    },
    *quux (x, y) {

    }
}
")))

(test es6/array-matching
  (it
    (test-parse-and-serialize "
var list = [ 1, 2, 3 ];
var [ a, , b ] = list;
[ b, a ] = [ a, b ];
")))

(test es6/array-matching
  (it
    (test-parse-and-serialize "
var list = [ 1, 2, 3 ];
var [ a, , b ] = list;
[ b, a ] = [ a, b ];
")))

(test es6/object-matching-shorthand-notation
  (it
    (test-parse-and-serialize "
var { op, lhs, rhs } = getASTNode();
")))

(test es6/object-matching-deep-matching
  (it
    (test-parse-and-serialize "
var { op: a, lhs: { op: b }, rhs: c } = getASTNode();
")))

(test es6/object-and-array-matching-with-default-value
  (it
    (test-parse-and-serialize "
var obj = { a: 1 };
var list = [ 1 ];
var { a, b = 2 } = obj;
var [ x, y = 2 ] = list;
")))

(test es6/parameter-context-matching
  (it
    (test-parse-and-serialize "
function f ([ name, val ]) {
    console.log(name, val);
}
function g ({ name: n, val: v }) {
    console.log(n, v);
}
function h ({ name, val }) {
    console.log(name, val);
}
")))

(test es6/fail-soft-destructuring
  (it
    (test-parse-and-serialize "
var [ a = 1, b = 2, c = 3, d ] = list;
")))

(test es6/export
  (it
    (test-parse-and-serialize "
export function sum (x, y) { return x + y };
export var pi = 3.141593;
export * from 'lib/math';
export { name1, name2, name3 } from 'foo';
export { default } from 'foo';
export { foo1 as bar1, foo2 as bar2, foo3 as bar3 } from 'zzz';
export class ClassName {};
export var e = 2.71828182846;
export default (x) => Math.exp(x);
")))

(test es6/import
  (it
    (test-parse-and-serialize "
import * as math from 'lib/math';
import { sum, pi } from 'lib/math';
import exp, { pi, e } from 'lib/mathplusplus';
")))

(test es6/class-definition
  (it
    (test-parse-and-serialize "
class Shape {
    constructor (id, x, y) {
        this.id = id;
        this.move(x, y);
    }
    move (x, y) {
        this.x = x;
        this.y = y;
    }
}
")))

(test es6/class-inheritance
  (it
    (test-parse-and-serialize "
class Rectangle extends Shape {
    constructor (id, x, y, width, height) {
        super(id, x, y);
        this.width  = width;
        this.height = height;
    }
}
class Circle extends Shape {
    constructor (id, x, y, radius) {
        super(id, x, y);
        this.radius = radius;
    }
}
")))

(test es6/static-member
  (it
    (test-parse-and-serialize "
class Rectangle extends Shape {
    static defaultRectangle () {
        return new Rectangle('default', 0, 0, 100, 100);
    }
}
class Circle extends Shape {
    static defaultCircle () {
        return new Circle('default', 0, 0, 100);
    }
}
")))

(test es6/getter-and-setter
  (it
    (test-parse-and-serialize "
class Rectangle {
    constructor (width, height) {
        this._width  = width;
        this._height = height;
    }
    set width  (width)  { this._width = width;               }
    get width  ()       { return this._width;                }
    set height (height) { this._height = height;             }
    get height ()       { return this._height;               }
    get area   ()       { return this._width * this._height; }
}
")))

(test es6/iterator
  (it
    (test-parse-and-serialize "
let fibonacci = {
    [Symbol.iterator]() {
        let pre = 0, cur = 1
        return {
           next () {
               [ pre, cur ] = [ cur, pre + cur ]
               return { done: false, value: cur }
           }
        }
    }
}
")))

(test es6/for-of
  (it
    (test-parse-and-serialize "
for (let n of fibonacci) {
    if (n > 1000)
        break
    console.log(n)
}
")))

(test es6/generator-function
  (it
    (test-parse-and-serialize "
function* range (start, end, step) {
    while (start < end) {
        yield start;
        start += step;
    }
}
")))

(test es6/generator-method
  (it
    (test-parse-and-serialize "
class Clz {
    * bar () {

    }
}
let Obj = {
    * foo () {

    }
}
")))
