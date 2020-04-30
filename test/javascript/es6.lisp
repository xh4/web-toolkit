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
