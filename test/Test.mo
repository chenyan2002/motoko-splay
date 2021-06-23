import M "mo:matchers/Matchers";
import Library "../src/Library";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";
import Int "mo:base/Int";
import Debug "mo:base/Debug";
import Array "mo:base/Array";

let t = Library.Splay<Int>(Int.compare);
t.fromArray([3,5,4,2,6,4,1,9,7,8]);
assert(t.find(2) == true);
assert(t.find(4) == true);
assert(t.find(10) == false);
assert(t.min() == ?1);
t.remove(4);
assert(t.find(4) == false);
assert(t.find(9) == true);
assert(t.min() == ?1);
t.remove(1);
assert(t.min() == ?2);

/*
let suite = S.suite("splay", [
    S.test("find",
           t.find(2),
           M.equals(T.bool(true))),
    S.test("christoph is not a palindrome",
      Library.isPalindrome("christoph"),
      M.equals(T.bool(false))),
]);

S.run(suite);
*/
