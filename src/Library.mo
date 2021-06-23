/// A shiny new library
///
/// Make it easy and fun to use your new library by including some module specific documentation here.
/// It's always a good idea to include a minimal working example:
///
/// ```motoko
/// import LibraryTemplate "mo:library-template/Library";
///
/// assert(LibraryTemplate.isPalindrome("anna"));
/// assert(not LibraryTemplate.isPalindrome("christoph"));
/// ```
// Based on Sam Westrick's SML implementation: https://github.com/shwestrick/pure-splay/blob/master/BottomUpSplay.sml

import O "mo:base/Order";
import List "mo:base/List";

module {
    public type Tree<X> = {
        #node: (Tree<X>, X, Tree<X>);
        #empty;
    };
    type Context<X> = { #right: (Tree<X>, X); #left: (X, Tree<X>) };
    type Path<X> = (Tree<X>, List.List<Context<X>>);

    func path<X>(compareTo: (X,X) -> O.Order, k: X, (t, anc): Path<X>) : Path<X> {
        switch t {
        case (#empty) { (t, anc) };
        case (#node(L,x,R)) {
                 switch (compareTo(k,x)) {
                 case (#less) { path(compareTo, k, (L, List.push(#left(x, R), anc))) };
                 case (#equal) { (t, anc) };
                 case (#greater) { path(compareTo, k, (R, List.push(#right(L, x), anc))) };
                 };
             };
        };
    };
    func splay_<X>(compareTo: (X,X) -> O.Order, A: Tree<X>, B: Tree<X>, anc: List.List<Context<X>>) : (Tree<X>, Tree<X>) {
        switch anc {
        case (null) { (A, B) };
        case (?(#left(p, C), null)) { (A, #node(B,p,C)) }; // zig
        case (?(#right(C, p), null)) { (#node(C,p,A), B) }; // zag
        case (?(#left(p, C), ?(#left(g, D), anc))) { // zig-zig
                 splay_(compareTo, A, #node(B,p,#node(C,g,D)), anc)
             };
        case (?(#right(D,p), ?(#right(C, g), anc))) { // zag-zag
                 splay_(compareTo, #node(#node(C,g,D),p,A), B, anc)
             };
        case (?(#right(C,p), ?(#left(g,D), anc))) { // zig-zag
                 splay_(compareTo, #node(C,p,A), #node(B,g,D), anc)
             };
        case (?(#left(p,D), ?(#right(C,g), anc))) { // zag-zig
                 splay_(compareTo, #node(C,g,A), #node(B,p,D), anc)
             };
        };
    };
    func splay<X>(compareTo: (X,X) -> O.Order, l: Tree<X>, x: X, r: Tree<X>, anc: List.List<Context<X>>) : Tree<X> {
        let (l_,r_) = splay_(compareTo, l, r, anc);
        #node(l_,x,r_)
    };
    func subtree_max<X>(t: Tree<X>) : ?X {
        switch t {
        case (#empty) { null };
        case (#node(_, x, #empty)) { ?x };
        case (#node(_,_,r)) { subtree_max(r) };
        };
    };
    func subtree_min<X>(t: Tree<X>) : ?X {
        switch t {
        case (#empty) { null };
        case (#node(#empty, x, _)) { ?x };
        case (#node(l,_,_)) { subtree_min(l) };
        };
    };
    public class Splay<X>(compareTo: (X,X) -> O.Order) {
        var tree : Tree<X> = #empty;
        public func insert(k: X) {
            switch (path(compareTo, k, (tree, null))) {
            case ((#node(l,_,r), anc)) {
                     tree := splay(compareTo, l, k, r, anc);
                 };
            case ((#empty, anc)) {
                     tree := splay(compareTo, #empty, k, #empty, anc);
                 };
            };
        };
        public func find(k: X) : Bool {
            switch (path(compareTo, k, (tree, null))) {
            case ((#node(l,_,r), anc)) {
                     tree := splay(compareTo, l, k, r, anc);
                     true
                 };
            case ((#empty, null)) { false };
            case ((#empty, ?(recent, anc))) {
                     let (l,x,r) = switch recent {
                     case (#left(x,r)) { (#empty, x, r) };
                     case (#right(l,x)) { (l, x, #empty) };
                     };
                     tree := splay(compareTo, l, x, r, anc);
                     false
                 };
            };
        };
        public func remove(k: X) {
            if (not find(k)) { return };
            switch tree {
            case (#empty) { assert false; };
            case (#node(l,_,r)) {
                     let l_max = subtree_max(l);
                     switch l_max {
                     case (null) { tree := r };
                     case (?l_max) {
                              switch (path(compareTo, l_max, (l, null))) {
                              case ((#node(l_,_,r_), anc)) {
                                       let l = splay(compareTo, l_, l_max, r_, anc);
                                       switch l {
                                       case (#node(l, l_max, #empty)) {
                                                tree := #node(l, l_max, r);
                                            };
                                       case _ { assert false };
                                       };
                                   };
                              case ((#empty, anc)) { assert false };
                              };
                          };
                     };
                 };
            };
        };
        public func min() : ?X {
            subtree_min(tree);
        };
        public func fromArray(arr: [X]) {
            for (x in arr.vals()) {
                insert(x);
            }
        };
    };
}

