
Cubiml is a simple ML-like programming language with subtyping and full type inference. You can try it out online in your browser [here](https://storyyeller.github.io/cubiml-demo/demo.html). Cubiml uses _cubic biunification_, a faster and simpler type inference algorithm based on [Algebraic Subtyping](https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf). Cubiml is not intended to be used in its own right, but rather to serve as a tutorial for implementing cubic biunification, and therefore has a deliberately minimal feature set. 

## Usage

You can try out cubiml online in your browser at https://storyyeller.github.io/cubiml-demo/demo.html. 

## A quick tour of cubiml

Cubiml syntax is mostly a subset of Ocaml syntax.

#### Conditionals

In cubiml `if` is an expression, not a statement. The general form is `if <expr> then <expr> else <expr>` where the `<expr>`s are sub-expressions. The first expression is evaluated, and depending on whether it is true or false, one of the other two subexpressions is evaluated, and the result of the `if` expression is that expression's value. For example, evaluating `if false then "Hello" else "World"` would result in `"World"`. You can think of this as similar to the ternary operator (`a ? b : c`) present in some programming languages.

#### Records and fields

Records are a grouping of zero or more named values similar to "objects" or "structs" in other programming languages and are defined via `{name1=val1; name2=val2; ...}`. You can access the value of a field using the usual `.name` syntax. For example `{a=true; b="Hello"; c={}}.b` would evaluate to `"Hello"`.

There is a special shorthand syntax for fields with the same name as their value - `{a; b; c=4}` is equivalent to `{a=a; b=b; c=4}`.

#### Functions

In cubiml, all functions are required to take exactly one argument for simplicity. They are defined by `fun <arg> -> <expr>`. For example, a function which returns its argument unchanged could be written as `fun x -> x`. Functions are called by simply suffixing an argument, i.e. writing `a b` where `a` is the function to be called and `b` is the argument. For example 

    (fun b -> if b then "Hello" else "World") true

would evaluate to `"Hello"`, while 

    (fun x -> x.foo) {bar=false; foo="Bob"}

would evaluate to `"Bob"`. 


You can work around the one-argument limitation and simulate multiple function arguments by passing in a record. For example, instead of 

```js
function sum(a, b) {
    return a + b;
}

sum(7, 8)
```

in cubiml, you can do

```ocaml
let sum = fun args -> args.a + args.b;

sum {a=7; b=8}
```

In fact, you can simplify this further using destructuring patterns:

```ocaml
let sum = fun {a; b} -> a + b;

sum {a=7; b=8}
```

> :warning: **Function calls have different precedence than in Ocaml.** `a b c` is parsed as `a (b c)` rather than `(a b) c` as it would be in Ocaml.

#### Let bindings

No programming language would be complete without the ability to bind values to a variable name for later reference. In cubiml, this is done slightly differently than you may be used to. The general format is `let <name> = <expr1> in <expr2>`, where the variable `<name>` is visible in the body of `<expr2>`. The entire thing is an expression which evaluates to whatever `<expr2>` evaluates to.

For example,
```ocaml
let x = 7 * 8 in {foo=x; bar=x}     
```
would evaluate to `{foo=56; bar=56}`.

Let bindings can of course be nested like any other expression. For example, 
```ocaml
let x = 3 + 4 in
    let y = x * 2 in
        {x=x; y=y}
```
would evaluate to `{x=7; y=14}`.

This provides an equivalent to traditional imperative style code like the following that you might see in other languages

```js
let x = 3 + 4;
let y = x * 2;
return {x=x, y=y};
```

Note that the above format produces an expression which can be used in any context where an expression is expected. Cubiml follows the ML philosophy that (nearly) everything is an expression, even conditionals, function definitions, variable bindings, and other things that are statements in some languages.

However, this style is inconvenient when  interactively entering code into a REPL ([Read Evaluate Print Loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)), because it requires you to input the entire program at once. To handle this, cubiml allows an alternate non-expression format at the top level of your code. At the top level, you can omit the `in <expr>` part, in which case the let statement produces a global binding which is visible to all subsequent code. For example, here is a possible session of code entered into the cubiml REPL. 

```
>> let x = {}

{}

>> let y = {x=x; other=false}

{x={}; other=false}

>> let z = {other=y.other; foo={y=y; x=x}}

{other=false; foo={y={x={}; other=false}; x=...}}
```

You can also separate multiple top level definitions with semicolons if you are entering multiple items at once.

```
>> let a = z.foo.y; let b = true

true

>> if b then y else x

{x={}; other=false}
```

#### Recursive let bindings

Sometimes, one wishes to have functions that call themselves recursively. Unfortunately, this is impossible with the above constructs since plain let-expressions can only refer to variables that were already defined. 

In order to support recursion, cubiml offers _recursive let expressions_ which are defined via `let rec` and allow the definition of the variable to refer to itself. For example, you could define a recursive fibonacci function as follows:

```ocaml
let rec fib = fun x ->
    if x <= 1 then 
        1
    else
        fib(x - 1) + fib(x - 2)
```

In order to avoid code referring to variables that don't exist yet, the right hand side of `let rec` variable definitions is restricted to be a function definition.


#### Mutual recursion

The above syntax works for a single function that refers to itself, but in some cases, you may want to have multiple functions that each refer to each other. Unlike in the case with `let`, simply nesting `let rec`s won't work. Therefore, `let rec` allows _multiple_ variable bindings, separated by `and`. For example, you can define mutually recursive `even` and `odd` functions as follows:

```ocaml
let rec even = fun x -> if x == 0 then true else odd(x - 1)
    and odd = fun x -> if x == 0 then false else even(x - 1)
```

#### Case types and matching

Sometimes you need to make different decisions based on runtime data in a type safe manner. Cubiml supports this via _case types_, also known as _sum types_ or _enums_. Basically, the way they work is that you can wrap a value with a tag, and then later match against it. The match expression has branches that execute different code depending on the runtime value of the tag. Crucially, each match branch has access to the static type of the original wrapped value for that specific tag. You can think of it like a simpler, statically checked version of Java's visitor pattern or a giant switch statement on an union in C.

To wrap a value, prefix it with a grave (\`) character and an uppercase Tag. E.g. `` `Foo {hello="Hello"}``

You can later match on it like follows

```ocaml
let calculate_area = fun shape ->
    match shape with
        | `Circle v -> v.rad *. v.rad *. 3.1415926
        | `Rectangle v -> v.length *. v.height;

calculate_area `Circle {rad=6.7}
calculate_area `Rectangle {height=1.1; length=2.2}
```

Notice that within the Circle branch, the code can access the rad field, and within the Rectangle branch, it can access the length and height field. Case types and matches let you essentially "unmix" distinct data types after they are mixed together in the program flow. Without case types, this would be impossible to do in a type safe manner.

#### Wildcard matches

Match expressions can optionally end with a wildcard match, which is the same as a regular case match except that it doesn't include a tag. The wildcard branch will be taken if the runtime tag of the matched value does not match any of the explicitly listed tags in the match expression.

```ocaml
let calculate_area = fun shape ->
    match shape with
        | `Circle v -> v.rad *. v.rad *. 3.1415926
        | `Rectangle v -> v.length *. v.height
        |  v -> "got something unexpected!"
```

Within a wildcard match, the bound variable has the same type as the input expression, except with the explicitly matched cases statically excluded. For example, in the `calculate_area` example above, `v` would have the type "same as `shape` except not a `Circle` or `Rectangle`".

This makes it possible to further match on the wildcard value elsewhere. For example, in the below code, the new `calculate_area2` function explicitly handles the `Square` case and otherwise defers to the previously defined function to handle the `Circle` and `Rectangle` cases. This works because the compiler knows that the `v` in the wildcard branch is not a `Square`, so it will not complain that the original `calculate_area` function fails to handle squares.

```ocaml
let calculate_area = fun shape ->
    match shape with
        | `Circle v -> v.rad *. v.rad *. 3.1415926
        | `Rectangle v -> v.length *. v.height;

let calculate_area2 = fun shape ->
    match shape with
        | `Square v -> v.len *. v.len
        |  v -> calculate_area v;

calculate_area2 `Circle {rad=6.7}
calculate_area2 `Square {len=9.17}
```

#### Literals 

Cubiml has boolean, int, float, string, and null literals. Integers are arbitrary precision and can't have leading zeros. Floating point literals must contain a decimal point, but the fraction part is optional. Strings are double quoted and use backslash escapes.

```ocaml
true;
false;
null;
0;
-213132;
999999999999999999999999999999999999999999999999;
8.213;
-1.;
-0.01e33;
7.e-77;
"";
"Hello world!";
"Quote -> \" backslash -> \\ Single quote -> \'"
```

#### Operators

Use `+, -, *, /, %` for integer math. For floating point math, add a `.` to the end of the operator, e.g. `7.5 /. 12.0`. String concatenation is `^`. 

`<, <=, >, >=` can compare both ints and floats. `==` and `!=` compare values of any type (values of different types compare unequal).

```ocaml
5 * 2 + 1;
-1 <= 0;
7.5 /. 12.0;
0 > 0.1;
9 == "what?";
"Hello," ^ " World!";
"" != {}
```

#### References

Cubiml supports mutability via ML-style references. You can simulate traditional mutable fields by storing a reference in a record field, and a mutable variable by storing a reference in a variable and so on.

ML references are not quite the same as what you may be used to. They are pointers to a mutable, garbage collected storage location on the heap and support three operations:

* `ref x` creates a new reference that initially holds the value `x`. Note that this _copies_ the value of `x` to a new location and returns a pointer to that location. `ref foo.bar` returns a pointer to a location that is initialized to the value of `foo.bar`, rather than a pointer to the field of `foo` itself.
* `!r` _dereferences_ the reference `r` and returns whatever value is currently stored inside. Note that this differs from the `!` operator in C style syntax.
* `r := x` _stores_ `x` inside the reference `r`, overwriting whatever value was previously stored there. Traditionally, this operation returns a unit value (i.e. empty record), but cubiml instead follows the approach of C-style assignments as Javascript does, where assignment returns the new value.

Note that references may be aliased. For example, we can create a reference `a` and copy it to the variable `b`. Then any changes made via `b` are visible via `a` and vice versa, as shown in the following REPL session.

```
>> let a = ref 42

ref 42

>> let b = a

ref 42

>> b := 77

77

>> !a

77
```

#### Record extension

When creating a record, you can optionally copy over all the fields from another record by writing `foo with` at the start of the record, where `foo` is the record you want to copy from.

```ocaml
let foo = {a=1; b=""; c=false};
let bar = {foo with a=true; d=-23}
```

The value you are copying fields from does not have to be a statically known value. It can be any arbitrary expression (as long as you surround it in parenthesis).

```
>> let baz = {(if foo.c then foo else bar) with c=true}

{a=true; b=""; c=true; d=-23}
```

#### Comments

Comments use `(* *)`. Comments cannot be nested. You can put comments before match arms, before record fields, and before expressions wherever a let declaration or function definition would be allowed. To put them in front of expressions with higher precedence, surround it in parenthesis first.

```ocaml
(* define x = 4 *)
let x = 4;

let y = x + (
    (* 2 is an int *) 
    (* we needed the ()s around 2 here since + has higher precdence *)
    2
);

let z = (* let's define a new record! *) {
    (* a is a record field *)
    a = x;

    b = 
        (* comments can also go before expressions *) 
        "Hello world!";

    c = match `Some y with
        | `Some y -> y
        (* this match arm isn't actually reachable *)
        | `None _ -> _
}
```

### Type annotations

Expressions can manually be annotated with a type via `(expr : type)`, e.g. `(24 : int)` or `(fun x -> x : str -> str)`. Type annotations can be one of the following:

#### Base types

The primitive types are `bool`, `float`, `int`, `str`, `null`, `number`, `top`, `bot`. 

`number` represents a value that can be an `int` _or_ a `float`.

`top` is the supertype of all types. `bot` is the subtype of all types. It's impossible to do anything useful with a value of type `top`, while it is impossible to _create_ a value of type `bot`. 

#### Nullable types

`type?`

#### Functions types

`type -> type`

Function type arrows have the lowest precedence. For example `int -> int?` is parsed as `int -> (int?)`, i.e. a function that takes an int and returns an int or null. To represent a _function_ or null, you need to instead write `(int -> int)?`.

#### Reference types

`type ref`, `type readonly ref`, or `type writeonly ref`

#### Record types

Explicit list of fields:

`{field1: type1; field2: type2}`

Explicit list of fields plus any number of fields not mentioned:

`{_ with field1: type1; field2: type2}`

Note: The list of fields cannot be empty. `{}` is not a valid type annotation. Use `top` instead.

#### Case types 

Explicit list of cases:

`[tag1 of type1 | tag2 of type2]`

Explicit list of cases plus any number of cases not mentioned:

`[_ | tag1 of type1 | tag2 of type2]`

Note: The list of cases cannot be empty. `[]` is not a valid type annotation. Use `bot` instead.

#### Holes

`_` creates a fresh type variable. Effectively, this leaves a hole which gets filled in with the corresponding part of the inferred type. It is useful if you only want to constrain part of the type with a type annotation. 

For example if you have a record `foo`, with fields `a` and `b` you could write `(foo : {a: int; b: _})` to ensure `foo.a` is an int while placing no constraints on `foo.b`. 

`_` can also be used to extend record and case type annotations with any number of fields or tags not specified. For example the above example could also be written `(foo : {_ with a: int})`. This says that `foo` has a field named `a` that is an `int` and can have any number of other fields with any types. Likewise you can write types like ``[_ | `A of int | `B of str]`` to represent a case type where the `A` tag has type `int`, the `B` tag has type `str`, and there can be any number of other tags not specified with any types.

#### Recursive types

You can give an explicit name to a type via `type as 'name` and then reference it later via `'name`. This lets you express recursive types. For example, the following code demonstrates a type annotation with a simple recursive list type:

```ocaml
let rec build_list = fun n ->
    if n < 0 then
        null
    else
        {val=n; next=build_list (n - 1)};

let list = (build_list 4 : {val: int; next: 'list}? as 'list)
```

Type aliases can appear anywhere within a type annotation, not just at the top level. This means you can define multiple type variables within a type annotation, allowing you to express mutually recursive types.


## Building cubiml from source

You will need to have lalrpop and wasm-pack installed. First, generate the parser code

    lalrpop src/grammar.lalr

Then build the wasm module and js wrapper with wasm-pack

    wasm-pack build --target web
