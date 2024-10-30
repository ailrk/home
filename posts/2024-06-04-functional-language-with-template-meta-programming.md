---
title: Functional language with template meta programming
date: 2024-06-04
description: How functional is C++ template?
tags: c++
---


![](/images/sicp-anime1.jpg)

---


We have all heard that C++ template metaprogramming is essentially functional programming, but how functional is it really? Let's find out!

In this article, I will use C++ templates as a programming language to implement an interpreter for a simple functional language called skibid-lang, which includes a reasonable set of features. This implementation is similar to the meta-circular interpreter in SICP.

Ok, let's begin!


## Warm up

Let's start with some simple utilities. Below is the definition of an identity function. Since we are operating at the type level, `struct` will be used as our function to map from type to type. Here we define a `struct` that takes a type `T` and returns the type `T`. The full evaluation is `identity<T>::type`. Note that a C++ type is a value in skibid-lang, and skibid-lang itself is duck typed, just like the template.


#### Identity

```c++
template <typename T> struct identity { using type = T; };
```

To handle all variables in a uniform manner, all values in skibid-lang are `struct`s and are subclassed from `template <typename T> struct identity`. This ensures that we can always consult the `::type`, regardless of the value. This also means skibid-lang values are lazy and need to be explicitly evaluated.

*Convention 1: Any skibid-lang value always contains a `::type` attribute*


#### Quotes & unquotes

Quotes allow you to control when an expression is evaluated. For example, `identity<int>::type` will be evaluated to `int`, but `quote<identity<int>::type>` is just a `quote` and can be passed around. To retrieve the value, you can use `unquote<quote<identity<int>>>::type`, which cancels out the `quote`.

```c++
template <typename T> struct quote : identity<quote<T>> {};
template <typename T> struct unquote<quote<T>> : T {};
```


#### Eval

We write an `eval` helper so we don't need to write `::type` all the time. This technique is commonly seen in template libraries as well.

```c++
template <typename T> struct eval : T::type {};
```

## Values

Let's define some data types for the language. C++ supports basic types like `int` and `bool` directly at the type level. We will try to reuse as much of C++ as possible to avoid implementing these types ourselves.

#### Integer

The following is the data constructor for a type-level integer. It's defined as a skibid-lang type with the additional `::value` attribute. `::value` is used to retrieve the constant value from a skibid-lang value.

```c++
template <int N> struct int_ : identity<int_<N>> { static const int value = N; };
```

*Convention 2: A skibidi constant has a `::value` attribute*

Here are some arithmetic operations for integers. Instead of implementing them within skibid-lang, we take the shortcut and reuse C++ facilities directly. Here we simply created some skibid-lang functions, unwrap parameters to retrieve the C++ integer from arguments, and perform C++ arithmetic operations. A skibid-lang function always comes with an `::apply` attribute that should be templated with some parameters.

```c++
struct plus : identity<plus> {
    template <typename A, typename B>
    using apply = int_<A::type::value + B::type::value>;
};

struct minus : identity<minus> { ... };
struct times : identity<times> { ... };
```

Now we can perform basic arithmetic with these operations. For instance, `plus::apply<int_<1>, int_<2>>::type::value` should evaluate to `int_<3>`.

*Convention 3: A skibidi function has a `::apply` attribute that takes some arguments and evaluates to some other skibidi value*

We can use the following helper to perform application instead of manually accessing `::apply`.

```c++
template <typename F, typename... Ts> struct apply {
    using type = typename F::template apply<Ts...>::type;
};
```

#### Boolean

Boolean is defined in a similar way.

```c++
template <bool N> struct bool_ : identity<bool_<N>> { static const bool value = N; };
```

We define `not_` using template specialization. First, we declare the function `not_`, and then we add cases for it. Each specialization is like a branch in pattern matching, and the inheritance here serves as a return. Since `bool_` is already a skibid-lang value, we don't need to define `::type` for `not_` again.


```c++
template <typename T> struct not_;
template <> struct not_<bool_<true>> : bool_<false> {};
template <> struct not_<bool_<false>> : bool_<true> {};
```

`and_` and `or_` are variadic templates. You can also make them binary operations, but the template syntax is already quite cluttered, so the fewer angle brackets, the better for us.

```c++
template <typename... Ts>
struct and_ : bool_<(Ts::type::value && ... && true)> {};

template <typename... Ts>
struct or_ : bool_<(Ts::type::value || ... || false)> {};
```


#### Ordering on integer

With boolean and integer defined, we can implement some ordering operations for integers as follows:

```c++
struct less_equal : identity<less_equal> {
    template <typename A, typename B>
    using apply = bool_<(A::type::value <= B::type::value)>;
};

struct less : identity<less> { ... };

struct greater : identity<greater> { ... };

struct greater_equal : identity<greater_equal> { ... };
```

This gives us the foundation to implemnet conditional later.



#### List

Let's define a type level list. First we need two constructors `cons` and `nil`.

```c++
template <typename H, typename T> struct cons : identity<cons<H, T>> {};
struct nil : identity<nil> {};
```

It's pretty straightforward to work with this type of list in templates. We can use template specialization to deconstruct the `cons` constructor and work with the head and tail individually. Since specialization can be recursive, we can write recursive functions to work on the entire list.

```c++
template <typename L, typename R> struct append;
template <typename R> struct append<nil, R> : R {};
template <typename H, typename T, typename R> struct append<cons<H, T>, R> : cons<H, append<T, R>> {};

template <typename T> struct last;
template <> struct last<nil> : nil {};
template <typename N> struct last<cons<N, nil>> : N {};
template <typename T, typename N> struct last<cons<T, N>> : last<N> {};

template <typename T> struct length;
template <> struct length<nil> : int_<0> {};
template <typename N, typename T> struct length<cons<N, T>> : apply<plus, int_<1>, length<T>> {};

template <typename T> struct null : bool_<false> {};
template <> struct null<nil> : bool_<true> {};

template <typename F, typename L> struct map;
template <typename F> struct map<F, nil> : nil {};
template <typename F, typename H, typename T> struct map<F, cons<H, T>> : cons<apply<F, H>, map<F, T>>{ };

template <typename T> struct reverse;
template <typename S1, typename S2> struct __reverse;
template <typename S2> struct __reverse<nil, S2> : S2 {};
template <typename H, typename T, typename S2> struct __reverse<cons<H, T>, S2> : __reverse<T, cons<H, S2>> {};
template <typename L> struct reverse : __reverse<L, nil> {};
```


## Conditional
The only conditional we need is `if ... then ... else ...`. The helper `if_impl` selects the skibidi value base on the condition.

```c++
template <typename C, typename T, typename F> struct if_ {
  private:
    template <bool C_, typename T_, typename F_> struct if_impl;
    template <typename T_, typename F_> struct if_impl<true, T_, F_> : T {};
    template <typename T_, typename F_> struct if_impl<false, T_, F_> : F {};

  public:
    using type = typename if_impl<C::type::value, T, F>::type;
};
```


## Variables?

Variables are defined as arbitrary empty structs. For example, to use a variable `x`, we need to first define `struct x {};`, then wrap it as `var<x>`.

```c++
template <typename Id> struct var : identity<var<Id>> {};
```

Different from most languages, in skibid-lang, before using any variable, we need to declare them first. So if you have a lambda that uses an `x`, you need to have the following definition somewhere before the usage.

```C++
struct x__ {};
using x = var<x__>;
```

This is rather daunting, we can make it less painful with some macros.

```c++
#define declare(n)                                                             \
    struct n##__tml__internal_defined_var_ {};                                 \
    using n = var<n##__tml__internal_defined_var_>


// we need to declare variables before being able to use them.
declare(_v0);
declare(_v1);
declare(_v2);
```


## Let

The `let` binding comes in the form `let <var> = <expr1> in <expr2>`. To evaluate a `let` expression, we need to substitute the occurrence of `<var>` in the body `<expr2>` with `<expr1>`. To do so, we need to quote `<expr1>` and `<expr2>` to move them around during the substitution, and then unquote everything to evaluate the substituted `<expr3>`.

We now declare our substitution function for `let`. Here we have `let_subst_1` and `let_subst_2`, two functions, but they're really just one function. The order of specialization matters; when there are multiple possible specializations, we want to prioritize some over others. `let_subst_1` acts like a filter; we only call `let_subst_2` after all specializations in `let_subst_1` have failed.

```c++
template <typename A, typename E, typename In> struct let_subst_1;
template <typename A, typename E, typename In> struct let_subst_2;
```

This case interfere with `let_subst_2<_A, _E, quote<V>>`, so we pick `V` out of the quote it here.

```c++
template <typename A, typename E, typename V>
struct let_subst_1<A, E, unquote<quote<V>>> : let_subst_1<A, E, V> {};
```

For expressions like `let x = y in x`, y is .redundant and can be discarded.
```c++
template <typename A, typename E> struct let_subst_1<A, E, A> {
    using type = E;
};
```

With all cases above out of the way, we move into `let_subst_2`.
```c++
template <typename A, typename E, typename In>
struct let_subst_1 : let_subst_2<A, E, In> {};
```

`let_subst_2` contains the full substitution.

If the body is quoted don't touch it quoted.

```c++
template <typename _A, typename _E, typename V>
struct let_subst_2<_A, _E, quote<V>> : quote<V> {};
```

If right-hand side expression is an application, we substitute left-hand side expression to the body of every arguments.
```c++
template <typename A, typename E, template <typename...> typename F,
          typename... Ts>
struct let_subst_2<A, E, F<Ts...>> {
    using type = F<typename let_subst_1<A, E, Ts>::type...>;
};
```

Finally, we simply evaluate the let body.
```c++
template <typename _A, typename _E, typename In> struct let_subst_2 {
    using type = In;
};
```

We define a new helper `strict_let` that extracts quoted `E` and `In`, call `let_subst_1` on it, and evaluates the resulted expression. We quote the result so during the substitution the evaluation is not performed. If we want to get the value back we can simply unquote the whole thing.


```c++
template <typename A, typename E, typename In> struct strict_let;

template <typename A, typename E, typename In>
struct strict_let<A, quote<E>, quote<In>>
    : quote<typename let_subst_1<A, E, In>::type> {};

template <typename A, typename E, typename In>
struct let_quoted :
    strict_let<typename A::type, typename E::type, typename In::type> {};
```

Finally we tight everything together to make `let`. `let` only have specialization when `In` is `in` so this forces us to use the `let ... in ...` syntax. `let` specializes to `unquote<typename let_quoted<A, quoted<E>, quote<In>>>`, it unquotes the quote added put by `strict_let`.

```c++
template <typename In> struct in : In {};

template <typename A, typename E, typename In> struct let;

template <typename A, typename E, typename In>
struct let<A, E, in<In>>
    : unquote<typename let_quoted<A, quote<E>, quote<In>>::type> {};
```

We use let like this:

```c++
let<x, int_<10>, in <
let<y, int_<20>, in <
    y>>>>;
```

## Lambda

lambda is represented as a pair of parameter list and quoted body expression. The implementation is very similar to `let`. Actually we can always convert a let expression `let x = <expr>  in <expr>` into `(\x -> <expr>) <expr>`.  Because the implementation is so close, I will not explain as detailed as the `let` section.


We have the similar mutual recursive substitution functions as `let`.

```c++
template <typename Body, typename... Ts> struct lambda_subst_1;
template <typename Body, typename...> struct lambda_subst_2;


template <typename Body, typename... Ts>
struct lambda_subst_1 : lambda_subst_2<Body, Ts...> {};

template <typename Body, typename... Ts>
struct lambda_subst_1<unquote<quote<Body>>, Ts...> : lambda_subst_1<Body, Ts...> {
};

template <typename Body, typename... Ts>
struct lambda_subst_2<quote<Body>, Ts...>
    : identity<lambda_subst_2<quote<Body>, Ts...>> {
    using apply = quote<Body>;
};

template <typename Body> struct lambda_subst_2<Body> : Body {};

template <typename Body, typename T, typename... Ts>
struct lambda_subst_2<Body, T, Ts...> : identity<lambda_subst_2<Body, T, Ts...>> {
    template <typename U>
    using apply = lambda_subst_1<let<T, U, in<Body>>, Ts...>;
};
```

Like `let`, we need to handle quotes for lambda as well.

```c++
template <typename Body, typename... Ts> struct strict_lambda;

template <typename Body, typename... Ts>
struct strict_lambda<quote<Body>, Ts...>
    : quote<typename lambda_subst_1<Body, Ts...>::type> {};

template <typename Body, typename... Ts>
struct lambda_quoted : strict_lambda<typename Body::type, typename Ts::type...> {};

template <typename... Ts> struct lambda {
    template <typename Body>
    using begin = unquote<typename lambda_quoted<quote<Body>, Ts...>::type>;
};
```

We can now define lambdas like this:

```c++
declare(x); declare(y); declare(z);
using func =
    lambda<x, y>::begin<
        let<z, int_<2>, in <
            apply < plus, apply <plus, x, y>
                  , apply <times, z, z>
                  >>>>;
```

## Put it together

We have implemented a lot of features so far, let's tight everything together and see how does skibidi-lang look like in action. The following program computes `10 - (3 + factorial(10))`.

```c++
declare(x); declare(y);

template <typename N>
struct factorial :
    if_ < apply <less, N, int_<1>>
        , int_<1>
        , apply <times, factorial <apply <minus, N, int_<1>>>, N>
        > {};

using plus3 =
        lambda<x>::begin
            <
               apply <plus, x, 3>
            >

using program =
    let <x, factorial<int_<10>, in <
    let <y, int_<10>,      in <
        apply <minus, y, apply <plus3, x>>
    >>>>>;


// output: -3628793
int main () {
    std::cout << program::type::value << std::endl;
}
```

It's probably not the prettiest, definitely not the most practical, but I like how chaotic it is.


## Conclusion

That's it! We've implemented a simple functional language interpreter using only C++ templates and functional programming techniques. While it's not practical for real-world use, it's a meaningful exercise to explore the power and flexibility of C++ templates. And the most important part is, it was tons of fun! The full code can be found on [github](https://github.com/ailrk/skibidi-lang/blob/main/skibidi.hh)
