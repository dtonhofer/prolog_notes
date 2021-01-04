# Operators

The goal of operators are to support syntax: They tune the parser so that you can write your expresssion (and read you expressions) more fluently.

This brings an interesting bag of complexity to any Prolog parser: The parser must be aware of what 
operators are in effect at the current line to be able to properly parse it. This is why a "simple grammar" won't do.

Operators say nothing about what they mean. What they mean depends on some processing predicate that is fed
the term that has been built out of an expression written using operators.

For example:

You may have the following expression (which is, at this point just a string) built using operators (in this case, the usual arithmetic operators):

```
1*3+4**5-66
```

As the operators `*`, `+`, `**`, `-` have been defined, the above string can be parsed into a term, and then 
printed using [`write_canonical/1`](https://eu.swi-prolog.org/pldoc/man?predicate=write_canonical%2f1).

```
?- 
read_term_from_atom('1*3+4**5-66', X, []),    % parse the text into a term, and bind the variable X to that term 
write_canonical(X).                           % write the term structure
```

which is just a roundabout way of doing the following, where parsing of `1*3+4**5-66` is done implicitly by the program text parser:

```
?- 
X = 1*3+4**5-66,                              % parse the text into a term, and bind the variable X to that term
write_canonical(X).                           % write the term structure
```

In both cases, we get:

```
-(+(*(1,3),**(4,5)),66)
X = 1*3+4**5-66.
```

Which is the following parse tree written in prefix notation:

Note that the Prolog reader has to make a decision on how to cut up the string `1*3+4**5-66` into distinct tokens: 

```
1*3+4**5-66  -> 1 * 3 + 4 ** 5 - 66
```

and these can then be assembled into a parse tree.
Sometimes this tokenization does not do what one wants. Then parentheses need to be used or separating whitespace needs to be added:

The first token here is `-2`:

```
?- 
X = -2**66,
write_canonical(X).

**(-2,66)
X = -2**66.
```

You may want `-(2)` (the token `-` and the token `2`, which is mapped to an unary operator):

```
?- 
X = -(2)**66,
write_canonical(X).

**(-(2),66)
X =  (- 2)**66.
```

Or maybe have the `-` in front of the `**` subexpression:

`` 
?- 
X = - 2**66,
write_canonical(X).

-(**(2,66))
X = - 2**66.
```

So, what do we do with the term `-(+(*(1,3),**(4,5)),66)`, which is a parse tree, with operators (and operands) 
as tree nodes? Well, for example, one can feed it to an arithmetic expression evaluation engine. But that is outside 
of the scope of "operators".

## The `op/3` command

The [`op/3`](https://www.swi-prolog.org/pldoc/doc_for?object=op/3) command, `op(+Precedence, +Type, :Name)` takes:

   - **Precedence**: A numeric value between ⁰ and 1200. I prefer to call this **Precedence Value** instead of
     **Precedence** because the two vary in inverse sense: The _higher_ the Precedence Value, the _lower_ the Precedence.
     If you build the parse tree bottom-up, operators with low Precedence Value glom to their neighboring tokens first,
     they are "near the leaves" of the parse tree. On the other hand. operators with high Precedence Value are left for late
     in parse tree construction, they are "near the root" of the parse tree.
   - **Name**: The atom designating the operator, which becomes the "functor name" of respective the node in the parse tree.
   - **Type**: Indication to the parser regarding
        - Whether this is a prefix operator, a postfix operator or an infix operator
        - How to disambiguate a sequence of non-parenthesized operators with the same precedence value
     
The parse tree from earlier, with precedence values and types indicates. Subtrees/Terms with precedence value 0
(i.e. subtrees which do not have an operators as their root or subtrees which have been parenthesized explicitly, of which there
are none here though) are shown in blue with no precedence. 

The following types exist:   
   
   - Postfix operator: 
      - `yf`: This operator can appear in a sequence of unparenthesized postfix operators that have the same precedence value and also type `yf`.
              In particular, it can form unparenthesized repeats, as in `g(x) f f f`. The leftmost unparenthesized prefixed expression can
              be composed of an operator of the same precedence value. The implied parenthetization is "the leftmost subexpression is innermost".              
              Of course, the leftmost prefixed expression can always be based on a higher-precedence unary or binary operator or be a 
              subexpression of precedence value 0, in particular it can be parenthesized. There is no ambiguity to be resolved there.
      - `xf:  This operator cannot appear in a sequence of unparenthesized postfix operators that have the same precedence value.
              In particular, it does not allow forming unparenthesized repeats. The postfixed subexpression **must be** of lower precedence value
              (which happens in particular if it is parenthesized).
   - Prefix operator:   
      - `fy`: This operator can appear in a sequence of unparenthesized prefix operators that have the same precedence value and also type `fy`.
              In particular, it can form unparenthesized repeats, as in `f f f g(x)`. The rightmost unparenthesized prefixed expression can
              be composed of an operator of the same precedence value. The implied parenthetization is "the rightmost subexpression is innermost".              
              Of course, the rightmost prefixed expression can always be based on a higher-precedence unary or binary operator or be a 
              subexpression of precedence value 0, in particular it can be parenthesized. There is no ambiguity to be resolved there.
      - `fx`: This operator cannot appear in a sequence of unparenthesized prefix operators that have the same precedence.
              In particular, it does not allow forming unparenthesized repeats. The prefixed subexpression **must be** of lower precedence value
              (which happens in particular if it is parenthesized).
   - Infix operator:   
      - `xfx`: This operator cannot appear in an expression with other operators that have the same precedence value, in particular, in 
               expression where there are several `f`. Add parentheses as needed.
      - `xfy`: This operator can appear in an unparenthesized expression with other operators that have the same precedence value and also type
               `xfy` (including `f` itself). The operator (and its neighboring operators) are considered **right-associative**.
               The expression is implicitly parenthesized and the parse tree built accordingly.
      - `yfx`: This operator can appear in an unparenthesized expression with other operators that have the same precedence value and also type
               `yfx` (including `f` itself). The operator (and its neighboring operators) are considered **left-associative**. 
               The expression is parenthesized and the parse tree built accordingly.              

## Example for prefix operators

Define the following, using some interesting characters from the
[Unicode Math pages](https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode):

% Precedence value to the right must be lower or equal (more leaflike, higher precedence) -> no parentheses needed, repeat operators will work 	 
% Precedence value to the right must be strictly lower (strictly more leaflike, higher precedence) -> repeat operators won't work

```
?-
op(500,fy,⊓), op(500,fx,⊔), 
op(400,fx,⊗), 
op(500,fy,∆), op(400,fy,⊞),
```

Unparenthesized repeats are allowed for ⊓, although whitespace is needed for proper tokenization:

```
?- 
X = ⊓ ⊓ ⊓ ⊓g(x), write_canonical(X).

⊓(⊓(⊓(⊓(g(x)))))
X = ⊓ ⊓ ⊓ ⊓g(x).
```

Mixing with a prefix operator having the same precedence value and type is ok:

```
?- X = ⊓ ∆ ∆ ⊓g(x), write_canonical(X).

⊓(∆(∆(⊓(g(x)))))
X = ⊓ ∆ ∆ ⊓g(x).
```

Mixing with a prefix operator having lower precedence value is only ok if you parenthesize or glom onto the precedence 0 element:

```
?- X = ⊓ ∆ ⊞ ⊓g(x), write_canonical(X).
ERROR: Syntax error: Operator priority clash
```

Parenthesize then:

```
?- X = ⊓ ∆ ⊞ (⊓g(x)), write_canonical(X).

⊓(∆(⊞(⊓(g(x)))))
X = ⊓ ∆ ⊞ (⊓g(x)).
```

```
?- X = ⊓ ∆ ⊞ g(x), write_canonical(X).

⊓(∆(⊞(g(x))))
X = ⊓ ∆ ⊞g(x).
```

`+` has precedence value, 500, yfx

```
?- X = ⊓ ⊓ 4 + 5, write_canonical(X).
+(⊓(⊓(4)),5)
X = ⊓ ⊓4+5.

?- X = ⊓ ⊓ (4 + 5), write_canonical(X).
⊓(⊓(+(4,5)))
X = ⊓ ⊓4+5.    % This looks like a bug!

?- X = ⊓ ⊓4+5, write_canonical(X).
+(⊓(⊓(4)),5)
X = ⊓ ⊓4+5.

`*` has precedence value, 400, yfx

?- X = ⊓ ⊓ 4 * 5, write_canonical(X).
⊓(⊓(*(4,5)))
X = ⊓ ⊓4*5.

?- X = ⊓ ⊓ (4 * 5), write_canonical(X).
⊓(⊓(*(4,5)))
X = ⊓ ⊓4*5.

?- X = ⊓ ⊓4*5, write_canonical(X).
⊓(⊓(*(4,5)))
X = ⊓ ⊓4*5.
```

Let's try out `op(500,fx,⊔).`

Unparenthesized repeats are not allowed:

```
?- X = ⊔ ⊔ ⊔ g(x), write_canonical(X).
ERROR: Syntax error: Operator priority clash
```

Parenthesized repeats are of course ok:

```
?- X = ⊔(⊔(⊔ g(x))), write_canonical(X). 
⊔(⊔(⊔(g(x))))
X = ⊔ (⊔ (⊔g(x))).
```

Operator `*` is precedence value 400, so has higher precedence. This means 



