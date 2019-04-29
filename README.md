# BinOps

Simple binary operation calculator, supports logN, sin, cos, tan, hexadecimal numbers

```matlab
~/Projects/BinOps% ./dist/build/BinOps/BinOps
Sigma :: + - * / ** mod unm logN sin cos tan 0xFF_FF 2.1 :q
Σ input your favorite math expression here (Thanks to IEEE1394 and Readline)

input :q :quit, press q key or Ctrl - D to leave
```

## Building

```bash
$ cabal build
[DuangSUSE@duangsuse]~/Projects/BinOps% cabal build
Preprocessing executable 'BinOps' for BinOps-0.1.0.0..
Building executable 'BinOps' for BinOps-0.1.0.0..

$ cabal run
[DuangSUSE@duangsuse]~/Projects/BinOps% cabal run
Preprocessing executable 'BinOps' for BinOps-0.1.0.0..
Building executable 'BinOps' for BinOps-0.1.0.0..
Running BinOps...
```

## Examples

```matlab
Σ 1+2+3
 = Just ((1.0 + 2.0) + 3.0)
 = 6.0
Σ 2**2 ** 3
 = Just (2.0 ** (2.0 ** 3.0))
 = 256.0
Σ unm 2 + 43 ** 4 + 2
 = Just ((-2.0 + (43.0 ** 4.0)) + 2.0)
 = 3418801.0
Σ 0xFF
 = Just 255.0
 = 255.0
Σ 0.5 * 10 * 100
 = Just ((0.5 * 10.0) * 100.0)
 = 500.0
Σ log2 10
 = Just log2 10.0
 = 3.0
Σ 10 mod 3
 = Just (10.0 mod 3.0)
 = 1.0
Σ 10 mod 3 mod 1
 = Just (10.0 mod (3.0 mod 1.0))
 = [E] Division by zero
Σ sin 0.3333
 = Just sin 0.3333
 = 0.32716319804950605
Σ ;) Bye
```

## Operators and number syntax

### Binary operator

<sub>Left associative <code>chainl1</code> sorted by precedence, ascending</sub>

+ Add: `+`, Substract: `-`
+ Multiply: `*`, Divide: `/`

<sub>Right associative <code>chainr1</code> sorted by precedence, ascending</sub>

+ Power, `**`
+ Modulo, `mod`

### Unary operator

+ Negate, `unm`
+ logN: `log2` `log10` `log`_N_...
+ `sin` `cos` `tan`
+ Paren: `(1)` `(1 + 2) * 3`

### Numbers

+ Hexadecimal, likely `0x1234` `0xFF` `0xFF_00`
+ Decimal, Rational number, likely `0.1` `0.5` `1_0000.5`

### Whitespaces

+ Newline `'\n'`, Carriage return `'\r'`
+ Space `' '`, Tab `'\t'`

## Document

This console application is self-documented

There is no command line arguments nor environment variables designed to change the behavior of this program

## Bugs

Just a toy calculator, don't create issues whatever you found

BTW. PRs are welcome

BTW. Known: Parser cannot handle unary operators and negative number literatal very well

## License

Unlicense (PublicDomain)
