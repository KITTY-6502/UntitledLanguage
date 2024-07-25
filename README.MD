# Untitled Programming Language

```
"EXAMPLE 10PRINT SCRIPT"
$6800:XY
!(
  r<$80:R
  $C0+R#:XY#
  XY#+$1:XY
XY#=$6C00!)
```

Commands are composed by a series of operations executed left-to-right, unlike other languages there is no instruction nesting, the currently available instructions are:
* `+` - Add operand and operator
* `-` - Subtract operand from operator
* `*` - Multiply operand by operator
* `/` - Divide operand by operator
* `:` - Write operator to memory location in operand
* `<` - Check if operator is lesser than operand
* `>` - Check if operator is greater than operand
* `=` - Check if operator is equal to operand
* `|` - Logical OR operator and operand
* `&` - Logical AND operator and operand
* `^` - Logical XOR operator and operand
* `(` - Start of conditional block, enter if operator is not zero
* `)` - End of conditional block, jump to start of block if operator is not zero 

Modifiers act on the operand and operators values of instructions:
* `\`` - Convert to ascii hex representation
* `#` - Gets value at address
* `!` - Logical NOT, value of 0 is converted to 1, value of non zero is converted to 0

Value Input:
* `$xxxx` - Hex Value
* `"xxx"` - Ascii String
* `A-Z`   - Preset variable addresses

Commands themselves are terminated through either a newline, semicolon or end of file (EOF).