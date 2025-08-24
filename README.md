# Lis 🦊 Lang

A vibe-coded scripting language.

```lis
let me = {
    name: "lis",
    age: 1,
};

fn greet(person) {
    if person.age < 30 then {
        print "sup " ++ person.name;
    }
    else {
        print "hello " ++ person.name;
    }
}

greet(me);
```

<!-- markdownlint-disable-file MD025 -->

# Syntax

Lis syntax is a mix of JavaScript and Rust.

## Statements

A program in Lis is a series of statements.

### Let statements

Introduce a new variable in the current scope.
Consist of a [pattern](#patterns) and an initializer [expression](#expressions).

```lis
let x = 1;
```

### If statements

Use `if-then-else` syntax.
The condition [expression](#expressions) must evaluate to a boolean and doesn't need parentheses.
The branches can be blocks or single [statements](#statements).

```lis
if x then {
    print "yes";
} else {
    print "no";
}
// or
if x then print "yes" else print "no";
```

### While statements

Use `while-do` syntax.
The condition [expression](#expressions) must evaluate to a boolean and doesn't need parentheses.
The body can be a block or a single [statement](#statements).

```lis
while x do {
    print "yes";
}
// or
while x do print "yes";
```

### Function declarations

Use `fn` keyword.
Each parameter is a [pattern](#patterns).
The body must be a block.

```lis
fn greet(person) {
    print "hello " ++ person.name;
}
```

## Expressions

Expressions evaluate to a value.

### Literal expressions

Evaluate to the value they represent.

```lis
1
"hello"
true
nil
```

### Variable expressions

Evaluate to the value of the variable.

```lis
x
```

### Object expressions

Create an object with the given properties.

```lis
{ x: 1, y: 2 }
```

### Call expressions

Call a function with the given arguments.
The callee can be any [expression](#expressions) and must evaluate to a function.
All arguments are evaluated before the call.

```lis
f(1)
```

### Access expressions

Access a property of an object.
The LHS [expression](#expressions) must evaluate to an object.
The RHS is a simple string identifier.

```lis
o.x
```

### Assign expressions

Assign value to a variable.
The LHS is a simple string identifier.
Variable with the given name must exist in current or any parent scope.

```lis
x = 1
```

### Unary expressions

Unary expressions apply an operator to a value.

```lis
-x
!true
```

### Binary expressions

Binary expressions apply an operator to two values.

```lis
1 + 1
true and true
```

## Patterns

Patterns can be used for a few things but ultimately they bind matched values to variables.

### Variable patterns

Bind the matched value to a variable.

```lis
x
```

### Object patterns

Destructure an object and match its properties with the given sub-[patterns](#patterns).

```lis
{ x: x, y: y }
```

### Default patterns

Provide a default value for the matched value.
The RHS [expression](#expressions) is evaluated only if the matched value is `nil` and its value is used instead to match the LHS sub-[pattern](#patterns).

```lis
x = 1
```
