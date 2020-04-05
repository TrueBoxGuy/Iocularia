<p align="center">
    <a href="https://trueboxguy.github.io/Iocularia/">Documentation</a>
</p>

***

# Iocularia
Iocularia is a language, which looks similar to Haskell, but everything is a function. It supports recursion in a non-naive way 
(i.e. cycles between functions can cause recursion). Its syntax is shown below: 
### Left Hand Side

The left hand side of an expression contains, separated by space, the name of the expression and the variables it binds to: 
```hs
main x
```

### Right Hand Side

The right hand side can either be a term (the name of another expression or a bound variable) or the application of two right hand sides. In this application, anything that is not a term must be put in brackets: 

```hs 
f x 
(f x) g 
f (x g)
```
### Expression 

An expression is a combination of a left hand side and a right hand side, and ends with a semicolon: 

```hs 
id x = x;
```


The final program that the transpiler outputs is the expression named `main`.

## Lambda Calculus 

The  outputted program is a form of indexed lambda calculus, which uses the opposite of the De Bruijn index: indexes start at 0 and refer to the lambda at a specific depth (the first lambda in the expression having index 0).
