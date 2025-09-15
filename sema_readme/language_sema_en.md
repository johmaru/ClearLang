# Language Reference

## Values

- u8  | unsigned 8bit integer
- i8  | signed 8bit integer
- u32 | unsigned 32bit integer
- i32 | signed 32bit integer
- u64 | unsigned 64bit integer
- i64 | signed 64bit integer
- f16 | 16bit floating point
- noreturn | not back a control
- unit or ()  | no value, return number of zero size value

Example

```
[EntryPoint] func test() -> i64 {    

    x : i64 = -35;     
    y : i64 = 61; // comment can be
    
    return x + y;
}
```

f16 Example

```
[EntryPoint]
func test() -> f16 {
    a: f16 = 50.5;
    b: f16 = 30.0;
    return decrease(a, b);
}

func add(a:f16, b:f16) -> f16 {
    return a + b;
}

func decrease(a:f16, b:f16) -> f16 {
    return a - b;
}
```

Unit Example

``` 
[EntryPoint] 
func test() -> unit {
    x : i64 = -35;
    y : i64 = 61;
    
    return();
}
```
execute result is no value.

If an untyped integer exists, it is identified as i32.