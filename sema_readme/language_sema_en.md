# Language Reference

## Values

- u8          | unsigned 8bit integer
- i8          | signed 8bit integer
- u16         | unsigned 16bit integer
- i16         | signed 16bit integer
- u32         | unsigned 32bit integer
- i32         | signed 32bit integer
- u64         | unsigned 64bit integer
- i64         | signed 64bit integer
- f16         | 16bit floating point
- string      | string value
- noreturn    | not back a control
- unit or ()  | no value, return number of zero size value
- as          | cast any type(Currentlly support any int -> any int but not support string -> float)
- as!         | force cast (Currentlly support string -> any int but not support string -> float) if contains literal,return error.

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

string Example
```
```

* string cant be connect other type. only string + string is ok.

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

as! Example

```
[EntryPoint]
func test() -> i16 {
    a: i16 = 50;
    b: string = "30";
    return decrease(a, b as! i16);
}

func add(a:i16, b:i16) -> i16 {
    return a + b;
}

func decrease(a:i16, b:i16) -> i16 {
    return a - b;
}
```

## Exported Function

Printf(string)
```
func test() -> i16 {
    a: i16 = 50;
    b: i16 = 30;
	c: string = "hello world!\n";
	__cl_printf(c);
    return decrease(a, b);
}
```