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
- f32         | 32bit floating point
- string      | string value
- bool        | boolean value (has true or false)
- is          | '=='
- not         | '!='
- or          | '||'
- and         | '&&'
- noreturn    | not back a control
- unit or ()  | no value, return number of zero size value
- as          | cast any type(Currentlly support any int -> any int but not support string -> float)
- as!         | force cast (Currentlly support string -> any int but not support string -> float) if contains literal,return error.
- if          | 
- else        | 

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

if Example
```
[EntryPoint]
func test() -> () {
    a: u8 = 50;
    b: string = "30";
	c: bool = false;
    e: u8 = if_test(a, b as! u8, c);
	__cl_u8_printfn(e);
}

func if_test(a:u8, b:u8, c:bool) -> u8 {
    if (c) {
		return a + b;
	}
	
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

__cl_i8_printf

__cl_i8_printfn

__cl_u8_printfn

__cl_i16_printfn

__cl_f16_printfn

__cl_f32_printfn

__cl_parse_i8

__cl_parse_u8

__cl_parse_i16

__cl_parse_u16

__cl_parse_i32

__cl_parse_u32

__cl_parse_i64

__cl_parse_u64

## For build,Export Function

__set_entry
__add_source
__set_output