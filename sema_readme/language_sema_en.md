# Language Reference

## Values

- u8 | unsigned 8bit integer
- i8 | signed 8bit integer
- u16 | unsigned 16bit integer
- i16 | signed 16bit integer
- u32 | unsigned 32bit integer
- i32 | signed 32bit integer
- u64 | unsigned 64bit integer
- i64 | signed 64bit integer
- f16 | 16bit floating point
- f32 | 32bit floating point
- string | string value
- bool | boolean value (has true or false)
- is | '=='
- not | '!='
- or | '||'
- and | '&&'
- noreturn | not back a control
- unit or () | no value, return number of zero size value
- as | cast any type(Currentlly support any int -> any int but not support string -> float)
- as! | force cast (Currentlly support string -> any int but not support string -> float) if contains literal,return error.
- if |
- else |
- import | package import
- package | assign package
- let | immutable variable
- var | mutable variable
- const | constant value

Example

```
[EntryPoint] func test() -> i64 {

    let x : i64 = -35;
    let y : i64 = 61; // comment can be

    return x + y;
}
```

f16 Example

```
[EntryPoint]
func test() -> f16 {
    let a: f16 = 50.5;
    let b: f16 = 30.0;
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

- string cant be connect other type. only string + string is ok.

Unit Example

```
[EntryPoint]
func test() -> unit {
    let x : i64 = -35;
    let y : i64 = 61;

    return();
}
```

execute result is no value.

If an untyped integer exists, it is identified as i32.

as! Example

```
[EntryPoint]
func test() -> i16 {
    let a: i16 = 50;
    let b: string = "30";
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
    let b: string = "30";
    let c: bool = false;
    let e: u8 = if_test(a, b as! u8, c);
    __cl_u8_printfn(e);
}

func if_test(a:u8, b:u8, c:bool) -> u8 {
    if (c) {
		return a + b;
	}

	return a - b;
}
```

## Build

Package name is must use 'build'.

Entry function is 'configure', curentlly this programming language is not support EntryPoint in the build.clr file.

Return Value must use unit.

An configure function must need that export function **set_entry, **add_source, **set_output, **set_target.

- \_\_set_entry(string function_full_name) | set entry function full name.
- \_\_add_source(string source_path) | add source path.
- \_\_set_output(string output_path) | set output path.
- \_\_set_target(string target_kind) | set target kind. (exe or dll or none)
- \_\_set_app_name(string app_name) | set application name.

Example

```
package build;
func configure() -> unit {
	__set_entry("main::main");
	__add_source("src");
	__set_output("./build");
    __set_target("exe");
}
```

## Exported Function

Printf(string)

```
func test() -> i16 {
    let a: i16 = 50;
    let b: i16 = 30;
    let c: string = "hello world!\n";
    __cl_printf(c);
    return decrease(a, b);
}
```

\_\_cl_i8_printf

\_\_cl_i8_printfn

\_\_cl_u8_printfn

\_\_cl_i16_printfn

\_\_cl_f16_printfn

\_\_cl_f32_printfn

\_\_cl_parse_i8

\_\_cl_parse_u8

\_\_cl_parse_i16

\_\_cl_parse_u16

\_\_cl_parse_i32

\_\_cl_parse_u32

\_\_cl_parse_i64

\_\_cl_parse_u64

## For build,Export Function

**set_entry
**add_source
\_\_set_output
