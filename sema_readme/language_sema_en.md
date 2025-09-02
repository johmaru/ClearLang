# Language Reference

## Values

- u8  | unsigned 8bit integer
- i8  | signed 8bit integer
- u32 | unsigned 32bit integer
- i32 | signed 32bit integer
- u64 | signed 64bit integer
- i64 | signed 64bit integer

Example

```
[EntryPoint] func test() -> i64 {    

    x : i64 = -35;     
    y : i64 = 61; // comment can be
    
    return x + y;
}
```

If an untyped integer exists, it is identified as i32.