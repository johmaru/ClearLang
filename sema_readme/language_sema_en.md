# Language Reference

## Values

- u8 | unsigned 8bit integer
- i32 | signed 32bit integer
- i64 | signed 64bit integer

Example

```
[EntryPoint] func test() 
{
    x : u8 = 1; 
    y : int = 2; // int =  i32
    x + y;
}
```

If an untyped integer exists, it is identified as i32.