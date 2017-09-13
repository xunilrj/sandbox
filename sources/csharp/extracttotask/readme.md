# Task Patterns

## Extract if-awaited-result-null to Non-Null-Task pattern

In a lot of situations a method that returns Task<T> returns a null instance. It is very common to test the nullity of the instance after awaiting its result.

The best solution is to transform the original Task<T> in a another Task<T> that guaranteed never returns null.

### Original Code

    public async Task<SomeResult> SomeMethod(SomeInput1 i1, SomeInput2 i2)
    {
        var i3 = await i1.SomeMethod1();
        if(i3 == null)
            return SomeResult.SomeDefaultValue;
        i3.SomeMethod2();        
        ...
        return value;
    }

### Final code 

    public async Task<SomeResult> SomeMethod(SomeInput1 i1, SomeInput2 i2)
    {
        var i3 = await i1.SomeMethod1().OnNull(() => SomeResult.SomeDefaultValue());
        // i3 here is guaranteed to never be null
        i3.SomeMethod2();        
        ...
        return value;
    }

    public static class TaskExtensions
    {
        public Task<T> OnNull<T>(this Task<F> task, Func<T> f)
        {
            return Task.Factory.StartNew(async () =>
                var r = default(T);
                await task.ContinueWith(t =>
                {
                    if((t.IsFaulted == false)&& t.Result == null)
                    {
                        r = f();
                    }
                    else
                    {
                        r = t.Result;
                    }
                }
                return r;
            });
        }
    }

## Extract if-awaited-result-enum to Expected-Enum-Set-Task pattern

In a lot of situations a method that returns Task<T> constains a Enum that somehow representes the success or failure of the call. It is very common to test (with a if or switch) this property after awaiting its result.

The best solution is to transform the original Task<T> in a another Task<T> that guaranteed never only returning on success values.

### Original Code

    public async Task<SomeResult> SomeMethod(SomeInput1 i1, SomeInput2 i2)
    {
        var i3 = await i1.SomeMethod1();
        var validValues = new [] {Enum1.Value1, Enum1.Value2};
        var invalidValues = new [] {Enum1.Value3, Enum1.Value4};
        if(invalidValues.Contains(i3.Status))
            return SomeResult.SomeDefaultValue;
        i3.SomeMethod2();        
        ...
        return value;
    }

### Final code 

    public async Task<SomeResult> SomeMethod(SomeInput1 i1, SomeInput2 i2)
    {
        var invalidValues = new [] {Enum1.Value3, Enum1.Value4};
        var i3 = await i1.SomeMethod1().FailOn(x => x.Status, invalidValues);
        //guaranteed to be in a valid state
        i3.SomeMethod2();        
        ...
        return value;
    }