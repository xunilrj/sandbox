using MachinaAurum.CSharp;
using System;
using System.Threading.Tasks;
using Xunit;

namespace MachinaAurum.CSharp.Tests
{
    public class MaybeTests 
    {
        [Fact]
        public void MaybeNoneShouldNotBind()
        {
            None().Bind(some: _ => Assert.True(false));
        }

        [Fact]
        public void MaybeNoneShouldBindToNone()
        {
            bool needsToBeTrue = false;
            None().Bind(none: () => needsToBeTrue = true);
            Assert.True(needsToBeTrue);
        }

        [Fact]
        public void MaybeNoneBindShouldReturnMaybeNone()
        {
            bool needsToBeTrue = false;
            var m = None().Bind();
            m.Bind(none: () => { needsToBeTrue = true; });
            Assert.True(needsToBeTrue);
        }

        [Fact]
        public async Task MaybeNoneShouldNotBindAsyncStyleWithTaskVoid()
        {
            await F();
            //Maybe will short-circuit these method and the exception
            //will never be thrown
            async Task F()
            {
                var t1 = await Calc(0);
                throw new InvalidProgramException();
            }
            async Maybe<int> Calc(int x)
            {
                await None();
                throw new InvalidProgramException();
            }
        }

        [Fact]
        public async Task MaybeNoneShouldNotBindAsyncStyleWithTaskInt()
        {
            var r = await F();
            Assert.Equal(default(int), r);
            //Maybe will short-circuit these method and the exception
            //will never be thrown
            async Task<int> F()
            {
                var t1 = await Calc(0);
                throw new InvalidProgramException();
            }
            async Maybe<int> Calc(int x)
            {
                await None();
                throw new InvalidProgramException();
            }
        }

        Maybe<int> None() => Maybe.None;
    }
}
