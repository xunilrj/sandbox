using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.Practices.Unity;
using System.Collections.Generic;
using System.Linq;

namespace UnityLifecycle
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void PerResolveLifetimeManagerDoesNotCallDispose()
        {
            var container = new UnityContainer();
            container.RegisterType<IUnitOfWork, UnitOfWork>(new PerResolveLifetimeManager());

            var uow = (UnitOfWork)container.Resolve<IUnitOfWork>();

            Assert.IsFalse(uow.Disposed);

            container.Dispose();

            Assert.IsFalse(uow.Disposed);
        }

        [TestMethod]
        public void WithChild()
        {
            var container = new UnityContainer();
            container.RegisterType<IUnitOfWork, UnitOfWork>(new HierarchicalLifetimeManager());

            for (int i = 0; i < 10; ++i)
            {
                var child = container.CreateChildContainer();
                var uow = (UnitOfWork)child.Resolve<IUnitOfWork>();

                Assert.IsFalse(uow.Disposed);

                child.Dispose();

                Assert.IsTrue(uow.Disposed);
            }
        }

[TestMethod]
public void DoNotForgetToCallTheDispose()
{
    var container = new UnityContainer();
    container.RegisterType<IUnitOfWork, UnitOfWork>(new HierarchicalLifetimeManager());

    List<UnitOfWork> objects = new List<UnitOfWork>();

    for (int i = 0; i < 10; ++i)
    {
        var child = container.CreateChildContainer();
        var uow = (UnitOfWork)child.Resolve<IUnitOfWork>();
        objects.Add(uow);

        Assert.IsFalse(uow.Disposed);
    }

    Assert.IsTrue(objects.All(x => x.Disposed)); // Will throw!
}


        public interface IUnitOfWork : IDisposable
        {
        }

        public class UnitOfWork : IUnitOfWork
        {
            public bool Disposed { get; set; }

            public void Dispose()
            {
                Disposed = true;
            }
        }
    }
}
