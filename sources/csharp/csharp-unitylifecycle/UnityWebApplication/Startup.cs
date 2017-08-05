using Microsoft.Owin;
using Owin;

[assembly: OwinStartupAttribute(typeof(UnityWebApplication.Startup))]
namespace UnityWebApplication
{
    public partial class Startup
    {
        public void Configuration(IAppBuilder app)
        {
            ConfigureAuth(app);
        }
    }
}
