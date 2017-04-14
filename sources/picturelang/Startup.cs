using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.WebSockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json;

namespace picturelang
{
    public class Startup
    {
        public void ConfigureServices(IServiceCollection services)
        {
        }

        public void Configure(IApplicationBuilder app, IHostingEnvironment env, ILoggerFactory loggerFactory)
        {
            loggerFactory.AddConsole();

            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            app.UseStaticFiles();
            app.UseWebSockets();
            app.Use(async (ctx,next) =>
            {
                if (ctx.WebSockets.IsWebSocketRequest)
                {
                    var webSocket = await ctx.WebSockets.AcceptWebSocketAsync();
                    for (int i = 0; i < 10; ++i)
                    {
                        await Task.Delay(1000);

                        var msg = JsonConvert.SerializeObject(new{
                            Path = "Server",
                            Value = DateTime.Now
                        });
                        var buffer = Encoding.UTF8.GetBytes(msg);
                        var segment = new ArraySegment<byte>(buffer);
                        await webSocket.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None);
                    }

                    await webSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "bye", CancellationToken.None);
                }
                else
                {
                    await next();
                }
            });

            app.Run(async (context) =>
            {
                var obj = JsonConvert.SerializeObject(new {
                    loading = false,
                    pictureUrl = "http://localhost:5000/pictures/monet_0001.jpg",
                    description = ""
                });
                await context.Response.WriteAsync(obj);
            });
        }
    }
}
