using System.Collections.Generic;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Newtonsoft.Json;
using System.IO;
using System;
using InfluxDB.LineProtocol.Client;
using InfluxDB.LineProtocol.Payload;
using System.Net.Http;
using System.Threading;

namespace backend
{
    public class Startup
    {
        // This method gets called by the runtime. Use this method to add services to the container.
        // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
        public void ConfigureServices(IServiceCollection services)
        {
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IHostingEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            app.Run(async (context) =>
            {
                var reader = new StreamReader(context.Request.Body);
                //var line = await reader.ReadLineAsync();

                var http = new HttpClient();
                while (true)
                {
                    var line = $"client_metrics,clientid=1 expenditure={new Random().Next(10)}";
                    Thread.Sleep(1000);
                    Console.Write(line);
                    var r = await http.PostAsync("http://localhost:8086/write?db=fraud&precision=ms", 
                        new StringContent(line));
                    Console.WriteLine(r.StatusCode);
                }
                await context.Response.WriteAsync("Hello World!");
            });
        }
    }
}
