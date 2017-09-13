using System;
using System.Collections.Concurrent;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

public class HttpClient
{
    public async Task<string> GetStringAsync(string uri, CancellationToken token)
    {
        var r = new Random();
        await Task.Delay(TimeSpan.FromMilliseconds(r.Next(100, 500)), token);
        return uri;
    }
}

public static class TaskAmb
{
    public static async Task<T> Run<T>(CancellationTokenSource s, TimeSpan timeout, params Task<T>[] tasks)
    {
        var semaphore = new SemaphoreSlim(0, tasks.Length);
        var queue = new ConcurrentQueue<T>();

        tasks = tasks.Select(x =>
        {
            return x.ContinueWith(t =>
            {
                //I dont care for canceled and faulted tasks.
                if (t.IsCanceled) return default(T);
                if (t.IsFaulted) return default(T);

                //I do care for OK tasks
                queue.Enqueue(t.Result);
                semaphore.Release();

                return t.Result;
            });
        }).ToArray();

        var success = false;
        try
        {
            success = await semaphore.WaitAsync(timeout, s.Token);
        }
        catch (OperationCanceledException)
        {
            success = false;
        }

        if (success)
        {
            queue.TryDequeue(out T result);
            return result;
        }
        else
        {
            return default(T);
        }
    }
}

public static class Program
{
    public static async Task<string> TestResult(string uri, CancellationToken token)
    {
        var client = new HttpClient();
        var result = await client.GetStringAsync(uri, token);

        if (result.Length > 0)
        {
            return result;
        }
        else
        {
            throw new Exception();
        }
    }
    public static void Main(string[] args)
    {
        for (int i = 0; i < 10; ++i)
        {
            var source = new CancellationTokenSource();
            var uris = new[] { "a", "b", "c", "d" };

            var tasks = uris.Select(x => TestResult(x, source.Token)).ToArray();
            var faster = TaskAmb.Run(source, TimeSpan.FromSeconds(5), tasks).Result;

            System.Console.WriteLine(faster);
        }

        System.Console.ReadLine();
    }
}