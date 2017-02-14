using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace StompMessages
{
    public class StompMessage
    {
        public string Command { get; set; }
        public IDictionary<string, string> Headers { get; set; }

        public string Body { get; set; }

        public StompMessage(string command, IEnumerable<Tuple<string, string>> headers, string body)
        {
            Command = command;

            Headers = new Dictionary<string, string>();
            foreach (var item in headers)
            {
                Headers.Add(item.Item1, item.Item2);
            }

            Body = body;
        }
    }
}
