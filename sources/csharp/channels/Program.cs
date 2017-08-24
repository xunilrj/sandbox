using System;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace channels
{
    class Program
    {
        static void Main(string[] args)
        {
            var dir = args[0];
            if (args.Length > 0 && args[1] == "relay")
            {
                Console.WriteLine("relay mode...");
                var tcpFile2 = new TcpFile(dir, "B", "A");
                while (true)
                {
                    tcpFile2.Relay();
                }
            }
            else
            {
                Console.WriteLine("active mode...");
                var str = new StringBuilder();
                string message;
                while(true)
                {
                    message = Console.ReadLine();
                    if (message == "END") break;
                    str.AppendLine(message);
                }

                var tcpFile = new TcpFile(dir, "A", "B");
                tcpFile.Send(str.ToString());

                var buffer = tcpFile.Receive();
                message = System.Text.Encoding.UTF8.GetString(buffer);
                Console.WriteLine(message);
            }
        }
    }

    public class TcpFile
    {
        System.IO.DirectoryInfo Directory;
        Int32 SendSequence = 0;
        Int32 ReadSequence = 0;
        string SendPrefix;
        string ReadPrefix;

        public TcpFile(string dir, string sendprefix, string readprefix)
        {
            Directory = new System.IO.DirectoryInfo(dir);
            SendPrefix = sendprefix;
            ReadPrefix = readprefix;
        }

        public void Send(string message)
        {
            var buffer = System.Text.Encoding.UTF8.GetBytes(message);
            Send(buffer);
        }

        public void Send(byte[] buffer)
        {
            var filePath = System.IO.Path.Combine(Directory.FullName, $"{SendPrefix}{SendSequence}");
            using (var f = System.IO.File.Create(filePath))
            {
                //32
                Write16(f, 100); //Source Port
                Write16(f, 80); //Destination Port
                //32
                Write32(f, SendSequence); // Sequence Number
                //32
                Write32(f, 0); // Ack Number
                //32
                Write8(f, 0); //DataOffset + Reserved + NS
                Write8(f, 0); // FLAGS
                Write16(f, buffer.Length); //Window
                //32
                Write16(f, 0); //Checksum
                Write16(f, 0); //Urgent
                //32
                Write32(f, 0); //Options + padding
                //Data
                f.Write(buffer, 0, buffer.Length);
            }
            SendSequence++;
        }

        void Write8(Stream f, int x)
        {
            f.WriteByte((Byte)x);
        }

        void Write16(Stream f, int x)
        {
            var sequenceBytes = BitConverter.GetBytes((Int16)x);
            f.Write(sequenceBytes, 0, 2);
        }
        void Write32(Stream f, int x)
        {
            var sequenceBytes = BitConverter.GetBytes((Int32)x);
            f.Write(sequenceBytes, 0, 4);
        }

        public byte[] Receive()
        {
            byte[] result;
            var filePath = System.IO.Path.Combine(Directory.FullName, $"{ReadPrefix}{ReadSequence}");
            ReadSequence++;

            while (System.IO.File.Exists(filePath) == false)
            {
                Thread.Sleep(1000);
            }

            using (var f = System.IO.File.OpenRead(filePath))
            {
                //32
                var sourcePort = Read16(f); //Source Port
                var destinationPort = Read16(f); //Destination Port
                //32
                var sequence = Read32(f); // Sequence Number
                //32
                var ackNumber = Read32(f); // Ack Number
                //32
                var dataoffset = Read8(f); //DataOffset + RESERVED + NS
                var flags = Read8(f);
                var length = Read16(f); //Window
                //32
                var checksum = Read16(f); //Checksum
                var urgente = Read16(f); //Urgent
                //32
                var options = Read32(f); //Options + padding
                //Data
                var buffer = new byte[length];
                var read = f.Read(buffer, 0, buffer.Length);

                using (var mem = new MemoryStream())
                {
                    mem.Write(buffer, 0, read);
                    mem.Position = 0;
                    result = mem.ToArray();
                }
            }

            System.IO.File.Delete(filePath);

            return result;
        }

        public void Relay()
        {
            byte[] buffer;
            var filePath = System.IO.Path.Combine(Directory.FullName, $"{ReadPrefix}{ReadSequence}");
            ReadSequence++;

            while (System.IO.File.Exists(filePath) == false)
            {
                Thread.Sleep(1000);
            }

            using (var f = System.IO.File.OpenRead(filePath))
            {
                //32
                var sourcePort = Read16(f); //Source Port
                var destinationPort = Read16(f); //Destination Port
                //32
                var sequence = Read32(f); // Sequence Number
                //32
                var ackNumber = Read32(f); // Ack Number
                //32
                var offset = Read8(f); //DataOffset + reserved + ns
                var flags = Read8(f);
                var length = Read16(f);
                //32
                var checksum = Read16(f); //Checksum
                var urgente = Read16(f); //Urgent
                //32
                var options = Read32(f); //Options + padding
                //Data
                buffer = new byte[length];
                f.Read(buffer, 0, buffer.Length);
            }

            System.IO.File.Delete(filePath);

            using (var tcpClient2 = new TcpClient())
            {
                tcpClient2.ConnectAsync("localhost", 80).Wait();
                using (var stream2 = tcpClient2.GetStream())
                {
                    stream2.Write(buffer, 0, buffer.Length);
                    var readingBuffer = new byte[1024 * 100];
                    var read = stream2.Read(readingBuffer, 0, readingBuffer.Length);
                    using (var mem = new MemoryStream())
                    {
                        mem.Write(readingBuffer, 0, read);
                        mem.Position = 0;
                        Send(mem.ToArray());
                    }
                }
            }
        }

        Byte Read8(Stream f)
        {
            return (byte)f.ReadByte();
        }

        Int16 Read16(Stream f)
        {
            var buffer = new byte[2];
            f.Read(buffer, 0, buffer.Length);
            return BitConverter.ToInt16(buffer, 0);
        }

        Int32 Read32(Stream f)
        {
            var buffer = new byte[4];
            f.Read(buffer, 0, buffer.Length);
            return BitConverter.ToInt32(buffer, 0);
        }
    }
}


