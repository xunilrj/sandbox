using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;
using System.Data.SqlClient;
using System.Data;
using System.IO;
using System.Text;

namespace CubeQbservable
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            //Chevy 1990 red 5 
            var dimensions = new Dictionary<string, object>();
            dimensions.Add("Model", "Chevy");
            dimensions.Add("Year", 1990);
            dimensions.Add("Color", "Red");

            var measures = new Dictionary<string, object>();
            measures.Add("Sales", 5);


            Apply(dimensions, measures, drop: true);

            //Chevy 1990 white 87 
            dimensions = new Dictionary<string, object>();
            dimensions.Add("Model", "Chevy");
            dimensions.Add("Year", 1990);
            dimensions.Add("Color", "White");

            measures = new Dictionary<string, object>();
            measures.Add("Sales", 87);


            Apply(dimensions, measures, drop: false);

            //Chevy 1990 blue 62 
            dimensions = new Dictionary<string, object>();
            dimensions.Add("Model", "Chevy");
            dimensions.Add("Year", 1990);
            dimensions.Add("Color", "Blue");

            measures = new Dictionary<string, object>();
            measures.Add("Sales", 62);


            Apply(dimensions, measures, drop: false);

            //Chevy 1991 red 54 
            dimensions = new Dictionary<string, object>();
            dimensions.Add("Model", "Chevy");
            dimensions.Add("Year", 1991);
            dimensions.Add("Color", "Red");

            measures = new Dictionary<string, object>();
            measures.Add("Sales", 54);


            Apply(dimensions, measures, drop: false);

            //Chevy 1991 white 95 
            dimensions = new Dictionary<string, object>();
            dimensions.Add("Model", "Chevy");
            dimensions.Add("Year", 1991);
            dimensions.Add("Color", "White");

            measures = new Dictionary<string, object>();
            measures.Add("Sales", 95);


            Apply(dimensions, measures, drop: false);

            //Chevy 1991 blue 49 
            dimensions = new Dictionary<string, object>();
            dimensions.Add("Model", "Chevy");
            dimensions.Add("Year", 1991);
            dimensions.Add("Color", "Blue");

            measures = new Dictionary<string, object>();
            measures.Add("Sales", 49);


            Apply(dimensions, measures, drop: false);

            //Chevy 1992 red 31 
            //Chevy 1992 white 54 
            //Chevy 1992 blue 71 
            //Ford 1990 red 64 
            //Ford 1990 white 62 
            //Ford 1990 blue 63 
            //Ford 1991 red 52 
            //Ford 1991 white 9 
            //Ford 1991 blue 55 
            //Ford 1992 red 27 
            //Ford 1992 white 62 
            //Ford 1992 blue 39            
        }

        [TestMethod]
        public void MyTestMethod()
        {
            var xml = @"<NewDataSet>
  <Table>
    <Sales>159</Sales>
    <Model>Chevy</Model>
    <Year>1990</Year>
  </Table>
  <Table>
    <Sales>198</Sales>
    <Model>Chevy</Model>
    <Year>1991</Year>
  </Table>
</NewDataSet>";

            var dataset = new DataSet();
            dataset.ReadXml(new StringReader(xml));

            Pivot(dataset, "Table", new[] { "Year" }, new[] { "Sales" });

            var mem = new MemoryStream();
            dataset.WriteXml(mem);
            var xml2 = Encoding.Default.GetString(mem.ToArray());



            Assert.AreEqual(@"<NewDataSet>
  <Table>    
    <Model>Chevy</Model>    
    <1990>159</1990>
    <1991>198</1991>
  </Table>  
</NewDataSet>", xml2);
        }

        private void Pivot(DataSet dataset, string tableName, string[] toPivot, string[] measures)
        {
            var table = dataset.Tables[tableName];
            var pivot = new Pivot(table);
            var rowsFields = table.Columns.OfType<DataColumn>().Select(x => x.ColumnName).Except(toPivot).Except(measures).ToArray();

            var newTable = pivot.PivotData(measures[0], AggregateFunction.Sum, rowsFields, toPivot);

            dataset.Tables.Remove(table);
            dataset.Tables.Add(newTable);
            dataset.AcceptChanges();
        }

        private void Apply(Dictionary<string, object> dimensions, Dictionary<string, object> measures, bool drop = true)
        {
            var cubeDML = Generate(dimensions, measures, drop);

            var connection = new SqlConnection("Data Source=.;Initial Catalog=Cube;Persist Security Info=True;User ID=riskmanagerapp;Password=12345678a");
            connection.Open();

            foreach (var item in cubeDML)
            {
                try
                {
                    Console.WriteLine("------------------------------------------------");
                    Console.WriteLine(item);
                    Console.WriteLine("------------------------------------------------");

                    using (var command = connection.CreateCommand())
                    {
                        command.CommandText = item;
                        command.CommandType = System.Data.CommandType.Text;
                        command.ExecuteNonQuery();
                    }
                }
                catch
                {
                    Console.WriteLine("ERROR");
                }
            }
        }

        private IEnumerable<string> Generate(Dictionary<string, object> dimensions, Dictionary<string, object> measures, bool drop = true)
        {
            var keys = GetPowerSet(dimensions.Keys.ToList());

            if (drop)
            {
                yield return "DROP TABLE CubeSales";

                var DDL = string.Format("CREATE TABLE CubeSales ({0}, {1})",
                    string.Join(",", dimensions.Keys.Select(x => "[" + x + "] nvarchar(max) NULL")),
                    string.Join(",", measures.Keys.Select(x => "[" + x + "] int NULL"))
                );

                yield return DDL;
            }


            foreach (var item in keys)
            {
                var dml = string.Format(@"MERGE INTO CubeSales as Target
USING (VALUES ({1})) 
AS Source ({0})
ON {2}
WHEN MATCHED THEN
    UPDATE SET Target.Sales = Target.Sales + Source.Sales
WHEN NOT MATCHED THEN
    INSERT ({0}) VALUES ({1});",
                    string.Join(",", item.Select(x => "[" + x + "]").Union(measures.Keys.Select(x => "[" + x + "]"))),
                    string.Join(",", item.Select(x => "'" + dimensions[x] + "'").Union(measures.Keys.Select(x => measures[x].ToString()))),
                    ReturnOnClause(item, dimensions.Keys.Except(item))
                );

                yield return dml;
            }
        }

        private static string ReturnOnClause(IEnumerable<string> item, IEnumerable<string> missing)
        {
            return string.Join(" AND ", item.Select(x => "Source.[" + x + "] = Target.[" + x + "]").Union(missing.Select(x => "[" + x + "] is null")));
        }

        public IEnumerable<IEnumerable<T>> GetPowerSet<T>(List<T> list)
        {
            return from m in Enumerable.Range(0, 1 << list.Count)
                   select
                       from i in Enumerable.Range(0, list.Count)
                       where (m & (1 << i)) != 0
                       select list[i];
        }
    }
}
