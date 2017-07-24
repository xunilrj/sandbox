using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Data;

public class Pivot
{
    private DataTable _SourceTable = new DataTable();
    private IEnumerable<DataRow> _Source = new List<DataRow>();

    public Pivot(DataTable SourceTable)
    {
        _SourceTable = SourceTable;
        _Source = SourceTable.Rows.Cast<DataRow>();
    }

    public DataTable PivotData(string rowField, string dataField, AggregateFunction aggregate, params string[] columnFields)
    {
        DataTable dt = new DataTable();
        string Separator = ".";
        List<string> rowList = _Source.Select(x => x[rowField].ToString()).Distinct().ToList();
        // Gets the list of columns .(dot) separated.
        var colList = _Source.Select(x => (columnFields.Select(n => x[n]).Aggregate((a, b) => a += Separator + b.ToString())).ToString()).Distinct().OrderBy(m => m);

        dt.Columns.Add(rowField);
        foreach (var colName in colList)
            dt.Columns.Add(colName);  // Cretes the result columns.//

        foreach (string rowName in rowList)
        {
            DataRow row = dt.NewRow();
            row[rowField] = rowName;
            foreach (string colName in colList)
            {
                string strFilter = rowField + " = '" + rowName + "'";
                string[] strColValues = colName.Split(Separator.ToCharArray(), StringSplitOptions.None);
                for (int i = 0; i < columnFields.Length; i++)
                    strFilter += " and " + columnFields[i] + " = '" + strColValues[i] + "'";
                row[colName] = GetData(strFilter, dataField, aggregate);
            }
            dt.Rows.Add(row);
        }
        return dt;
    }

    public DataTable PivotData(string rowField, string dataField, AggregateFunction aggregate, bool showSubTotal, params string[] columnFields)
    {
        DataTable dt = new DataTable();
        string Separator = ".";
        List<string> rowList = _Source.Select(x => x[rowField].ToString()).Distinct().ToList();
        // Gets the list of columns .(dot) separated.
        List<string> colList = _Source.Select(x => columnFields.Aggregate((a, b) => x[a].ToString() + Separator + x[b].ToString())).Distinct().OrderBy(m => m).ToList();

        if (showSubTotal && columnFields.Length > 1)
        {
            string totalField = string.Empty;
            for (int i = 0; i < columnFields.Length - 1; i++)
                totalField += columnFields[i] + "(Total)" + Separator;
            List<string> totalList = _Source.Select(x => totalField + x[columnFields.Last()].ToString()).Distinct().OrderBy(m => m).ToList();
            colList.InsertRange(0, totalList);
        }

        dt.Columns.Add(rowField);
        colList.ForEach(x => dt.Columns.Add(x));

        foreach (string rowName in rowList)
        {
            DataRow row = dt.NewRow();
            row[rowField] = rowName;
            foreach (string colName in colList)
            {
                string filter = rowField + " = '" + rowName + "'";
                string[] colValues = colName.Split(Separator.ToCharArray(), StringSplitOptions.None);
                for (int i = 0; i < columnFields.Length; i++)
                    if (!colValues[i].Contains("(Total)"))
                        filter += " and " + columnFields[i] + " = '" + colValues[i] + "'";
                row[colName] = GetData(filter, dataField, colName.Contains("(Total)") ? AggregateFunction.Sum : aggregate);
            }
            dt.Rows.Add(row);
        }
        return dt;
    }

    public DataTable PivotData(string DataField, AggregateFunction Aggregate, string[] RowFields, string[] ColumnFields)
    {
        DataTable dt = new DataTable();
        string Separator = ".";
        var RowList = _SourceTable.DefaultView.ToTable(true, RowFields).AsEnumerable().ToList();
        for (int index = RowFields.Count() - 1; index >= 0; index--)
            RowList = RowList.OrderBy(x => x.Field<object>(RowFields[index])).ToList();
        // Gets the list of columns .(dot) separated.
        var ColList = (from x in _SourceTable.AsEnumerable()
                       select new
                       {
                           Name = ColumnFields.Select(n => x.Field<object>(n))
                               .Aggregate((a, b) => a += Separator + b.ToString())
                       })
                           .Distinct()
                           .OrderBy(m => m.Name);

        //dt.Columns.Add(RowFields);
        foreach (string s in RowFields)
            dt.Columns.Add(s);

        foreach (var col in ColList)
            dt.Columns.Add(col.Name.ToString());  // Cretes the result columns.//

        foreach (var RowName in RowList)
        {
            DataRow row = dt.NewRow();
            string strFilter = string.Empty;

            foreach (string Field in RowFields)
            {
                row[Field] = RowName[Field];
                strFilter += " and " + Field + " = '" + RowName[Field].ToString() + "'";
            }
            strFilter = strFilter.Substring(5);

            foreach (var col in ColList)
            {
                string filter = strFilter;
                string[] strColValues = col.Name.ToString().Split(Separator.ToCharArray(), StringSplitOptions.None);
                for (int i = 0; i < ColumnFields.Length; i++)
                    filter += " and " + ColumnFields[i] + " = '" + strColValues[i] + "'";
                row[col.Name.ToString()] = GetData(filter, DataField, Aggregate);
            }
            dt.Rows.Add(row);
        }
        return dt;
    }

    /// <summary>
    /// Retrives the data for matching RowField value and ColumnFields values with Aggregate function applied on them.
    /// </summary>
    /// <param name="Filter">DataTable Filter condition as a string</param>
    /// <param name="DataField">The column name which needs to spread out in Data Part of the Pivoted table</param>
    /// <param name="Aggregate">Enumeration to determine which function to apply to aggregate the data</param>
    /// <returns></returns>
    private object GetData(string Filter, string DataField, AggregateFunction Aggregate)
    {
        try
        {
            DataRow[] FilteredRows = _SourceTable.Select(Filter);
            object[] objList = FilteredRows.Select(x => x.Field<object>(DataField)).ToArray();

            switch (Aggregate)
            {
                case AggregateFunction.Average:
                    return GetAverage(objList);
                case AggregateFunction.Count:
                    return objList.Count();
                case AggregateFunction.Exists:
                    return (objList.Count() == 0) ? "False" : "True";
                case AggregateFunction.First:
                    return GetFirst(objList);
                case AggregateFunction.Last:
                    return GetLast(objList);
                case AggregateFunction.Max:
                    return GetMax(objList);
                case AggregateFunction.Min:
                    return GetMin(objList);
                case AggregateFunction.Sum:
                    return GetSum(objList);
                default:
                    return null;
            }
        }
        catch (Exception ex)
        {
            return "#Error";
        }
    }

    private object GetAverage(object[] objList)
    {
        return objList.Count() == 0 ? null : (object)(Convert.ToDecimal(GetSum(objList)) / objList.Count());
    }
    private object GetSum(object[] objList)
    {
        return objList.Count() == 0 ? null : (object)(objList.Aggregate(new decimal(), (x, y) => x += Convert.ToDecimal(y)));
    }
    private object GetFirst(object[] objList)
    {
        return (objList.Count() == 0) ? null : objList.First();
    }
    private object GetLast(object[] objList)
    {
        return (objList.Count() == 0) ? null : objList.Last();
    }
    private object GetMax(object[] objList)
    {
        return (objList.Count() == 0) ? null : objList.Max();
    }
    private object GetMin(object[] objList)
    {
        return (objList.Count() == 0) ? null : objList.Min();
    }
}

public enum AggregateFunction
{
    Count = 1,
    Sum = 2,
    First = 3,
    Last = 4,
    Average = 5,
    Max = 6,
    Min = 7,
    Exists = 8
}
