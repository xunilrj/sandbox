Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports <xmlns:ns="https://raw.githubusercontent.com/Komodo/KomodoEdit/master/contrib/catalogs/html5/html5.dtd">

Namespace VBHtml


    Public Class IncrementControl
        Function Count()
            Return <div></div>
        End Function
        Function Render()
            Dim book As XElement =
                <div>
                    <button onclick=<%= "asd" %>/>
                    <%= Count() %>
                    <%= From p In Enumerable.Range(0, 10)
                        Select
                        <phone type=<%= p.ToString() %>><%= p.ToString() %></phone>
                    %>
                    <button onclick=""/>
                </div>
            Return book
        End Function
        ' Insert variable, property, procedure, and event declarations.
    End Class

    <TestClass>
    Public Class VBHtml
        <TestMethod>
        Sub WriteVBHtml()
            Dim html = New IncrementControl
            Console.WriteLine(html.Render().ToString())
        End Sub
    End Class
End Namespace

