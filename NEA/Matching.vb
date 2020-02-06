Public Class Matching
    Implements IMenu
    Private Lsize As Integer
    Private Nsize As Integer
    Private UserInputTable As String(,)
    Private ExistingConnections As New List(Of Integer)
    Private NoOfEdges As Integer

    Public Sub New()
        Dim sizes As New List(Of Integer)
        sizes = OneDMenu(New List(Of String)({"Enter the size of group 1", "Enter the size of group 2"}), "Enter the sizes of the two parts of the bipartite graph", "Integer")
        Lsize = sizes(0) - 1
        Nsize = sizes(1) - 1
        For l = 0 To Lsize
            For n = 0 To Nsize
                ExistingConnections.Add(1)
            Next
        Next
        ExistingConnections = OneDMenu(VariableNames(), "Enter 1 if these arcs exist and 0 if they do not", "Boolean") 'This gives us the edges present in the bipartite graph
        ' CREATE USERINPUT TABLE
        Dim Edges As List(Of String) = VariableNames()
        NoOfEdges = Edges.Count - 1
        Dim TableLength As Integer = NoOfEdges + 2
        Dim TableHeight As Integer = NoOfEdges + 4 + Lsize + Nsize
        Dim ValidRow As Boolean
        ReDim UserInputTable(NoOfEdges + 2, NoOfEdges + Lsize + Nsize + 4)
        For y = 0 To TableHeight
            ValidRow = False
            For x = 0 To TableLength

                If x = TableLength Then 'Less than, equal to or greater than
                    UserInputTable(TableLength, y) = "L"
                ElseIf x = TableLength - 1 Then 'Value column
                    If ValidRow Then
                        UserInputTable(TableLength - 1, y) = "1"
                    Else
                        UserInputTable(TableLength - 1, y) = "0"
                    End If


                ElseIf y = 0 Then
                    UserInputTable(x, 0) = "1"
                    ValidRow = True
                ElseIf y <= Lsize + 1 Then 'A B C...
                    If Mid(Edges(x), 1, 1) = Chr(y + 64) Then
                        UserInputTable(x, y) = "1"
                        ValidRow = True
                    Else
                        UserInputTable(x, y) = "0"
                    End If
                ElseIf y <= Lsize + Nsize + 2 Then '1 2 3...
                    If Right(Edges(x), Len(Edges(x)) - 1) = CStr(y - Lsize - 2) Then
                        UserInputTable(x, y) = "1"
                        ValidRow = True
                    Else
                        UserInputTable(x, y) = "0"
                    End If
                Else
                    If x = y - Lsize - Nsize - 3 Then
                        UserInputTable(x, y) = "1"
                        ValidRow = True
                    Else
                        UserInputTable(x, y) = "0"
                    End If
                End If
            Next
        Next
    End Sub

    Public Function GetMode() As Integer Implements IMenu.GetMode
        Return 1
    End Function

    Public Function GetConstraints() As String(,) Implements IMenu.GetConstraints
        Return UserInputTable
    End Function

    Public Function VariableNames() As List(Of String) Implements IMenu.VariableNames
        Dim variables As New List(Of String)
        For l = 0 To Lsize
            For n = 0 To Nsize
                If ExistingConnections(l * Lsize + n) = 1 Then
                    variables.Add(Chr(65 + l) & n)
                End If
            Next
        Next
        Return variables
    End Function
End Class
