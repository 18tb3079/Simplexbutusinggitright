Public Class Flow
    Implements IMenu
    Private NoOfNodes As Integer
    Private UserInputTable As String(,)
    Private ExistingConnections As New List(Of Integer)

    Public Sub New()
        Dim NextEdgeCount As Integer = -1
        NoOfNodes = OneDMenu(New List(Of String)({"Enter the amount of nodes in the graph"}), "", "Integer")(0) - 1
        If NoOfNodes <= 1 Or NoOfNodes >= 19 Then
            Throw New ArgumentException("Number of nodes is out of range (2-19)")
        End If
        For i = 0 To NoOfNodes * (NoOfNodes - 1) + 1
            ExistingConnections.Add(1)
        Next
        ExistingConnections = OneDMenu(VariableNames, "Enter the weights of these edges (Enter 0 if they do not exist)", "Integer") 'Gets the arc weights of the graph
        Dim Edges As List(Of String) = VariableNames()
        Dim NoOfEdges As Integer = Edges.Count - 1

        Dim TableLength As Integer = NoOfEdges + 2
        Dim TableHeight As Integer = NoOfNodes + NoOfEdges
        ReDim UserInputTable(TableLength, TableHeight)
        For y = 0 To TableHeight
            For x = 0 To TableLength
                If x = TableLength Then
                    If y = 0 Then
                        UserInputTable(x, y) = "L"
                    ElseIf y <= NoOfNodes - 1 Then 'Less than, equal to or greater than
                        UserInputTable(x, y) = "E"
                    Else
                        UserInputTable(x, y) = "L"
                    End If
                ElseIf x = TableLength - 1 Then 'Value column
                    If y <= NoOfNodes - 1 Then
                        UserInputTable(x, y) = 0
                    Else ' Capacties
                        Do
                            NextEdgeCount += 1
                        Loop Until ExistingConnections(NextEdgeCount) <> 0
                        UserInputTable(x, y) = ExistingConnections(NextEdgeCount)
                    End If
                ElseIf y = 0 Then 'objective function
                    If Left(Edges(x), 1) = "S" Then
                        UserInputTable(x, y) = 1
                    Else
                        UserInputTable(x, y) = 0
                    End If
                ElseIf y < NoOfNodes Then 'Flow in = Flow out for each vertex
                    If Left(Edges(x), 1) = Chr(y + 64) Then
                        UserInputTable(x, y) = 1
                    ElseIf Right(Edges(x), 1) = Chr(y + 64) Then
                        UserInputTable(x, y) = -1
                    Else
                        UserInputTable(x, y) = 0
                    End If
                Else 'capacities
                    If x = y - NoOfNodes Then
                        UserInputTable(x, y) = 1
                    Else
                        UserInputTable(x, y) = 0
                    End If
                End If
            Next
        Next
        ReDim Preserve UserInputTable(TableLength, TableHeight + 1) 'This is to match the formatting of the tableau class
    End Sub

    Public Function GetMode() As Integer Implements IMenu.GetMode
        Return 2
    End Function

    Public Function GetConstraints() As String(,) Implements IMenu.GetConstraints
        Return UserInputTable
    End Function

    Private Function NtoL(number As Integer) As String 'Converts a number to a letter
        If number = 0 Then
            Return "S"
        ElseIf number = NoOfNodes Then
            Return "T"
        Else
            Return Chr(64 + number)
        End If
    End Function

    Public Function VariableNames() As List(Of String) Implements IMenu.VariableNames
        Dim variables As New List(Of String)
        Dim count As Integer = 0
        For i = 0 To NoOfNodes - 1
            For j = 1 To NoOfNodes
                If i <> j Then
                    If ExistingConnections(count) <> 0 Then
                        variables.Add(NtoL(i) & NtoL(j))
                    End If
                    count += 1
                End If
            Next
        Next
        Return variables
    End Function
End Class
