Imports NEA

Public Class Tree
    Implements IMenu
    Private NoOfNodes As Integer
    Private UserInputTable As String(,)
    Private ExistingConnections As New List(Of Integer)
    Private Edges As List(Of String)
    Private NoOfEdges As Integer

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
        Edges = VariableNames()
        NoOfEdges = Edges.Count - 1
        Dim cycles As New List(Of String)
        R(VariableNames(), cycles, "") 'FINDS ALL THE CYCLES
        For Each cycle In cycles
            Console.WriteLine(cycle)
        Next
        Console.ReadLine()

        Dim tableLength As Integer = NoOfEdges + 2
        Dim tableHeight As Integer = cycles.Count + NoOfEdges + 2
        ReDim UserInputTable(tableLength, tableHeight)

        For y = 0 To tableHeight
            For x = 0 To tableLength
                If x = tableLength Then
                    If y <> 1 Then
                        UserInputTable(x, y) = "L"
                    Else
                        UserInputTable(x, y) = "E"
                    End If
                ElseIf y = 0 Then
                    If x <> tableLength - 1 Then
                        Do
                            NextEdgeCount += 1
                        Loop Until ExistingConnections(NextEdgeCount) <> 0
                        UserInputTable(x, y) = ExistingConnections(NextEdgeCount)
                    End If
                ElseIf y = 1 Then
                    If x <> tableLength - 1 Then
                        UserInputTable(x, y) = 1
                    Else
                        UserInputTable(x, y) = NoOfNodes
                    End If
                ElseIf y < cycles.Count + 2 Then
                    If x <> tableLength - 1 Then

                        If cycles(y - 2).Contains(Edges(x)) Then
                            UserInputTable(x, y) = 1
                        ElseIf StrReverse(cycles(y - 2)).Contains(Edges(x)) Then
                            UserInputTable(x, y) = 1
                        Else
                            UserInputTable(x, y) = 0
                        End If
                    Else
                        UserInputTable(x, y) = Len(cycles(y - 2)) - 2
                    End If
                Else
                    If x = y - 5 Or x = tableLength - 1 Then
                        UserInputTable(x, y) = 1
                    Else
                        UserInputTable(x, y) = 0
                    End If
                End If
            Next
        Next
        ReDim Preserve UserInputTable(tableLength, tableHeight + 1) 'This is to match the formatting of the tableau class
    End Sub

    Sub R(ByRef subgraph As List(Of String), ByRef cycles As List(Of String), currentpath As String) 'recursive subroutine which finds all the cycles
        If currentpath = "" Then
            currentpath = subgraph(0)
        End If
        Dim check As Boolean
        For Each edge In subgraph
            If Left(edge, 1) = Right(currentpath, 1) Then
                check = True
                For Each letter In currentpath
                    If Right(edge, 1) = letter Then
                        If letter = Left(currentpath, 1) And Len(currentpath) > 2 Then
                            cycles.Add(currentpath & letter)
                        End If
                        check = False
                    End If
                Next
                If check = True Then
                    R(subgraph, cycles, currentpath & Right(edge, 1))
                End If
            ElseIf Right(edge, 1) = Right(currentpath, 1) Then
                check = True
                For Each letter In currentpath
                    If Left(edge, 1) = letter Then
                        If letter = Left(currentpath, 1) And Len(currentpath) > 2 Then
                            cycles.Add(currentpath & letter)
                        End If
                        check = False
                    End If
                Next
                If check = True Then
                    R(subgraph, cycles, currentpath & Left(edge, 1))
                End If
            End If
        Next

        If currentpath = subgraph(0) And subgraph.Count > 1 Then
            subgraph.Remove(currentpath)
            R(subgraph, cycles, "")
        End If
    End Sub


    Public Function GetMode() As Integer Implements IMenu.GetMode
        Return 3
    End Function

    Public Function GetConstraints() As String(,) Implements IMenu.GetConstraints
        Return UserInputTable
    End Function

    Public Function VariableNames() As List(Of String) Implements IMenu.VariableNames
        Dim variables As New List(Of String)
        Dim count As Integer = 0
        For i = 0 To NoOfNodes
            For j = 0 To NoOfNodes
                If i < j Then
                    If ExistingConnections(count) <> 0 Then
                        variables.Add(Chr(65 + i) & Chr(65 + j))
                    End If
                    count += 1
                End If
            Next
        Next
        Return variables
    End Function
End Class