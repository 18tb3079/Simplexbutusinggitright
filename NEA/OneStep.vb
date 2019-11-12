Public Class OneStep
    Inherits Tableau
    Public Sub New(mymenu As IMenu, mydisplay As Integer) 'This subprogram creates the simplex tableau
        MyBase.New(mymenu, mydisplay)
    End Sub

    Public Sub New(simplextableau As Double(,), MyTopRow As List(Of String), mymenu As IMenu, mydisplay As Integer)
        MyBase.New(simplextableau, MyTopRow, mymenu, mydisplay)
    End Sub

    Private Sub CompletedTable() 'This subprogram interprets the table to output the values for the variables
        If DisplayMode = 2 Then Display(-2, -2)
        Console.SetCursorPosition(0, Console.CursorTop - 1)
        Console.WriteLine("Tableau is complete! Values for all the variables are shown below: ")
        Dim basicvariabletest As Double
        Dim isbasic As Boolean
        Dim basicrow As Integer
        TopRow.Remove("Value")
        For Each variable In TopRow
            basicvariabletest = 0
            isbasic = True
            basicrow = 0
            For y = 0 To TableHeight
                If Tableau(TopRow.IndexOf(variable), y) <> 0 Then
                    If basicvariabletest = 0 Then
                        basicvariabletest = Tableau(TopRow.IndexOf(variable), y)
                        basicrow = y
                    Else
                        isbasic = False
                        Exit For
                    End If
                End If
            Next
            If isbasic Then
                Console.Write(variable & " = " & Tableau(TableLength, basicrow) / Tableau(TopRow.IndexOf(variable), basicrow) & " , ")
            Else
                Console.Write(variable & " = 0 , ")
            End If
        Next
        Console.SetCursorPosition(Console.CursorLeft - 2, Console.CursorTop)
        Console.Write("  ")
    End Sub

    Private Function MinimiseCompletedTable() 'This subprogram outputs the values but only if it is from a minimisation problem
        Console.SetCursorPosition(0, Console.CursorTop - 1)
        Console.WriteLine("Tableau is complete! Values for all the variables are shown below: ")
        Dim values As New List(Of Double)
        Console.Write("P = " & Tableau(TableLength, 0) & " , ")
        For i = 1 To TableHeight
            values.Add(Tableau(TableLength - i, 0))
        Next
        Return values
    End Function

    Public Overrides Function Simplex() 'This subprogram executes the simplex algorithm
        Dim PivotColumn As Integer = -1
        Dim TopOfPivotColumn As Integer
        Dim PivotRow As Integer = -1
        Dim RatioTest As Integer
        Dim BannedColumns As New List(Of Integer)
        'Check for negative values and pivot column
        Do
            BannedColumns.Clear()
            Display(PivotColumn, PivotRow)
            Do
                PivotColumn = -1
                TopOfPivotColumn = 0
                For x = 1 To TableLength
                    If Tableau(x, 0) < TopOfPivotColumn And BannedColumns.Contains(x) = False Then
                        TopOfPivotColumn = Tableau(x, 0)
                        PivotColumn = x
                    End If
                Next
                If PivotColumn = -1 Then
                    If menu.GetMode() <> 3 Then
                        CompletedTable()
                        Return Nothing
                    Else
                        Return MinimiseCompletedTable()
                    End If
                End If

                'Ratio Test
                PivotRow = -1
                RatioTest = Integer.MaxValue
                For y = 1 To TableHeight
                    Try
                        If Tableau(TableLength, y) / Tableau(PivotColumn, y) < RatioTest And Tableau(TableLength, y) / Tableau(PivotColumn, y) >= 0 Then
                            If Tableau(PivotColumn, y) > 0 Or Tableau(TableLength, y) <> 0 Then
                                PivotRow = y
                                RatioTest = Tableau(TableLength, y) / Tableau(PivotColumn, y)
                            End If
                        End If
                    Catch
                    End Try
                Next

                If PivotRow = -1 Then 'move onto next variable
                    BannedColumns.Add(PivotColumn)
                End If
            Loop Until PivotRow <> -1

            'Making Pivot Value 1
            For x = 1 To TableLength
                If x <> PivotColumn Then
                    Tableau(x, PivotRow) /= Tableau(PivotColumn, PivotRow)
                End If
            Next
            Tableau(PivotColumn, PivotRow) = 1

            'Making all the other values in the pivot column 0
            For y = 0 To TableHeight
                If y <> PivotRow Then
                    For x = 1 To TableLength
                        If x <> PivotColumn Then
                            Tableau(x, y) = Tableau(x, y) - Tableau(PivotColumn, y) * Tableau(x, PivotRow)
                        End If
                    Next
                    Tableau(PivotColumn, y) = 0
                End If
            Next
        Loop
    End Function
End Class
