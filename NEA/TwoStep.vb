Public Class TwoStep
    Inherits Tableau
    Private mymenu As IMenu
    'x + 2y
    'x >= 1
    ' x + y <= 2

    Public Sub New(mymenu As IMenu, mydisplay As List(Of Integer)) 'This subprogram creates the simplex tableau
        MyBase.New(mymenu, mydisplay)
    End Sub
    Public Overrides Function Simplex()
        Dim PivotColumn As Integer = -1
        Dim BottomOfPivotColumn As Integer
        Dim PivotRow As Integer = -1
        Dim RatioTest As Integer
        Dim BannedColumns As New List(Of Integer)
        If DisplayMode(0) = 1 Then Display(PivotColumn, PivotRow)
        Do
            'Find the biggest value in the second objective function (if it is zero then end)
            If DisplayMode(0) = 0 Then Display(PivotColumn, PivotRow)
            BannedColumns.Clear()
            Do
                PivotColumn = -1
                BottomOfPivotColumn = 0
                For x = 2 To TableLength
                    If Tableau(x, TableHeight) > BottomOfPivotColumn And BannedColumns.Contains(x) = False Then
                        BottomOfPivotColumn = Tableau(x, TableHeight)
                        PivotColumn = x
                    End If
                Next


                If PivotColumn = -1 Then 'Reducing the tableau to a onestep tableau
                    Dim xReduced As Integer
                    Dim ReducedTableau(TableLength - 1 - artificials, TableHeight - 1) As Double
                    Dim ReducedTopRow As New List(Of String)
                    For y = 0 To TableHeight - 1
                        xReduced = 0
                        For x = 1 To TableLength
                            If TopRow(x) <> "Q" And Mid(TopRow(x), 1, 1) <> "a" Then
                                If y = 0 Then
                                    ReducedTopRow.Add(TopRow(x))
                                End If
                                ReducedTableau(xReduced, y) = Tableau(x, y)
                                xReduced += 1
                            End If
                        Next
                    Next

                    Dim MyOneStepTableau As New OneStep(ReducedTableau, ReducedTopRow, menu, DisplayMode)
                    MyOneStepTableau.Simplex()
                    Return Nothing
                End If

                'Ratio Test
                PivotRow = -1
                RatioTest = Integer.MaxValue
                For y = 1 To TableHeight - 1
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
            If DisplayMode(0) = 1 Then Display(PivotColumn, PivotRow)
            'Making all the other values in the pivot column 0
            For y = 0 To TableHeight
                If y <> PivotRow Then
                    For x = 1 To TableLength
                        If x <> PivotColumn Then
                            Tableau(x, y) = Tableau(x, y) - Tableau(PivotColumn, y) * Tableau(x, PivotRow)
                        End If
                    Next
                    Tableau(PivotColumn, y) = 0
                    If DisplayMode(0) = 1 Then Display(PivotColumn, PivotRow)
                End If
            Next
        Loop
    End Function
End Class
