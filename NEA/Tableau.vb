Public MustInherit Class Tableau
    Protected TopRow As New List(Of String) 'Holds the variable names
    Protected Tableau(,) As Double 'Holds the entire tableau
    Protected TableLength As Integer
    Protected TableHeight As Integer
    Protected artificials As Integer 'number of artificial variables
    Protected menu As IMenu
    Private Mode As Integer ' 1 = onestep, 2 = two step, 3 = minimalisation
    Protected DisplayMode As List(Of Integer)

    Public Sub New(mymenu As IMenu, mydisplay As List(Of Integer)) 'This subprogram creates the simplex tableau
        DisplayMode = mydisplay
        menu = mymenu
        Dim inputtableau As String(,) = menu.GetConstraints
        TableHeight = inputtableau.GetLength(1) - 2
        Dim xlimit As Integer = inputtableau.GetLength(0) - 1
        artificials = 0
        Dim SecondObjectiveFunction As Boolean = False
        For y = 0 To TableHeight 'if two step is required
            If inputtableau(xlimit, y) = "G" Then 'greater than
                artificials += 1
                SecondObjectiveFunction = True
            ElseIf inputtableau(xlimit, y) = "E" Then ' equal to
                artificials += 1
                TableHeight += 1
                ReDim Preserve inputtableau(inputtableau.GetLength(0) - 1, inputtableau.GetLength(1))
                For x = 0 To xlimit - 1
                    inputtableau(x, inputtableau.GetLength(1) - 2) = inputtableau(x, y)
                Next
                inputtableau(xlimit, inputtableau.GetLength(1) - 2) = "L"
                SecondObjectiveFunction = True
            End If
        Next

        If SecondObjectiveFunction Then TopRow.Add("Q") 'Generates the variables for the top row
        TopRow.Add("P")
        TopRow.AddRange(menu.VariableNames())
        For i = 1 To TableHeight
            TopRow.Add("s" & i)
        Next
        For i = 1 To artificials
            TopRow.Add("a" & i)
        Next
        TopRow.Add("Value")

        TableLength = TopRow.Count - 1
        If SecondObjectiveFunction Then TableHeight += 1 'to make room for second objective function

        ReDim Tableau(TableLength, TableHeight) 'Reads the inputted table and translates it to the simplex tableau
        Dim ArtifNo As Integer = 1
        Dim SlackNo As Integer = 1
        Dim xstart As Integer
        Dim addedslack As Boolean = True
        Dim addedarti As Boolean = True
        If SecondObjectiveFunction Then
            xstart = 2
        Else
            xstart = 1
        End If
        If SecondObjectiveFunction Then
            Tableau(1, 0) = 1
            Tableau(0, TableHeight) = 1
        Else
            Tableau(0, 0) = 1
        End If

        Dim Negative As Integer = -1 'The objective function row turns the variables negative
        For y = 0 To TableHeight
            If y = TableHeight And SecondObjectiveFunction Then
                Exit For
            End If
            For x = xstart To TopRow.Count - 1
                If x = TopRow.Count - 1 Then 'the 'value' row
                    If y <> 0 Then
                        Tableau(x, y) = inputtableau(xlimit - 1, y)
                        If inputtableau(xlimit, y) <> "L" Then
                            Tableau(x, TableHeight) = Tableau(x, y) + Tableau(x, TableHeight)
                        End If
                    End If
                ElseIf Mid(TopRow(x), 1, 1) <> "s" And Mid(TopRow(x), 1, 1) <> "a" Then 'not a slack variable or an artificial variable
                    If SecondObjectiveFunction Then
                        Tableau(x, y) = inputtableau(x - 2, y) * Negative
                    Else
                        Tableau(x, y) = inputtableau(x - 1, y) * Negative
                    End If
                    If inputtableau(xlimit, y) <> "L" Then
                        Tableau(x, TableHeight) = Tableau(x, y) + Tableau(x, TableHeight)
                    End If
                ElseIf Mid(TopRow(x), 2, 1) = SlackNo And Mid(TopRow(x), 1, 1) = "s" Then 'adding slack
                    If Not addedslack Then
                        addedslack = True
                        If inputtableau(xlimit, y) = "L" Then
                            Tableau(x, y) = 1
                        Else 'if constraint is artificial then it needs a surplus variable instead
                            Tableau(x, y) = -1
                            Tableau(x, TableHeight) = Tableau(x, TableHeight) - 1
                        End If
                        SlackNo += 1
                    End If
                ElseIf Mid(TopRow(x), 2, 1) = ArtifNo And Mid(TopRow(x), 1, 1) = "a" Then 'adding artificial variables
                    If Not addedarti Then
                        addedarti = True
                        If inputtableau(xlimit, y) <> "L" Then
                            Tableau(x, y) = 1
                            ArtifNo += 1
                        End If
                    End If
                End If
            Next
            addedslack = False
            addedarti = False
            Negative = 1
        Next

        'Selecting which table it is
        If SecondObjectiveFunction Then
            Mode = 2
        Else
            Mode = 1
        End If
    End Sub

    Public Sub New(simplextableau As Double(,), MyTopRow As List(Of String), mymenu As IMenu, mydisplay As List(Of Integer)) 'We need this so we can convert a two step tableau into a one step tableau
        DisplayMode = mydisplay
        menu = mymenu
        Tableau = simplextableau
        TableLength = simplextableau.GetLength(0) - 1
        TableHeight = simplextableau.GetLength(1) - 1
        TopRow = MyTopRow
    End Sub

    Public Sub New(mydisplay As List(Of Integer))
        DisplayMode = mydisplay
    End Sub
    Public Function GetMode() As Integer
        Return Mode
    End Function

    Public MustOverride Function Simplex()

    Public Overridable Sub OutputConstraintsFromTableau()
        Dim DeletePlus As Boolean
        Console.WriteLine(vbCrLf)
        Console.WriteLine("Your Constrains are: " & vbCrLf)
        For y = 0 To TableHeight
            DeletePlus = True
            For x = 0 To TableLength - 1
                If Tableau(x, y) > 0 Then
                    If DeletePlus = False Then
                        Console.Write("+")
                    Else
                        DeletePlus = False
                    End If
                    If Tableau(x, y) <> 1 Then
                        Console.Write(Tableau(x, y))
                    End If
                    Console.Write(TopRow(x))
                ElseIf Tableau(x, y) < 0 Then
                    DeletePlus = False
                    If Tableau(x, y) <> -1 Then
                        Console.Write(Tableau(x, y))
                    Else
                        Console.Write("-")
                    End If
                    Console.Write(TopRow(x))
                End If
            Next
            Console.Write("=" & Tableau(TableLength, y) & vbCrLf)
        Next
        Console.ReadLine()
    End Sub

    Public Sub Display(pivotcolumn As Integer, pivotrow As Integer) 'This subprogram displays the tableau
        Console.Clear()
        For x = 0 To TableLength
            For i = 6 To Len(TopRow(x)) Step -1 'spacing
                Console.Write(" ")
            Next
            Console.Write(TopRow(x))
            Console.Write("|")
        Next
        Console.WriteLine()
        For y = 0 To TableHeight
            For x = 0 To TableLength
                If x = pivotcolumn And y = pivotrow And DisplayMode(1) = 1 And DisplayMode(2) = 1 Then
                    Console.ForegroundColor = ConsoleColor.Magenta
                ElseIf x = pivotcolumn And DisplayMode(2) = 1 Then
                    Console.ForegroundColor = ConsoleColor.Green
                ElseIf y = pivotrow And DisplayMode(1) = 1 Then
                    Console.ForegroundColor = ConsoleColor.Red
                End If
                If Len(CStr(Tableau(x, y))) > 7 Then 'more spacing
                    Console.Write(Mid(Tableau(x, y), 1, 7))
                Else
                    For i = 6 To Len(CStr(Tableau(x, y))) Step -1
                        Console.Write(" ")
                    Next
                    Console.Write(Tableau(x, y))
                End If
                Console.ForegroundColor = ConsoleColor.Gray
                Console.Write("|")
            Next
            Console.WriteLine()
        Next
        Console.Write("Press enter to continue: ")
        Console.ReadLine()
    End Sub

End Class