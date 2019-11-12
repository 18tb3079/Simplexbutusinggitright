Public Class SimplexMenu
    Implements IMenu
    Private NoOfVariables As Integer
    Private NoOfConstraints As Integer
    Private UserInputTable As String(,)
    Private Mode As Integer
    Public Sub New()
        UserInputTable = Menu2(OneDMenu(New List(Of String)({"Enter the number of variables", "Enter the number of constraints"}), "Enter required values", "Integer"))
        Mode = 1
        For y = 1 To UserInputTable.GetLength(1) - 2 'Selects the mode of the table (one step or two step)
            If UserInputTable(UserInputTable.GetLength(0) - 1, y) = "L" Then
            Else
                Mode = 2
            End If
        Next
        If UserInputTable(UserInputTable.GetLength(0) - 1, 0) = "MIN" Then 'minisation
            Mode = 3
        End If
    End Sub

    Public Function GetConstraints() As String(,) Implements IMenu.GetConstraints
        Return UserInputTable
    End Function

    Public Function GetMode() As Integer Implements IMenu.GetMode
        Return Mode
    End Function

    Public Function VariableNames() As List(Of String) Implements IMenu.VariableNames
        Dim variables As New List(Of String)
        For i = 1 To NoOfVariables
            If i = 1 And NoOfVariables <= 3 Then
                variables.Add("x")
            ElseIf i = 2 And NoOfVariables <= 3 Then
                variables.Add("y")
            ElseIf i = 3 And NoOfVariables <= 3 Then
                variables.Add("z")
            Else
                variables.Add("x" & i)
            End If
        Next
        Return variables
    End Function

    Private Sub Highlight2(text As String, x As Integer, y As Integer, xlimit As Integer, ylimit As Integer) 'This subprogram highlights the selected cell in menu2
        Console.BackgroundColor = ConsoleColor.Red
        For i = 6 To Len(text) Step -1
            Console.Write(" ")
        Next
        Console.Write(text)
        Console.BackgroundColor = ConsoleColor.Black
    End Sub

    Private Sub Unhighlight2(text As String, x As Integer, y As Integer, xlimit As Integer, ylimit As Integer) 'This subprogram unhighlights the previous cell in menu2
        For i = 6 To Len(text) Step -1
            Console.Write(" ")
        Next
        Console.Write(text)
    End Sub

    Private Sub S(Constraints(,) As String, x As Integer, y As Integer, xlimit As Integer, ylimit As Integer) 'This subprogram spaces the variables in Menu2
        For i = 6 To Len(Constraints(x, y)) Step -1
            Console.Write(" ")
        Next

        Console.Write(Constraints(x, y))
        AddPlus(Constraints, x, y, xlimit, ylimit)
    End Sub

    Private Sub AddPlus(Constraints(,) As String, x As Integer, y As Integer, xlimit As Integer, ylimit As Integer) 'This subprogram adds operation signs
        If y = ylimit + 1 Then
        ElseIf x = xlimit - 1 Then
            If y <> 0 Then
                Select Case Constraints(xlimit + 1, y) 'Checks for sign
                    Case "L"
                        Console.Write("<=")
                    Case "E"
                        Console.Write("= ")
                    Case "G"
                        Console.Write(">=")
                End Select
            End If
        ElseIf x = xlimit Then
        Else
            Console.Write(" +")
        End If
    End Sub

    Private Sub Backspace2(ByRef Constraints(,) As String, x As Integer, y As Integer, xlimit As Integer, ylimit As Integer) 'This subprogram handles pressing backspace in menu2
        Console.SetCursorPosition(x * 9 + 11, y)
        If x = xlimit Then
            Constraints(x, y) = Left(Constraints(x, y), Len(Constraints(x, y)) - 1)
        ElseIf xlimit > 9 And x > 8 Then
            Constraints(x, y) = Left(Constraints(x, y), Len(Constraints(x, y)) - 4) & Right(Constraints(x, y), 3)
        ElseIf xlimit > 3 Then
            Constraints(x, y) = Left(Constraints(x, y), Len(Constraints(x, y)) - 3) & Right(Constraints(x, y), 2)
        Else
            Constraints(x, y) = Left(Constraints(x, y), Len(Constraints(x, y)) - 2) & Right(Constraints(x, y), 1)
        End If
        S(Constraints, x, y, xlimit, ylimit)
    End Sub

    Private Function Menu2(menu1 As List(Of Integer)) As String(,) 'Handles the second menu
        Console.Clear()
        SideNote("M: switch between maximise and minimise,G: Greater than,E-Equal to,L: Less than", Console.CursorLeft, Console.CursorTop)

        NoOfVariables = menu1(0)
        NoOfConstraints = menu1(1)
        Dim variables As New List(Of String)
        variables = VariableNames()
        If NoOfVariables < 2 Then
            Throw New ArgumentOutOfRangeException("Number of variables must be at least 2")
        End If
        If NoOfConstraints = 0 Then
            Throw New ArgumentOutOfRangeException("Number of constraints must be at least 1")
        End If
        Dim Constraints(NoOfVariables + 1, NoOfConstraints + 1) As String 'This is the array holding the coefficients of the variables in the constraints

        For i = 0 To NoOfVariables 'Sets all coefficients to 0 and the signs to <=
            For j = 0 To NoOfConstraints
                If i < NoOfVariables Then
                    Constraints(i, j) = "0" & variables(i)
                Else
                    If j <> 0 Then
                        Constraints(i, j) = "0"
                    End If
                End If
            Next
        Next
        Constraints(NoOfVariables + 1, 0) = "MAX"
        For i = 1 To NoOfConstraints
            Constraints(NoOfVariables + 1, i) = "L"
        Next

        Console.Write("Maximise:  ") 'writes the objective function
        For i = 0 To NoOfVariables
            S(Constraints, i, 0, NoOfVariables, NoOfConstraints)
        Next

        Console.Write(vbCrLf & "Subject to:") 'writes the constraints
        For j = 1 To NoOfConstraints
            If j > 1 Then
                Console.Write("           ")
            End If
            For i = 0 To NoOfVariables
                S(Constraints, i, j, NoOfVariables, NoOfConstraints)
            Next
            Console.WriteLine()
        Next
        Console.WriteLine("Press enter here to continue:       ")

        Dim xord As Integer = 0
        Dim yord As Integer = 0
        Dim key As ConsoleKey
        Do
            If yord = NoOfConstraints + 1 Then
                xord = 2
                Console.SetCursorPosition(xord * 9 + 11, yord)
                Highlight2("", xord, yord, NoOfVariables, NoOfConstraints)
            Else
                Console.SetCursorPosition(xord * 9 + 11, yord)
                Highlight2(Constraints(xord, yord), xord, yord, NoOfVariables, NoOfConstraints)
            End If
            key = Console.ReadKey(True).Key
            Select Case key
                Case ConsoleKey.UpArrow
                    Console.SetCursorPosition(xord * 9 + 11, yord)
                    Unhighlight2(Constraints(xord, yord), xord, yord, NoOfVariables, NoOfConstraints)
                    yord -= 1
                    If yord = -1 Then
                        yord = 0
                    ElseIf xord = NoOfVariables And yord = 0 Then
                        xord = xord - 1
                    End If
                Case ConsoleKey.DownArrow
                    Console.SetCursorPosition(xord * 9 + 11, yord)
                    Unhighlight2(Constraints(xord, yord), xord, yord, NoOfVariables, NoOfConstraints)
                    yord += 1
                    If yord = NoOfConstraints + 2 Then
                        yord -= 1
                    End If
                Case ConsoleKey.LeftArrow
                    Console.SetCursorPosition(xord * 9 + 11, yord)
                    Unhighlight2(Constraints(xord, yord), xord, yord, NoOfVariables, NoOfConstraints)
                    xord -= 1
                    If xord = -1 Then
                        xord = 0
                    End If
                Case ConsoleKey.RightArrow
                    Console.SetCursorPosition(xord * 9 + 11, yord)
                    Unhighlight2(Constraints(xord, yord), xord, yord, NoOfVariables, NoOfConstraints)
                    xord += 1
                    If xord = NoOfVariables + 1 Then
                        xord -= 1
                    ElseIf xord = NoOfVariables And yord = 0 Then
                        yord = yord + 1
                    End If
                Case ConsoleKey.Enter
                    If yord = NoOfConstraints + 1 Then 'When the cursor is here it means the user wants to continue
                        If NoOfVariables < 4 Then 'This part removes the variable names from the table so we are only left with the coefficients
                            For x = 0 To NoOfVariables - 1
                                For y = 0 To NoOfConstraints
                                    If Len(Constraints(x, y)) > 1 Then
                                        Constraints(x, y) = Left(Constraints(x, y), Len(Constraints(x, y)) - 1)
                                    End If
                                Next
                            Next
                        Else
                            For x = 0 To NoOfVariables - 1
                                For y = 0 To NoOfConstraints
                                    If Len(Constraints(x, y)) > 1 Then
                                        Constraints(x, y) = Left(Constraints(x, y), Len(Constraints(x, y)) - 2)
                                    End If
                                Next
                            Next
                        End If
                        'ReDim Preserve Constraints(NoOfVariables + 1, NoOfConstraints)
                        Return Constraints
                    Else
                        Console.SetCursorPosition(xord * 9 + 11, yord) 'Moves cursor
                        Unhighlight2(Constraints(xord, yord), xord, yord, NoOfVariables, NoOfConstraints)
                        If xord = NoOfVariables Or (xord = NoOfVariables - 1 And yord = 0) Then
                            xord = 0
                            yord = yord + 1
                        Else
                            xord += 1
                        End If
                    End If
                Case ConsoleKey.Backspace 'Backspace
                    If Left(Constraints(xord, yord), 1) <> "0" Or Left(Constraints(xord, yord), 2) = "0." Then
                        Backspace2(Constraints, xord, yord, NoOfVariables, NoOfConstraints)
                    End If
                    If Not IsNumeric(Left(Constraints(xord, yord), 1)) And Left(Constraints(xord, yord), 1) <> "-" Then
                        Constraints(xord, yord) = "0" & Constraints(xord, yord)
                    End If
                    If Left(Constraints(xord, yord), 1) = "-" And Not IsNumeric(Mid(Constraints(xord, yord), 2, 1)) Then
                        Backspace2(Constraints, xord, yord, NoOfVariables, NoOfConstraints)
                        Constraints(xord, yord) = "0" & Constraints(xord, yord)
                        Constraints(xord, yord) = "-" & Constraints(xord, yord)
                    End If
                Case ConsoleKey.OemPeriod 'Decimal point
                    If InStr(Constraints(xord, yord), ".") = 0 Then
                        Console.SetCursorPosition(xord * 9 + 11, yord)
                        If xord = NoOfVariables Then
                            Constraints(xord, yord) &= "."
                        ElseIf NoOfVariables > 9 And xord > 8 Then
                            Constraints(xord, yord) = Left(Constraints(xord, yord), Len(Constraints(xord, yord)) - 3) & "." & Right(Constraints(xord, yord), 3)
                        ElseIf NoOfVariables > 3 Then
                            Constraints(xord, yord) = Left(Constraints(xord, yord), Len(Constraints(xord, yord)) - 2) & "." & Right(Constraints(xord, yord), 2)
                        Else
                            Constraints(xord, yord) = Left(Constraints(xord, yord), Len(Constraints(xord, yord)) - 1) & "." & Right(Constraints(xord, yord), 1)
                        End If
                        S(Constraints, xord, yord, NoOfVariables, NoOfConstraints)
                    End If
                Case ConsoleKey.OemMinus 'Negative sign
                    If Left(Constraints(xord, yord), 1) = "-" Then
                        Constraints(xord, yord) = Constraints(xord, yord).Remove(0, 1)
                        If Not IsNumeric(Left(Constraints(xord, yord), 1)) Then
                            Constraints(xord, yord) = "0" & Constraints(xord, yord)
                        End If
                    Else
                        If Len(Constraints(xord, yord)) < 6 And xord <> NoOfVariables Then
                            'If Left(Constraints(xord, yord), 1) = "0" And Left(Constraints(xord, yord), 2) <> "0." Then
                            '    Constraints(xord, yord) = Constraints(xord, yord).Remove(0, 1)
                            'End If
                            Constraints(xord, yord) = "-" & Constraints(xord, yord)
                        End If
                    End If
                Case ConsoleKey.L 'Less than or equal to
                    Constraints(NoOfVariables + 1, yord) = "L"
                    Console.SetCursorPosition((NoOfVariables - 1) * 9 + 11, yord)
                    S(Constraints, NoOfVariables - 1, yord, NoOfVariables, NoOfConstraints)
                Case ConsoleKey.E 'Equal to
                    Constraints(NoOfVariables + 1, yord) = "E"
                    Console.SetCursorPosition((NoOfVariables - 1) * 9 + 11, yord)
                    S(Constraints, NoOfVariables - 1, yord, NoOfVariables, NoOfConstraints)
                Case ConsoleKey.G 'Greater than or equal to
                    Constraints(NoOfVariables + 1, yord) = "G"
                    Console.SetCursorPosition((NoOfVariables - 1) * 9 + 11, yord)
                    S(Constraints, NoOfVariables - 1, yord, NoOfVariables, NoOfConstraints)
                Case ConsoleKey.M 'Switches MAX and MIN
                    If Constraints(NoOfVariables + 1, 0) = "MAX" Then
                        Constraints(NoOfVariables + 1, 0) = "MIN"
                        Console.SetCursorPosition(0, 0)
                        Console.Write("Minimise:  ")
                        Console.SetCursorPosition(xord * 9 + 11, yord)
                    Else
                        Constraints(NoOfVariables + 1, 0) = "MAX"
                        Console.SetCursorPosition(0, 0)
                        Console.Write("Maximise:  ")
                        Console.SetCursorPosition(xord * 9 + 11, yord)
                    End If

                Case Else
                    key = key - 48 'Converts key into a number
                    If Len(Constraints(xord, yord)) < 6 Then
                        If key >= 0 And key <= 9 Then
                            Console.SetCursorPosition(xord * 9 + 11, yord)
                            If Left(Constraints(xord, yord), 1) = "0" And Left(Constraints(xord, yord), 2) <> "0." Then
                                Constraints(xord, yord) = Constraints(xord, yord).Remove(0, 1)
                            End If
                            If Left(Constraints(xord, yord), 2) = "-0" And Left(Constraints(xord, yord), 3) <> "-0." Then
                                Constraints(xord, yord) = Constraints(xord, yord).Remove(1, 1)
                            End If
                            If xord = NoOfVariables Then
                                Constraints(xord, yord) &= key
                            ElseIf NoOfVariables > 9 And xord > 8 Then
                                Constraints(xord, yord) = Left(Constraints(xord, yord), Len(Constraints(xord, yord)) - 3) & key & Right(Constraints(xord, yord), 3)
                            ElseIf NoOfVariables > 3 Then
                                Constraints(xord, yord) = Left(Constraints(xord, yord), Len(Constraints(xord, yord)) - 2) & key & Right(Constraints(xord, yord), 2)
                            Else
                                Constraints(xord, yord) = Left(Constraints(xord, yord), Len(Constraints(xord, yord)) - 1) & key & Right(Constraints(xord, yord), 1)
                            End If
                            S(Constraints, xord, yord, NoOfVariables, NoOfConstraints)
                        End If
                    End If
            End Select
        Loop

    End Function
End Class
