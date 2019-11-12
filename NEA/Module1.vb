Module Module1
    Function OneDMenu(Questions As List(Of String), Instruction As String, Type As String) As List(Of Integer)
        If Type <> "Blank" And Instruction <> "" Then Questions.Add("Press enter here to continue")
        Dim NoOfQs As Integer = Questions.Count - 1
        Dim Answers As New List(Of Integer)
        For i = 0 To Questions.Count - 1
            Questions(i) &= ": "
            Answers.Add(0)
        Next
        Dim Line As Integer = 0
        Dim key As ConsoleKey


        Console.Clear()
        Console.WriteLine(Instruction)
        For l = 0 To NoOfQs
            Console.Write(Questions(l))
            If (l <> NoOfQs Or Instruction = "") And Type <> "Blank" Then Console.Write(Answers(l))
            Console.Write(vbCrLf)
        Next
        Do
            Console.SetCursorPosition(Len(Questions(Line)), Line + 1)
            If (Line <> NoOfQs Or Instruction = "") And Type <> "Blank" Then Console.Write(Answers(Line))
            Console.ForegroundColor = ConsoleColor.Red
            Console.Write(" <--")
            Console.ForegroundColor = ConsoleColor.Gray
            Console.SetCursorPosition(Len(Questions(Line)), Line + 1)
            key = Console.ReadKey(True).Key
            Select Case key
                Case ConsoleKey.UpArrow
                    If Line <> 0 Then
                        Console.SetCursorPosition(Len(Questions(Line) & Answers(Line)), Line + 1)
                        Console.Write("          ")
                        Line -= 1
                    Else
                        Console.SetCursorPosition(Len(Questions(Line) & Answers(Line)), Line + 1)
                        Console.Write("          ")
                        Line = NoOfQs
                    End If
                Case ConsoleKey.DownArrow
                    If Line <> NoOfQs Then
                        Console.SetCursorPosition(Len(Questions(Line) & Answers(Line)), Line + 1)
                        Console.Write("          ")
                        Line += 1
                    Else
                        Console.SetCursorPosition(Len(Questions(Line) & Answers(Line)), Line + 1)
                        Console.Write("          ")
                        Line = 0
                    End If
                Case ConsoleKey.Enter
                    If Type = "Blank" Then
                        Answers.Clear()
                        Answers.Add(Line)
                        Return Answers
                    End If
                    If Instruction = "" Then
                        Return Answers
                    End If
                    If Line = NoOfQs Then
                        Answers.RemoveAt(NoOfQs)
                        Return Answers
                    Else
                        Console.SetCursorPosition(Len(Questions(Line) & Answers(Line)), Line + 1)
                        Console.Write("          ")
                        Line += 1
                    End If
                Case ConsoleKey.Backspace
                    Answers(Line) = Answers(Line) \ 10
                Case Else
                    If Type = "Integer" Then
                        If Line <> NoOfQs Or Instruction = "" Then
                            key = key - 48 'Converts inputted key into an integer
                            If key >= 0 And key <= 9 Then
                                If Answers(Line) <= 9 Then
                                    Answers(Line) = Answers(Line) * 10 + key
                                End If

                            End If
                        End If
                    ElseIf Type = "Boolean" Then '1 or 0
                        If Line <> NoOfQs Or Instruction = "" Then
                            key = key - 48 'Converts inputted key into an integer
                            If key = 0 Or key = 1 Then
                                Answers(Line) = key
                            End If
                        End If
                    Else 'blank (no text)
                    End If
            End Select
        Loop
        Console.ReadKey()
    End Function

    Sub SideNote(message As String, left As Integer, top As Integer)
        Console.SetCursorPosition(60, 0)
        For Each letter In message
            If letter = "," Then
                Console.SetCursorPosition(60, Console.CursorTop + 1)
            Else
                Console.Write(letter)
            End If
        Next
        Console.SetCursorPosition(left, top)
    End Sub

    Sub Main()
        Console.WriteLine("It is recommended to go full screen (press enter to continue)")
        Console.ReadLine()
        Console.CursorVisible = False
        Dim MyMenu As IMenu
        Dim choice As ConsoleKey = 0
        Dim OptimisationProblem As Integer
        Dim displaymode As Integer = 0
        Do
            Console.Clear()
            'Try
            Do
                If choice <> 2 Then OptimisationProblem = OneDMenu(New List(Of String)({"Custom Constraints", "Maximal Matching", "Maximum Flow", "Shortest Path", "Minimum Spanning Tree", "Settings"}), "Select Mode", "Blank")(0)
                If OptimisationProblem = 0 Then
                        MyMenu = New SimplexMenu()
                    ElseIf OptimisationProblem = 1 Then
                        MyMenu = New Matching()
                    ElseIf OptimisationProblem = 2 Then
                        MyMenu = New Flow()
                    ElseIf OptimisationProblem = 3 Then
                        MyMenu = New ShortestPath()
                    ElseIf OptimisationProblem = 4 Then
                        MyMenu = New Tree()
                    ElseIf OptimisationProblem = 5 Then 'Settings
                        Console.CursorVisible = False
                        displaymode = OneDMenu(New List(Of String)({"Display each iteration", "Display each step", "Display only the completed tableau"}), "Select Display Mode", "Blank")(0)
                    Else
                        MyMenu = New SimplexMenu()
                    End If
                Loop Until OptimisationProblem <> 5

                Dim MyTableau As Tableau
                Do
                    If MyMenu.GetMode = 1 Then
                        MyTableau = New OneStep(MyMenu, displaymode)
                    ElseIf MyMenu.GetMode = 2 Then
                        MyTableau = New TwoStep(MyMenu, displaymode)
                    Else ' mode = 3
                        MyTableau = New MinimiseStep(MyMenu, displaymode)
                    End If
                    MyTableau.OutputConstraintsFromTableau()
                    MyTableau.Simplex()
                    Console.WriteLine(vbCrLf & "Please enter your next action")
                    Console.WriteLine("1. Run the simplex algorithm again with these constraints")
                    Console.WriteLine("2. Choose different constraints")
                    Console.WriteLine("3. Go back to the main menu")
                    Console.WriteLine("4. Exit")
                    While Console.KeyAvailable
                        choice = Console.ReadKey(True).Key
                    End While
                choice = Console.ReadKey(True).Key
            Loop Until choice >= ConsoleKey.D2
            ' Catch ex As Exception
            'Console.Clear()
            'Console.WriteLine(ex.Message)
            'Console.WriteLine("Press enter to restart: ")
            'Console.ReadLine()
            'End Try                                                                                                                                                                                                                                                                 
        Loop Until choice >= ConsoleKey.D4
    End Sub

End Module