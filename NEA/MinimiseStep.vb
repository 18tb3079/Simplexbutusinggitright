Public Class MinimiseStep
    Inherits Tableau
    Private MyNewMenu As IMenu
    Private MyNewTableau As Tableau
    Public Sub New(mymenu As IMenu, mydisplay As List(Of Integer)) 'This subprogram creates the simplex tableau
        MyBase.New(mydisplay)
        menu = mymenu
        'This class is different to the other two as it requires an intermidiate step before creating the simplex tableau
        Dim inputtableau As String(,) = menu.GetConstraints
        Dim MatrixLength As Integer = inputtableau.GetLength(0) - 2
        Dim MatrixHeight As Integer = inputtableau.GetLength(1) - 2
        Dim Equalto As Integer = 0
        For y = 0 To inputtableau.GetLength(1) - 2
            If inputtableau(MatrixLength + 1, y) = "E" Then 'Each equal to constraint requires two lines
                Equalto += 1
            End If
        Next
        Dim Matrix(MatrixLength, MatrixHeight + Equalto) As Double
        For y = 0 To MatrixHeight
            For x = 0 To MatrixLength
                If y = 0 Then
                    Matrix(x, MatrixHeight + Equalto) = inputtableau(x, y)
                Else
                    If inputtableau(MatrixLength + 1, y) = "L" Then
                        Matrix(x, y - 1) = -1 * inputtableau(x, y)
                    ElseIf inputtableau(MatrixLength + 1, y) = "E" Then
                        Matrix(x, y - 1) = -1 * inputtableau(x, y)
                        Matrix(x, MatrixHeight + Equalto - 1) = inputtableau(x, y)
                        If x = MatrixLength Then Equalto -= 1
                    Else
                        Matrix(x, y - 1) = inputtableau(x, y)
                    End If
                End If
            Next
        Next



        Dim TMatrixLength As Integer = Matrix.GetLength(1) - 1
        Console.WriteLine()
        For i = 0 To menu.VariableNames.Count - 1
            Console.Write(menu.VariableNames(i) & " ")
        Next
        DisplayMatrix(Matrix, MatrixLength, TMatrixLength)

        Dim TMatrix(TMatrixLength, MatrixLength) As Double
        For y = 0 To MatrixLength 'Transpose
            For x = 0 To TMatrixLength
                TMatrix(x, y) = Matrix(y, x)
            Next
        Next


        DisplayMatrix(TMatrix, TMatrixLength, MatrixLength)

        Dim NewInputtableau(TMatrixLength + 1, MatrixLength + 1) As String 'Formatting matrix into a one step array
        For y = 0 To MatrixLength
            For x = 0 To TMatrixLength + 1
                If x = TMatrixLength + 1 Then
                    NewInputtableau(x, y) = "L"
                Else
                    If y = MatrixLength Then
                        NewInputtableau(x, 0) = TMatrix(x, y)
                    Else
                        NewInputtableau(x, y + 1) = TMatrix(x, y)
                    End If
                End If
            Next
        Next
        Console.ReadKey()
        MyNewMenu = New MinimiseMenu(NewInputtableau, TMatrixLength)
        MyNewTableau = New OneStep(MyNewMenu, mydisplay)
    End Sub

    Public Sub DisplayMatrix(matrix As Double(,), length As Integer, height As Integer)
        Console.WriteLine()
        For y = 0 To height
            Console.Write("[ ")
            For x = 0 To length
                Console.Write(matrix(x, y) & " ")
            Next
            Console.WriteLine("]")
        Next

    End Sub

    Public Overrides Sub OutputConstraintsFromTableau()
        Console.WriteLine("lmao havent coded this yet dab yeet")
    End Sub
    Public Overrides Function Simplex()
        Dim values As List(Of Double) = MyNewTableau.Simplex()
        Dim variables As List(Of String) = menu.VariableNames()
        For i = 0 To values.Count - 1
            Console.Write(variables(i) & "=" & values(values.Count - i - 1) & " , ")
        Next
        Return Nothing
    End Function
End Class
