Imports NEA

Public Class MinimiseMenu
    Implements IMenu
    Private constraints As String(,)
    Private NoOfvariables As Integer

    Public Sub New(mycontraints As String(,), myvariables As Integer)
        constraints = mycontraints
        NoOfvariables = myvariables
    End Sub

    Public Function GetMode() As Integer Implements IMenu.GetMode
        Return 3
    End Function

    Public Function GetConstraints() As String(,) Implements IMenu.GetConstraints
        Return constraints
    End Function

    Public Function VariableNames() As List(Of String) Implements IMenu.VariableNames
        Dim variables As New List(Of String)
        For i = 1 To NoOfvariables
            If i = 1 And NoOfvariables <= 3 Then
                variables.Add("x")
            ElseIf i = 2 And NoOfvariables <= 3 Then
                variables.Add("y")
            ElseIf i = 3 And NoOfvariables <= 3 Then
                variables.Add("z")
            Else
                variables.Add("x" & i)
            End If
        Next
        Return variables
    End Function
End Class
