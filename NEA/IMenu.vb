Public Interface IMenu
    Function GetMode() As Integer
    Function GetConstraints() As String(,)
    Function VariableNames() As List(Of String)

End Interface
