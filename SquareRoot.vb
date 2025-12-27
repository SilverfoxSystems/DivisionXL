    Private Function SqrXL(ByRef x As Hyper) As Hyper

  Dim r As New Hyper(0, 0)
        Dim half As New Hyper(0, -1)
        half(0) = 1
        half(-1) = &H8000000000000000
  
        If Not x.IsNotZero Then
            Return r
        End If

        hiExp0% = x.FindHighExponent

        If hiExp0 >= 0 Then
            hiExp >>= 1
            'hiExp += 1
        Else 'if hiexp
            hiExp += 1
            hiExp \= 2
            hiExp -= 1

        End If

        If x(hiExp0) = 1 Then
            r(hiExp) = 1

            GoTo skipp

        End If
        rut# = Math.Sqrt(x(hiExp0))

        If (hiExp0 And 1) Then
            tmp = New Hyper((1 / rut).ToString) ' * (2 ^ 63)
            r(hiExp) = tmp(-1)
        Else
            r(hiExp) = rut
        End If


    skipp:
    
        Dim tmp As Hyper

  
        For i = 0 To nIterationsAtSqr

            tmp = New Hyper(x)
            DivideXL(tmp, r, XLdivPrec, nIterationsAtDiv)
  
            tmp.Add(r)
            tmp *= half
  
            r = tmp.Clone
            r.StripZeros()


            '  r(New) = 0.5 * (r + (x / r))


            If r.GetBottExp < -OALprec Then r.Round(OALprec)
        Next

        Return r

    End Function
