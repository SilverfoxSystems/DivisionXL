    Private Function SqrXL(ByRef x As Hyper) As Hyper

  Dim r As New Hyper(0, 0)
        Dim half As New Hyper(0, -1)
        half(0) = 1
        half(-1) = &H8000000000000000
  
        If Not x.IsNotZero Then
            Return r
        End If

        If x > New Hyper("1") Then
            r(0) = 1
        Else
            r = x.Clone
            r.Multiply(2)

        End If
  
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
