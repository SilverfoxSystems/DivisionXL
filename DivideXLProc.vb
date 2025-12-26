    Sub DivideXL(ByRef r1 As Hyper, ByRef d As Hyper, precision%, Optional nIterations% = 5500) ' As Hyper


        Dim r As New Hyper(0, 0)
        hiXp% = d.FindHighExponent()
        z& = d(hiXp)
        Dim rest As New Hyper(d)
        rest.StripZeros()
        rest(hiXp) = 0
        rest.PartSize = rest.BufferSize - 1

        one% = 1

        For ii% = 0 To nIterations

            r1.Divide(z, precision)
            If (ii And one) Then r.Subtract(r1) Else r.Add(r1)

            r1 *= rest
            r1.StripZeros()

        Next

        r.PartSize += hiXp

        r1 = r
        
    End Sub

