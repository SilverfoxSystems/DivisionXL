
    Private Function Reciprocal(ByRef d As Hyper, prec%, nIterations%) As Hyper
        'r(new) = r(2- h*r)

        Dim two As New Hyper(0, 0)
        two(0) = 2
        Dim r As New Hyper(0, 0)
        e = d.FindHighExponent
        r(0) = 1
        r.Divide(d(e), 2) ' PrecOnDivByInt64)
        r.PartSize += e


        For i = 1 To nIterations
            r *= (two - d * r)
            If r.GetBottExp < -prec Then r.Round(prec)
            r.StripZeros()
        Next

        Return r
    End Function

    Private Function NthRoot(ByRef h As Hyper, n%)
        prec% = XLdivPrec
        Dim r As New Hyper(0, 0)
        'Dim rcpN As New Hyper(Reciprocal(h))
        Dim rcpN As New Hyper("1")
        rcpN.Divide(n, PrecOnDivByInt64)
        i% = 0

        nMinus1 = n - 1

        Dim tmp As Hyper
        Dim tmp1 As Hyper

        hiExp0% = h.FindHighExponent
        hiExp% = hiExp0
        If hiExp0 >= 0 Then
            hiExp \= n
        Else 'if hiexp
            hiExp += 1
            hiExp \= n
            hiExp -= 1

        End If

        oddity% = hiExp0 Mod n

        If oddity = 0 Then
            rut# = h(hiExp0) ^ (1 / n)
            r(hiExp) = rut

        Else
            If oddity < 0 Then oddity += n

            tmp = New Hyper(oddity, oddity)
            tmp(oddity) = h(hiExp0)
            rut# = 0
            tmp.ExtractDouble(rut)
            rut ^= (1 / n)
        
            r(hiExp) = rut
        End If


        For i = 1 To nIterationsAtSqr
            tmp = New Hyper(r)
            tmp1 = New Hyper(tmp)

            For i2% = 2 To nMinus1
                tmp *= tmp1
            Next

            r = h.Clone

            DivideXLnwt(r, tmp, XLdivPrec, nIterationsAtDiv)
            tmp1.Multiply(nMinus1)
            tmp1.Add(r)

            r = tmp1 * rcpN
            r.Round(prec)
            r.StripZeros()


        Next

        Return r
        'r(new)= (1/n) * ((n-1)r + h / r^n-1

    End Function


    Private Function NthPower(ByRef h As Hyper, n%)

        i2% = 1
        Dim tmp As New Hyper(h)
        'Dim tmp As New Hyper("1")

        Dim r As New Hyper(0, 0)
        r(0) = 1

        'binary

        'For i% = 0 To 30

        While i2 < n
            If i2 And n Then
                r *= tmp
            End If

            i2 <<= 1
            tmp *= tmp
            tmp.StripZeros()
            If tmp.GetBottExp < -XLdivPrec Then tmp.Round(XLdivPrec)

        End While

        Return r

    End Function




Private Function SqrXL(ByRef x As Hyper) As Hyper

  Dim r As New Hyper(0, 0)
        Dim half As New Hyper(0, -1)
        half(0) = 1
        half(-1) = &H8000000000000000
        Dim tmp As Hyper

        If Not x.IsNotZero Then
            Return r
        End If

        hiExp0% = x.FindHighExponent
hiExp = hiExp0
    
        If hiExp0 >= 0 Then
            hiExp >>= 1
            
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
            tmp = New Hyper((1 / rut).ToString) 
            r(hiExp) = tmp(-1)
        Else
            r(hiExp) = rut
        End If


    skipp:
    
  
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
