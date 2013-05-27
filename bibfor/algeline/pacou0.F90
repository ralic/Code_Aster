subroutine pacou0(x, fvec, qt, r, c,&
                  d, fvcold, g, p, s,&
                  t, w, xold, work, check,&
                  vecr1, vecr2, typflu, vecr3, amor,&
                  masg, vecr4, vecr5, veci1, vg,&
                  indic, nbm, nmode, nt)
    implicit none
! -----------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE  CRP_21
!------------------------------------------------------------------
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
    include 'asterfort/pacou1.h'
    include 'asterfort/pacou2.h'
    include 'asterfort/pacou3.h'
    include 'asterfort/pacou4.h'
    include 'asterfort/pacou5.h'
    include 'asterfort/pacou7.h'
    real(kind=8) :: qt(nt, *), r(nt, *), x(*), fvec(*)
    real(kind=8) :: c(*), d(*), fvcold(*), g(*), p(*)
    real(kind=8) :: s(*), t(*), w(*), xold(*), work(*)
    real(kind=8) :: masg(*), amor(*)
    real(kind=8) :: vecr1(*), vecr2(*), vecr3(*), vecr4(*), vecr5(*)
    integer :: veci1(*)
    logical :: restrt, sing, skip, check
    character(len=8) :: typflu
!-----------------------------------------------------------------------
    integer :: i, indic, its, j, k, maxits, n
    integer :: nbm, nmode, nt
    real(kind=8) :: den, eps, f, fold, stpmax, stpmx, sum
    real(kind=8) :: temp, test, tolf, tolmin, tolx, vg
!-----------------------------------------------------------------------
    parameter (eps=1.0d-8,tolx=eps,tolmin=10.d0*eps)
    parameter (tolf=1.d-04)
    parameter (maxits=200,stpmx=100.d0)
!
! FONCTION FMIN
! -------------
!
! -------------------------------------------------------------------
!
! --- TEST SEVERE POUR VOIR SI ON N'EST PAS DEJA SUR UN ZERO.
!
    check = .false.
    n = nt
!
    f = pacou2 (&
        x, fvec, vecr1, vecr2, typflu, vecr3, amor, masg, vecr4, vecr5, veci1, vg, indic, nbm,&
        nmode, nt&
        )
    test = 0.0d0
    do 11 i = 1, n
        if (abs(fvec(i)) .gt. test) test = abs(fvec(i))
11  end do
    if (test .lt. 0.01d0*tolf) goto 9999
!
    sum = 0.0d0
    do 12 i = 1, n
        sum = sum + x(i)**2
12  end do
    stpmax = stpmx*max(sqrt(sum),dble(n))
    restrt = .true.
!
! --- BOUCLE PRINCIPALE.
!
    do 44 its = 1, maxits
        if (restrt) then
!
            call pacou1(x, fvec, r, work, sqrt(eps),&
                        vecr1, vecr2, typflu, vecr3, amor,&
                        masg, vecr4, vecr5, veci1, vg,&
                        indic, nbm, nmode, nt)
            call pacou4(r, n, c, d, sing)
            if (sing) then
                check = .true.
                goto 9999
            endif
            do 14 i = 1, n
                do 13 j = 1, n
                    qt(i,j) = 0.0d0
13              continue
                qt(i,i) = 1.0d0
14          continue
            do 18 k = 1, n - 1
                if (abs(c(k)) .gt. 1.0d-30) then
                    do 17 j = 1, n
                        sum = 0.0d0
                        do 15 i = k, n
                            sum = sum + r(i,k)*qt(i,j)
15                      continue
                        sum = sum/c(k)
                        do 16 i = k, n
                            qt(i,j) = qt(i,j) - sum*r(i,k)
16                      continue
17                  continue
                endif
18          continue
            do 21 i = 1, n
                r(i,i) = d(i)
                do 19 j = 1, i - 1
                    r(i,j) = 0.0d0
19              continue
21          continue
        else
            do 22 i = 1, n
                s(i) = x(i) - xold(i)
22          continue
            do 24 i = 1, n
                sum = 0.0d0
                do 23 j = 1, n
                    sum = sum + r(i,j)*s(j)
23              continue
                t(i) = sum
24          continue
            skip = .true.
            do 26 i = 1, n
                sum = 0.0d0
                do 25 j = 1, n
                    sum = sum + qt(j,i)*t(j)
25              continue
                w(i) = fvec(i) - fvcold(i) - sum
                if (abs(w(i)) .ge. eps* (abs(fvec(i))+abs(fvcold(i)))) then
                    skip = .false.
!
                else
                    w(i) = 0.0d0
                endif
26          continue
            if (.not.skip) then
                do 28 i = 1, n
                    sum = 0.0d0
                    do 27 j = 1, n
                        sum = sum + qt(i,j)*w(j)
27                  continue
                    t(i) = sum
28              continue
                den = 0.0d0
                do 29 i = 1, n
                    den = den + s(i)**2
29              continue
                do 31 i = 1, n
                    s(i) = s(i)/den
31              continue
!
                call pacou5(r, qt, n, t, s)
                do 32 i = 1, n
                    if (abs(r(i,i)) .le. 1.0d-30) then
                        check = .true.
                        goto 9999
                    endif
                    d(i) = r(i,i)
32              continue
            endif
        endif
!
        do 34 i = 1, n
            sum = 0.0d0
            do 33 j = 1, n
                sum = sum + qt(i,j)*fvec(j)
33          continue
            g(i) = sum
34      continue
        do 36 i = n, 1, -1
            sum = 0.0d0
            do 35 j = 1, i
                sum = sum + r(j,i)*g(j)
35          continue
            g(i) = sum
36      continue
        do 37 i = 1, n
            xold(i) = x(i)
            fvcold(i) = fvec(i)
37      continue
        fold = f
        do 39 i = 1, n
            sum = 0.0d0
            do 38 j = 1, n
                sum = sum + qt(i,j)*fvec(j)
38          continue
            p(i) = -sum
39      continue
!
        call pacou7(r, n, d, p)
!
        call pacou3(xold, fold, g, p, x,&
                    f, fvec, stpmax, check, tolx,&
                    vecr1, vecr2, typflu, vecr3, amor,&
                    masg, vecr4, vecr5, veci1, vg,&
                    indic, nbm, nmode, nt)
        test = 0.0d0
        do 41 i = 1, n
            if (abs(fvec(i)) .gt. test) test = abs(fvec(i))
41      continue
        if (test .lt. tolf) then
            check = .false.
            goto 9999
        endif
        if (check) then
            if (restrt) then
                goto 9999
            else
                test = 0.00d0
                den = max(f,.50d0*dble(n))
                do 42 i = 1, n
                    temp = abs(g(i))*max(abs(x(i)),1.0d0)/den
                    if (temp .gt. test) test = temp
42              continue
                if (test .lt. tolmin) then
                    goto 9999
                else
                    restrt = .true.
                endif
            endif
        else
            restrt = .false.
            test = 0.0d0
            do 43 i = 1, n
                temp = (abs(x(i)-xold(i)))/max(abs(x(i)),1.0d0)
                if (temp .gt. test) test = temp
43          continue
            if (test .lt. tolx) goto 9999
        endif
44  end do
    check = .true.
!
9999  continue
end subroutine
