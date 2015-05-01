subroutine smcomo(coef, fmod, tempe, nbhist, ftrc,&
                  trc)
    implicit   none
    integer :: nbhist
    real(kind=8) :: coef(*), fmod(*), tempe
    real(kind=8) :: ftrc((3*nbhist), 3), trc((3*nbhist), 5)
!.......................................................................
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     CONSTRUCTION DES POINTS EXPERIMENTAUX DE MEME TEMPERATURE        .
!
!   CONSTRUCTION DES TPOINT ET DES T POUR T ET POUR TOUTES LES HISTOIRES
!   AVEC COURBE PILOTE DE TRC -FONC(T(TPS))=LOG(TPS(T))- INTERPOLEE
!   POLYNOMIALE D ORDRE 5 ENTRE 199 ET 829  C ET DERIVEE DE FONC(T)
!
!.......................................................................
!
    integer :: i, j, k, lg, nlexp
    real(kind=8) :: temps, coeffz, fp7, f7, ft, fpt, a, b, c, d, e, f, t
    real(kind=8) :: un, zero, cinq
!     ------------------------------------------------------------------
!
    zero = 0.d0
    un = 1.d0
    cinq = 5.d0
    t = 700.d0
!
    do 10 i = 1, nbhist
        a = coef(3+9*(i-1))
        b = coef(4+9*(i-1))
        c = coef(5+9*(i-1))
        d = coef(6+9*(i-1))
        e = coef(7+9*(i-1))
        f = coef(8+9*(i-1))
        if ((a.ne.zero) .and. (b.ne.zero) .and. (c.ne.zero) .and. (d.ne.zero) .and.&
            (e.ne.zero) .and. (f.ne.zero)) then
            f7 = a + b*t + c*t**2 + d*t**3 + e*t**4 + f*t**5
            ft = a+b*tempe+c*tempe**2+d*tempe**3+e*tempe**4+f*tempe** 5
            fpt = b+2*c*tempe+3*d*tempe**2+4*e*tempe**3+5*f*tempe**4
            fp7 = b+2*c*t+3*d*t**2+4*e*t**3+5*f*t**4
            temps = ft - f7 - log(fp7*coef(1+9*(i-1)))
            trc(i,4) = un / ( fpt*exp(temps) )
        else
            trc(i,4) = coef(1+9*(i-1))
        endif
        trc(i,5) = tempe
10  end do
!
! CONSTRUCTION DES Z1(H) Z2(H) Z3(H) ET DZT1(H) DZT2(H) DZT3(H) ASSOCIES
!
    lg = 0
    do 30 i = 1, nbhist
        nlexp = nint( coef(9+9*(i-1)) )
        do 20 j = 1, nlexp-1
            if ((tempe .le. fmod(4*(lg+j))) .and. (tempe .ge. (fmod( 4*(lg+j+1))-1.d-9))) then
                coeffz = ( tempe-fmod( 4*(lg+j))) / (fmod(4*(lg+j+1))- fmod(4*(lg+j)) )
                trc(i,1) = fmod( 4*(lg+j)-3) + (fmod(4*(lg+j+1)-3)- fmod(4*(lg+j)-3) )*coeffz
                trc(i,2) = fmod( 4*(lg+j)-2) + (fmod(4*(lg+j+1)-2)- fmod(4*(lg+j)-2) )*coeffz
                trc(i,3) = fmod( 4*(lg+j)-1) + (fmod(4*(lg+j+1)-1)- fmod(4*(lg+j)-1) )*coeffz
                ftrc(i,1) = (&
                            fmod(&
                            4*(lg+j)-3)-fmod(4*(lg+j+1)-3)) / (fmod(4*(lg+j))-fmod(4*(lg+j+1))&
                            )
                ftrc(i,2) = (&
                            fmod(&
                            4*(lg+j)-2)-fmod(4*(lg+j+1)-2)) / (fmod(4*(lg+j))-fmod(4*(lg+j+1))&
                            )
                ftrc(i,3) = (&
                            fmod(&
                            4*(lg+j)-1)-fmod(4*(lg+j+1)-1)) / (fmod(4*(lg+j))-fmod(4*(lg+j+1))&
                            )
            else
                if (tempe .lt. fmod(4*(nlexp+lg))) then
                    trc(i,1) = fmod(4*(nlexp+lg)-3)
                    trc(i,2) = fmod(4*(nlexp+lg)-2)
                    trc(i,3) = fmod(4*(nlexp+lg)-1)
                    ftrc(i,1) = zero
                    ftrc(i,2) = zero
                    ftrc(i,3) = zero
                endif
            endif
20      continue
        lg = lg + nlexp
30  end do
!
! CONSTRUCTION DES TPOINT POUR T+5 C ET POUR TOUTES LES HISTOIRES
!
    tempe = tempe + cinq
    do 11 i = nbhist+1, (2*nbhist)
        k = i - nbhist
        nlexp = nint( coef(9+9*(k-1)) )
        a = coef(3+9*(k-1))
        b = coef(4+9*(k-1))
        c = coef(5+9*(k-1))
        d = coef(6+9*(k-1))
        e = coef(7+9*(k-1))
        f = coef(8+9*(k-1))
        if ((a.ne.zero) .and. (b.ne.zero) .and. (c.ne.zero) .and. (d.ne.zero) .and.&
            (e.ne.zero) .and. (f.ne.zero)) then
            f7 = a + b*t + c*t**2 + d*t**3 + e*t**4 + f*t**5
            ft = a+b*tempe+c*tempe**2+d*tempe**3+e*tempe**4+f*tempe** 5
            fpt = b+2*c*tempe+3*d*tempe**2+4*e*tempe**3+5*f*tempe**4
            fp7 = b+2*c*t+3*d*t**2+4*e*t**3+5*f*t**4
            temps = ft - f7 - log( fp7*coef(1+9*(k-1)) )
            trc(i,4) = un / (fpt*exp(temps))
            trc(i,5) = tempe
        else
            trc(i,4) = coef(1+9*(k-1))
        endif
        trc(i,5) = tempe
11  end do
    tempe = tempe - cinq
!
! CONSTRUCTION DES Z1(H) Z2(H) Z3(H) ET DZT1(H) DZT2(H) DZT3(H) ASSOCIES
!
    lg = 0
    do 31 i = nbhist+1, (2*nbhist)
        k = i - nbhist
        nlexp = nint( coef(9+9*(k-1)) )
        do 21 j = 1, nlexp - 1
            if ((tempe+cinq .le. fmod(4*(lg+j))) .and.&
                (tempe+cinq .ge. (fmod(4*(lg+j+1))-1.d-9))) then
                coeffz = ( tempe+cinq-fmod( 4*(lg+j))) / (fmod(4*(lg+j+ 1))-fmod(4*(lg+j)) )
                trc(i,1) = fmod( 4*(lg+j)-3) + (fmod(4*(lg+j+1)-3)- fmod(4*(lg+j)-3) )*coeffz
                trc(i,2) = fmod( 4*(lg+j)-2) + (fmod(4*(lg+j+1)-2)- fmod(4*(lg+j)-2) )*coeffz
                trc(i,3) = fmod( 4*(lg+j)-1) + (fmod(4*(lg+j+1)-1)- fmod(4*(lg+j)-1) )*coeffz
                ftrc(i,1) = (&
                            fmod(&
                            4*(lg+j)-3)-fmod(4*(lg+j+1)-3)) / (fmod(4*(lg+j))-fmod(4*(lg+j+1))&
                            )
                ftrc(i,2) = (&
                            fmod(&
                            4*(lg+j)-2)-fmod(4*(lg+j+1)-2)) / (fmod(4*(lg+j))-fmod(4*(lg+j+1))&
                            )
                ftrc(i,3) = (&
                            fmod(&
                            4*(lg+j)-1)-fmod(4*(lg+j+1)-1)) / (fmod(4*(lg+j))-fmod(4*(lg+j+1))&
                            )
            else
                if (tempe+cinq .lt. fmod(4*(nlexp+lg))) then
                    trc(i,1)= fmod(4*(nlexp+lg)-3)
                    trc(i,2)= fmod(4*(nlexp+lg)-2)
                    trc(i,3)= fmod(4*(nlexp+lg)-1)
                    ftrc(i,1)=zero
                    ftrc(i,2)=zero
                    ftrc(i,3)=zero
                endif
            endif
21      continue
        lg = lg + nlexp
31  end do
!
! CONSTRUCTION DES TPOINT POUR T-5 C ET POUR TOUTES LES HISTOIRES
!
    tempe = tempe - cinq
    do 12 i = (2*nbhist)+1, (3*nbhist)
        k = i - 2*nbhist
        a = coef(3+9*(k-1))
        b = coef(4+9*(k-1))
        c = coef(5+9*(k-1))
        d = coef(6+9*(k-1))
        e = coef(7+9*(k-1))
        f = coef(8+9*(k-1))
        if ((a.ne.zero) .and. (b.ne.zero) .and. (c.ne.zero) .and. (d.ne.zero) .and.&
            (e.ne.zero) .and. (f.ne.zero)) then
            f7 = a + b*t + c*t**2 + d*t**3 + e*t**4 + f*t**5
            ft = a+b*tempe+c*tempe**2+d*tempe**3+e*tempe**4+f*tempe** 5
            fpt = b+2*c*tempe+3*d*tempe**2+4*e*tempe**3+5*f*tempe**4
            fp7 = b+2*c*t+3*d*t**2+4*e*t**3+5*f*t**4
            temps = ft - f7 - log( fp7*coef(1+9*(k-1)) )
            trc(i,4) = un/(fpt*exp(temps))
            trc(i,5) = tempe
        else
            trc(i,4) = coef(1+9*(k-1))
        endif
        trc(i,5) = tempe
12  end do
    tempe = tempe + cinq
!
! CONSTRUCTION DES Z1(H) Z2(H) Z3(H) ET DZT1(H) DZT2(H) DZT3(H) ASSOCIES
!
    lg = 0
    do 32 i = (2*nbhist+1), (3*nbhist)
        k = i - 2*nbhist
        nlexp = nint( coef(9+9*(k-1)) )
        do 22 j = 1, nlexp-1
            if ((tempe-cinq .le. fmod(4*(lg+j))) .and.&
                (tempe-cinq .ge. (fmod(4*(lg+j+1))-1.d-9))) then
                coeffz = ( tempe-cinq-fmod( 4*(lg+j))) / ( fmod(4*(lg+j+ 1)) - fmod(4*(lg+j)) )
                trc(i,1) = fmod( 4*(lg+j)-3) + (fmod(4*(lg+j+1)-3)- fmod(4*(lg+j)-3) )*coeffz
                trc(i,2) = fmod( 4*(lg+j)-2) + (fmod(4*(lg+j+1)-2)- fmod(4*(lg+j)-2) )*coeffz
                trc(i,3) = fmod( 4*(lg+j)-1) + (fmod(4*(lg+j+1)-1)- fmod(4*(lg+j)-1) )*coeffz
                ftrc(i,1) = (&
                            fmod(&
                            4*(lg+j)-3)-fmod(4*(lg+j+1)-3)) / ( fmod(4*(lg+j)) - fmod(4*(lg+j+1))&
                            )
                ftrc(i,2) = (&
                            fmod(&
                            4*(lg+j)-2)-fmod(4*(lg+j+1)-2)) / ( fmod(4*(lg+j)) - fmod(4*(lg+j+1))&
                            )
                ftrc(i,3) = (&
                            fmod(&
                            4*(lg+j)-1)-fmod(4*(lg+j+1)-1)) / ( fmod(4*(lg+j)) - fmod(4*(lg+j+1))&
                            )
            else
                if (tempe-cinq .lt. fmod(4*(nlexp+lg))) then
                    trc(i,1) = fmod(4*(nlexp+lg)-3)
                    trc(i,2) = fmod(4*(nlexp+lg)-2)
                    trc(i,3) = fmod(4*(nlexp+lg)-1)
                    ftrc(i,1) = zero
                    ftrc(i,2) = zero
                    ftrc(i,3) = zero
                endif
            endif
22      continue
        lg = lg + nlexp
32  end do
!
end subroutine
