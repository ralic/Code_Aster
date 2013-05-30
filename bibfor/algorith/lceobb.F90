subroutine lceobb(intmax, toler, epsm, deps, bm,&
                  dm, lambda, mu, alpha, ecrob,&
                  ecrod, rk, rk1, rk2, b,&
                  d, mult, elas, dbloq, iret)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/diago3.h'
    include 'asterfort/lceob1.h'
    include 'asterfort/lceob2.h'
    include 'asterfort/lceob3.h'
    include 'asterfort/r8inir.h'
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: bm(6), dm, b(6), d, mult
    real(kind=8) :: lambda, mu, alpha, rk, rk1, rk2, ecrob, ecrod
    real(kind=8) :: toler
    integer :: intmax, iret
    logical :: elas, dbloq
!
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
!     ROUTINE DE DECOUPAGE DE L'INCREMENT DE CHARGE LORSQUE
!     L ENDOMMAGEMENT APPROCHE DE 1
!
!  IN INTMAX  : NBRE D'ITERATION MAX POUR LE NEWTON LOCAL
!  IN TOLER   : RESIDU TOLERE POUR LE NEWTON LOCAL
!  IN  NDIM    : DIMENSION DE L'ESPACE
!  IN  TYPMOD  : TYPE DE MODELISATION
!  IN  IMATE   : NATURE DU MATERIAU
!  IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
!  IN  EPSM    : DEFORMATION EN T- REPERE GLOBAL
!  IN  DEPS    : INCREMENT DE DEFORMATION
!  IN  BM DM     : VARIABLES INTERNES EN T-
!  IN LAMBDA     : /
!  IN MU        : / COEFFICIENTS DE LAME
!  IN  ALPHA    : /
!  IN  ECROB    : /
!  IN  ECROD    : /
!  IN  RK    : /
!  IN  RK1    : /
!  IN  RK2    : / PARAMETRES DU MODELE
!
! OUT  B D     : VARIABLES INTERNES EN T+
! OUT MULT     : MULTIPLICATEUR PLASTIQUE DU PRINCIPE DE NORMALITE
! OUT ELAS     : ELASTIQUE OU DISSIPATION?
! OUT DBLOQ  : BLOQUAGE DE L'ENDOMMAGEMENT DE COMPRESSION
! OUT IRET     : CODE RETOUR
! ----------------------------------------------------------------------
!
    logical :: reinit, tot1, tot2, tot3
    integer :: i, j, k, l
    integer :: bdim, compte, t(3, 3)
!
    real(kind=8) :: tolb, un, deux
    real(kind=8) :: valbm(3), vecbm(3, 3), valbr(3), vecbr(3, 3)
    real(kind=8) :: valb(3), vecb(3, 3), seuil
    real(kind=8) :: bmr(6), br(6), epsr(6)
    real(kind=8) :: interm(3, 3), binter(6), epi(6)
    real(kind=8) :: epsi(6), epst(6), epsf(6), trepsm
!
    un=1.d0
    deux=2.d0
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
    tolb=1.d-2
    compte=0
!-------------------------------------------------
! -- DEFORMATIONS
!-------------------------------------------------
    call r8inir(6, 0.d0, epsi, 1)
    call r8inir(6, 0.d0, epsf, 1)
    call r8inir(6, 0.d0, epst, 1)
!
    do 1 k = 1, 6
        epsf(k) = epsm(k)+deps(k)
        epsi(k) = epsm(k)
        epst(k) = (epsf(k)+epsi(k))/deux
 1  end do
!
    reinit=.false.
!
999  continue
    if ((&
        (&
        (epsi(1).ne.epsf(1)) .or. (epsi(2).ne.epsf(2)) .or. (epsi(3).ne.epsf(3)) .or.&
        (epsi(4).ne.epsf(4)) .or. (epsi(5).ne.epsf(5)) .or. (epsi(6).ne.epsf(6))&
        )&
        .or. reinit&
        )&
        .and. (compte.le.100)) then
!
        reinit=.false.
        compte=compte+1
!
        if (compte .eq. 100) then
            iret=0
            goto 9999
        endif
!
        call diago3(bm, vecbm, valbm)
        bdim=3
        do 201 i = 1, 3
            if (valbm(i)-tolb .le. 0.d0) then
                bdim=bdim-1
            endif
201      continue
!
        trepsm=epst(1)+epst(2)+epst(3)
        if (trepsm .gt. 0.d0) then
            trepsm=0.d0
        endif
!
        seuil=rk-rk1*trepsm*(atan2(-trepsm/rk2,un))
!
!----CAS OU LES 3 VALEURS PROPRES SONT NON NULLES---------------------
        if (bdim .eq. 3) then
!
            call lceob3(intmax, toler, epst, bm, dm,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, bdim, b, d, mult,&
                        elas, dbloq, iret)
!
            call diago3(b, vecb, valb)
            reinit=.false.
            if (compte .lt. 100) then
                do 101 i = 1, 3
                    if ((valb(i).lt.0) .or. (d.gt.1.d0)) then
                        reinit=.true.
                    else
                        if (valb(i)-tolb .le. 0.d0) then
                            valb(i)=tolb-r8prem()
                        endif
                        if (un-d-tolb .le. 0.d0) then
                            d=un-tolb+r8prem()
                            dbloq=.true.
                        endif
                    endif
101              continue
!
                if (reinit) then
                    do 800 i = 1, 6
                        epst(i)=(epst(i)+epsi(i))/deux
800                  continue
                    goto 999
                else
                    call r8inir(6, 0.d0, b, 1)
                    call r8inir(6, 0.d0, bm, 1)
                    do 212 i = 1, 3
                        do 213 j = i, 3
                            do 214 k = 1, 3
                                b(t(i,j))=b(t(i,j))+vecb(i,k)*valb(k)*&
                                vecb(j,k)
                                bm(t(i,j))=bm(t(i,j))+vecb(i,k)*valb(&
                                k)*vecb(j,k)
214                          continue
213                      continue
212                  continue
                    dm=d
                    do 801 i = 1, 6
                        epsi(i)=epst(i)
                        epst(i)=epsf(i)
801                  continue
                    goto 999
                endif
            else
                do 701 i = 1, 3
                    if ((valb(i).lt.0) .and. (abs(valb(i))-tolb.le. 0.d0)) then
                        valb(i)=tolb-r8prem()
                    endif
701              continue
                if (abs(un-d)-tolb .le. 0.d0) then
                    d=un-tolb+r8prem()
                    dbloq=.true.
                endif
                call r8inir(6, 0.d0, b, 1)
                call r8inir(6, 0.d0, bm, 1)
                do 712 i = 1, 3
                    do 713 j = i, 3
                        do 714 k = 1, 3
                            b(t(i,j))=b(t(i,j))+vecb(i,k)*valb(k)*&
                            vecb(j,k)
                            bm(t(i,j))=bm(t(i,j))+vecb(i,k)*valb(k)*&
                            vecb(j,k)
714                      continue
713                  continue
712              continue
                dm=d
                do 901 i = 1, 6
                    epsi(i)=epst(i)
                    epst(i)=epsf(i)
901              continue
!
            endif
!
!----CAS OU 1 VALEUR PROPRE EST NULLE---------------------------------
!
        else if (bdim.eq.2) then
!
            call r8inir(9, 0.d0, interm, 1)
            call r8inir(6, 0.d0, epi, 1)
            do 202 i = 1, 3
                do 203 l = 1, 3
                    do 204 k = 1, 3
                        interm(i,l)=interm(i,l)+vecbm(k,i)*epst(t(k,l)&
                        )
204                  continue
                    do 205 j = i, 3
                        epi(t(i,j))=epi(t(i,j))+interm(i,l)*vecbm(l,j)
205                  continue
203              continue
202          continue
            tot1=.false.
            tot2=.false.
            tot3=.false.
            call r8inir(6, 0.d0, bmr, 1)
            if (valbm(1)-tolb .le. 0.d0) then
!
                bmr(1)=valbm(2)
                bmr(2)=valbm(3)
                epsr(1)=epi(2)
                epsr(2)=epi(3)
                epsr(3)=epi(1)
                epsr(4)=epi(6)
                epsr(5)=epi(4)
                epsr(6)=epi(5)
                tot1=.true.
            else if (valbm(2)-tolb.le.0.d0) then
!
                bmr(1)=valbm(3)
                bmr(2)=valbm(1)
                epsr(1)=epi(3)
                epsr(2)=epi(1)
                epsr(3)=epi(2)
                epsr(4)=epi(5)
                epsr(5)=epi(6)
                epsr(6)=epi(4)
                tot2=.true.
!
            else if (valbm(3)-tolb.le.0.d0) then
!
                bmr(1)=valbm(1)
                bmr(2)=valbm(2)
                epsr(1)=epi(1)
                epsr(2)=epi(2)
                epsr(3)=epi(3)
                epsr(4)=epi(4)
                epsr(5)=epi(5)
                epsr(6)=epi(6)
                tot3=.true.
!
            endif
!
            call lceob2(intmax, toler, epsr, bmr, dm,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, bdim, br, d, mult,&
                        elas, dbloq, iret)
!
            call diago3(br, vecbr, valbr)
!
            if (compte .lt. 100) then
!
                do 102 i = 1, 2
                    if (valbr(i) .lt. 0) then
                        reinit=.true.
                    endif
                    if (valbr(i)-tolb .le. 0.d0) then
                        valbr(i)=tolb-r8prem()
                    endif
102              continue
                if (d .gt. 1.d0) then
                    reinit=.true.
                endif
                if (un-d-tolb .le. 0.d0) then
                    d=un-tolb+r8prem()
                    dbloq=.true.
                endif
            else
!
                reinit=.false.
                do 902 i = 1, 2
                    if (valbr(i)-tolb .le. 0.d0) then
                        valbr(i)=tolb-r8prem()
                    endif
902              continue
                if (d-(un-tolb) .ge. 0.d0) then
                    d=un-tolb+r8prem()
                    dbloq=.true.
                endif
!
            endif
!
            if (reinit) then
                do 802 i = 1, 6
                    epst(i)=(epst(i)+epsi(i))/2
802              continue
                goto 999
            else
!
                call r8inir(6, 0.d0, br, 1)
                do 222 i = 1, 3
                    do 223 j = i, 3
                        do 224 k = 1, 3
                            br(t(i,j))=br(t(i,j))+vecbr(i,k)*valbr(k)*&
                            vecbr(j,k)
224                      continue
223                  continue
222              continue
!
                if (tot1) then
                    binter(1)=tolb-r8prem()
                    binter(2)=br(1)
                    binter(3)=br(2)
                    binter(4)=0.d0
                    binter(5)=0.d0
                    binter(6)=br(4)
                else if (tot2) then
                    binter(1)=br(2)
                    binter(2)=tolb-r8prem()
                    binter(3)=br(1)
                    binter(4)=0.d0
                    binter(5)=br(4)
                    binter(6)=0.d0
                else if (tot3) then
                    binter(1)=br(1)
                    binter(2)=br(2)
                    binter(3)=tolb-r8prem()
                    binter(4)=br(4)
                    binter(5)=0.d0
                    binter(6)=0.d0
                endif
!
                call r8inir(9, 0.d0, interm, 1)
                call r8inir(6, 0.d0, b, 1)
                call r8inir(6, 0.d0, bm, 1)
                do 232 i = 1, 3
                    do 233 l = 1, 3
                        do 234 k = 1, 3
                            interm(i,l)=interm(i,l)+vecbm(i,k)*binter(&
                            t(k,l))
234                      continue
                        do 235 j = i, 3
                            b(t(i,j))=b(t(i,j))+interm(i,l)*vecbm(j,l)
                            bm(t(i,j))=bm(t(i,j))+interm(i,l)*vecbm(j,&
                            l)
235                      continue
233                  continue
232              continue
                dm=d
!
                do 803 i = 1, 6
                    epsi(i)=epst(i)
                    epst(i)=epsf(i)
803              continue
                goto 999
            endif
!
!---- CAS OU 2 VALEURS PROPRES SONT NULLES-----------------------------
!
        else if (bdim.eq.1) then
!
            call r8inir(9, 0.d0, interm, 1)
            call r8inir(6, 0.d0, epi, 1)
            do 242 i = 1, 3
                do 243 l = 1, 3
                    do 244 k = 1, 3
                        interm(i,l)=interm(i,l)+vecbm(k,i)*epst(t(k,l)&
                        )
244                  continue
                    do 245 j = i, 3
                        epi(t(i,j))=epi(t(i,j))+interm(i,l)*vecbm(l,j)
245                  continue
243              continue
242          continue
!
            tot1=.false.
            tot2=.false.
            tot3=.false.
            call r8inir(6, 0.d0, bmr, 1)
            if (valbm(1)-tolb .gt. 0.d0) then
                bmr(1)=valbm(1)
                epsr(1)=epi(1)
                epsr(2)=epi(2)
                epsr(3)=epi(3)
                epsr(4)=epi(4)
                epsr(5)=epi(5)
                epsr(6)=epi(6)
                tot1=.true.
!
            else if (valbm(2)-tolb.gt.0.d0) then
                bmr(1)=valbm(2)
                epsr(1)=epi(2)
                epsr(2)=epi(3)
                epsr(3)=epi(1)
                epsr(4)=epi(6)
                epsr(5)=epi(4)
                epsr(6)=epi(5)
                tot2=.true.
!
            else if (valbm(3)-tolb.gt.0.d0) then
                bmr(1)=valbm(3)
                epsr(1)=epi(3)
                epsr(2)=epi(1)
                epsr(3)=epi(2)
                epsr(4)=epi(5)
                epsr(5)=epi(6)
                epsr(6)=epi(4)
                tot3=.true.
            endif
!
            call lceob1(intmax, toler, epsr, bmr, dm,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, bdim, br, d, mult,&
                        elas, dbloq, iret)
!
            if (compte .lt. 100) then
!
                if (br(1) .lt. 0) then
                    reinit=.true.
                endif
                if (br(1)-tolb .le. 0.d0) then
                    br(1)=tolb-r8prem()
                endif
                if (d .gt. 1.d0) then
                    reinit=.true.
                endif
                if (un-d-tolb .le. 0.d0) then
                    d=un-tolb+r8prem()
                    dbloq=.true.
                endif
!
            else
!
                reinit=.false.
                if (br(1)-tolb .le. 0.d0) then
                    br(1)=tolb-r8prem()
                endif
                if (d-(un-tolb) .ge. 0.d0) then
                    d=un-tolb+r8prem()
                    dbloq=.true.
                endif
!
            endif
!
            if (reinit) then
                do 804 i = 1, 6
                    epst(i)=(epst(i)+epsi(i))/2
804              continue
                goto 999
!
            else
                valb(1)=tolb-r8prem()
                valb(2)=tolb-r8prem()
                valb(3)=tolb-r8prem()
                if (tot1) valb(1)=br(1)
                if (tot2) valb(2)=br(1)
                if (tot3) valb(3)=br(1)
                call r8inir(6, 0.d0, b, 1)
                call r8inir(6, 0.d0, bm, 1)
                do 252 i = 1, 3
                    do 253 j = i, 3
                        do 254 k = 1, 3
                            b(t(i,j))=b(t(i,j))+vecbm(i,k)*valb(k)*&
                            vecbm(j,k)
                            bm(t(i,j))=bm(t(i,j))+vecbm(i,k)*valbm(k)*&
                            vecbm(k,j)
254                      continue
253                  continue
252              continue
                dm=d
                do 805 i = 1, 6
                    epsi(i)=epst(i)
                    epst(i)=epsf(i)
805              continue
                goto 999
            endif
        endif
!
    endif
9999  continue
!
end subroutine
