subroutine zacier(matos, nbhist, ftrc, trc, coef,&
                  fmod, ckm, nbtrc, tpg0, tpg1,&
                  tpg2, dt10, dt21, tamp, metapg)
!
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
!
    implicit none
!
!
!-----------------------------------------------------------------------
!
!
!       EVOLUTION METALLURGIQUE POUR ACIER
!
!   - FONCTION :                                                       C
!       CALCUL DE Z(N+1) CONNAISSANT T(N), TPOINT(N), Z(N) ET T(N+1)
!   - ENTREES :
!       MATOS            : MATERIAU
!
!       NBHIST           : NBRE D HISTOIRES EXPERIMENTALE DE DEFI_TRC  C
!       FTRC(3*NBHIST,3) : VECTEUR DZ/DT EXPERIMENTAUX (VIDE EN ENTREE)C
!       TRC (3*NBHIST,5) : VECTEUR Z,T EXPERIMENTAUX (VIDE EN ENTREE)  C
!       FMOD(*)          : ENSEMBLE DES HISTOIRES EXPERIMENTALES       C
!       CKM(6*NBTRC)     : VECTEUR DES LOIS MS(Z) SEUIL,AKM,BKM,TPLM C
!                          ET TAILLE DE GRAIN AUSTENITIQUE DREF,A     C
!       NBTRC            : NBRE DE LOIS MS(Z)                          C
!       TPG0             : TEMPERATURE AU POINT DE GAUSS INSTANT N-1   C
!       TPG1             : TEMPERATURE AU POINT DE GAUSS INSTANT N     C
!       TPG2             : TEMPERATURE AU POINT DE GAUSS INSTANT N+1   C
!       DT10             : DELTATT ENTRE N-1 ET N                      C
!       DT21             : DELTATT ENTRE N  ET N+                      C
!       TAMP(7)          : PHASMETA(N) ZF,ZP,ZB,ZM,P,T,MS           C
!   - SORTIES :                                                        C
!       METAPG(7)          : PHASMETA(N+1) ZF,ZP,ZB,ZM,P,T,MS          C
!
!-----------------------------------------------------------------------
!
!
#include "asterfort/rcvalb.h"
#include "asterfort/smcarc.h"
#include "asterfort/utmess.h"
    real(kind=8) :: metapg(7), tamp(7), tempo(7)
    integer :: matos, nbhist, nbtrc
    real(kind=8) :: ftrc((3*nbhist), 3), trc((3*nbhist), 5), fmod(*)
    real(kind=8) :: ckm(6*nbtrc), coef(*), dt10, dt21, tpg0, tpg1, tpg2
    real(kind=8) :: lambd0, qsrk, d10, wsrk, lambda, unsurl, dmoins
    real(kind=8) :: dlim, dz
!-----------------------------------------------------------------------
!
!
    character(len=24) :: nomres(11)
    integer :: icodre(11), kpg, spt
    real(kind=8) :: tpoint, zero, ti, tpi
    real(kind=8) :: ac1, ac3, taux1, taux3, zeq1, zeq2, z1, z2, ar3, epsi
    real(kind=8) :: ctes(11), un
    real(kind=8) :: zeq1i, zeq2i, ti1, ti2, taux, z1i
    real(kind=8) :: a, b, c, delta
    character(len=8) :: fami, poum
!
    integer :: i, j, nbpas
    logical(kind=1) :: lrefr
!
!-----------------------------------------------------------------------
!
    zero = 0.d0
    epsi = 1.d-10
    un = 1.d0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
!        --- DETERMINATION DU SENS DE L'EVOLUTION METALLURGIQUE ---
!
    nomres(1) = 'AR3'
    nomres(2) = 'ALPHA'
    nomres(3) = 'MS0'
    nomres(4) = 'AC1'
    nomres(5) = 'AC3'
    nomres(6) = 'TAUX_1'
    nomres(7) = 'TAUX_3'
    call rcvalb(fami, kpg, spt, poum, matos,&
                ' ', 'META_ACIER', 1, 'INST', [0.d0],&
                7, nomres, ctes, icodre, 1)
    ar3 = ctes(1)
    ac1 = ctes(4)
    ac3 = ctes(5)
    taux1 = ctes(6)
    taux3 = ctes(7)
!
    nomres(8) = 'LAMBDA0'
    nomres(9) = 'QSR_K'
    nomres(10)= 'D10'
    nomres(11)= 'WSR_K'
    call rcvalb(fami, kpg, spt, poum, matos,&
                ' ', 'META_ACIER', 1, 'INST', [0.d0],&
                4, nomres(8), ctes(8), icodre(8), 0)
!
    if (icodre(8) .eq. 1) ctes(8) = 0.d0
    if (icodre(9) .eq. 1) ctes(9) = 0.d0
    if (icodre(10) .eq. 1) ctes(10) = 0.d0
    if (icodre (11) .eq. 1) ctes(11)=0.d0
    lambd0 = ctes(8)
    qsrk = ctes(9)
    d10 = ctes(10)
    wsrk = ctes(11)
    if ((icodre(8) .eq.0) .and. (icodre(10) .eq.1)) then
        call utmess('F', 'ALGORITH11_73')
    endif
!
    metapg(6) = tpg2
    metapg(7) = tamp(7)
    tpoint = (tpg1-tpg0)/dt10
!
!
!
!
    z1 = zero
    do 209 j = 1, 4
        z1 = z1+tamp(j)
209  continue
    z1 = un - z1
    zeq1 = min( (tpg1-ac1)/(ac3-ac1) , un )
    zeq2 = min( (tpg2-ac1)/(ac3-ac1) , un )
    if (tpoint .gt. zero) then
        lrefr = .false.
    else if (tpg2 .gt. ar3) then
        lrefr = .false.
    else if (tpg2 .lt. ac1) then
        lrefr = .true.
    else if (tpoint .lt. zero) then
        lrefr = .true.
    else if (z1 .le. zeq2) then
        lrefr = .false.
    else
        lrefr = .true.
    endif
    if (lrefr) then
!
!
        if (abs(tpg2-tpg1) .gt. 5.001d0) then
            nbpas = int(abs(tpg2-tpg1)/5.d0-0.001d0)+1
            dt21 = dt21/dble(nbpas)
            do 33 j = 1, 7
                tempo(j) = tamp(j)
33          continue
            do 50 i = 1, nbpas
                ti = tpg1+(tpg2-tpg1)*dble(i-1)/dble(nbpas)
                metapg(6) = tpg1+(dble(i)*(tpg2-tpg1)) /dble(nbpas)
                tpi = (metapg(6)-ti)/dt21
                call smcarc(nbhist, ftrc, trc, coef, fmod,&
                            ctes, ckm, nbtrc, ti, tpi,&
                            dt10, tempo, metapg)
                do 40 j = 1, 7
                    tempo(j) = metapg(j)
40              continue
50          continue
        else
            call smcarc(nbhist, ftrc, trc, coef, fmod,&
                        ctes, ckm, nbtrc, tpg1, tpoint,&
                        dt10, tamp, metapg)
!
        endif
    else
        if (abs(tpg2-tpg1) .gt. 5.001d0) then
! ----------------SUBDIVISION EN PAS DE CING DEGRE MAX
            nbpas = int(abs(tpg2-tpg1)/5.d0-0.001d0)+1
            dt21 = dt21/dble(nbpas)
            z1i = z1
            dmoins=tamp(5)
            do 51 i = 1, nbpas
                ti1 = tpg1+(tpg2-tpg1)*dble(i-1)/dble(nbpas)
                ti2 = tpg1+(tpg2-tpg1)*dble(i)/dble(nbpas)
                tpoint = (ti2-ti1)/dt21
                zeq1i = min( (ti1-ac1)/(ac3-ac1) , un )
                zeq2i = min( (ti2-ac1)/(ac3-ac1) , un )
                taux = taux1 + (taux3-taux1)*zeq1i
                if ((ti1.lt.(ac1-epsi)) .or. (z1i.ge.un)) then
                    z2 = z1i
                else
                    if (zeq2i .ge. (un-epsi)) tpoint = zero
                    if (z1i .gt. zeq1i) then
                        z2 = (taux*tpoint/(ac3-ac1))
                        z2 = z2*exp(-dt21/taux)
                        z2 = ( (-taux*tpoint/(ac3-ac1))+zeq2i+z2-zeq1i) *(un-z1i )/(un-zeq1i )
                        z2 = z2+z1i
                    else
                        z2 = (taux*tpoint/(ac3-ac1))-zeq1i+z1i
                        z2 = z2*exp(-dt21/taux)
                        z2 = (-taux*tpoint/(ac3-ac1))+zeq2i+z2
                    endif
                endif
!
!                 CALCUL TAILLE DE GRAIN
!
                if (icodre(8) .eq. 1) then
                    unsurl = 0.d0
                    metapg(5) = ckm(5)
                else
                    if (z2 .lt. 1.d-3) then
                        metapg(5)=0.d0
                    else
                        lambda = lambd0*exp(qsrk/(ti1+273.d0))
                        unsurl = 1/lambda
                        dlim = d10*exp(-wsrk/(ti1+273.d0))
                        dz = z2-z1i
                        a = 1.d0
                        b = dmoins*(1-dz/z2)-(dt10*unsurl/dlim)
                        c = dt21*unsurl
                        delta = (b**2)+(4.d0*a*c)
                        metapg(5) = (b+delta**0.5d0)/(2.d0*a)
                    endif
                    z1i = z2
                    dmoins = metapg(5)
                endif
!
51          continue
        else
!
            taux = taux1 + (taux3-taux1)*zeq1
            if ((tpg1.lt.(ac1-epsi)) .or. (z1.ge.un)) then
                z2 = z1
            else
                if (zeq2 .ge. (un-epsi)) tpoint = zero
                if (z1 .gt. zeq1) then
                    z2 = (taux*tpoint/(ac3-ac1))
                    z2 = z2*exp(-dt21/taux)
                    z2 = ( (-taux*tpoint/(ac3-ac1))+zeq2+z2-zeq1) *(un-z1 )/(un-zeq1 )
                    z2 = z2+z1
                else
                    z2 = (taux*tpoint/(ac3-ac1))-zeq1+z1
                    z2 = z2*exp(-dt21/taux)
                    z2 = (-taux*tpoint/(ac3-ac1))+zeq2+z2
                endif
            endif
! ---          CALCUL TAILLE DE GRAIN
            if (icodre(8) .eq. 1) then
                unsurl = 0.d0
                metapg(5) = ckm(5)
            else
                if (z2 .lt. 1.d-3) then
                    metapg(5)=0.d0
                else
                    dmoins = tamp(5)
                    lambda = lambd0*exp(qsrk/(tpg1+273.d0))
                    unsurl = 1/lambda
                    dlim = d10*exp(-wsrk/(tpg1+273.d0))
                    dz = z2-z1
                    a = 1.d0
                    b = dmoins*(1-dz/z2)-(dt10*unsurl/dlim)
                    c = dt21*unsurl
                    delta = (b**2)+(4.d0*a*c)
                    metapg(5) = (b+delta**0.5d0)/(2.d0*a)
!
                endif
            endif
        endif
!           REPARTITION DE DZGAMMA SUR DZALPHA
        if (z2 .gt. (un-epsi)) z2 = un
        if (z1 .ne. un) then
            do 210 j = 1, 4
                metapg(j) = tamp(j)*(un-(z2-z1)/(un-z1))
210          continue
        else
            do 211 j = 1, 4
                metapg(j) = tamp(j)
211          continue
        endif
!
    endif
end subroutine
