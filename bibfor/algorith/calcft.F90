subroutine calcft(option, thmc, imate, ndim, dimdef,&
                  dimcon, yamec, yap1, yap2, addete,&
                  addeme, addep1, addep2, adcote, congep,&
                  dsde, t, grat, phi, pvp,&
                  rgaz, tbiot, sat, dsatp1, lambp,&
                  dlambp, lambs, dlambs, tlambt, tdlamt,&
                  mamolv, tlamct, rho11, h11, h12,&
                  angmas, aniso, phenom)
! ======================================================================
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
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
! ======================================================================
! ROUTINE CALC_FLUX_THERM ----------------------------------------------
! CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE DES FLUX -
! ======================================================================
! aslint: disable=W1504    
    implicit none
#include "asterfort/dilata.h"
#include "asterfort/unsmfi.h"
    integer :: ndim, dimdef, dimcon, imate, aniso
    integer :: yamec, yap1, yap2
    integer :: addete, addeme, addep1, addep2, adcote
    real(kind=8) :: congep(1:dimcon)
    real(kind=8) :: dsde(1:dimcon, 1:dimdef), mamolv
    integer :: i, j, k
    real(kind=8) :: t, grat(3), phi, sat, dsatp1, pvp
    real(kind=8) :: rgaz, tbiot(6), biot(3, 3)
    real(kind=8) :: lambp, dlambp
    real(kind=8) :: lambs, dlambs
    real(kind=8) :: rho11, h11, h12, rho12
    real(kind=8) :: angmas(3)
    real(kind=8) :: tlambt(ndim, ndim), tlamct(ndim, ndim)
    real(kind=8) :: tdlamt(ndim, ndim)
    real(kind=8) :: alphfi, cs, kron(ndim, ndim)
    real(kind=8) :: lamdt1(ndim, ndim), lamdt2(ndim, ndim)
    real(kind=8) :: lamdt3(ndim, ndim)
    real(kind=8) :: lamdt4(ndim, ndim), lamdt5(ndim, ndim)
    character(len=16) :: option, thmc, phenom
! =====================================================================
! --- DEFINITION DU SYMBOLE DE KRONECKER ------------------------------
! =====================================================================
    do 10 i = 1, ndim
        do 20 j = 1, ndim
            kron(i,j) = 0.d0
20      continue
10  end do
    do 30 i = 1, ndim
        kron(i,i) = 1.d0
30  end do
!
    call matini(ndim, ndim, 0.d0, lamdt1)
    call matini(ndim, ndim, 0.d0, lamdt2)
    call matini(ndim, ndim, 0.d0, lamdt3)
    call matini(ndim, ndim, 0.d0, lamdt4)
    call matini(ndim, ndim, 0.d0, lamdt5)
!
! =====================================================================
! --- REDEFINITION DU TENSEUR DE BIOT ---------------------------------
! =====================================================================
     biot(1,1)=tbiot(1)
     biot(2,2)=tbiot(2)
     biot(3,3)=tbiot(3)
     biot(1,2)=tbiot(4)
     biot(1,3)=tbiot(5)
     biot(2,3)=tbiot(6)
     biot(2,1)=biot(1,2)
     biot(3,1)=biot(1,3)
     biot(3,2)=biot(2,3)
! =====================================================================
! --- RECUPERATION DES COEFFICIENTS MECANIQUES ALPHAFI ET CS-----------
! =====================================================================
    if (yamec .eq. 1) then
        call dilata(imate, phi, alphfi, t, aniso,&
                    angmas, tbiot, phenom)
        call unsmfi(imate, phi, cs, t, tbiot,&
                    aniso, ndim, phenom)
    else
! =====================================================================
! --- EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0 -----------------------
! =====================================================================
        alphfi = 0.d0
        cs = 0.d0
    endif
    if (thmc .eq. 'GAZ') then
        sat = 0.d0
        dsatp1 = 0.d0
    else if (thmc.eq.'LIQU_SATU') then
        sat = 1.d0
        dsatp1 = 0.d0
    endif
! =====================================================================
!           LAMDT1 : LAMBDA
!           LAMDT2 : DLAMB / DEPSV
!           LAMDT3 : DLAMB / DP1
!           LAMDT4 : DLAMB / DP2
!           LAMDT5 : DLAMB / DT
! =====================================================================
    if (thmc .eq. 'LIQU_VAPE') then
        rho12=mamolv*pvp/rgaz/t
        do 35 i = 1, ndim
            do 36 j = 1, ndim
                lamdt1(i,j) =lamdt1(i,j)+lambs*lambp*tlambt(i,j)&
                + tlamct(i,j)
                lamdt2(i,j) =lamdt2(i,j)+(biot(i,j)-phi*kron(i,j))&
                *tlambt(j,i)*dlambp*lambs
                lamdt3(i,j) =lamdt3(i,j)+(rho12/rho11-1.d0)*lambp&
                *dlambs*tlambt(i,j)*dsatp1 +cs*(sat+(1.d0-sat)*rho12/&
                rho11)* dlambp*lambs*tlambt(i,j)
                lamdt4(i,j) = 0.d0
                lamdt5(i,j) =lamdt5(i,j)+lambs*lambp*tdlamt(i,j)&
                +(-3.d0*alphfi+cs*(1.d0-sat)* rho12*(h12-h11)/t)*&
                dlambp*lambs* tlambt(i,j) +lambp*dlambs*tlambt(i,j)*&
                dsatp1*rho12* (h12-h11)/t
36          continue
35      continue
    else
        do 50 i = 1, ndim
            do 51 j = 1, ndim
                lamdt1(i,j) =lamdt1(i,j)+lambs*lambp*tlambt(i,j)&
                + tlamct(i,j)
                lamdt2(i,j) =lamdt2(i,j)+(biot(i,j)-phi*kron(i,j))&
                *tlambt(j,i)*dlambp*lambs
                lamdt3(i,j) =lamdt3(i,j)+lambp*dlambs*tlambt(i,j)*&
                dsatp1 -sat*cs*dlambp*lambs*tlambt(i,j)
                lamdt4(i,j) =lamdt4(i,j)+ cs*dlambp*lambs*tlambt(i,j)
                lamdt5(i,j) =lamdt5(i,j)+ lambs*lambp*tdlamt(i,j)&
                -3.d0*alphfi*dlambp*lambs*tlambt(i,j)
51          continue
50      continue
    endif
!
! =====================================================================
! --- CALCUL DU FLUX THERMIQUE ----------------------------------------
! =====================================================================
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        do 100 i = 1, ndim
            do 101 j = 1, ndim
                dsde(adcote+i,addete+j)=dsde(adcote+i,addete+j)-&
                lamdt1(i,j)
                dsde(adcote+i,addete)=dsde(adcote+i,addete)- lamdt5(i,&
                j)*grat(j)
101          continue
            if (yamec .eq. 1) then
                do 110 j = 1, 6
                    do 111 k = 1, ndim
                        dsde(adcote+i,addeme+ndim-1+j)= dsde(adcote+i,&
                        addeme+ndim-1+j)-lamdt2(i,k)*grat(k)
111                  continue
110              continue
            endif
            if (yap1 .eq. 1) then
                do 112 j = 1, ndim
                    dsde(adcote+i,addep1)=dsde(adcote+i,addep1)&
                    - lamdt3(i,j)*grat(j)
112              continue
                if (yap2 .eq. 1) then
                    do 113 j = 1, ndim
                        dsde(adcote+i,addep2)=dsde(adcote+i,addep2)&
                        -lamdt4(i,j)*grat(j)
113                  continue
                endif
            endif
100      continue
    endif
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        do 102 i = 1, ndim
            congep(adcote+i)=0.d0
            do 103 j = 1, ndim
                congep(adcote+i)=congep(adcote+i)-lamdt1(i,j)*grat(j)
103          continue
102      continue
    endif
! =====================================================================
end subroutine
