subroutine rkdhay(mod, nvi, vini, coeft, nmat,&
                  sigi, dvin, iret)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ==================================================================
!      MODELE VISCOPLASTIQUE A ECROUISSAGE ISOTROPE COUPLE A DE
!      L ENDOMMAGEMENT ISOTROPE
! ==================================================================
! INTEGRATION DE LA LOI (HAYHURST) PAR UNE METHODE DE RUNGE KUTTA
!
!     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
!     INTERNES DU MODELE
!       IN  MOD     :  TYPE DE MODELISATION
!           NVI     :  NOMBRE DE VARIABLES INTERNES
!           VINI    :  VARIABLES INTERNES A T
!           COEFT   :  COEFFICIENTS MATERIAU INELASTIQUE A T
!           NMAT    :  DIMENSION MAXI DE COEFT
!           SIGI    :  CONTRAINTES A L'INSTANT COURANT, AVEC SQRT(2)
!     OUT:
!           DVIN    :  DERIVEES DES VARIABLES INTERNES A T
!           IRET    :  CODE RETOUR =0 SI OK, =1 SI PB
!     ----------------------------------------------------------------
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/fgequi.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    character(len=8) :: mod
!
    integer :: iret, itens, ndi, nmat, nvi, ndt, ndim
!
    real(kind=8) :: coeft(nmat), vini(nvi), dvin(nvi), smx(6), sigi(*)
    real(kind=8) :: ecrou(2), h, dmg, dmgmi
    real(kind=8) :: devi(6), devcum, decrou(2), ddmg, ddmgmi
    real(kind=8) :: ze, td, sinn, grj0
    real(kind=8) :: eps0, pk, h1, h2, delta1, delta2, h1st, h2st, pkc
    real(kind=8) :: sig0, biga, alphad
    real(kind=8) :: trsig, grj2v, grj1, epsi, terme1, shmax, sequi
    real(kind=8) :: equi(17), rmin, sequid
!     ----------------------------------------------------------------
    parameter(ze=0.0d0)
    parameter(td=1.5d0)
!
    common /tdim/   ndt,    ndi
!     ----------------------------------------------------------------
!
! -- TEMPERATURE
!
!      IF (MOD(1:2).EQ.'3D')THEN
    if (ndt .eq. 6) then
        ndim=3
    else if (ndt.eq.4) then
        ndim=2
        smx(5)=0.d0
        smx(6)=0.d0
        devi(5)=0.d0
        devi(6)=0.d0
    endif
    iret=0
    rmin=r8miem()
    shmax=50.d0
!
! --    COEFFICIENTS MATERIAU INELASTIQUES
!
    eps0 = coeft(1)
    pk = coeft(2)
    h1 = coeft(3)
    h2 = coeft(4)
    delta1 = coeft(5)
    delta2 = coeft(6)
    h1st = coeft(7)
    h2st = coeft(8)
    biga = coeft(9)
    sig0 = coeft(10)
    pkc = coeft(11)
    alphad = coeft(12)
    sequid = coeft(13)
!
    epsi=r8prem()*pk
!
! --  VARIABLES INTERNES
!
    do 7 itens = 1, 2
        ecrou(itens) = vini(itens+7)
 7  end do
!
    dmg = vini(11)
    dmgmi = vini(10)
    h=ecrou(1)+ecrou(2)
!
!----------------------------------------------------------------
    call dcopy(ndt, sigi, 1, smx, 1)
!
!------------CALCUL DES INVARIANTS DE CONTRAINTE  -------
!     attention FGEQUI ne prend pas en compte les SQRT(2)
    call dscal(3, 1.d0/sqrt(2.d0), smx(4), 1)
    call fgequi(smx, 'SIGM_DIR', ndim, equi)
!     on retablit le tenseur
    call dscal(3, sqrt(2.d0), smx(4), 1)
    trsig=equi(16)
    grj0=max(equi(3),equi(4))
    grj0=max(grj0,equi(5))
    grj1= trsig
    grj2v=equi(1)
    if (sequid .eq. 0.d0) then
        sequi=grj0
    else
        sequi=grj1
    endif
!------------ CALCUL DU TENSEUR DEVIATORIQUE DES CONTRAINTES ---
    do 10 itens = 1, ndt
        if (itens .le. 3) smx(itens)=smx(itens)-grj1/3.d0
10  end do
!
!----- EQUATION DONNANT LA DERIVEE DU MICROENDOMMAG
    ddmgmi=(pkc/3)*((1-dmgmi)**4)
!
!----- EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
!----- CUMULEE
!
    terme1=(grj2v*(1-h))/(pk*(1-dmgmi)*(1-dmg))
    if (grj2v .le. epsi) then
        devcum=ze
    else if (abs(terme1).lt.shmax) then
        devcum=eps0*(sinh(terme1))
    else
        iret=1
        goto 9999
    endif
!
!----- EQUATION DONNANT LA DERIVEE DE H
!
    if (grj2v .le. epsi) then
!       DIVISION PAR ZERO EVITEE
        decrou(1)=ze
        decrou(2)=ze
    else
        if (ecrou(1) .le. (h1st-rmin)) then
            decrou(1)=(h1/grj2v)*(h1st-(delta1*ecrou(1)))*devcum
            decrou(2)=(h2/grj2v)*(h2st-(delta2*ecrou(2)))*devcum
        else
            iret=1
            goto 9999
        endif
    endif
!
!
!----- EQUATION DONNANT LA DERIVEE DE L ENDOMMAGEMENT
!
    if (sequi .ge. ze) then
        sinn=alphad*sequi+((1.d0-alphad)*grj2v)
    else
        sinn=(1.d0-alphad)*grj2v
    endif
    if ((sinn/sig0) .lt. shmax) then
        ddmg=biga*sinh(sinn/sig0)
    else
        iret=1
        goto 9999
    endif
!
!------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
!
    if (grj2v .le. epsi) then
        do 33 itens = 1, ndt
            devi(itens)=ze
33      continue
    else
        do 12 itens = 1, ndt
            devi(itens)=td*devcum*smx(itens)/grj2v
12      continue
    endif
!
! --    DERIVEES DES VARIABLES INTERNES
!
    do 30 itens = 1, 6
        dvin(itens) = devi(itens)
30  end do
    dvin(7) = devcum
    dvin(8) = decrou(1)
    dvin(9) = decrou(2)
    dvin(10) = ddmgmi
    dvin(11) = ddmg
! VARIABLE INTERNE INUTILE CAR ON Y STOCKE L'INDICATEUR DE PLASTICITE
! DANS LCDPEC
    dvin(12) = ze
!
9999  continue
end subroutine
