subroutine rkdvec(fami, kpg, ksp, imat, matcst,&
                  nvi, vini, coeft, x, dtime,&
                  nmat, sigi, dvin)
    implicit none
!       ================================================================
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
! ==================================================================
!      MODELE VISCOPLASTIQUE A ECROUISSAGE ISOTRROPE COUPLE A DE
!      L ENDOMMAGEMENT ISOTROPE
! ==================================================================
! INTEGRATION DE LA LOI (VENDOCHAB) PAR UNE METHODE DE RUNGE KUTTA
!
!     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
!     INTERNES DU MODELE
!     ----------------------------------------------------------------
#include "asterfort/calcj0.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
    character(len=8) :: nomcoe, nompar(2)
    character(len=3) :: matcst
    character(len=*) :: fami
    integer :: nvi, nmat, imat, icodre(1), kpg, ksp, ndt
    real(kind=8) :: x, dtime, hsdt, tperd, dtperd, temp, tf, coeft(nmat)
    real(kind=8) :: vini(nvi), dvin(nvi), sedvp, critv, epsiec, smx(6), sigi(6)
    real(kind=8) :: ecrou, dmg, devi(6), devcum, decrou, ddmg, ze, td, valp(3)
    real(kind=8) :: s, alphad, betad, n, unsmvp, kvp, rd, ad, kd(1), vpar(2)
!     ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iret1, iret2, itens, ndi
    real(kind=8) :: detat, domcpl, epsi, grj0, grj1, grj2v, trsig
!
!-----------------------------------------------------------------------
    parameter(ze=0.0d0)
    parameter(td=1.5d0)
    parameter ( epsiec = 1.d-8 )
    parameter ( epsi = 1.d-15 )
!
    common /tdim/   ndt,    ndi
!     ----------------------------------------------------------------
!
!
! --    COEFFICIENTS MATERIAU INELASTIQUES
!
    s = coeft(4)
    alphad = coeft(5)
    betad = coeft(6)
    n = coeft(1)
    unsmvp = coeft(2)
    kvp = (1.d0/coeft(3))
    rd = coeft(7)
    ad = coeft(8)
!     ----------------------------------------------------------------
!     KD VA ETRE EXTRAIT PLUS LOIN UNE FOIS QUE SEDVP
!     (CONTRAINTE EQUIVALENTE D ENDOMMAGEMENT VISCOPLASTIQUE)
!     SERA CALCULEE
!     ----------------------------------------------------------------
!
! --  VARIABLES INTERNES
!
    ecrou = vini(8)
!     D---------------------18/06/96----------------------------
    if (ecrou .le. epsiec) then
        ecrou=epsiec
    endif
!     F---------------------18/06/96----------------------------
    dmg = vini(9)
!
!----------------------------------------------------------------
!----- VARIABLES INTERNES
    trsig=(sigi(1)+sigi(2)+sigi(3))
!----- CALCUL DE GRJ0(SIGI) : MAX DES CONTRAINTES PRINCIPALES
    if (alphad .le. (1.0d-15)) then
        grj0=0.0d0
    else
        call calcj0(sigi, grj0, valp)
    endif
!     -------------------------PROVISOIRE----------------------------
!----- CALCUL DE GRJ1(SIGI) : PREMIER INVARIANT (TRACE)
    grj1= trsig
!----- CALCUL DE GRJ2(SIGI) : SECOND INVARIANT (SIGEQ DE VON MISES)
    grj2v=0.0d0
    do 10 itens = 1, 6
        smx(itens)=sigi(itens)
        if (itens .le. 3) smx(itens)=smx(itens)-grj1/3.d0
        grj2v=grj2v+smx(itens)**2
10  end do
    grj2v=sqrt(1.5d0*grj2v)
!----- CALCUL DE SEDVP : CONTRAINTE EQUIVALENTE DE FLUAGE
    sedvp=alphad*grj0+betad*grj1+(1-alphad-betad)*grj2v
!
!----- CALCUL DE KD A PARTIR DU MATERIAU CODE
!
    if (matcst .eq. 'NAP') then
!
        nomcoe = 'K_D     '
        nompar(1) = 'TEMP    '
        nompar(2) = 'X       '
!
! -- TEMPERATURE
!
        call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                    ksp, tperd, iret1)
        if (iret1 .ne. 0) tperd = 0.d0
        call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                    ksp, tf, iret2)
        if (iret2 .ne. 0) tf = 0.d0
        dtperd = tf-tperd
!
        hsdt=x/dtime
        temp=tperd+hsdt*dtperd
!
        vpar(1) = temp
        vpar(2) = sedvp
!
        call rcvalb(fami, 1, 1, '+', imat,&
                    ' ', 'VENDOCHAB', 2, nompar, vpar,&
                    1, nomcoe, kd, icodre, 2)
    else
        kd(1) = coeft(9)
    endif
!
!----- LA FONCTION SEUIL NE FAIT PAS APPARAITRE D INFLUENCE DE L
!----- ECROUISSAGE
    critv=grj2v-s*(1-dmg)
    if (critv .le. 0.0d0) then
        devcum=0.0d0
        decrou=0.0d0
        ddmg=0.0d0
        do 11 itens = 1, 6
            devi(itens)=0.0d0
11      continue
    else
!
!------ EQUATION DONNANT LA DERIVEE DE L ENDOMMAGEMENT
!
        ddmg=sedvp/ad
        domcpl=max(epsi,(1-dmg))
        ddmg=(max(0.d0,ddmg)**rd)*(1/domcpl**kd(1))
!
!------ EQUATION DONNANT LA DERIVEE DE L ECROUISSAGE
!
        decrou=critv/((1-dmg)*kvp*ecrou**unsmvp)
        decrou=max(0.d0,decrou)
        decrou=decrou**n
!
!------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
!------ CUMULEE
!
        devcum=decrou/(1-dmg)
!
!------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
!
        do 12 itens = 1, 6
            devi(itens)=td*devcum*smx(itens)/grj2v
12      continue
    endif
!------ NE SERT A RIEN
    detat=ze
!
!
! --    DERIVEES DES VARIABLES INTERNES
!
    do 30 itens = 1, 6
        dvin(itens) = devi(itens)
30  end do
    dvin(7) = devcum
    dvin(8) = decrou
    dvin(9) = ddmg
    dvin(10) = detat
!
end subroutine
