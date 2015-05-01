subroutine rgilin3d(xmat, nmat, var0, varf, nvari,&
                    dt, depst, nstrs, sigf, mfr,&
                    errb3d, teta1, teta2, fl3d, ifour,&
                    istep)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      sous programme de !alcul des avancements chimiques rgi
!      rag et rsi + repercussion poro elastique des volumes neoformes
!      sellier oct 2012
!=====================================================================
    implicit none
#include "asterf_types.h"
#include "asterfort/hydr_xmat.h"
#include "asterfort/elas_iso_3d.h"
#include "asterfort/hydr_vari.h"
#include "asterfort/b3d_dth.h"
#include "asterfort/rsi_3d.h"
#include "asterfort/utmess.h"
!   ********************************************************************
!      declaration externe
    integer :: nmat, nvari, nstrs, mfr, ifour, istep
    real(kind=8) :: depst(nstrs)
    real(kind=8) :: xmat(nmat), var0(nvari), varf(nvari), sigf(nstrs)
    real(kind=8) :: dt
    integer :: errb3d, i
    real(kind=8) :: teta1, teta2
    real(kind=8) :: e0, xnu0, alpha, hydra0, hydra1, hydras, e1, xnu1
    real(kind=8) :: vtot0, dvtot, vtot1, dt80, dth0, alpharag, ear, temp1
    real(kind=8) :: tref0, ar, aar0, sr1, srsrag, aar1, aar2, sig0, vrag1
    real(kind=8) :: brag1, srrag, prag1, srag, corg, xnasol0, xnasol1, dt1, vdef0, vmaft
    real(kind=8) :: dvdef2, xmbdef, v0def, bdef, srdef, pdef, sdef, dvdef1, vdef2
    aster_logical :: fl3d
!   ********************************************************************
!     declaration locale
    integer :: nmelast, erreur
!     deformation totale   (rem les depst castem sont des gammas)
    real(kind=8) :: depst6(6), dsige6(6), sig6(6)
!     elasticite isotrope
    parameter (nmelast=4)
!     variables rsi
    real(kind=8) :: X0(16), X1(16), beton(19)
!   ********************************************************************
!      print*,xmat
!      read*
!     young
    e0=xmat(1)
!     poisson
    xnu0=xmat(2)
!     coeff de dilatation thermique
    alpha=xmat(3)
    if (xnu0 .gt. 0.49) then
        print*,'Coeff de Poisson trop grand dans rgilin3d'
        errb3d=1
        call utmess('F', 'COMPOR1_90')
    end if
!     recuperation de l hydratation debut de pas
    hydra0=var0(7)
!     actualisation hydratation fin de pas
    hydra1=xmat(nmelast+7)
    varf(7)=hydra1
!     seuil d hydratation
    hydras=xmat(nmelast+8)
!   ********************************************************************
!     prise en compte de l hydratation sur les variables materiau
!     recuperation des caracteristiques de l hydratation
!     effet de l hydratation sur Young
!      print*,'Av hydr_xmat endo3d',E00,E0,hydra1,hydras,0.66d0,erreur
    erreur=0
    call hydr_xmat(e0, e1, hydra1, hydras, 0.5d0,&
                   erreur)
!     effet de l hydratation sur Poisson neglige (comme dans cendo3d car
!     significatif que avant le seuil
    xnu1=xnu0
!     test pb hydratation
    if (erreur .ne. 0) then
        print*,'pb hydratation dans endo3d'
        errb3d=1
        call utmess('F', 'COMPOR1_90')
    end if
!     ******************************************************************
!      recuperation des increments de deformations
    if (mfr .ne. 33) then
!       on est pas en formulation poreux
!       print*,'increment de deformation'
        if (ifour .eq. 2) then
!        massif 3d non poreux
            do i = 1, 6
                depst6(i)=depst(i)
!         print*,'depst',i,'=',depst(i)
            end do
        else
!        deformation plane ou axisymetrique non poreux
            do i = 1, nstrs
                depst6(i)=depst(i)
            end do
            if (nstrs .lt. 6) then
                do i = (nstrs+1), 6
                    depst6(i)=0.d0
                end do
            end if
        end if
    else
        print*,'formulation poreux non achevee ds endo3d...'
        errb3d=1
        call utmess('F', 'COMPOR1_90')
    end if
!
!     increment de deformation volumique (mecanique)
    dvtot=0.d0
    do i = 1, 3
        dvtot=dvtot+depst6(i)
    end do
!
!     actualisation deformation volumique mecanique
    vtot0=var0(21)
    vtot1=vtot0+dvtot
    varf(21)=vtot1
!
!   ********************************************************************
!      tir elastique
!      increment elastique
    call elas_iso_3d(depst6, e1, xnu1, dsige6)
    do i = 1, 6
!       prise en compte d une eventuelle degradation chimique
        if (hydra1 .lt. hydra0) then
            sig0=var0(i)*hydra1/hydra0
        else
            sig0=var0(i)
        end if
        sig6(i)=sig0+dsige6(i)
        varf(i)=sig6(i)
!        print*,'sigf=',sig6(i)
    end do
!       read*
!
!   ********************************************************************
!       on calcule l endo thermique
!       recuperation de l endo thermique et effet de l'hydratation
    dt80=xmat(nmelast+21)
    call hydr_vari(var0(25), dth0, hydra0, hydra1, hydras,&
                   erreur)
    call b3d_dth(teta2, dth0, dt80)
!       mise a jour de l endo thermique
    varf(25)=dth0
!   ********************************************************************
!
!     calcul de rag
    alpharag=(xmat(nmelast+10))**(-1)
!     prise en compte de l activation thermique
    ear=5875.d0
    temp1=0.5d0*(teta2+teta1)+273.d0
    tref0=293.d0
    ar=dexp(-ear*((1.d0/temp1)-(1.d0/tref0)))
    alpharag=alpharag*ar
!     recup avancement precedent
    aar0=var0(22)
!     recup hydrique saturation actuelle
    sr1=xmat(nmelast+6)/xmat(nmelast+5)
!     recup saturation seuil pour la rag
    srsrag=xmat(nmelast+11)
!     calcul de l avancement chimique
    if (aar0 .lt. sr1) then
        if (sr1 .gt. srsrag) then
            alpharag=alpharag*(sr1-srsrag)/(1.d0-srsrag)
            aar1=sr1-(sr1-aar0)*exp(-alpharag*dt)
        else
            aar1=aar0
        end if
    else
        aar1=aar0
    end if
!     attenuation par hydratation
!      call hydr_vari(aar1,aar2,hydra0,hydra1,hydras,erreur)
    aar2=aar1
!     stockage avancement chimique
    varf(22)=aar2
!     calcul de la pression de rag
    vrag1=aar2*xmat(nmelast+13)
!     stockage volume de gel
    varf(23)=vrag1
    brag1=xmat(nmelast+17)
!     saturation en rag
    srrag=vrag1/xmat(nmelast+5)
    prag1=max(xmat(nmelast+19)*(vrag1-xmat(nmelast+15)-&
     brag1*srrag*vtot1),0.d0)
!     contrainte induite par la rag
    srag=-brag1*prag1*srrag
!
!***********************************************************************
!      couplage RAG-RSI les alcalins entrent dans les granulats
!      avec l avancement de la rag
    corg=xmat(nmelast+23)
!     nal (la teneur en alcalin libre est  cste sur le pas de temps)
!     la RAG consomme les lacalins
    xnasol0=max(var0(24),1.d-4)
    X0(4)=xnasol0
    beton(14)=xnasol0
    xnasol1=xmat(nmelast+9)*(1.d0-aar2*corg)
    beton(15)=xnasol1
!
!***********************************************************************
!     calcul chimie de la RSI
!     recuperation des donnnees materiaux  pour la chimie
!     saturation seuil pour la def
    beton(1)=xmat(nmelast+12)
!     aluc
    beton(2)=xmat(nmelast+1)
!     sulfate
    beton(3)=xmat(nmelast+2)
!     silice
    beton(4)=xmat(nmelast+3)
!     temps caracteristique de micro diffusion (pour ID=ID1 dans rsi_3d)
    beton(5)=xmat(nmelast+4)
!     phi (on le passe en l/m3) en * par 1000
    beton(6)=1.d3*xmat(nmelast+5)
!     sr
    beton(7)=xmat(nmelast+6)/xmat(nmelast+5)
!     sotockage vw pour pas suivant
    varf(26)=xmat(nmelast+6)
!     calcul teneur en eau initiale
    if (var0(26) .eq. 0) then
!        on a sans doute pas initialise la teneur en eau init
!        on impose la precedente egale a l actuelle
        beton(16)=xmat(nmelast+6)/xmat(nmelast+5)
    else
        beton(16)=var0(26)/xmat(nmelast+5)
    endif
!     id min
    beton(17)=xmat(nmelast+24)
!     id pessimum
    beton(18)=xmat(nmelast+25)
!     id max
    beton(19)=xmat(nmelast+26)
!     alpha0
    beton(8)=hydra0
!     alpha1
    beton(9)=hydra1
!     temp0  (°K)
    beton(10)=teta1+273.d0
!     temp1  (°K)
    beton(11)=teta2+273.d0
!     endommagement thermique
    beton(12)=dth0
!     coeff de couplage avec l endo thermique
    beton(13)=xmat(nmelast+22)
!     recuperation des variables internes pour la chimie
!     hydratation
    X0(1)=var0(7)
!     temp (°K)
    X0(2)=beton(10)
!     csh
    X0(3)=var0(8)
!     LIBRE
!     X0(5)=var0(1)
!     alsol
    X0(6)=var0(9)
!     alf
    X0(7)=var0(10)
!     ssol
    X0(8)=var0(11)
!     sf
    X0(9)=var0(12)
!     aft
    X0(10)=var0(13)
!     afm
    X0(11)=var0(14)
!     ID
    X0(12)=var0(15)
!     cash
    X0(13)=var0(16)
!     csheff
    X0(14)=var0(17)
!     tau micro diffusion
    X0(15)=var0(18)
!     casol
    X0(16)=var0(19)
!     ******************************************************************
!     appel de l increment chimique pour la rsi
    dt1=dt
    call rsi_3d(X0, X1, beton, dt1)
!     stockage des variables internes pour la chimie
!     csh
    varf(8)=X1(3)
!     nasol
    varf(24)=X1(4)
!     alsol
    varf(9)=X1(6)
!     alf
    varf(10)=X1(7)
!     ssol
    varf(11)=X1(8)
!     sf
    varf(12)=X1(9)
!     aft
    varf(13)=X1(10)
!     afm
    varf(14)=X1(11)
!     ID
    varf(15)=X1(12)
!     cash
    varf(16)=X1(13)
!     csheff
    varf(17)=X1(14)
!     tau micro diffusion
    varf(18)=X1(15)
!     casol
    varf(19)=X1(16)
!
!     *******repercussion meca de la rsi *******************************
!
!     volume d ettringite
    vdef0=var0(20)
!     volume molaire de l ettringite
    vmaft=xmat(nmelast+14)
!     volume ettringite final avant prise en compte hydratation
    dvdef1=(varf(13)-var0(13))*vmaft
!     calcul de la partie efficace de l ettringite
    call hydr_vari(dvdef1, dvdef2, hydra0, hydra1, hydras,&
                   erreur)
!     stocakge volume d ettringite efficace
    vdef2=max(vdef0+dvdef2,0.d0)
    varf(20)=vdef2
!     calcul de la pression due a l ettringite
!     module de raideur ettringite
    xmbdef=xmat(nmelast+20)
!     vides connectes
    v0def=xmat(nmelast+16)
!     recup biot ettringite
    bdef=xmat(nmelast+18)
!     saturation ettringite
    srdef=vdef2/xmat(nmelast+5)
    pdef=max(xmbdef*(vdef2-v0def-bdef*srdef*vtot1),0.d0)
!     calcul de la contrainte isotrope induite sur la structure
    sdef=-bdef*pdef*srdef
!
!***********************************************************************
!     affectation des contraintes dans le vecteur de sortie
!      write(6,*)'***************************'
!!      write(6,*)'deps = ',(depst(i),i=1,6)
!      write(6,*)'dsige = ',(dsige6(i),i=1,6)
!      write(6,*)'sdef = ',sdef
!      write(6,*)'srag = ',srag
!      write(6,*)
    do i = 1, nstrs
        sigf(i)=sig6(i)
        if (i .le. 3) then
            sigf(i)=sigf(i)+sdef+srag
        end if
!       print*,'sigf rsilin3d=',sigf(i)
    end do
!      read*
    if (erreur .ne. 0) then
        print*,'erreur dans endo3d'
        errb3d=1
        call utmess('F', 'COMPOR1_90')
    end if
end subroutine
