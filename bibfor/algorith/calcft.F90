subroutine calcft(option, thmc, imate, ndim, dimdef,&
                  dimcon, yamec, yap1, yap2, addete,&
                  addeme, addep1, addep2, adcote, congep,&
                  dsde, t, grat, phi, pvp,&
                  rgaz, biot, sat, dsatp1, lambp,&
                  dlambp, lambs, dlambs, lambt, dlambt,&
                  mamolv, lambct, rho11, h11, h12)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
! ======================================================================
! ROUTINE CALC_FLUX_THERM ----------------------------------------------
! CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE DES FLUX -
! ======================================================================
    implicit      none
    include 'asterfort/rcvalb.h'
    integer :: ndim, dimdef, dimcon, imate
    integer :: yamec, yap1, yap2
    integer :: addete, addeme, addep1, addep2, adcote
    real(kind=8) :: congep(1:dimcon)
    real(kind=8) :: dsde(1:dimcon, 1:dimdef), mamolv
    integer :: i, j
    real(kind=8) :: t, grat(3), phi, sat, dsatp1, pvp, lambdt(5)
    real(kind=8) :: rgaz, biot
    real(kind=8) :: lambp, dlambp
    real(kind=8) :: lambs, dlambs, lambt
    real(kind=8) :: dlambt
    real(kind=8) :: lambct, rho11, h11, h12, rho12
    character(len=16) :: option, thmc
!    PARAMETRE POUR LA RECUP DES COEF MECA
    integer :: nelas, kpg, spt
    parameter  ( nelas=4 )
    real(kind=8) :: elas(nelas), young, alpha0, cs, k0, nu
    character(len=8) :: ncra1(nelas), fami, poum
    integer :: icodre(nelas)
! ======================================================================
! --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES -----------
! ======================================================================
    data ncra1/'E','NU','ALPHA','RHO'/
! =====================================================================
! ---       RECUPERATION DES COEFFICIENTS MECANIQUES ------------------
! =====================================================================
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    if (yamec .eq. 1) then
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', 'ELAS', 1, 'TEMP', t,&
                    3, ncra1(1), elas(1), icodre, 1)
        young = elas(1)
        nu = elas(2)
        alpha0 = elas(3)
        k0 = young/3.d0/ (1.d0-2.d0*nu)
        cs = (1.d0-biot)/k0
    else
! =====================================================================
! --- EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0       -------------
! =====================================================================
        alpha0 = 0.d0
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
!           LAMBDT(1) : LAMBDA
!           LAMBDT(2) : DLAMB / DEPSV
!           LAMBDT(3) : DLAMB / DP1
!           LAMBDT(4) : DLAMB / DP2
!           LAMBDT(5) : DLAMB / DT
! =====================================================================
!
    if (thmc .eq. 'LIQU_VAPE') then
        rho12=mamolv*pvp/rgaz/t
        lambdt(1) = lambs*lambp*lambt + lambct
        lambdt(2) = (biot-phi)*dlambp*lambs*lambt
        lambdt(3) =(rho12/rho11-1.d0)* lambp*dlambs*lambt*dsatp1&
        +cs*(sat+(1.d0-sat)*rho12/rho11)*(biot-phi)* dlambp*lambs*&
        lambt
        lambdt(4) =0.d0
        lambdt(5) = lambs*lambp*dlambt +(biot-phi)*(-3.d0*alpha0+cs*( 1.d0-sat)* rho12*(h12-h11)/&
                    &t)*dlambp*lambs*lambt +lambp* dlambs*lambt*dsatp1*rho12*(h12-h11)/t
    else
        lambdt(1) = lambs*lambp*lambt + lambct
        lambdt(2) = (biot-phi)*dlambp*lambs*lambt
        lambdt(3) = lambp*dlambs*lambt*dsatp1 -sat*cs*(biot-phi )* dlambp*lambs*lambt
        lambdt(4) = cs*(biot-phi)*dlambp*lambs*lambt
        lambdt(5) = lambs*lambp*dlambt -(biot-phi )*3.d0*alpha0*dlambp* lambs*lambt
    endif
!
! =====================================================================
! --- CALCUL DU FLUX THERMIQUE ----------------------------------------
! =====================================================================
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        do 100 i = 1, ndim
            dsde(adcote+i,addete+i)=dsde(adcote+i,addete+i)-lambdt(1)
            dsde(adcote+i,addete)=dsde(adcote+i,addete) - lambdt(5)*&
            grat(i)
            if (yamec .eq. 1) then
                do 101 j = 1, 3
                    dsde(adcote+i,addeme+ndim-1+j)= dsde(adcote+i,&
                    addeme+ndim-1+j)-lambdt(2)*grat(i)
101              continue
            endif
            if (yap1 .eq. 1) then
                dsde(adcote+i,addep1)=dsde(adcote+i,addep1) - lambdt(&
                3)*grat(i)
                if (yap2 .eq. 1) then
                    dsde(adcote+i,addep2)=dsde(adcote+i,addep2)&
                    - lambdt(4)*grat(i)
                endif
            endif
100      continue
    endif
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        do 102 i = 1, ndim
            congep(adcote+i)=-lambdt(1)*grat(i)
102      continue
    endif
! =====================================================================
end subroutine
