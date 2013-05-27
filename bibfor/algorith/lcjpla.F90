subroutine lcjpla(fami, kpg, ksp, loi, mod,&
                  nr, imat, nmat, mater, nvi,&
                  deps, sigf, vin, dsde, sigd,&
                  vind, vp, vecp, theta, dt,&
                  devg, devgii, codret)
    implicit   none
!       ================================================================
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
!       ----------------------------------------------------------------
!       MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT ELASTO-PLASTIQUE
!       VISCO-PLASTIQUE EN VITESSE A T+DT OU T
!       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
!           LOI    :  MODELE DE COMPORTEMENT
!           MOD    :  TYPE DE MODELISATION
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           NMAT   :  DIMENSION MATER
!           MATER  :  COEFFICIENTS MATERIAU
!           NVI    :  NB VARIABLES INTERNES
!           NR     :  NB DE TERME DANS LE SYSTEME DE NEWTOW
!           DEPS   :  INCREMENT DE DEFORMATION
!           SIGF   :  CONTRAINTE A L INSTANT +
!           VIN    :  VARIABLES INTERNES A L INSTANT +
!           SIGD   :  CONTRAINTE A L INSTANT -
!           VIND   :  VARIABLES INTERNES A L INSTANT -
!           THETA  :  ?? COMP_INCR/PARM_THETA
!           DT     :  ??
!           DEVG   :  ??
!           DEVGII :  ??
!       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
!           VP     : VALEURS PROPRES DU DEVIATEUR ELASTIQUE (HOEK-BROWN)
!           VECP   : VECTEURS PROPRES DU DEVIATEUR ELASTIQUE(HOEK-BROWN)
!           CODRET : CODE RETOUR
!                    = 0, TOUT VA BIEN PAS DE REDECOUPAGE
!                    = 1 ou 2, CORRESPOND AU CODE RETOUR DE PLASTI.F
!       ----------------------------------------------------------------
! TOLE CRP_21
    include 'asterfort/hbrjpl.h'
    include 'asterfort/irrjpl.h'
    include 'asterfort/lgljpl.h'
    include 'asterfort/rsljpl.h'
    integer :: imat, nmat, nvi, nr, kpg, ksp, codret
    real(kind=8) :: dsde(6, 6), devg(*), devgii, sigf(6), deps(6)
    real(kind=8) :: vin(*), vind(*), theta, dt, mater(nmat, 2)
    real(kind=8) :: vp(3), vecp(3, 3), sigd(6)
    character(len=8) :: mod
    character(len=16) :: loi
    character(len=*) :: fami
!       ----------------------------------------------------------------
!
    codret = 0
!
    if (loi(1:8) .eq. 'ROUSS_PR' .or. loi(1:10) .eq. 'ROUSS_VISC') then
        call rsljpl(fami, kpg, ksp, loi, imat,&
                    nmat, mater, sigf, vin, vind,&
                    deps, theta, dt, dsde)
!
    else if (loi(1:6) .eq. 'LAIGLE') then
        call lgljpl(mod, nmat, mater, sigf, devg,&
                    devgii, vin, dsde, codret)
!
        elseif ( (loi(1:10) .eq. 'HOEK_BROWN').or. (loi(1:14) .eq.&
    'HOEK_BROWN_EFF') )then
        call hbrjpl(mod, nmat, mater, sigf, vin,&
                    vind, vp, vecp, dsde)
!
    else if (loi(1:7) .eq. 'IRRAD3M') then
        call irrjpl(mod, nmat, mater, sigf, vind,&
                    vin, dsde)
    endif
!
end subroutine
