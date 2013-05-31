subroutine lc0004(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
! aslint: disable=W1504
    implicit none
    include 'asterfort/nmchab.h'
    integer :: kpg, ksp, ndim, imate, codret, icomp, nvi
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
    real(kind=8) :: angmas(*), tampon(*)
    real(kind=8) :: crit(1), instam, instap, epsm(6), deps(6)
    real(kind=8) :: sigm(6), vim(*), sigp(6), vip(*), dsidep(6, 6)
!
!
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!
!     INTEGRATION LOCALE DES LOIS DE COMPORTEMENT DE CHABOCHE
!          RELATIONS : 'VMIS_CIN1_CHAB' 'VMIS_CIN2_CHAB'
!          RELATIONS : 'VISC_CIN1_CHAB' 'VISC_CIN2_CHAB'
!          RELATIONS : 'VMIS_CIN2_MEMO' 'VISC_CIN2_MEMO'
!
!     ARGUMENTS :
!       IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!       IN      TYPMOD  TYPE DE MODELISATION
!               IMATE   ADRESSE DU MATERIAU CODE
!               COMPOR  COMPORTEMENT DE L ELEMENT
!                       COMPOR(1) = RELATION DE COMPORTEMENT
!                       COMPOR(2) = NB DE VARIABLES INTERNES
!                       COMPOR(3) = TYPE DE DEFORMATION
!               CRIT    CRITERES  LOCAUX, EN PARTICULIER
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!               INSTAM  INSTANT T
!               INSTAP  INSTANT T+DT
!               DEPS    INCREMENT DE DEFORMATION TOTALE
!               SIGM    CONTRAINTE A T
!               VIM     VARIABLES INTERNES A T    + INDICATEUR ETAT T
!    ATTENTION  VIM     VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
!               OPTION     OPTION DE CALCUL A FAIRE
!                             'RIGI_MECA_TANG'> DSIDEP(T)
!                             'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
!                             'RAPH_MECA'     > SIG(T+DT)
!       OUT     SIGP    CONTRAINTE A T+DT
!               VIP     VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSIDEP  MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!               IRET    CODE RETOUR DE  L'INTEGRATION DE LA LDC
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ABSENCE DE CONVERGENCE
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
!               -----------------------------------------------------
!
!
!
    call nmchab(fami, kpg, ksp, ndim, typmod,&
                imate, compor, crit, instam, instap,&
                deps, sigm, vim, option, sigp,&
                vip, dsidep, codret)
end subroutine
