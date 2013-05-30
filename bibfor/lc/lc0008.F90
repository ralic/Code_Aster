subroutine lc0008(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
    implicit none
    include 'asterfort/lcmaza.h'
    include 'asterfort/lcmzcp.h'
    include 'asterfort/lcmzge.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/u2mesk.h'
    integer :: imate, ndim, kpg, ksp
    integer :: icomp, nvi
    integer :: codret
    real(kind=8) :: angmas(*), tampon(*)
    real(kind=8) :: crit(*), instam, instap, sigm(*)
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigp(6)
    real(kind=8) :: vim(*), vip(*)
    real(kind=8) :: dsidep(6, 6)
    character(len=16) :: compor(6), option
    character(len=8) :: typmod(2)
    character(len=*) :: fami
!
! TOLE CRP_21
! ======================================================================
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
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!.......................................................................
!
!     BUT: LOI D'ENDOMMAGEMENT DE MAZARS
!
!          RELATION : 'MAZARS'
!
!       IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!               TYPMOD  TYPE DE MODELISATION
!               IMATE    ADRESSE DU MATERIAU CODE
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
!                                 (ITER_INTE_PAS == ITEDEC)
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!               EPSM   DEFORMATION TOTALE A T
!               DEPS   INCREMENT DE DEFORMATION TOTALE
!               VIM    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!    ATTENTION  VIM    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
!               OPTION     OPTION DE CALCUL A FAIRE
!                             'RIGI_MECA_TANG'> DSIDEP(T)
!                             'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
!                             'RAPH_MECA'     > SIG(T+DT)
!               TAMPON  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
!                       FIXEE EN DUR)
!       OUT     SIGP    CONTRAINTE A T+DT
!               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!               CODRET    CODE RETOUR
!
    real(kind=8) :: tp, tm, tref
    integer :: iret
    logical :: cplane, coup
!
!     NORMALEMENT, LES VERIF ONT ETE FAITES AVANT POUR INTERDIRE
!     GRAD_VARI
!     SIMO_MIEHE
!     EXPLICITE
!     GRAD_EPSI ET AXIS
!
!     FORMULATION NON-LOCALE AVEC REGULARISATION DES DEFORMATIONS
    if (typmod(2) .eq. 'GRADEPSI') then
        call lcmzge(fami, kpg, ksp, ndim, typmod,&
                    imate, epsm, deps, vim, option,&
                    sigp, vip, dsidep, tampon)
!
!     FORMULATION LOCALE
    else
        call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                    ksp, tm, iret)
        call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                    ksp, tp, iret)
        call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                    ksp, tref, iret)
!
        cplane = (typmod(1).eq.'C_PLAN  ').and. (compor(1)(1:9).eq.'MAZARS_GC')
        if (cplane) then
!           PAS DE COUPLAGE UMLV EN CP
            coup = (option(6:9).eq.'COUP')
            if (coup) call u2mesk('F', 'ALGORITH4_10', 1, compor(1))
            call lcmzcp(fami, kpg, ksp, ndim, imate,&
                        epsm, deps, vim, tm, tp,&
                        tref, option, sigp, vip, dsidep)
        else
            call lcmaza(fami, kpg, ksp, ndim, typmod,&
                        imate, compor, epsm, deps, vim,&
                        tm, tp, tref, option, sigp,&
                        vip, dsidep)
        endif
    endif
end subroutine
