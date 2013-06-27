subroutine lc0015(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
! aslint: disable=W1504
    implicit none
    include 'asterfort/lcedga.h'
    include 'asterfort/lcgdpm.h'
    include 'asterfort/nzcifw.h'
    include 'asterfort/nzcizi.h'
    include 'asterfort/nzedga.h'
    include 'asterfort/nzgdzi.h'
    include 'asterfort/nzisfw.h'
    include 'asterfort/postsm.h'
    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi
    real(kind=8) :: crit(*), angmas(*)
    real(kind=8) :: instam, instap, tampon(*)
    real(kind=8) :: epsm(*), deps(*)
    real(kind=8) :: sigm(*), sigp(*)
    real(kind=8) :: vim(*), vip(*)
    real(kind=8) :: dsidep(*)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
! ----------------------------------------------------------------------
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
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!.......................................................................
!
!     BUT: LOI DE COMPORTEMENT EN METALLURGIE
!
!          RELATIONS :
!
!                 META_P_IL
!                 META_P_IL_PT
!                 META_P_IL_RE
!                 META_P_IL_PT_RE
!                 META_V_IL
!                 META_V_IL_PT
!                 META_V_IL_RE
!                 META_V_IL_PT_RE
!                 META_P_INL
!                 META_P_INL_PT
!                 META_P_INL_RE
!                 META_P_INL_PT_RE
!                 META_V_INL
!                 META_V_INL_PT
!                 META_V_INL_RE
!                 META_V_INL_PT_RE
!                 META_P_CL
!                 META_P_CL_PT
!                 META_P_CL_RE
!                 META_P_CL_PT_RE
!                 META_V_CL
!                 META_V_CL_PT
!                 META_V_CL_RE
!                 META_V_CL_PT_RE
!                 META_LEMA_ANI
!
!
!
!
!       IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!               TYPMOD  TYPE DE MODELISATION
!               IMATE    ADRESSE DU MATERIAU CODE
!               COMPOR    COMPORTEMENT DE L ELEMENT
!                     COMPOR(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
!                     COMPOR(2) = NB DE VARIABLES INTERNES
!                     COMPOR(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
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
!               INSTAM   INSTANT T
!               INSTAP   INSTANT T+DT
!               EPSM   DEFORMATION TOTALE A T
!               DEPS   INCREMENT DE DEFORMATION TOTALE
!               SIGM    CONTRAINTE A T
!               VIM    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!    ATTENTION  VIM    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
!               OPTION     OPTION DE CALCUL A FAIRE
!                             'RIGI_MECA_TANG'> DSIDEP(T)
!                             'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
!                             'RAPH_MECA'     > SIG(T+DT)
!               TAMPON  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
!                       FIXEE EN DUR)
!               ANGMAS  ANGLES DU REPERE DU MATERIAU (AFFE_CARA_ELEM)
!       OUT     SIGP    CONTRAINTE A T+DT
!               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!.......................................................................
!               CODRET
!
!
!
!
!     NORMALEMENT, LES VERIF ONT ETE FAITES AVANT POUR INTERDIRE
!     SIMO_MIEHE AVEC ECROUISSAGE CINEMATIQUE
!     META_LEMA_ANI AVEC ACIER
!
!     DEFORMATIONS DE SIMO_MIEHE
    if (compor(3) .eq. 'SIMO_MIEHE') then
!
        if (compor(8) .eq. 'ACIER') then
!
            call lcgdpm(fami, kpg, ksp, ndim, imate,&
                        compor, crit, instam, instap, epsm,&
                        deps, sigm, vim, option, sigp,&
                        vip, dsidep, codret)
!
        else if (compor(8).eq.'ZIRC') then
!
            call nzgdzi(fami, kpg, ksp, ndim, imate,&
                        compor, crit, instam, instap, epsm,&
                        deps, sigm, vim, option, sigp,&
                        vip, dsidep, codret)
!
        endif
        call postsm(option, epsm, deps, sigm, sigp,&
                    dsidep)
!
!     PETITES DEFORMATIONS
    else
!
!       ECROUISSAGE ISOTROPE
        if (compor(1)(8:8) .eq. 'I') then
!
            if (compor(8) .eq. 'ACIER') then
!
                call nzisfw(fami, kpg, ksp, ndim, imate,&
                            compor, crit, instam, instap, epsm,&
                            deps, sigm, vim, option, sigp,&
                            vip, dsidep, codret)
!
            else if (compor(8).eq.'ZIRC') then
!
                call nzedga(fami, kpg, ksp, ndim, imate,&
                            compor, crit, instam, instap, epsm,&
                            deps, sigm, vim, option, sigp,&
                            vip, dsidep, codret)
!
            endif
!
!       ECROUISSAGE CINEMATIQUE
        else if (compor(1)(8:8).eq.'C') then
!
            if (compor(8) .eq. 'ACIER') then
!
                call nzcifw(fami, kpg, ksp, ndim, imate,&
                            compor, crit, instam, instap, epsm,&
                            deps, sigm, vim, option, sigp,&
                            vip, dsidep, codret)
!
            else if (compor(8).eq.'ZIRC') then
!
                call nzcizi(fami, kpg, ksp, ndim, imate,&
                            compor, crit, instam, instap, epsm,&
                            deps, sigm, vim, option, sigp,&
                            vip, dsidep, codret)
!
            endif
!
!       ECROUISSAGE ANISOTROPE
        else if (compor(1).eq.'META_LEMA_ANI') then
!
            call lcedga(fami, kpg, ksp, ndim, imate,&
                        crit, typmod, instam, instap, tampon,&
                        deps, sigm, vim, option, sigp,&
                        vip, dsidep, codret)
!
        endif
!
    endif
!
end subroutine
