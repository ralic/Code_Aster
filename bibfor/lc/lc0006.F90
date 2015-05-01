subroutine lc0006(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, neps,&
                  epsm, deps, nsig, sigm, vim,&
                  option, angmas, sigp, vip, nwkin,&
                  wkin, typmod, icomp, nvi, ndsde,&
                  dsidep, nwkout, wkout, codret)
! aslint: disable=W1504
    implicit none
#include "asterfort/eibex.h"
#include "asterfort/lcdsbe.h"
#include "asterfort/lceigv.h"
#include "asterfort/lcldsb.h"
#include "asterfort/rcvarc.h"
    integer :: imate, ndim, kpg, ksp
    integer :: neps, nsig, nwkin, nwkout, ndsde
    integer :: icomp, nvi
    integer :: codret
    real(kind=8) :: angmas(*), wkin(nwkin), wkout(nwkout)
    real(kind=8) :: crit(*), sigm(*)
    real(kind=8) :: instam, instap
    real(kind=8) :: epsm(*), deps(*)
    real(kind=8) :: sigp(*)
    real(kind=8) :: vim(*), vip(*)
    real(kind=8) :: dsidep(*)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(2)
    character(len=*) :: fami
!
!
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!.......................................................................
!
!     BUT: LOI D'ENDOMMAGEMENT ASYMETRIQUE DES BETONS,
!          AVEC EFFET DE RESTAURATION DE RIGIDITE
!
!          RELATION : 'ENDO_ISOT_BETON'
!
!       IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!               TYPMOD  TYPE DE MODELISATION
!               IMATE    ADRESSE DU MATERIAU CODE
!               COMP    COMPORTEMENT DE L ELEMENT
!                       COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
!                       COMP(2) = NB DE VARIABLES INTERNES
!                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                                 9 = METHODE IMPLEX
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
!                             'RIGI_MECA_IMPLEX' > DSIDEP(T), SIGEXTR
!               WKIN  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
!                       FIXEE EN DUR)
!       OUT     SIGP    CONTRAINTE A T+DT
!               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!
    real(kind=8) :: tp, tm, tref
    integer :: iret
!
!
!     NORMALEMENT, LES VERIF ONT ETE FAITES AVANT POUR INTERDIRE
!     SIMO_MIEHE
!     EXPLICITE
!
!
!     FORMULATION NON-LOCALE A GRADIENT D'ENDOMMAGEMENT
    if (typmod(2) .eq. 'GRADVARI') then
!
        call lceigv(fami, kpg, ksp, neps, imate,&
                    compor, epsm, deps, vim, option,&
                    sigp, vip, dsidep)
!
!     FORMULATION NON-LOCALE AVEC REGULARISATION DES DEFORMATIONS
    else if (typmod(2).eq.'GRADEPSI') then
!
        call lcdsbe(fami, ndim, typmod, imate, compor,&
                    epsm, deps, vim, option, sigp,&
                    vip, dsidep, wkout)
!     FORMULATION LOCALE
    else
!
        call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                    ksp, tm, iret)
        call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                    ksp, tp, iret)
        call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                    ksp, tref, iret)
!
        if (crit(2) .ne. 9) then
            call lcldsb(fami, kpg, ksp, ndim, typmod,&
                        imate, compor, epsm, deps, vim,&
                        tm, tp, tref, option, sigp,&
                        vip, dsidep, crit)
        else
            call eibex(fami, kpg, ksp, ndim, imate,&
                       compor, instam, instap, epsm, deps,&
                       vim, option, sigp, vip, dsidep,&
                       codret)
        endif
!
    endif
!
end subroutine
