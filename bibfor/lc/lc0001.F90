subroutine lc0001(fami, kpg, ksp, ndim, imate,&
                  neps, deps, nsig, sigm, option,&
                  angmas, sigp, vip, typmod, ndsde,&
                  dsidep, codret)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/nmelas.h"
#include "asterfort/nmorth.h"
#include "asterfort/rccoma.h"
    integer :: imate, ndim, kpg, ksp, codret, icodre
    integer :: neps, nsig, ndsde
    real(kind=8) :: angmas(3), deps(neps), sigm(nsig), sigp(nsig)
    real(kind=8) :: vip(1), dsidep(ndsde)
    character(len=16) :: option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    character(len=16) :: mcmate
!
! ----------------------------------------------------------------------
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
!     BUT: LOI DE COMPORTEMENT ELASTIQUE
!
!          RELATIONS : 'ELAS'
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
!                                 9 = methode IMPLEX
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
!               OPTION     OPTION DE CALCUL A FAIRE
!                             'RIGI_MECA_TANG'> DSIDEP(T)
!                             'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
!                             'RAPH_MECA'     > SIG(T+DT)
!                             'RIGI_MECA_IMPLEX' > DSIDEP(T), SIGEXTR
!               WKIN  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
!                       FIXEE EN DUR)
!               ANGMAS
!       OUT     SIGP    CONTRAINTE A T+DT
!               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!.......................................................................
!               CODRET
!
!
!
!     RECUPERATION DE MCMATER (APPELE A TORT 'PHENOMENE')
    call rccoma(imate, 'ELAS', 1, mcmate, icodre)
    ASSERT(icodre.eq.0)
!
    if (mcmate .eq. 'ELAS') then
!
        call nmelas(fami, kpg, ksp, ndim, typmod,&
                    imate, deps, sigm, option, sigp,&
                    vip, dsidep, codret)
!
    else if (mcmate.eq.'ELAS_ORTH'.or.mcmate.eq.'ELAS_ISTR') then
!
        call nmorth(fami, kpg, ksp, ndim, mcmate,&
                    imate, 'T', deps, sigm, option,&
                    angmas, sigp, vip(1), dsidep)
    endif
!
!
end subroutine
