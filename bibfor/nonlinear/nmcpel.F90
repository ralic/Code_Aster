subroutine nmcpel(fami, kpg, ksp, poum, ndim,&
                  typmod, angmas, imate, compor, crit,&
                  option, eps, sig, vi, dsidep,&
                  codret)
    implicit none
#include "asterfort/hypela.h"
#include "asterfort/nmelnl.h"
#include "asterfort/nmorth.h"
#include "asterfort/rccoma.h"
#include "asterfort/utmess.h"
    integer :: kpg, ksp, ndim, imate, codret
    real(kind=8) :: angmas(3), crit(3), eps(6), sig(6), vi(*), dsidep(6, 6)
    character(len=*) :: fami, poum
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), option
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     LOIS DE COMPORTEMENT HYPER-ELASTIQUES (ELEMENTS ISOPARAMETRIQUES)
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT  (1) = TYPE DE RELATION COMPORTEMENT
!                             (2) = NB VARIABLES INTERNES / PG
!                             (3) = HYPOTHESE SUR LES DEFORMATIONS
!                             (4) = COMP_ELAS (OU COMP_INCR)
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
!                               (1) = NB ITERATIONS MAXI A CONVERGENCE
!                                     (ITER_INTE_MAXI == ITECREL)
!                               (2) = TYPE DE JACOBIEN A T+DT
!                                     (TYPE_MATR_COMP == MACOMP)
!                                     0 = EN VITESSE     >SYMETRIQUE
!                                     1 = EN INCREMENTAL >NON-SYMETRIQUE
!                               (3) = VALEUR TOLERANCE DE CONVERGENCE
!                                     (RESI_INTE_RELA == RESCREL)
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! IN  EPS     : DEFORMATIONS
! OUT SIG     : CONTRAINTES
! OUT VI(1)     : VARIABLE INTERNE
! OUT DSIDEP  : RIGIDITE TANGENTE
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX YY ZZ SQRT(2)*XY SQRT(2)*XZ SQRT(2)*YZ
!
! ----------------------------------------------------------------------
!
    integer :: i, icodre(1)
    real(kind=8) :: sigm(6), energi(2), r8bid
    character(len=16) :: phenom
    real(kind=8) :: crit_loca(13)
!
!-----------------------------------------------------------------------
!
    codret = 0
!
    call rccoma(imate, 'ELAS', 1, phenom, icodre(1))
!
! - ORTHOTROPIE OU ISOTROPIE TRANSVERSE LINEAIRE
    if (icodre(1) .eq. 0 .and. (phenom(1:6).eq.'ELAS_O'.or.phenom(1:6).eq.'ELAS_I')) then
        if (compor(1)(1:5) .eq. 'ELAS ') then
            do 30 i = 1, 6
                sigm(i)=0.d0
30          continue
            call nmorth(fami, kpg, ksp, ndim, phenom,&
                        imate, poum, eps, sigm, option,&
                        angmas, sig, r8bid, dsidep)
        else
            call utmess('F', 'ALGORITH7_7')
        endif
!
!   ELASTICITE NON LINEAIRE ISOTROPE
        else if (compor(1)(1:5) .eq. 'ELAS ' .or. compor(1)(1:14) .eq.&
    'ELAS_VMIS_LINE' .or. compor(1)(1:14) .eq. 'ELAS_VMIS_PUIS' .or.&
    compor(1)(1:14) .eq. 'ELAS_VMIS_TRAC' ) then
        crit_loca(1) = crit(1)
        crit_loca(2) = crit(2)
        crit_loca(3) = crit(3)
        crit_loca(8) = crit(3)
        crit_loca(9) = 10
        call nmelnl(fami, kpg, ksp, 0, poum, ndim,&
                    typmod, imate, compor, crit_loca, option,&
                    eps, sig, vi(1), dsidep, energi)
!
!    LOI ELASTIQUE POUR MODELE SIGNORINI
    else if (compor(1)(1:10).eq. 'ELAS_HYPER') then
        if (compor(3) .ne. 'GROT_GDEP') then
            call utmess('F', 'ALGORITH6_96')
        endif
        crit_loca(1) = crit(1)
        crit_loca(2) = crit(2)
        crit_loca(3) = crit(3)
        crit_loca(8) = crit(3)
        crit_loca(9) = 10
        call hypela(fami, kpg, ksp, poum, ndim,&
                    typmod, imate, compor, crit_loca, eps,&
                    sig, dsidep, codret)
!
    else
        call utmess('F', 'ALGORITH7_8')
    endif
end subroutine
