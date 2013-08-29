subroutine lc0055(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
!
    implicit none
! aslint: disable=W1504,W0104

! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

    character(*) :: fami
    integer :: kpg
    integer :: ksp
    integer :: ndim
    integer :: imate
    character(len=16) :: compor(*)
    real(kind=8) :: crit(*)
    real(kind=8) :: instam
    real(kind=8) :: instap
    real(kind=8) :: epsm(6)
    real(kind=8) :: deps(6)
    real(kind=8) :: sigm(6)
    real(kind=8) :: vim(*)
    character(len=16) :: option
    real(kind=8) :: angmas(3)
    real(kind=8) :: sigp(6)
    real(kind=8) :: vip(*)
    real(kind=8) :: tampon(*)
    character(len=8) :: typmod(*)
    integer :: icomp
    integer :: nvi
    real(kind=8) :: dsidep(6, 6)
    integer :: codret
! Declaration of integer type variables
#include "asterfort/lcmohr.h"
#include "asterfort/matini.h"
#include "asterfort/mctgel.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvarc.h"
    integer :: iret, icode(3)
!
! Declaration of real type variables
    real(kind=8) :: tp, tm, tref, rprops(3), r0
!
! Declaration of character variables
    character(len=8) :: nomres(3)
!
! Declaration of constant variables
    data r0   / 0.0d0 /
! ======================================================================
!
!     BUT: LOI DE COMPORTEMENT DE MOHR-COULOMB
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
!
!       OUT     SIGP    CONTRAINTE A T+DT
!               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!               CODRET
!
! ======================================================================
!
!       APPEL DE RCVARC POUR LA RECUPERATION DE LA TEMPERATURE
!       RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret)
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret)
!
    call matini(6, 6, r0, dsidep)
!
    if (option(1:14) .eq. 'RIGI_MECA_TANG') then
!
!       write(6,'(A)')
!       write(6,'(A)')'> LC0055 :: RIGI_MECA_TANG -> Elastic Matrix'
!
        nomres(1)= 'ALPHA   '
        nomres(2)= 'E       '
        nomres(3)= 'NU      '
        call rcvala(imate, ' ', 'ELAS', 0, '   ',&
                    tp, 3, nomres, rprops, icode,&
                    2)
!
        call mctgel(dsidep, rprops)
!
    else
!
!       write(6,'(A)')
!       write(6,'(A)')'> LC0055 :: entering LCMOHR'
        call lcmohr(ndim, typmod, imate,  option, tm,&
                    deps, sigm, sigp, vim, vip,&
                    dsidep, codret)
!
    endif
!
end subroutine
