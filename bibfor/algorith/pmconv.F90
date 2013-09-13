subroutine pmconv(r, rini, r1, inst, sigp,&
                  coef, iter, indimp, parcri, conver,&
                  itemax)
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
!-----------------------------------------------------------------------
!           OPERATEUR    CALC_POINT_MAT CALCUL D'ERREUR ET CONVERGENCE
!-----------------------------------------------------------------------
! IN   R      : RESIDU ACTUEL
! IN   RINI   : RESIDU INITIAL
! IN/OUT R1   : RESIDU PREMIERE ITERATION
! IN   INST   : INSTANT ACTUEL
! IN   SIGP   : CONTRAINTES ACTUELLES (POUR CONSTRUIRE LE DENOMINATEUR)
! IN   COEF   : COEF POUR ADIMENSIONNALISER LE PB
! IN   ITER   : NUMERO D'ITERATION
! IN   PARCRI : PARAMETRES DE CONVERGENCE GLOBAUX
! OUT  ITEMAX : .TRUE. SI ITERATION MAXIMUM ATTEINTE
! OUT  CONVER : .TRUE. SI CONVERGENCE REALISEE
!
!-----------------------------------------------------------------------
    implicit none
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/pmimpr.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    integer :: ind, indimp(6), i, itmax, iter, irela
    real(kind=8) :: inst, parcri(*)
    real(kind=8) :: r(12), rini(12), r1(12), sigp(6), coef, r8b(12)
    real(kind=8) :: ee, e1, e2, toler, e1ini, e2ini, er1, eini
    logical :: itemax, conver
    character(len=8) :: fonimp(6)
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!     VERIFICATION DE LA CONVERGENCE EN DY  ET RE-INTEGRATION ?
!-----------------------------------------------------------------------
    conver=.false.
    e1=0.d0
    e2=0.d0
    e1ini=0.d0
    e2ini=0.d0
    er1=0.d0
    itemax=.false.
    if (iter .eq. 1) then
!        SAUVEGARDE DE R(DY0) POUR TEST DE CONVERGENCE
        call dcopy(6, sigp, 1, r1(1), 1)
        call dscal(6, 1.d0/coef, r1(1), 1)
        call dcopy(6, r(7), 1, r1(7), 1)
        do 11 i = 1, 12
            er1 = max(er1, abs(r1(i)))
11      continue
        if (er1 .le. r8prem()) then
            ee=er1
            ind=4
            conver=.true.
            goto 9999
        endif
    endif
!
    do 101 i = 1, 6
        e1 = max(e1, abs(r(i)))
        e1ini = max(e1ini, abs(rini(i)))
        e1ini = max(e1ini, abs(r1(i)))
101  end do
    do 102 i = 7, 12
        e2 = max(e2, abs(r(i)))
        e2ini = max(e2ini, abs(rini(i)))
        e2ini = max(e2ini, abs(r1(i)))
102  end do
    eini=max(e1ini,e2ini)
!
!     TEST RELATIF OU ABSOLU
    if (parcri(2) .ne. r8vide()) then
        irela=1
    else
        irela=0
    endif
    if (irela .eq. 1) then
        toler=parcri(2)
        if (eini .gt. r8prem()) then
            e1=e1/eini
            e2=e2/eini
            ee=max(e1,e2)
            ind=3
        endif
    else
        toler=parcri(3)
        ee=max(e1,e2)
        ind=4
    endif
    itemax=.false.
    itmax=nint(parcri(1))
!
    if (iter .lt. itmax) then
! -      NON CONVERGENCE ITERATION SUIVANTE
        if (ee .gt. toler) then
            conver = .false.
        else
            conver = .true.
        endif
    else
! -      NB ITERATION MAXIMUM ATTEINT SANS CONVERGENCE
        conver=.false.
        itemax=.true.
        call utmess('I', 'COMPOR2_5')
    endif
9999  continue
!
    call pmimpr(ind, inst, indimp, fonimp, r8b,&
                iter, r8b, r8b, r8b, 1,&
                r8b, ee, eini)
end subroutine
