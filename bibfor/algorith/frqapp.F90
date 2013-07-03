subroutine frqapp(dt, neq, dep1, dep2, acc1,&
                  acc2, vmin, freq)
!
    implicit none
#include "asterc/r8depi.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
    integer :: i
    real(kind=8) :: dep1(*), dep2(*), acc1(*), acc2(*), vmin(*)
    real(kind=8) :: freq, dt, a, b, temp
!-----------------------------------------------------------------------
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
! CALCUL DE LA FREQUENCE APPARENTE POUR LE CALCUL AVEC PAS ADAPTATIF
! ----------------------------------------------------------------------
! IN  : DT     : PAS DE TEMPS
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : DEP1   : DEPLACEMENTS AU TEMPS T
! IN  : DEP2   : DEPLACEMENTS AU TEMPS T+DT
! IN  : ACC1   : ACCELERATIONS AU TEMPS T
! IN  : ACC2   : ACCELERATIONS AU TEMPS T+DT
! IN  : VMIN   : VITESSES DE REFERENCE
! OUT : FREQ   : FREQUENCE APPARENTE
! ------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: neq
    real(kind=8) :: dd, dmin, eps, epsmi
    real(kind=8) :: tt
!-----------------------------------------------------------------------
    temp = r8prem()
    eps = r8prem()
    epsmi = r8miem()
    do 10 i = 1, neq
        a = acc2(i)-acc1(i)
        dmin = vmin(i)*dt
        dd = abs(dep2(i)-dep1(i))
        if (dmin .ge. dd) then
            b = dmin
        else
            b = dd
        endif
!       B = MAX(VMIN(I)*DT,ABS(DEP2(I)-DEP1(I)))
!       B DEVRAIT ETRE TOUJOURS NON NUL
        if (b .le. epsmi) b=eps
        tt = abs(a/b)
        if (temp .le. tt) temp = tt
!       TEMP = MAX(TEMP,ABS(A/B))
10  end do
    freq = sqrt(temp)/r8depi()
end subroutine
