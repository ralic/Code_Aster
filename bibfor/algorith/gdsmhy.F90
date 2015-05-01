subroutine gdsmhy(je, e)
!
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
    implicit none
#include "asterc/r8maem.h"
#include "asterfort/lcdete.h"
#include "asterfort/lcdevi.h"
#include "asterfort/zerop3.h"
#include "blas/ddot.h"
    real(kind=8) :: je, e(6)
!
! ----------------------------------------------------------------------
!            GRANDES DEFORMATIONS SIMO-MIEHE OU CANO-LORENTZ
!         CORRECTION HYDROSTATIQUE DE LA DEFORMATION ELASTIQUE
! ----------------------------------------------------------------------
! IN  JE      DETERMINANT DE E CIBLE
! VAR E       DEFORMATION ELASTIQUE (XX,YY,ZZ,RAC2*XY,RAC2*XZ,RAC2*YZ)
! ----------------------------------------------------------------------
    integer :: nrac, i, iopt
    real(kind=8) :: dve(6), eh, eqe2, detdve, p0, p1, p2, rac(3), dismin
! ----------------------------------------------------------------------
!
!
!
    call lcdevi(e, dve)
    eqe2 = 1.5d0 * ddot(6,dve,1,dve,1)
    call lcdete(dve, detdve)
    eh = (e(1)+e(2)+e(3))/3.d0
!
    p0 = 8*detdve + je**2
    p1 = - 4.d0/3.d0*eqe2
    p2 = 0
    call zerop3(p2, p1, p0, rac, nrac)
    do 10 i = 1, nrac
        rac(i) = (rac(i)+1)/2
10  end do
!
    dismin = r8maem()
    do 20 i = 1, nrac
        if (abs(rac(i)-eh) .lt. dismin) then
            iopt = i
            dismin = abs(rac(i)-eh)
        endif
20  end do
    eh = rac(iopt)
!
    do 30 i = 1, 3
        e(i) = eh + dve(i)
30  end do
!
end subroutine
