subroutine angrot(v1, v2, axe, angle)
    implicit none
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
! ======================================================================
!     CALCUL DE L'ANGLE ENTRE DEUX VECTEURS EN 3D, SUIVANT UN AXE (SENS)
!     IN : V1(3)
!     IN : V2(3)
!     IN : AXE(3) : VECTEUR COLINEAIRE A V1 X V2
!     OUT : ANGLE
!     ATTENTION L'ORDRE V1, V2 EST IMPORTANT POUR LE SIGNE DE ANGLE
!     L'ANGLE RETOURNE EST COMPRIS ENTRE -PI ET PI (ATAN2)
!
#include "asterc/r8pi.h"
#include "asterfort/angvec.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: v1(3), v2(3), angle, v1v2(3), nv3
    real(kind=8) :: verif(3), pscal, axe(3), axe2(3)
    real(kind=8) :: epsi
    integer :: i
!
    epsi=1.d-10
    call angvec(v1, v2, angle)
    if (abs(angle) .le. 1.d-6) then
        goto 9999
    endif
    if (abs(angle-r8pi()) .le. 1.d-6) then
        goto 9999
    endif
    do 10 i = 1, 3
        axe2(i)=axe(i)
10  end do
    call normev(axe2, nv3)
    if (nv3 .lt. epsi) then
        call u2mess('F', 'UTILITAI_5')
    endif
    call provec(v1, v2, v1v2)
    call provec(v1v2, axe2, verif)
    call normev(verif, nv3)
    if (nv3 .gt. epsi) then
        call u2mess('F', 'UTILITAI_6')
    endif
    call normev(v1v2, nv3)
    if (nv3 .lt. epsi) then
        pscal=0.d0
        do 30 i = 1, 3
            pscal=pscal+v1(i)*v2(i)
30      continue
        if (pscal .gt. 0.d0) then
            angle=0.d0
        else
            angle=r8pi()
        endif
    else
        pscal = 0.d0
        do 20 i = 1, 3
            pscal = pscal + axe2(i)*v1v2(i)
20      continue
        if (abs(abs(pscal)-1.d0) .gt. epsi) then
            call u2mess('F', 'UTILITAI_7')
        endif
        angle= angle*pscal
    endif
9999  continue
end subroutine
