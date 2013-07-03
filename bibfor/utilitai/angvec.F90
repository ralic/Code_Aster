subroutine angvec(v1, v2, angle)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DE L'ANGLE ENTRE DEUX VECTEURS EN 3D.
!     IN : V1(3)
!     IN : V2(3)
!     OUT : ANGLE
!     ATTENTION L'ORDRE V1, V2 EST IMPORTANT POUR LE SIGNE DE ANGLE
!     L'ANGLE RETOURNE EST COMPRIS ENTRE -PI ET PI (ATAN2)
!
#include "asterfort/normev.h"
#include "asterfort/provec.h"
    real(kind=8) :: v1(3), v2(3), angle, cosphi, sinphi, v1v2(3), nv1, nv2
    real(kind=8) :: norm(3), nv3
    integer :: i
!
    call normev(v1, nv1)
    call normev(v2, nv2)
    cosphi = 0.d0
    do 10 i = 1, 3
        cosphi = cosphi + v1(i)*v2(i)
10  end do
    call provec(v1, v2, norm)
    call normev(norm, nv3)
    call provec(v1, v2, v1v2)
    sinphi = 0.d0
    do 20 i = 1, 3
        sinphi = sinphi + norm(i)*v1v2(i)
20  end do
    angle= atan2(sinphi,cosphi)
end subroutine
