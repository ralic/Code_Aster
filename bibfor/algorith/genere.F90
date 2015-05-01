subroutine genere(r, dim, v, x)
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
    implicit none
!       POUR CHAQUE POINT DE LA DISCRETISATION FREQUENTIELLE CALCULE
!                   V = R*X
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getran.h"
#include "asterc/r8pi.h"
    integer :: dim, i, j
    real(kind=8) :: u, pi
    complex(kind=8) :: r(dim, dim), v(dim), phi, x(dim)
!     -----------------------------------------------------------------
    pi =r8pi()
    do 10 i = 1, dim
!
        call getran(u)
        u = u * 2.d0 * pi
        phi = dcmplx(0.d0,u)
        x(i) = exp(phi)
        v(i) = dcmplx(0.d0,0.d0)
10  end do
    do 20 i = 1, dim
        do 30 j = 1, dim
            v(i) = v(i) + r(i,j)*x(j)
30      continue
20  end do
end subroutine
