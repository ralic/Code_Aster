!> Shape function derivatives for 3D elements
!>
!
!> Evaluation of shape function derivatives for a given 3D element at a given gauss point.
!> Jacobian & inverse jacobian matrix only can be retrieved with this routine.
!>
!
!> @param[in]  nno      number of nodes in element
!> @param[in]  ipg      current gauss point index
!> @param[in]  ipoids   element gauss point weight index (gauss point family of the element)
!> @param[in]  idfde    index of the element shape function derivatives in parametric space
!> @param[in]  coor     element node coordinates
!> @param[out] invjac   inverse jacobian matrix
!> @param[out] jac      jacobian (determinant of jacobian matrix)
!> @param[out] dfdx     (optional) shape function derivatives with respect to x axis
!> @param[out] dfdy     (optional) shape function derivatives with respect to y axis
!> @param[out] dfdz     (optional) shape function derivatives with respect to z axis
!
subroutine dfdmshb(nno, ipg, ipoids, idfde, coor,&
                  invjac, jac, dfdx, dfdy, dfdz)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/matini.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"
!
    integer, intent(in) :: nno, ipg, ipoids, idfde
    real(kind=8), intent(in) :: coor(*)
    real(kind=8), intent(out) ::  invjac(3,3)
    real(kind=8), intent(out) ::  jac
    real(kind=8), optional, intent(out) :: dfdx(*)
    real(kind=8), optional, intent(out) :: dfdy(*)
    real(kind=8), optional, intent(out) :: dfdz(*)
!
    integer :: i, ii, k, iadzi, iazk24
    real(kind=8) :: poids, g(3,3), unsjac
    real(kind=8) :: de, dn, dk
    character(len=8) :: nomail
!
! ......................................................................
!
    poids = zr(ipoids+ipg-1)
    k = 3*nno*(ipg-1)
!
    call matini(3, 3, 0.d0, g)
!
    do 1 i = 1, nno
       ii = 3*(i-1)
       de = zr(idfde-1+k+ii+1)
       dn = zr(idfde-1+k+ii+2)
       dk = zr(idfde-1+k+ii+3)
       g(1,1) = g(1,1) + coor(ii+1) * de
       g(2,1) = g(2,1) + coor(ii+1) * dn
       g(3,1) = g(3,1) + coor(ii+1) * dk
       g(1,2) = g(1,2) + coor(ii+2) * de
       g(2,2) = g(2,2) + coor(ii+2) * dn
       g(3,2) = g(3,2) + coor(ii+2) * dk
       g(1,3) = g(1,3) + coor(ii+3) * de
       g(2,3) = g(2,3) + coor(ii+3) * dn
       g(3,3) = g(3,3) + coor(ii+3) * dk
1   continue
!
    invjac(1,1) = g(2,2) * g(3,3) - g(2,3) * g(3,2)
    invjac(2,1) = g(3,1) * g(2,3) - g(2,1) * g(3,3)
    invjac(3,1) = g(2,1) * g(3,2) - g(3,1) * g(2,2)
    invjac(1,2) = g(1,3) * g(3,2) - g(1,2) * g(3,3)
    invjac(2,2) = g(1,1) * g(3,3) - g(1,3) * g(3,1)
    invjac(3,2) = g(1,2) * g(3,1) - g(3,2) * g(1,1)
    invjac(1,3) = g(1,2) * g(2,3) - g(1,3) * g(2,2)
    invjac(2,3) = g(2,1) * g(1,3) - g(2,3) * g(1,1)
    invjac(3,3) = g(1,1) * g(2,2) - g(1,2) * g(2,1)
!
    jac = g(1,1)*invjac(1,1) + g(1,2)*invjac(2,1) + g(1,3)*invjac(3,1)
    unsjac = 1.d0 / jac
!
    if (abs(jac) .le. 1.d0/r8gaem()) then
       call tecael(iadzi, iazk24)
       nomail= zk24(iazk24-1+3)(1:8)
       call utmess('F', 'ALGORITH2_59', sk=nomail)
    endif
!
    if (present(dfdx)) then
       ASSERT(present(dfdy))
       ASSERT(present(dfdz))
       do 2 i = 1, nno
          ii = 3*(i-1)
          de = zr(idfde-1+k+ii+1)
          dn = zr(idfde-1+k+ii+2)
          dk = zr(idfde-1+k+ii+3)
          dfdx(i) = (invjac(1,1)*de + invjac(1,2)*dn + invjac(1,3)*dk) * unsjac
          dfdy(i) = (invjac(2,1)*de + invjac(2,2)*dn + invjac(2,3)*dk) * unsjac
          dfdz(i) = (invjac(3,1)*de + invjac(3,2)*dn + invjac(3,3)*dk) * unsjac
2     continue
    else
      ASSERT(.not.present(dfdy))
      ASSERT(.not.present(dfdz))
    endif
!
    do 3 i = 1, 3
       invjac(1,i) = invjac(1,i) * unsjac
       invjac(2,i) = invjac(2,i) * unsjac
       invjac(3,i) = invjac(3,i) * unsjac
3   continue
!
    jac = abs(jac)*poids
!
end subroutine
