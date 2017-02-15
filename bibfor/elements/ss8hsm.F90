subroutine ss8hsm(geom, para, matuu)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
! Solid-Shell 8 Hourglass Stabilization Matrix
!
!
! Hourglass stabilization Matrix evaluated at the center of the element
! in a corotional frame
!
!
! IN  geom       element node coordinates
! IN  para       material parameters E and nu
! INOUT  matuu  stiffness matrix completed with stabilization terms
!
#include "jeveux.h"
#include "asterfort/asvgam.h"
#include "asterfort/fbbbh8.h"
#include "asterfort/hgksca.h"
#include "asterfort/r8inir.h"
#include "asterfort/ss8rco.h"
#include "asterfort/utbtab.h"
#include "asterfort/utpvgl.h"
!
    real(kind=8), intent(in) :: geom(24)
    real(kind=8), intent(in) :: para(2)
    real(kind=8), intent(inout) :: matuu(*)
!
    integer :: i, j, k
    real(kind=8) :: xeloc(24)
    real(kind=8) :: xk(3,2), gam(4,8)
    real(kind=8), dimension(3,3) :: rr33
    real(kind=8), dimension(24,24) :: rr2424, kstablc, kstabgl, work2424
    real(kind=8) :: bbar(3,8)
!
! --------------------------------------------------------------------------------------------------
!
!      Evaluate rr33(3,3) transformation matrix from global to local
!      corotational frame
!
       call ss8rco(geom, rr33)
!
!      Expressing element nodal coordinates into corotational frame
!
       call utpvgl(8, 3, rr33, geom, xeloc)
!
!      Evaluate [B] bar matrix in corotational frame
!      Use of Flanagan-Belytschko 'mean' form
!
       call fbbbh8(xeloc, bbar)
!
!      Gamma ratio for assumed strain method
!
       call asvgam(2, xeloc, bbar, gam)
!
!      Evaluate hourglass stabilization matrix in corotational frame
!
       call hgksca(para, xeloc, gam, xk, kstablc)
!
!      Assemble rr2424(24,24) transformation matrix from r33(3,3)
!      to express stabilization matrix in global coordinate system
!
       call r8inir(576, 0.d0, rr2424, 1)
       do 10 k = 1, 8
          do 20 j = 1, 3
             do 30 i = 1, 3
                rr2424((k-1)*3+i,(k-1)*3+j) = rr33(i,j)
30           continue
20        continue
10     continue
!
!      Express hourglass stabilization matrix in global coordinate system
!
       call utbtab('ZERO', 24, 24, kstablc, rr2424, work2424, kstabgl)
!
!      Adding stabilization matrix to elementary stiffness matrix considering
!      symmetry
!
       k = 0
       do 40 i = 1, 24
          do 50 j = 1, i
             k = k + 1
             matuu(k) = matuu(k) + kstabgl(i,j)
50        continue
40     continue
!
end subroutine

