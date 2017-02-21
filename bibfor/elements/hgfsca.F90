subroutine hgfsca(geom, para, depl, fstm, vectu, fstp)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
! HOURGLASS FORCE STABILIZATION VECTOR FROM COMBESCURE AND ABED-MERAIM
! ====================================================================
!
! Evaluation of force stabilization vector to prevent hourglass in SHB8
! element
!
! IN     geom        Node coordinates array
! IN     para        Material parameters nu and E
! IN     depl        Increment of displacement at T+
! IN     fstm        Stabilization forces at T-
! INOUT  vectu   Internal forces (B^T.sigma) corrected with stabilization forces
! OUT    fstp       Stabilization forces at T+ (kept for next increment)
!
#include "blas/dgemm.h"
#include "asterfort/asvgam.h"
#include "asterfort/fbbbh8.h"
#include "asterfort/hgksca.h"
#include "asterfort/ss8rco.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
!
    real(kind=8), intent(in) :: geom(*)
    real(kind=8), intent(in) :: para(2)
    real(kind=8), intent(in) :: depl(*)
    real(kind=8), intent(in) :: fstm(3,4)
    real(kind=8), intent(inout) :: vectu(3,8)
    real(kind=8), intent(out) :: fstp(3,4)
!
    integer :: i, j
    real(kind=8) ::      b(3,8),   epsen(3,4), disploc(3,8)
    real(kind=8) :: fhgloc(3,8),  fhgglo(3,8),    gam(4, 8)
    real(kind=8) ::   xmglo(24),    xpglo(24),   x12glo(24)
    real(kind=8) ::   xmloc(24),    xploc(24),   x12loc(24)
    real(kind=8) ::   rr1(3, 3),    rr2(3, 3),   rr12(3, 3)
    real(kind=8) ::     xk(3,2)
!
! ......................................................................
!
!   Evaluation of nodal coordinates at current increment, beginning of increment
!   and mid-step
!
    do 10 i = 1, 24
       xpglo(i)  = geom(i)
       xmglo(i)  = geom(i) - depl(i)
       x12glo(i) = geom(i) - 0.5d0*depl(i)
10  continue
!
!   Evaluate transformation matrices from global to local corotational frame
!   for each configurations
!
    call ss8rco(xpglo,  rr2)
    call ss8rco(xmglo,  rr1)
    call ss8rco(x12glo, rr12)
!
!   Expressing element nodal coordinates into corotational frames
!
    call utpvgl(8, 3,  rr2,  xpglo,  xploc)
    call utpvgl(8, 3,  rr1,  xmglo,  xmloc)
    call utpvgl(8, 3, rr12, x12glo, x12loc)
!
!   Evaluate [B] bar matrix in corotational frame
!   Use of Flanagan-Belytschko 'mean' form
!
    call fbbbh8(x12loc, b)
!
!   Gamma ratio for assumed strain method at mid-step
!
    call asvgam(2, x12loc, b, gam)
!
!   Evaluate hourglass stabilization matrix components in corotational frame
!   at mid-step
!
    call hgksca(para, x12loc, gam, xk)
!
!   Evaluate displacement at mid-step in their related corotational frames
!
    do 20 i = 1, 8
       do 30 j = 1, 3
          disploc(j,i) = xploc((i-1)*3+j) - xmloc((i-1)*3+j)
30     continue
20  continue
!
!   Evaluate generalized strains
!
!   epsen(3,4) <- disploc(3,8) . gs(8,4)
    call dgemm('N', 'T', 3, 4, 8,&
               1.d0, disploc, 3, gam, 4,&
               0.d0, epsen, 3)
!
!   Update stabilization forces
!
    fstp(1,1) = fstm(1,1)
    fstp(2,2) = fstm(2,2)
    fstp(3,3) = fstm(3,3)
!
    fstp(1,2) = fstm(1,2)
    fstp(1,3) = fstm(1,3) + xk(1,1)*epsen(1,3)
    fstp(1,4) = fstm(1,4) + xk(1,2)*epsen(1,4)
!
    fstp(2,1) = fstm(2,1)
    fstp(2,3) = fstm(2,3) + xk(2,1)*epsen(2,3)
    fstp(2,4) = fstm(2,4) + xk(2,2)*epsen(2,4)
!
    fstp(3,1) = fstm(3,1)
    fstp(3,2) = fstm(3,2)
    fstp(3,4) = fstm(3,4) + xk(3,2)*epsen(3,4)
!
!   Evaluate hourglass stabilization forces
!
!   fhgloc(3,8) <- fstp(3,4) . gs^T(8,4)
    call dgemm('N', 'N', 3, 8, 4,&
               1.d0, fstp, 3, gam, 4,&
               0.d0, fhgloc, 3)
!
!   Expressing hourglass stabilization forces into global frame
!
    call utpvlg(8, 3, rr12, fhgloc, fhgglo)
!
!   Update B^T . sigma
!
    do 40 i = 1, 8
       do 41 j = 1, 3
          vectu(j,i) = vectu(j,i) + fhgglo(j,i)
41     continue
40  continue
!
end subroutine
