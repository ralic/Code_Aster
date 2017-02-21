subroutine ss6bgl(resi, kpg, geom, ipoids, idfde, icoopg, pgl, jac, bg)
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
! Solid-Shell 6 B discretized gradient operator in GLobal coordinate system
!
!
! Solid-Shell 6 Discretized Gradient Operator for SHB6 element evaluated in
! a form to be used with a Hook matrix expressed in global coordinates.
!
! To define it, the following reasonning has been derived. Let's consider:
!      Dloc: 6x6 Hook matrix in local coordinates
!      Dglo: 6x6 Hook matrix in global coordinates
!      P: 6x6 transformation matrix from global to local coordinates
!      Bloc: 6x18 discretized gradient operator expressed in local coordinates
!      Bglo: 6x18 discretized gradient operator expressed in "global"
!            coordinates
!      R: 18x18 transformation matrix from global to local
!      w: Gauss point weight
!
! Gauss point stiffness contribution in global coordinates is expressed as
!   w * ( R^T . Bloc^T                .       Dloc      .           Bloc . R )
! = w * ( R^T . Bloc^T . ( P . P^(-1) )^T    .Dloc.  (P .  P^(-1)). Bloc . R )
! = w * ( R^T . Bloc^T . (P^(-1))^T ) . (P^T .Dloc.  P) . (P^(-1) . Bloc . R )
! = w * (       Bglo^T                .       Dglo      .           Bglo     )
!
! We have thus      Bglo = P^(-1) . Bloc . R
!
! One should keep in mind that this way of obtaining B in global coordinates
! is required because of the use of a correction factor on shear terms only
! meaningful in local coordinates.
! Would we have no correction factor, one could have expressed directly B in
! global coordinates using projected form of shape function derivatives
! expressed in global coordinate system (using node coordinates in global
! coordinates instead of local coordinate system in dfdmshb routine).
!
!
! IN  kpg      Gauss point number
! IN  geom     Node coordinates array
! IN  ipoids   Gauss point weight index
! IN  idfde    shape function derivatives index
! IN  icoopg   Gauss point coordinates index
! IN  pgl      (3,3) transformation matrix from global to local coordinate system
! IN  jac     Jacobian determinant
! OUT bg      B discretized gradient operator in global coordinate system
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/r8inir.h"
#include "blas/dgemm.h"
!
#include "asterfort/asvedh.h"
#include "asterfort/asvgam.h"
#include "asterfort/dfdmshb.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/hbbmp6.h"
#include "asterfort/mpsoqo.h"
#include "asterfort/utpvgl.h"
!
    aster_logical, intent(in) :: resi
    integer, intent(in) :: kpg
    real(kind=8), intent(in) :: geom(3,6)
    integer, intent(in) :: ipoids
    integer, intent(in) :: idfde
    integer, intent(in) :: icoopg
    real(kind=8), intent(in) :: pgl(3,3)
    real(kind=8), intent(out) :: jac
    real(kind=8), intent(out) :: bg(6,18)
!
    integer :: i, ino
    integer :: j, k
    integer :: nbinco, nbsig, nno
    real(kind=8) :: tmp
    real(kind=8) :: cfact, un, zero
    real(kind=8) :: xeloc(24), dh(4,3)
    real(kind=8), dimension(3,3) :: pglt, invjac, invjact
    real(kind=8), dimension(6,6) :: pglqoi
    real(kind=8) :: gam(4,8)
    real(kind=8) :: bl(6,18), btmp(6,18)
    real(kind=8) :: bhl(3,6)
    real(kind=8) :: rr(18,18)
!
! --------------------------------------------------------------------------------------------------
!
!   Some parameters
    parameter (un = 1.d0)
    parameter (zero = 0.d0)
    parameter (nbsig = 6)
    parameter (nno = 6)
    parameter (nbinco = 18)
!   SHB6 correction factor
    parameter (cfact = 0.45d0)
!
! - Compute matrix [B]: displacement -> strain (first order)
!
!   Transforming element nodal coordinates into shell-like coordinate system
    call utpvgl(6, 3, pgl, geom, xeloc)
!
!   Evaluate [B] matrix components in Hallquist form
!   A possible improvement might be achieved by evaluating these components
!   in the center of the elements instead of at position (0,0,0) (which is not
!   the center of the element).
    call hbbmp6(xeloc, bhl)
!
!   Gamma ratio for assumed strain method
    call asvgam(1, xeloc, bhl, gam)
!
!   dh vectors for assumed strain method
    call dfdmshb(6, kpg, ipoids, idfde, xeloc, invjac, jac)
!
!   Glut \ Start
!   To comply with initial code, it is required to use transposed matrix of invjac
    do 35 i = 1, nno
       do 36 j = 1, 3
          invjact(j,i) = invjac(i,j)
36     continue
35  continue
    do 45 i = 1, 3
       do 46 j = 1, 3
          invjac(j,i) = invjact(j,i)
46     continue
45  continue
!   Glut \ End
!
    call asvedh(1, zr(icoopg-3+kpg*3), invjac, dh)
!
!   Assembly of [B] matrix
    call r8inir(6*nbinco, 0.d0, bl, 1)
!
    do 55 ino = 1, nno
!   Glut \ Start
!   To comply with initial code, it is necessary to invert 5th &nd 6th lines
       j= 3*(ino-1) + 1
       bl(1,j)   =         bhl(1,ino) + dh(2,1)*gam(1,ino) + dh(1,1)*gam(2,ino)
       bl(2,j+1) =         bhl(2,ino) + dh(2,2)*gam(1,ino) + dh(1,2)*gam(2,ino)
       bl(3,j+2) =         bhl(3,ino) + dh(2,3)*gam(1,ino) + dh(1,3)*gam(2,ino)
       bl(4,j)   =         bhl(2,ino) + dh(2,2)*gam(1,ino) + dh(1,2)*gam(2,ino)
       bl(4,j+1) =         bhl(1,ino) + dh(2,1)*gam(1,ino) + dh(1,1)*gam(2,ino)
!       bl(5,j)   = cfact* (bhl(3,ino) + dh(2,3)*gam(1,ino) + dh(1,3)*gam(2,ino))
!       bl(5,j+2) = cfact* (bhl(1,ino) + dh(2,1)*gam(1,ino) + dh(1,1)*gam(2,ino))
       bl(6,j)   = cfact* (bhl(3,ino) + dh(2,3)*gam(1,ino) + dh(1,3)*gam(2,ino))
       bl(6,j+2) = cfact* (bhl(1,ino) + dh(2,1)*gam(1,ino) + dh(1,1)*gam(2,ino))
!       bl(6,j+1) = cfact* (bhl(3,ino) + dh(2,3)*gam(1,ino) + dh(1,3)*gam(2,ino))
!       bl(6,j+2) = cfact* (bhl(2,ino) + dh(2,2)*gam(1,ino) + dh(1,2)*gam(2,ino))
       bl(5,j+1) = cfact* (bhl(3,ino) + dh(2,3)*gam(1,ino) + dh(1,3)*gam(2,ino))
       bl(5,j+2) = cfact* (bhl(2,ino) + dh(2,2)*gam(1,ino) + dh(1,2)*gam(2,ino))
!   Glut \ End
55  continue
!
!   Assembly of rr(18,18) transformation matrix from pgl(3,3)
!   to express elementary stiffness matrix in global coordinate system
    call r8inir(324, 0.d0, rr, 1)
    do 65 k = 1, nno
       do 66 j = 1, 3
          do 67 i = 1, 3
             rr((k-1)*3+i,(k-1)*3+j) = pgl(i,j)
67        continue
66     continue
65  continue
!
!   Assembly of pglqoi(6,6) (pglqo^-1) transformation matrix modified into
!   fourth order transformation matrix from pglt(3,3)
    do 92 i = 1, 6
       do 82 j = 1, 6
          pglt(j,i) = pgl(i,j)
82     continue
92  continue
    call mpsoqo(pglt, pglqoi)
!
!   Glut \ Start
!   Glue required to comply with initial code when running B^T.sigma evaluation
!   in nmssgr and nmsspl and te0484 (FORC_NODA for SHB elements)
!   When removing, make sure to remove resi boolean variable, and in ss6bgl
!   interface and header file as well as in routines that call it
    if (resi) then
!   To comply with initial code, it is necessary to invert 5th & 6th lines
!   & 5th & 6th columns
!   Line inversion
    do 903 j = 1, 6
       tmp = pglqoi(5, j)
       pglqoi(5, j) = pglqoi(6, j)
       pglqoi(6, j) = tmp
903 continue
!   Column inversion
    do 904 j = 1, 6
       tmp = pglqoi(j, 5)
       pglqoi(j, 5) = pglqoi(j, 6)
       pglqoi(j, 6) = tmp
904 continue
    endif
!   Glut \ End
!
!   Assembly of pglqoi & rr with bl to have it (somehow) in global coordinates
!   btmp(6,18) <- pglqoi(6,6) . bl(6,18)
    call dgemm('N', 'N', nbsig, nbinco, nbsig,&
               un, pglqoi, nbsig, bl, nbsig,&
               zero, btmp, nbsig)
!   bg(6,18) <- btmp(6,18) . rr(18,18)
    call dgemm('N', 'N', nbsig, nbinco, nbinco,&
               un, btmp, nbsig, rr, nbinco,&
               zero, bg, nbsig)
!
end subroutine

