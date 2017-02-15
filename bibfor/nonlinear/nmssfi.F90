subroutine nmssfi(shb6, geom, idfde, ipoids, icoopg, pgl,&
                  ndim, nno, kpg,&
                  rigi,&
                  sigma, matsym, vectu)
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
! Non linear  Solid-Shell Forces Internal
! Evaluation of internal forces for solid-shell elements
! for GROT_GDEP deformation model.
!
! IN  shb6     true if element is a SHB6
! IN  geom     element node coordinates
! IN  idfde    index of the element shape function derivatives in parametric space
! IN  ipoids   element gauss point weight index
! IN  icoopg   pointer to Gauss point coordinates
! IN  pgl      transformation matrix from global to local solid-shell frame
! IN  ndim     dimension: 3D
! IN  nno      number of nodes in element
! IN  kpg      Gauss point index
! IN  rigi     true if option = RIGI_MECA_TANG or FULL_MECA
! IN  sigma    Cauchy stress at current T (with root square 2 for shear components)
! IN  matsym   true if tangent stiffness matrix is symmetric
! OUT vectu     internal forces
!
#include "asterf_types.h"
!
#include "asterfort/ss6bgl.h"
#include "asterfort/tpsivp_shb.h"
#include "asterfort/dfdmshb.h"
#include "asterfort/nmfdff.h"
#include "asterfort/nm3dfi_shb.h"
!
! aslint: disable=W1504
!
    aster_logical, intent(in) :: shb6
    integer, intent(in) :: nno
    real(kind=8), intent(in) :: geom(3,nno)
    integer, intent(in) :: idfde
    integer, intent(in) :: ipoids
    integer, intent(in) :: icoopg
    real(kind=8), intent(in) :: pgl(3,3)
    integer, intent(in) :: ndim
    integer, intent(in) :: kpg
    aster_logical, intent(in) :: rigi
    real(kind=8), intent(in) ::  sigma(6)
    aster_logical, intent(in) :: matsym
    real(kind=8), intent(inout) :: vectu(3,nno)
!
    integer :: i, j, n
    real(kind=8) :: poids, rac2, tmp
    real(kind=8) :: vff(1)
    real(kind=8) :: sigmag(6)
    real(kind=8), dimension(3,3) :: iden, invjac, fp
    real(kind=8) :: bg(6,3*nno), def(6,nno,3), dfdi(nno,3), pff(6,nno,nno)
!
! ......................................................................
!
! - Initializations
!
    data iden/1.d0, 0.d0, 0.d0,&
              0.d0, 1.d0, 0.d0,&
              0.d0, 0.d0, 1.d0/
!
    parameter (rac2 = sqrt(2.d0))
!
!   Evaluating the product 'def" as the transformation gradient
!   with shape function derivatives
!
    if (shb6) then
!      For SHB6 elements
!
       call ss6bgl(.true._1, kpg, geom, ipoids, idfde, icoopg, pgl, poids, bg)
!
       do 47 n = 1, nno
          j= 3*(n-1) + 1
          do 37 i = 1, 3
             def(1,n,i) = bg(1,j+i-1)
             def(2,n,i) = bg(2,j+i-1)
             def(3,n,i) = bg(3,j+i-1)
             def(4,n,i) = bg(4,j+i-1)
             def(5,n,i) = bg(5,j+i-1)
             def(6,n,i) = bg(6,j+i-1)
37        continue
47     continue
!
    else
!       For other SHB elements
!
        call dfdmshb(nno, kpg, ipoids, idfde, geom, invjac, poids,&
                    dfdi(1, 1), dfdi(1, 2), dfdi(1, 3))
!
!       For other SHB elements, one can directly re-use def from nmfdff
!       Setting fp to iden to comply with initial SHB code
        fp=iden
!
        call nmfdff(ndim, nno, .false._1, kpg, 0.d0,&
                    rigi, matsym, fp, vff, dfdi,&
                    def, pff)
!
!       Updating def so that it complies with initial SHB code
        do 46 n = 1, nno
           do 36 i = 1, 3
              def(4,n,i) = def(4,n,i)*rac2
! Glut \ Start
!             To comply with initial SHB code, need to invert 5th & 6th lines
!              def(5,n,i) = def(5,n,i)*rac2
!              def(6,n,i) = def(6,n,i)*rac2
              tmp        = def(6,n,i)*rac2
              def(6,n,i) = def(5,n,i)*rac2
              def(5,n,i) = tmp
! Glut \ End
36         continue
46      continue
!
    endif
!   For all SHB elements
!
!   Transforming stress from local to global coordinate system
    do 480 i = 1, 6
       sigmag(i) = sigma(i)
480 continue
!
!   Glut \ 1/2 \ Start
    tmp       = sigmag(5) 
    sigmag(5) = sigmag(6)
    sigmag(6) = tmp
!   Glut \ 1/2 \ End
!   Expressing stress tensor at T- from global to local frame
    call tpsivp_shb(pgl, sigmag, .false._1)
!   Glut \ 2/2 \ Start
    tmp       = sigmag(5) 
    sigmag(5) = sigmag(6)
    sigmag(6) = tmp
!   Glut \ 2/2 \ End
!
!   Evaluating internal forces in global frame
    call nm3dfi_shb(nno, poids, def, sigmag,vectu)
!
end subroutine
