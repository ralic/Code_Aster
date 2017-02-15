subroutine nmsstg(shb6, geom, idfde, ipoids, icoopg, pgl, para,&
                  ndim, nno, poids, kpg,&
                  dfdi, option,&
                  dsidep, sign,&
                  sigma, matsym, matuu)
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
! Non linear Solid-Shell Tangent stiffness matrix
!
!
! Evaluation of tangent stiffness matrix for solid-shell elements
! for GROT_GDEP deformation model.
!
!
! IN  shb6     true if element is a SHB6
! IN  geom     element node coordinates
! IN  idfde    index of the element shape function derivatives in parametric space
! IN  ipoids   element gauss point weight index
! IN  icoopg   pointer to Gauss point coordinates
! IN  pgl      transformation matrix from global to local solid-shell frame
! IN  para     Young's modulus and Poisson's ratio for SHB Hook matrix
! IN  ndim     dimension: 3D
! IN  nno      number of nodes in element
! INOUT  poids    Gauss point weight
! IN  kpg      Gauss point index
! IN  dfdi     shape function derivatives
! IN  option   elementary computation to evaluate
! IN  dsidep   terms for computation of modified Hook's matrix
! IN  sign     Cauchy stress at T- (with root square 2 for shear components)
! IN  sigma    Cauchy stress at current T (with root square 2 for shear components)
! IN  matsym   true if tangent stiffness matrix is symmetric
! INOUT  matuu     tangent stiffness matrix
!
#include "asterf_types.h"
!
#include "asterfort/mhomss.h"
#include "asterfort/mpsoqo.h"
#include "asterfort/nmfdff.h"
#include "asterfort/nmgrt3_shb.h"
#include "asterfort/ss6bgl.h"
#include "asterfort/tpsivp_shb.h"
#include "asterfort/utbtab.h"
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
    real(kind=8), intent(in) :: para(2)
    integer, intent(in) :: ndim
    real(kind=8), intent(inout) :: poids
    integer, intent(in) :: kpg
    real(kind=8), intent(in) :: dfdi(nno,3)
    character(len=16), intent(in) :: option
    real(kind=8), intent(inout) :: dsidep(6,6)
    real(kind=8), intent(inout) ::  sign(6)
    real(kind=8), intent(in) ::  sigma(6)
    aster_logical, intent(in) :: matsym
    real(kind=8), intent(inout) :: matuu(*)
!
    integer :: i, j, n
    aster_logical :: rigi
    real(kind=8) :: rac2, scfact, tmp
    real(kind=8) :: vff(1)
    real(kind=8), dimension(3,3) :: iden, fp
    real(kind=8), dimension(6,6) :: cmatlo, pglqo, work66
    real(kind=8) :: bg(6,3*nno), def(6,nno,3), pff(6,nno,nno)
!
! ......................................................................
!
! - Initializations
!
    data iden/1.d0, 0.d0, 0.d0,&
              0.d0, 1.d0, 0.d0,&
              0.d0, 0.d0, 1.d0/
!
    parameter (scfact = 0.2025d0)
    parameter (rac2 = sqrt(2.d0))
    parameter (rigi = .true._1)
!
!   Modified Hooke matrix
!   Filling cmatlo with dsidep values (equivalent to Hooke matrix)
!   from previous time step, stored considering local coordinate system
    call mhomss(para(1), para(2), cmatlo, dsidep)
!
!   pgl(3,3) transformation matrix modified into fourth order transformation
!   matrix pglqo(6,6)
    call mpsoqo(pgl, pglqo)
!
!   Transforming modified Hooke matrix in global coordinate system,
!   and storing it back in dsidep
    call utbtab('ZERO', 6, 6, cmatlo, pglqo, work66, dsidep)
!
!   Setting fp to iden to comply with initial SHB code
    fp=iden
!
!   Glut / Start
!   Normally, one should be able to use matsym variable in following nmfdff call
!   (having it equal to true)
!   However, doing so, following test cases are broken and stop kind of
!   at random:  ssns109b, ssns101d, ssns101e, ssns101f, ssns101g (uninitialized
!   variable somewhere? I couldn't succeed to pinpoint it)
!   One thus forces matsym variable to .false._1 to keep test case unbroken
    call nmfdff(ndim, nno, .false._1, kpg, 0.d0,&
                rigi, .false._1, fp, vff, dfdi,&
                def, pff)
!   Correct call would be the one commented below
!    call nmfdff(ndim, nno, .false._1, kpg, 0.d0,&
!                rigi, matsym, fp, vff, dfdi,&
!                def, pff)
!   Glut / End
!
!   Updating pff so that it complies with initial SHB code
    do 125 i = 1, nno
       do 126 j = 1, nno
          pff(4,i,j) = pff(4,i,j)*rac2
          pff(5,i,j) = pff(5,i,j)*rac2
          pff(6,i,j) = pff(6,i,j)*rac2
126    continue
125 continue
!
    if (shb6) then
!      For SHB6 element, one has to apply correction factor square on 5th
!      and 6th shear terms
       do 127 i = 1, nno
          do 128 j = 1, nno
             pff(5,i,j) = scfact*pff(5,i,j)
             pff(6,i,j) = scfact*pff(6,i,j)
128       continue
127    continue
!
!      For SHB6 element, one has also to use projected form of B discretized
!      gradient operator in 'material behaviour'- related contribution for
!      tangent matrix
!      We modify def accordingly
       call ss6bgl(.false._1, kpg, geom, ipoids, idfde, icoopg, pgl, poids, bg)
!
       do 41 n = 1, nno
          j= 3*(n-1) + 1
          do 31 i = 1, 3
             def(1,n,i) = bg(1,j+i-1)
             def(2,n,i) = bg(2,j+i-1)
             def(3,n,i) = bg(3,j+i-1)
             def(4,n,i) = bg(4,j+i-1)
             def(5,n,i) = bg(5,j+i-1)
             def(6,n,i) = bg(6,j+i-1)
31        continue
41     continue
    else
!      For other SHB elements, one can directly re-use def from nmfdff
!      Updating def so that it complies with initial SHB code
       do 40 n = 1, nno
          do 30 i = 1, 3
             def(4,n,i) = def(4,n,i)*rac2
             def(5,n,i) = def(5,n,i)*rac2
             def(6,n,i) = def(6,n,i)*rac2
30        continue
40     continue
    endif
!
!   Expressing stress tensor at T- from local to global frame
!   Glut \ 1/2 \ Start
    tmp     = sign(5) 
    sign(5) = sign(6)
    sign(6) = tmp
!   Glut \ 1/2 \ End
    call tpsivp_shb(pgl, sign, .false._1)
!   Glut \ 2/2 \ Start
    tmp     = sign(5) 
    sign(5) = sign(6)
    sign(6) = tmp
!   Glut \ 2/2 \ End
!
!   Evaluating tangent matrix in global frame
!   To be noticed: sigma variable was set to 0.d0 previously in nmssgr
!   Does it mean FULL_MECA is not active for SHB elements?
    call nmgrt3_shb(nno, poids, def, pff, option,&
                dsidep, sign, sigma, matsym, matuu)
!
end subroutine
