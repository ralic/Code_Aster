subroutine ss6eps(xe, pgl, kpg, ipoids,&
                  idfde, poids, dfdig, deplm, epsm, deplp, epsp)
! ==============================================================================
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
! ==============================================================================
    implicit none
!
! Solid-Shell 6-node EPSilon (strain)
! Evaluate strain from nodal displacement for SHB6 element
!
! Evaluate strain from nodal displacement for SHB6 element at a given Gauss point
! in local coordinate system.
!
!
! IN  xe       element nodal coordinates
! IN  pgl      (3,3) transformation matrix from global to local coordinate system
! IN  kpg      Gauss point index that is being processed
! IN  ipoids   Gauss point weight index
! IN  idfde    shape function derivatives index
! IN  deplm    displacement at previous time
! IN  deplp    displacement at current time
! OUT poids   Gauss point weight
! OUT dfdig   shape function derivatives in global coordinates
! OUT epsp    strain tensor increment evaluated from displacement
!                      increment at current iteration
! OUT epsm    strain tensor evaluated from displacement at last converged increment
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdmshb.h"
#include "asterfort/nmgeog.h"
#include "asterfort/utpvgl.h"
!
    real(kind=8), intent(in) :: xe(*)
    real(kind=8), intent(in) :: pgl(3,3)
    integer, intent(in) :: kpg
    integer, intent(in) :: ipoids
    integer, intent(in) :: idfde
    real(kind=8), intent(out) :: poids
    real(kind=8), intent(out) :: dfdig(6,3)
    real(kind=8), intent(in) :: deplm(*)
    real(kind=8), intent(out) :: epsm(6)
    real(kind=8), optional, intent(in) :: deplp(*)
    real(kind=8), optional, intent(out) :: epsp(6)
!
    integer :: nno, i, j
    aster_logical :: grand
    real(kind=8) :: cfact, rac2, tmp
    real(kind=8), dimension(18) :: deplml, deplpl
    real(kind=8), dimension(6,3) :: dfdil
    real(kind=8), dimension(3,6) :: dfdilt, dfdigt
!   Dummy arguments for nmgeog and/or dfdmshb
    integer :: ivf
    real(kind=8), dimension(3,3) :: f, invjac
    real(kind=8), dimension(3,6) :: geom
    real(kind=8) :: r
!
! ------------------------------------------------------------------------------
!
! - Initializations
!
    parameter (nno = 6)
    parameter (cfact = 0.45d0)
    parameter (rac2 = sqrt(2.d0))
!
!   Shape function derivatives evaluated at current Gauss point location
!   in global coordinate systm
    call dfdmshb(nno, kpg, ipoids, idfde, xe, invjac, poids,&
                dfdig(1, 1), dfdig(1, 2), dfdig(1, 3))
!
!   Transforming shape function derivatives into shell-like coordinate system
!
    do 10 i = 1, nno
        do 11 j = 1, 3
            dfdigt(j,i) = dfdig(i,j)
11      continue
10  continue
    call utpvgl(nno, 3, pgl, dfdigt, dfdilt)
    do 15 i = 1, nno
        do 16 j = 1, 3
            dfdil(i,j) = dfdilt(j,i)
16      continue
15  continue
!
!   Transforming element nodal displacement into shell-like coordinate system
!   (at T-: deplm is total displacement from previous increment)
!
    call utpvgl(nno, 3, pgl, deplm, deplml)
!
! - First call to nmgeog to evaluate epsm (strain at T-) in local frame
!   To have equivalent code to original SHB code, 'grand' is set to false.
!   To be noticed, in 3D ISO element, it is set to true.
!   Could an evaluation with 'grand' set to true be interesting?
    grand = .false._1
    call nmgeog(3, nno, .false._1, grand, geom,&
                kpg, ivf, deplml,&
                .false._1, poids, dfdil, f, epsm,&
                r)
!   Glut \ Start
!   To comply with the initial SHB code, the following modifications to
!   epsm are required
    epsm(4) =         epsm(4)*rac2
!    epsm(5) = cfact * epsm(5)*rac2
!    epsm(6) = cfact * epsm(6)*rac2
    tmp     = cfact * epsm(5)*rac2
    epsm(5) = cfact * epsm(6)*rac2
    epsm(6) = tmp
!   Glut \ End
!
!
    if (present(deplp)) then
       ASSERT(present(epsp))
!
!      Transforming element nodal displacement into shell-like coordinate system
!      (at T+: deplp is displacement delta in current iteration)
       call utpvgl(nno, 3, pgl, deplp, deplpl)
!
! -    Second call to nmgeom to evaluate epsp (strain at T+) in local frame
!      To have equivalent code to original SHB code, 'grand' is set to false.
!      To be noticed, in 3D ISO element, it is set to true.
!      Could an evaluation with 'grand' set to true be interesting?
!      To be updated: epsp is here deps: vairiable name should be corrected
!      to avoid misunderstanding
       grand = .false._1
       call nmgeog(3, nno, .false._1, grand, geom,&
                   kpg, ivf, deplpl,&
                   .false._1, poids, dfdil, f, epsp,&
                   r)
!      Glut \ Start
!      To comply with the initial SHB code, the following modifications to
!      epsm are required
       epsp(4) =         epsp(4)*rac2
!      epsp(5) = cfact * epsp(5)*rac2
!      epsp(6) = cfact * epsp(6)*rac2
       tmp     = cfact * epsp(5)*rac2
       epsp(5) = cfact * epsp(6)*rac2
       epsp(6) = tmp
!      Glut \ End
    else
      ASSERT(.not.present(epsp))
    endif
!
end subroutine
