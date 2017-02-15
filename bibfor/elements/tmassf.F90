subroutine tmassf(geom, icoopg, kpg, hexa, pgl)
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
! Transformation MAtrix to Solid-Shell Frame
!
!
! Evaluation of transformation matrix from global coordinates to solid-shell
! local coordinates.
!
!
! IN  geom     element's nodes coordinates
! IN  icoopg   index to element Gauss point coordinates
! IN  kpg      current Gauss point index
! IN  hexa     true if current solid-shell element is a hexa
! OUT pgl      transformation matrix
!
#include "jeveux.h"
#include "asterf_types.h"
!
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
!
    real(kind=8), intent(in) :: geom(*)
    integer, intent(in) :: icoopg
    integer, intent(in) :: kpg
    aster_logical, intent(in) :: hexa
    real(kind=8), intent(out) :: pgl(3,3)
!
    integer :: i, j, nnob, nshift, iret
    real(kind=8) :: xcoq(3,4)
    real(kind=8) :: zeta, zlamb
!
! --------------------------------------------------------------------------------------------------
!
!      Initializations
!
    if (hexa) then
!      Initializations for SHB8 or SHB20
       nnob=4
       nshift=9
    else
!      Initializations for SHB6 or SHB15
       nnob=3
       nshift=6
    endif
!
!      Identification of the 3 (SHB6 or SHB15) or 4 nodes (SHB8 or SHB20)
!      defining the equivalent 'shell' plane
!
       zeta = zr(icoopg-1+kpg*3)
       zlamb = 0.5d0*(1.d0-zeta)
       do 10 i = 1, nnob
          do 11 j = 1, 3
             xcoq(j,i) = zlamb*geom((i-1)*3+j) + (1.d0-zlamb)*geom(+3*i+nshift+j)
11       continue
10    continue
!
!      Evaluate pgl(3,3) transformation matrix from global coordinate system
!      to local 'shell' coordinate system
!
       if (hexa) then
!         SHB8 OU SHB20
          call dxqpgl(xcoq, pgl, 'S', iret)
       else
!         SHB6 OU SHB15
          call dxtpgl(xcoq, pgl)
       endif
!
end subroutine

