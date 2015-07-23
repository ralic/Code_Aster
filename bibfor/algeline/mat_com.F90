function mat_com(ndim,m)
!
      implicit none
!
#include "asterfort/det_mat.h"
      integer,intent(in) :: ndim
      real(kind=8), intent(in) :: m(ndim,ndim)
      real(kind=8) :: mat_com(ndim,ndim)
!
!-----------------------------------------------------------------------
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
!======================================================================
!
!     COMATRICE D UNE MATRICE DE PETITE TAILLE
!
! IN  NDIM : DIMENSION DE LA MATRICE
! IN  M    : MATRICE NDIM*NDIM
!
      integer :: i,j, im1, ip1, jm1, jp1
      real(kind=8) :: signe, signe2, mat_red(ndim-1,ndim-1)
!
      signe = 1.d0
      signe2 = 1.d0
      do i=1,ndim
          signe = signe2
          do j=1,ndim
              im1 = i-1
              jm1 = j-1
              ip1 = i+1
              jp1 = j+1
              if(i.gt.1.and.j.gt.1) then
                  mat_red(1:im1,1:jm1) = m(1:im1,1:jm1)
              endif
              if(i.gt.1.and.j.lt.ndim) then
                  mat_red(1:im1,j:) = m(1:im1,jp1:)
              endif
              if(i.lt.ndim.and.j.gt.1) then
                  mat_red(i:,1:jm1) = m(ip1:,1:jm1)
              endif
              if(i.lt.ndim.and.j.lt.ndim) then
                  mat_red(i:,j:) = m(ip1:,jp1:)
              endif
              mat_com(i,j) = signe*det_mat(ndim-1,mat_red)
              signe = -1.d0*signe
          end do
          signe2 = -1.d0*signe2
      end do
!
end function
