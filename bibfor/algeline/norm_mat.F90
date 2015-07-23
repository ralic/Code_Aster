function norm_mat(ndim,m)
!
      implicit none
!
      integer, intent(in) :: ndim
      real(kind=8), intent(in) :: m(ndim,ndim)
      real(kind=8) :: norm_mat
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
!     NORME D UNE MATRICE
!
! IN  NDIM : DIMENSION DE LA MATRICE
! IN  M    : MATRICE NDIM*NDIM
!
      integer :: i,j
!
      norm_mat = 0.d0
      do i=1,ndim
          do j=1,ndim
              norm_mat = norm_mat + m(i,j)*m(i,j)
          end do
      end do
      norm_mat = sqrt(norm_mat)
!
end function
