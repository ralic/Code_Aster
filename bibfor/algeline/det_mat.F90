recursive function det_mat(ndim,m) result(det) 
!
      implicit none
#include "asterfort/assert.h"
!
      real(kind=8) :: det
      integer, intent(in) :: ndim
      real(kind=8), intent(in) :: m(ndim,ndim)
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
!     DETERMINANT MATRICE TRES PETITE TAILLE (COUT FACTORIELLE N!!)
!
! IN NDIM : DIMENSION
! IN  M  : MATRICE
!
!
    real(kind=8) :: sub_m(ndim-1,ndim-1), signe, temp_det
    integer :: i,j
!
    if(ndim.gt.10) then
        write(6,*)'det_mat.F90 pour matrices taille 10 maxi'
        ASSERT(.false.)
    endif
!
    if(ndim.eq.1) then
        det = m(1,1)
    else if(ndim.eq.2) then
        det = m(1,1)*m(2,2) - m(1,2)*m(2,1)
    else if(ndim.eq.3) then
        det =  m(1,1)*(m(2,2)*m(3,3)-m(2,3)*m(3,2)) &
              -m(1,2)*(m(2,1)*m(3,3)-m(3,1)*m(2,3)) &
              +m(1,3)*(m(2,1)*m(3,2)-m(3,1)*m(2,2))
    else
        temp_det = 0.d0
        signe = 1.d0
        do i=1,ndim
            if(i.gt.1) then
                j = i-1
                sub_m(:,1:j) = m(2:,1:j)
            endif
            if(i.lt.ndim) then
                j = i+1
                sub_m(:,i:) = m(2:,j:)
            endif
            temp_det = signe*det_mat(ndim-1,sub_m)
            signe = -1.d0*signe
        end do
        det = temp_det
    endif
!
end function det_mat
