!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine nmfdff(ndim, nno, axi, g, r,&
                      rigi, matsym, fr, vff, dff,&
                      def, pff)
        integer :: nno
        integer :: ndim
        aster_logical :: axi
        integer :: g
        real(kind=8) :: r
        aster_logical :: rigi
        aster_logical :: matsym
        real(kind=8) :: fr(3, 3)
        real(kind=8) :: vff(nno, *)
        real(kind=8) :: dff(nno, *)
        real(kind=8) :: def(2*ndim, nno, ndim)
        real(kind=8) :: pff(2*ndim, nno, nno)
    end subroutine nmfdff
end interface
