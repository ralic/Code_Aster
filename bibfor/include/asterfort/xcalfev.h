! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xcalfev(elrefp, ndim, nnop, basloc, stano, he,&
                       lsn, lst, geom, kappa, mu, ff, fk,&
                       dfdi, dkdgl, face,&
                       nnop_lin, ff_lin, dfdi_lin)
        character(len=8), intent(in) :: elrefp
        integer :: nnop
        integer :: ndim
        integer :: stano(*)
        real(kind=8) :: he
        real(kind=8) :: ff(*)
        real(kind=8) :: lsn(*)
        real(kind=8) :: lst(*)
        real(kind=8) :: basloc(*)
        real(kind=8) :: kappa
        real(kind=8) :: mu
        real(kind=8) :: fk(27,3,3)
        real(kind=8), optional :: dkdgl(27,3,3,3)
        real(kind=8), optional :: dfdi(nnop,ndim)
        character(len=4), optional :: face
        real(kind=8) :: geom(*)
        integer, optional :: nnop_lin
        real(kind=8), optional :: ff_lin(:)
        real(kind=8), optional :: dfdi_lin(:,:)
    end subroutine xcalfev
end interface
