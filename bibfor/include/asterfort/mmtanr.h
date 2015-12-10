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
    subroutine mmtanr(mesh, model_ndim, ds_contact, i_zone,&
                      lexfro, node_slav_indx, ksi1, ksi2, elem_mast_indx,&
                      elem_mast_nume, tau1m, tau2m, tau1, tau2)
        use NonLin_Datastructure_type
        character(len=8) :: mesh
        integer :: i_zone
        integer :: model_ndim
        integer :: node_slav_indx, elem_mast_indx, elem_mast_nume
        real(kind=8) :: ksi1, ksi2
        type(NL_DS_Contact), intent(in) :: ds_contact
        real(kind=8) :: tau1m(3), tau2m(3)
        real(kind=8) :: tau1(3), tau2(3)
        aster_logical :: lexfro
    end subroutine mmtanr
end interface
