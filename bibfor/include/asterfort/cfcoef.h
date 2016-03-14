!
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
interface
    subroutine cfcoef(ds_contact    , model_ndim , nb_node_mast, nods_mast_indx, coef_node,&
                      node_slav_indx, norm       , tau1        , tau2          , coef_cont,&
                      coef_fric_x   , coef_fric_y, nb_dof_tot  , dof_indx)
        use NonLin_Datastructure_type
        type(NL_DS_Contact), intent(in) :: ds_contact
        integer, intent(in) :: model_ndim
        integer, intent(in) :: nb_node_mast
        integer, intent(in) :: nods_mast_indx(9)
        integer, intent(in) :: node_slav_indx
        real(kind=8), intent(in) :: coef_node(9)
        real(kind=8), intent(in) :: norm(3)
        real(kind=8), intent(in) :: tau1(3)
        real(kind=8), intent(in) :: tau2(3)
        real(kind=8), intent(out) :: coef_cont(30)
        real(kind=8), intent(out) :: coef_fric_x(30)
        real(kind=8), intent(out) :: coef_fric_y(30)
        integer, intent(out) :: dof_indx(30)
        integer, intent(out) :: nb_dof_tot
    end subroutine cfcoef
end interface
