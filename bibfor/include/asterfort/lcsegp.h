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
    subroutine lcsegp(elem_dime   , nb_lagr       , indi_lagc     ,&
                      nb_node_mast, elin_mast_coor, elin_mast_code,&
                      nb_node_slav, elin_slav_coor, elin_slav_code,&
                      poidspg     , gauss_coot    , jaco_upda,&
                      vtmp)
        integer, intent(in) :: elem_dime
        integer, intent(in) :: nb_lagr
        integer, intent(in) :: indi_lagc(10)
        integer, intent(in) :: nb_node_mast
        real(kind=8), intent(in) :: elin_mast_coor(elem_dime,nb_node_mast)
        character(len=8), intent(in) :: elin_mast_code
        integer, intent(in) :: nb_node_slav
        real(kind=8), intent(in) :: elin_slav_coor(elem_dime,nb_node_slav)
        character(len=8), intent(in) :: elin_slav_code
        real(kind=8), intent(in) :: poidspg
        real(kind=8), intent(in) :: gauss_coot(2)
        real(kind=8), intent(in) :: jaco_upda
        real(kind=8), intent(inout) :: vtmp(55)
    end subroutine lcsegp
end interface
