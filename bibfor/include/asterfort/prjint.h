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
    subroutine prjint(pair_tole     , elem_dime       ,&
                      elin_slav_coor, elin_slav_nbnode, elin_slav_code,&
                      elin_mast_coor, elin_mast_nbnode, elin_mast_code,&
                      poin_inte     , inte_weight     , nb_poin_inte  ,&
                      inte_neigh_)
        real(kind=8), intent(in) :: pair_tole
        integer, intent(in) :: elem_dime
        real(kind=8), intent(in) :: elin_slav_coor(3,9)
        integer, intent(in) :: elin_slav_nbnode
        character(len=8), intent(in) :: elin_slav_code
        real(kind=8), intent(in) :: elin_mast_coor(3,9)
        integer, intent(in) :: elin_mast_nbnode
        character(len=8), intent(in) :: elin_mast_code
        real(kind=8), intent(out) :: poin_inte(elem_dime-1,16)
        real(kind=8), intent(out) :: inte_weight
        integer, intent(out) :: nb_poin_inte
        integer, optional, intent(inout) :: inte_neigh_(4)
    end subroutine prjint
end interface
