!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine load_neut_data(i_type_neum    , nb_type_neumz, type_calc_,&
                              load_type_ligr_, load_opti_r_ , load_opti_f_, load_para_r_,&
                              load_para_f_   , load_keyw_   , load_obje_  , nb_obje_)
        integer, intent(in) :: i_type_neum
        integer, intent(in) :: nb_type_neumz
        character(len=4), optional, intent(in) :: type_calc_
        character(len=6), optional, intent(out) :: load_type_ligr_
        character(len=16), optional, intent(out) :: load_opti_r_
        character(len=16), optional, intent(out) :: load_opti_f_
        character(len=8), optional, intent(out) :: load_para_r_(2)
        character(len=8), optional, intent(out) :: load_para_f_(2)
        character(len=24), optional, intent(out) :: load_keyw_
        character(len=10), optional, intent(out) :: load_obje_(2)
        integer, optional, intent(out) :: nb_obje_
    end subroutine load_neut_data
end interface
