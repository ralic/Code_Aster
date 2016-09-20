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
    subroutine comp_nbvari_kit(kit_comp  , defo_comp   , nb_vari_rela, &
                               l_kit_meta, l_kit_thm   , l_kit_ddi   , l_kit_cg,&
                               nb_vari   , nb_vari_comp, nume_comp   , l_meca_mfront)
        character(len=16), intent(in) :: kit_comp(4)
        character(len=16), intent(in) :: defo_comp
        integer, intent(in) :: nb_vari_rela
        aster_logical, intent(in) :: l_kit_meta
        aster_logical, intent(in) :: l_kit_thm
        aster_logical, intent(in) :: l_kit_ddi
        aster_logical, intent(in) :: l_kit_cg
        integer, intent(inout) :: nb_vari
        integer, intent(inout) :: nume_comp(4)
        integer, intent(out) :: nb_vari_comp(4)
        aster_logical, intent(out) :: l_meca_mfront
    end subroutine comp_nbvari_kit
end interface
