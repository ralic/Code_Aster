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
! aslint: disable=W1504
!
interface
    subroutine comp_nbvari_std(rela_comp , defo_comp, type_cpla     , nb_vari   ,&
                               kit_comp_ , type_matg_, post_iter_   , mult_comp_,&
                               l_cristal_, nume_comp_, nb_vari_rela_)
        character(len=16), intent(in) :: rela_comp
        character(len=16), intent(in) :: defo_comp
        character(len=16), intent(in) :: type_cpla
        integer, intent(out) :: nb_vari
        character(len=16), optional, intent(in) :: kit_comp_(4)
        character(len=16), optional, intent(in) :: type_matg_
        character(len=16), optional, intent(in) :: post_iter_
        character(len=16), optional, intent(in) :: mult_comp_
        aster_logical, optional, intent(in) :: l_cristal_
        integer, optional, intent(out) :: nb_vari_rela_
        integer, optional, intent(out) :: nume_comp_(4)
    end subroutine comp_nbvari_std
end interface
