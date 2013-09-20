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
    subroutine ccchuc(sdresu_in, sdresu_out, field_type, nume_field_out, type_comp, &
                      crit, norm, nb_form, name_form, list_ordr, &
                      nb_ordr)
        character(len=8), intent(in) :: sdresu_in
        character(len=8), intent(in) :: sdresu_out
        character(len=16), intent(in) :: field_type
        character(len=16), intent(in) :: type_comp
        character(len=16), intent(in) :: crit
        character(len=16), intent(in) :: norm
        integer, intent(in) :: nb_form
        character(len=8), intent(in) :: name_form(nb_form)
        integer , intent(in) :: nume_field_out
        character(len=19), intent(in) :: list_ordr
        integer , intent(in) :: nb_ordr
    end subroutine ccchuc
end interface
