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
    subroutine liscad(list_load      , i_load    , load_namez  , load_funcz, nb_info_typez,&
                      list_info_typez, info_typez, i_neum_laplz)
        character(len=19), intent(in) :: list_load
        integer, intent(in) :: i_load
        character(len=*), intent(in) :: load_namez
        character(len=*), intent(in) :: load_funcz
        integer, optional, intent(in) :: nb_info_typez
        character(len=*), optional, intent(in) :: list_info_typez(*)
        character(len=*), optional, intent(in) :: info_typez
        integer, optional, intent(in) :: i_neum_laplz
    end subroutine liscad
end interface
