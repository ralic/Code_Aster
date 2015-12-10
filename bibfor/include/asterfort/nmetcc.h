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
interface
    subroutine nmetcc(field_type, algo_name, init_name,&
                      compor    , sddyna   , sdpost   , ds_contact,&
                      hydr      , temp_init, hydr_init)
        use NonLin_Datastructure_type 
        character(len=24), intent(in) :: field_type
        character(len=24), intent(out) :: algo_name
        character(len=24), intent(out) :: init_name
        type(NL_DS_Contact), optional, intent(in) :: ds_contact
        character(len=19), optional, intent(in) :: compor
        character(len=19), optional, intent(in) :: sddyna
        character(len=19), optional, intent(in) :: sdpost
        character(len=24), optional, intent(in) :: hydr
        character(len=24), optional, intent(in) :: hydr_init
        character(len=24), optional, intent(in) :: temp_init
    end subroutine nmetcc
end interface
