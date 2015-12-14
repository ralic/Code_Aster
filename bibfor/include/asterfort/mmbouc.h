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
    subroutine mmbouc(ds_contact   , loop_type  , operation_ ,&
                      loop_counter_, loop_state_, loop_locus_, loop_vale_)
        use NonLin_Datastructure_type
        type(NL_DS_Contact), intent(inout) :: ds_contact
        character(len=4), intent(in) :: loop_type
        character(len=*), intent(in) :: operation_
        integer, intent(out), optional :: loop_counter_
        aster_logical, intent(out), optional :: loop_state_
        character(len=16), intent(inout), optional :: loop_locus_
        real(kind=8), intent(inout), optional :: loop_vale_
    end subroutine mmbouc
end interface
