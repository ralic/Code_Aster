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
    subroutine nmevac(sddisc, sderro   , i_echec_acti, nume_inst   , iterat,&
                      retact, ds_print_, ds_contact_)
        use NonLin_Datastructure_type
        character(len=19), intent(in) :: sddisc
        character(len=24), intent(in) :: sderro
        integer, intent(in) :: i_echec_acti
        integer, intent(in) :: nume_inst
        integer, intent(in) :: iterat
        integer, intent(out) :: retact
        type(NL_DS_Print), optional, intent(in) :: ds_print_
        type(NL_DS_Contact), optional, intent(in) :: ds_contact_
    end subroutine nmevac
end interface
