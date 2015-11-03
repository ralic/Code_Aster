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
#include "asterf_types.h"
!
interface
    subroutine mmbclc(mesh  , model , numedd  , iterat    , numins,&
                      sddisc, sddyna, ds_print, ds_contact, valinc,&
                      solalg, sdtime, sdstat  , mmcvca    , instan)
        use NonLin_Datastructure_type
        character(len=8), intent(in) :: mesh
        character(len=8), intent(in) :: model
        character(len=24), intent(in) :: numedd
        integer, intent(in) :: iterat
        integer, intent(in) :: numins
        character(len=19), intent(in) :: sddisc
        character(len=19), intent(in) :: sddyna
        type(NL_DS_Print), intent(inout) :: ds_print
        type(NL_DS_Contact), intent(inout) :: ds_contact
        character(len=19), intent(in) :: valinc(*)
        character(len=19), intent(in) :: solalg(*)
        character(len=24), intent(in) :: sdtime
        character(len=24), intent(in) :: sdstat
        aster_logical, intent(out) :: mmcvca
        real(kind=8) :: instan
    end subroutine mmbclc
end interface
