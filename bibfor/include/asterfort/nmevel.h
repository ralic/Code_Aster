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
#include "asterf_types.h"
!
interface
    subroutine nmevel(sddisc, nume_inst  , vale  , loop_name, lsvimx,&
                      ldvres, lresmx     , linsta, lerrcv   , lerror,&
                      conver, ds_contact_)
        use NonLin_Datastructure_type
        character(len=19), intent(in) :: vale(*)
        character(len=19), intent(in) :: sddisc
        character(len=4), intent(in) :: loop_name
        integer, intent(in) :: nume_inst
        aster_logical, intent(in) :: lsvimx
        aster_logical, intent(in) :: ldvres
        aster_logical, intent(in) :: lresmx
        aster_logical, intent(in) :: linsta
        aster_logical, intent(in) :: lerrcv
        aster_logical, intent(in) :: lerror
        aster_logical, intent(in) :: conver
        type(NL_DS_Contact), optional, intent(in) :: ds_contact_
    end subroutine nmevel
end interface
