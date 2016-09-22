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
    subroutine merimo(base           , model , cara_elem, mate  , varc_refe,&
                      ds_constitutive, iterat, acti_func, sddyna, hval_incr,&
                      hval_algo      , merigi, vefint   , optioz, tabret   )
        use NonLin_Datastructure_type
        character(len=1), intent(in) :: base
        integer, intent(in) :: iterat
        character(len=*), intent(in) :: mate
        character(len=19), intent(in) :: sddyna
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: cara_elem
        type(NL_DS_Constitutive), intent(in) :: ds_constitutive
        character(len=24), intent(in) :: varc_refe
        integer, intent(in) :: acti_func(*)
        character(len=19), intent(in) :: hval_incr(*)
        character(len=19), intent(in) :: hval_algo(*)
        character(len=*), intent(in) :: optioz
        character(len=19), intent(in) :: merigi
        character(len=19), intent(in) :: vefint
        aster_logical, intent(out) :: tabret(0:10)
    end subroutine merimo
end interface
