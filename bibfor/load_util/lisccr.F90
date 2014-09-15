subroutine lisccr(list_load, nb_loadz, base)
!
implicit none
!
#include "asterfort/detrsd.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=19), intent(in) :: list_load
    integer, intent(in) :: nb_loadz
    character(len=1), intent(in) :: base
!
! --------------------------------------------------------------------------------------------------
!
! List of loads - Utility
!
! Create datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  list_load      : name of datastructure for list of loads
! In  nb_loadz       : number of loads
! In  base           : JEVEUX base
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_load
    character(len=24) :: lload_name, lload_info, lload_func
    integer, pointer :: v_load_info(:) => null()
    character(len=24), pointer :: v_load_name(:) => null()
    character(len=24), pointer :: v_load_func(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_load = nb_loadz
!
! - Datastructure access
!
    lload_name = list_load(1:19)//'.LCHA'
    lload_info = list_load(1:19)//'.INFC'
    lload_func = list_load(1:19)//'.FCHA'
!
! - No loads datastructure
!
    if (nb_loadz.eq.0) then
        nb_load = 1
    endif
!
    call detrsd('LISTE_CHARGES', list_load)
    call wkvect(lload_name, base//' V K24', nb_load    , vk24 = v_load_name)
    call wkvect(lload_info, base//' V IS' , 4*nb_load+7, vi   = v_load_info)
    call wkvect(lload_func, base//' V K24', nb_load    , vk24 = v_load_func)
    v_load_info(1) = nb_load
!
! - No loads datastructure
!
    if (nb_loadz.eq.0) then
        nb_load = 0
    endif
!
end subroutine
