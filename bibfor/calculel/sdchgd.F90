subroutine sdchgd(fieldz, type_scalz)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: fieldz
    character(len=*), intent(in) :: type_scalz
!
! --------------------------------------------------------------------------------------------------
!
! Field utility
!
! Change type of GRANDEUR in a field
!
! --------------------------------------------------------------------------------------------------
!
! In  field     : name of field
! In  type_scal : new type of GRANDEUR (R, C or F)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: field
    character(len=8) :: gd_name_old, gd_name_new
    integer :: i_exi, i_gd_old, i_gd_new
    integer, pointer :: p_desc(:) => null()
    character(len=3) :: type_scal
!
! --------------------------------------------------------------------------------------------------
!
    field     = fieldz
    type_scal = type_scalz
!
! - Field type
!
    call jeexin(field//'.DESC', i_exi)
    if (i_exi .gt. 0) then
        call jeveuo(field//'.DESC', 'E', vi = p_desc)  
    else
        call jeveuo(field//'.CELD', 'E', vi = p_desc)
    endif
!
! - Old GRANDEUR
!
    i_gd_old = p_desc(1)
    call jenuno(jexnum('&CATA.GD.NOMGD', i_gd_old), gd_name_old)
!
! - New GRANDEUR
!
    gd_name_new = gd_name_old(1:5)//type_scal
    call jenonu(jexnom('&CATA.GD.NOMGD', gd_name_new), i_gd_new)
    ASSERT(i_gd_new.ne.0)
    p_desc(1) = i_gd_new

end subroutine
