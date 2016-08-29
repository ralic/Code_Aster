subroutine rs_get_caraelem(result_, nume, cara_elem, codret)
!
implicit none
!
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/getvid.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: result_
    integer, intent(in) :: nume
    character(len=*), intent(out) :: cara_elem
    integer, intent(out) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get elementary characteristics at index stored in results datastructure or from command file
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! In  nume             : index to find in results datastructure
! Out cara_elem        : name of elementary characteristics (field)
! Out codret           : return code
!                        -1 - No cara_elem found
!                         1 - Cara_elem from command file
!                         2 - Cara_elem from results datastructure
!                         3 - Cara_elem from results datastructure and command file (the same)
!                         4 - Cara_elem from command file is different from results datastructure
!                        
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result, cara_elem_resu, cara_elem_comm
    integer :: nocc, jv_para
!
! --------------------------------------------------------------------------------------------------
!
    result     = result_
    cara_elem  = ' '
    nocc       = 0
    codret     = -1
!
! - Get from command file
!
    cara_elem_comm = ' '
    if (getexm(' ','CARA_ELEM') .eq. 1) then
        call getvid(' ', 'CARA_ELEM', scal=cara_elem_comm, nbret=nocc)
    else
        cara_elem_comm = ' '
        nocc           = 0
    endif
!
! - Get from results datastructure
!
    cara_elem_resu = ' '
    call rsadpa(result, 'L', 1, 'CARAELEM', nume,&
                0, sjv=jv_para)
    cara_elem_resu = zk8(jv_para)
!
! - Select cara_elem
!
    if (cara_elem_resu .eq. ' ') then
        if (nocc .eq. 0) then
            cara_elem  = ' '
            codret     = -1
        else
            cara_elem  = cara_elem_comm
            codret     = 1
        endif
    else
        if (nocc .eq. 0) then
            cara_elem  = cara_elem_resu
            codret     = 2
        else if (cara_elem_resu .eq. cara_elem_comm) then
            cara_elem  = cara_elem_comm
            codret     = 3
        else
            cara_elem  = cara_elem_comm
            codret     = 4
        endif
    endif
!
end subroutine
