subroutine CreateTable(result, table)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jeexin.h"
#include "asterfort/ltcrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbajpa.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: result
    type(NL_DS_Table), intent(inout) :: table
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Table management
!
! Create table in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! IO  table            : datastructure for table
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=19) :: table_name
    character(len=24) :: table_type
!
! --------------------------------------------------------------------------------------------------
!
    table_type = table%table_type
!
! - Create list of tables if necessary
!  
    call jeexin(result//'           .LTNT', iret)
    if (iret .eq. 0) then
        call ltcrsd(result, 'G')
    endif
!
! - Get name of table
! 
    call ltnotb(result, table_type, table_name)
!
! - Create table if necessary
!
    call exisd('TABLE', table_name, iret)
    if (iret .eq. 0) then
        call tbcrsd(table_name, 'G')
        call tbajpa(table_name, table%nb_para, table%list_para, table%type_para)
    endif
!
! - Set table parameters
!
    table%result     = result
    table%table_name = table_name
!
end subroutine
