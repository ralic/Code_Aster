subroutine romAlgoNLTableCreate(result, ds_algorom)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infniv.h"
#include "asterfort/exisd.h"
#include "asterfort/jeexin.h"
#include "asterfort/ltcrsd.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: result
    type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Solving non-linear problem
!
! Create table for the reduced coordinates
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of datastructure for results
! IO  ds_algorom       : datastructure for ROM parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer, parameter :: nb_para = 4
    character(len=8), parameter :: para_type(nb_para) = (/'I','I','R','R'/)
    character(len=16), parameter :: para_name(nb_para) = (/'NUME_MODE  ','NUME_ORDRE ',&
                                                           'INST       ','COOR_REDUIT'/)
    character(len=19) :: tabl_name = ' '
    integer :: iret
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_38')
    endif
!
! - Create list of tables in results datastructure (if necessary)
!
    call jeexin(result//'           .LTNT', iret)
    if (iret .eq. 0) then
        call ltcrsd(result, 'G')
    endif
!
! - Get name of reduced coordinates
!
    call ltnotb(result, 'COOR_REDUIT', tabl_name)   
!
! - Create observation table (if necessary)
!
    call exisd('TABLE', tabl_name, iret)
    if (iret .eq. 0) then
        call tbcrsd(tabl_name, 'G')
        call tbajpa(tabl_name, nb_para, para_name, para_type)
    endif
!
! - Save name of table
!
    ds_algorom%tabl_name = tabl_name
!
end subroutine
