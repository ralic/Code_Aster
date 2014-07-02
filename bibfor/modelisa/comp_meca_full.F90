subroutine comp_meca_full(model, comp_elas, full_elem_s)
!
    implicit none
!
#include "asterf_types.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: model
    character(len=19), intent(in) :: comp_elas
    character(len=19), intent(in) :: full_elem_s
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Create <CHELEM_S> of FULL_MECA option for checking
!
! --------------------------------------------------------------------------------------------------
!
! In  model       : name of model
! In  comp_elas   : name of ELAS <CARTE> COMPOR
! In  full_elem_s : name of <CHELEM_S> of FULL_MECA option
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbocc, ibid, iexi
    character(len=16) :: keywordfact
    character(len=19) :: elas_elem_s, elas_elem, ligrel
    aster_logical :: l_comp
!
! --------------------------------------------------------------------------------------------------
!
    nbocc = 0
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
    l_comp = nbocc .gt. 0
!
    if (l_comp) then
        ligrel = model(1:8)//'.MODELE    .LIEL'
        elas_elem_s = '&&CRCMEL.CES1'
        elas_elem = '&&CRCMEL.CEL1'
        call carces(comp_elas, 'ELEM', ' ', 'V', elas_elem_s,&
                    'A', ibid)
        call cescel(elas_elem_s, ligrel, 'FULL_MECA', 'PCOMPOR', 'OUI',&
                    ibid, 'V', elas_elem, 'A', ibid)
        call exisd('CHAMP', elas_elem, iexi)
        if (iexi .eq. 0) then
            call utmess('F', 'MECANONLINE_3')
        endif
        call celces(elas_elem, 'V', full_elem_s)
        call detrsd('CHAMP', elas_elem_s)
        call detrsd('CHAMP', elas_elem)
    endif
!
end subroutine
