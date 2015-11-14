subroutine ReadPrint(ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Read parameters for printing
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=16) :: keywf, repk
    aster_logical :: l_csv, l_info_resi, l_info_time
    integer :: noc, unit_csv, reac_print
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Read parameters for printing'
    endif
!
! - Initializations
!
    keywf       = 'AFFICHAGE'
    l_info_resi = .false.
    l_info_time = .false.
    unit_csv    = 0
    reac_print  = 1
    l_csv       = .false.
    repk        = 'NON'
!
! - Read parameters
!
    call getvtx(keywf, 'INFO_RESIDU', iocc=1, scal=repk)
    l_info_resi = repk .eq. 'OUI'
    call getvtx(keywf, 'INFO_TEMPS', iocc=1, scal=repk)
    l_info_time = repk .eq. 'OUI'
    call getvis(keywf, 'UNITE', iocc=1, scal=unit_csv, nbret=noc)
    if (noc .eq. 0) then
        l_csv = .false.
    else
        if (unit_csv .eq. 0) then
            l_csv = .false.
        else
            l_csv = .true.
        endif
    endif
    call getvis(keywf, 'PAS', iocc=1, scal=reac_print, nbret=noc)
    if (noc .eq. 0) then
        reac_print = 1
    endif
!
! - Save parameters
!
    ds_print%l_info_resi = l_info_resi
    ds_print%l_info_time = l_info_time
    ds_print%l_tcvg_csv  = l_csv
    ds_print%tcvg_unit   = unit_csv
    ds_print%reac_print  = reac_print
!
end subroutine
