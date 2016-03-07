subroutine ReadMeasure(ds_measure)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infniv.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
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
    type(NL_DS_Measure), intent(inout) :: ds_measure
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Contact management
!
! Read parameters for measure and statistics management
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_measure       : datastructure for measure and statistics management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=16) :: keywfact, answer
    aster_logical :: l_csv, l_table
    integer :: unit_csv, noc
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Read parameters for measure and statistics management'
    endif
!
! - Initializations
!
    keywfact = 'MESURE'
    unit_csv = 0
    l_table  = .false.
    l_csv    = .false.
!
! - Read parameters
!
    call getvtx(keywfact, 'TABLE', iocc=1, scal=answer)
    l_table = answer.eq.'OUI'
    call getvis(keywfact, 'UNITE', iocc=1, scal=unit_csv, nbret=noc)
    if (noc .eq. 0) then
        l_csv = .false.
    else
        if (unit_csv .eq. 0) then
            l_csv = .false.
        else
            l_csv = .true.
        endif
    endif
!
! - Save parameters
!
    ds_measure%l_table        = l_table
    ds_measure%table%l_csv    = l_csv
    ds_measure%table%unit_csv = unit_csv
!
end subroutine
