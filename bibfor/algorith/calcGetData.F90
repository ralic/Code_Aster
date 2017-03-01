subroutine calcGetData(table_new, table_old  ,&
                       nb_option, list_option,&
                       nume_inst, list_inst  ,&
                       phenom)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/utmess.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/diinst.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(out) :: table_new
    character(len=8), intent(out) :: table_old
    integer, intent(out) :: nb_option
    character(len=16), intent(out) :: list_option(:)
    integer, intent(out) :: nume_inst
    character(len=19), intent(out) :: list_inst
    character(len=16), intent(out) :: phenom
!
! --------------------------------------------------------------------------------------------------
!
! Command CALCUL
!
! Get commons data
!
! --------------------------------------------------------------------------------------------------
!
! Out table_new        : name of created table
! Out table_old        : name of old table
! Out nb_option        : number of options to compute
! Out list_option      : list of options to compute
! Out nume_inst        : index of current step time
! Out list_inst        : list of step time
! Out phenom           : phenomenon (MECANIQUE/THERMIQUE/ACOUSTIQUE)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: k24dummy
    integer :: nocc
!
! --------------------------------------------------------------------------------------------------
!
    table_new      = ' '
    table_old      = ' '
    nb_option      = 0
    list_option(:) = ' '
    nume_inst      = 0
    list_inst      = ' '
    phenom         = ' '
!
! - Name of created table
!
    call getres(table_new, k24dummy, k24dummy)
!
! - Name of reused table
!
    call getvid(' ', 'TABLE', nbval=0, nbret=nocc)
    if (nocc .eq. 0) then
        table_old = ' '
    else
        call getvid(' ', 'TABLE', nbval=1, scal = table_old)
        if (table_old .ne. table_new) then
            call utmess('F', 'CALCUL1_3')
        endif
    endif
!
! - Options
!
    call getvtx(' ', 'OPTION', nbval=6, vect=list_option, nbret=nb_option)
!
! - Phenomen
!
    phenom = 'MECANIQUE'
!
! - Get current time
!
    call getvis('INCREMENT', 'NUME_ORDRE', iocc=1, scal=nume_inst)
    call getvid('INCREMENT', 'LIST_INST' , iocc=1, scal=list_inst)
!
end subroutine
