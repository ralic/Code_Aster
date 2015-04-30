subroutine nmdifi(keywf, list_inst, tole, nb_inst, nume_end)
!
implicit none
!
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utacli.h"
#include "asterfort/utmess.h"
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
    character(len=16), intent(in) :: keywf
    character(len=19), intent(in) :: list_inst
    real(kind=8), intent(in) :: tole
    integer, intent(in) :: nb_inst
    integer, intent(out) :: nume_end
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Time discretization datastructure
!
! Index of final time
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword
! In  list_inst        : list of times from INCREMENT/LIST_INST
! In  tole             : tolerance to search time
! In  nb_inst          : number of time steps in list
! Out nume_end         : index of final time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n1, n2
    real(kind=8) :: inst
    real(kind=8), pointer :: v_list_inst(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nume_end = 0
!
! - Acces to list of times
!
    call jeveuo(list_inst, 'L', vr = v_list_inst)
!
! - Get keywords
!
    call getvis(keywf, 'NUME_INST_FIN', iocc=1, scal=nume_end, nbret=n1)
    call getvr8(keywf, 'INST_FIN'     , iocc=1, scal=inst    , nbret=n2)
!
! - No NUME_INST_FIN/INST_FIN
!
    if (n1+n2 .eq. 0) then
        nume_end = nb_inst-1
!
! - INST_FIN
!
    else if (n1 .eq. 0) then
        call utacli(inst, v_list_inst, nb_inst, tole, nume_end)
    endif
!
! - Checks
!
    if (nume_end .lt. 0 .or. nume_end .gt. (nb_inst-1)) then
        call utmess('F', 'DISCRETISATION_94')
        nume_end = nb_inst - 1
    endif
!
end subroutine
