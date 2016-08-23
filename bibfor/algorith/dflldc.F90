subroutine dflldc(keywf       , i_fail       , dtmin     , event_type,&
                  subd_methode, subd_pas_mini,&
                  subd_niveau , subd_pas     ,&
                  subd_auto   , subd_inst    , subd_duree)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
!
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: i_fail
    real(kind=8), intent(in) :: dtmin
    character(len=16), intent(in) :: event_type
    character(len=16), intent(out) :: subd_methode
    real(kind=8), intent(out) :: subd_pas_mini
    integer, intent(out) :: subd_niveau
    integer, intent(out) :: subd_pas
    character(len=16), intent(out) :: subd_auto
    real(kind=8), intent(out) :: subd_inst
    real(kind=8), intent(out) :: subd_duree
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_LIST_INST
!
! Get parameters of ACTION=DECOUPE for current failure keyword
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword to read failures
! In  i_fail           : index of current factor keyword to read failure
! In  dtmin            : minimum time increment in list of times
! In  event_type       : type of event
! Out subd_methode     : value of SUBD_METHODE for ACTION=DECOUPE
! Out subd_pas_mini    : value of SUBD_PAS_MINI for ACTION=DECOUPE
! Out subd_niveau      : value of SUBD_NIVEAU for ACTION=DECOUPE
! Out subd_pas         : value of SUBD_PAS for ACTION=DECOUPE
! Out subd_auto        : value of SUBD_AUTO for ACTION=DECOUPE
! Out subd_inst        : value of SUBD_INST for ACTION=DECOUPE
! Out subd_duree       : value of SUBD_DUREE for ACTION=DECOUPE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbret
!
! --------------------------------------------------------------------------------------------------
!
    subd_methode  = ' '
    subd_auto     = ' '
    subd_niveau   = 0
    subd_pas      = 0
    subd_pas_mini = 0.d0
    subd_inst     = 0.d0
    subd_duree    = 0.d0
!
! - Type of step cut
!
    call getvtx(keywf, 'SUBD_METHODE', iocc=i_fail, scal=subd_methode)
!
! - Get parameters
!
    call getvr8(keywf, 'SUBD_PAS_MINI', iocc=i_fail, scal=subd_pas_mini, nbret = nbret)
    ASSERT(nbret .le. 1)
    if (subd_pas_mini .gt. dtmin) then
        call utmess('F', 'DISCRETISATION_2')
    endif
    if (subd_methode .eq. 'MANUEL') then
        call getvis(keywf, 'SUBD_NIVEAU', iocc=i_fail, scal=subd_niveau)
        call getvis(keywf, 'SUBD_PAS', iocc=i_fail, scal=subd_pas)
        ASSERT(subd_pas .ge. 2)
    else if (subd_methode .eq. 'AUTO') then
        if (event_type .eq. 'COLLISION') then
            call getvr8(keywf, 'SUBD_INST', iocc=i_fail, scal=subd_inst)
            call getvr8(keywf, 'SUBD_DUREE', iocc=i_fail, scal=subd_duree)
            subd_auto = 'COLLISION'
        else
            subd_auto = 'EXTRAPOLE'
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
