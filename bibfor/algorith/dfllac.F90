subroutine dfllac(keywf          , i_fail       , dtmin     , event_type,&
                  action_type    ,&
                  subd_methode   , subd_pas_mini,&
                  subd_niveau    , subd_pas     ,&
                  subd_auto      , subd_inst    , subd_duree,&
                  pcent_iter_plus, coef_maxi    )
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dfllae.h"
#include "asterfort/dflldc.h"
#include "asterfort/dfllin.h"
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
!
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: i_fail
    real(kind=8), intent(in) :: dtmin
    character(len=16), intent(in) :: event_type
    character(len=16), intent(out) :: action_type
    character(len=16), intent(out) :: subd_methode
    real(kind=8), intent(out) :: subd_pas_mini
    integer, intent(out) :: subd_niveau
    integer, intent(out) :: subd_pas
    character(len=16), intent(out) :: subd_auto
    real(kind=8), intent(out) :: subd_inst
    real(kind=8), intent(out) :: subd_duree
    real(kind=8), intent(out) :: pcent_iter_plus
    real(kind=8), intent(out) :: coef_maxi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_LIST_INST
!
! Get parameters of ACTION for current failure keyword
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword to read failures
! In  i_fail           : index of current factor keyword to read failure
! In  dtmin            : minimum time increment in list of times
! In  event_type       : type of event
! Out action_type      : type of action
! Out subd_methode     : value of SUBD_METHODE for ACTION=DECOUPE
! Out subd_pas_mini    : value of SUBD_PAS_MINI for ACTION=DECOUPE
! Out subd_niveau      : value of SUBD_NIVEAU for ACTION=DECOUPE
! Out subd_pas         : value of SUBD_PAS for ACTION=DECOUPE
! Out subd_auto        : value of SUBD_AUTO for ACTION=DECOUPE
! Out subd_inst        : value of SUBD_INST for ACTION=DECOUPE
! Out subd_duree       : value of SUBD_DUREE for ACTION=DECOUPE
! Out pcent_iter_plus  : value of PCENT_ITER_PLUS for ACTION=ITER_SUPPL
! Out coef_maxi        : value of COEF_MAXI for ACTION=ADAPT_COEF_PENA
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc
!
! --------------------------------------------------------------------------------------------------
!
    action_type = ' '
!
! - Read
!
    call getvtx(keywf, 'ACTION', iocc=i_fail, scal=action_type, nbret=nocc)
    ASSERT(nocc .gt. 0)
    if (action_type .eq. 'ARRET') then
!
    else if (action_type.eq.'DECOUPE') then
        call dflldc(keywf       , i_fail       , dtmin     , event_type,&
                    subd_methode, subd_pas_mini,&
                    subd_niveau , subd_pas     ,&
                    subd_auto   , subd_inst    , subd_duree)
    else if (action_type.eq.'ITER_SUPPL') then
        call dfllae(keywf, i_fail, pcent_iter_plus)
        call dflldc(keywf       , i_fail       , dtmin     , event_type,&
                    subd_methode, subd_pas_mini,&
                    subd_niveau , subd_pas     ,&
                    subd_auto   , subd_inst    , subd_duree)
    else if (action_type.eq.'ADAPT_COEF_PENA') then
        call dfllin(keywf, i_fail, coef_maxi)
    else if (action_type.eq.'AUTRE_PILOTAGE') then
        call dflldc(keywf       , i_fail       , dtmin     , event_type,&
                    subd_methode, subd_pas_mini,&
                    subd_niveau , subd_pas     ,&
                    subd_auto   , subd_inst    , subd_duree)
    else if (action_type.eq.'CONTINUE') then
!
    else
        ASSERT(.false.)
    endif
!
end subroutine
