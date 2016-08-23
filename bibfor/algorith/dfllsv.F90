subroutine dfllsv(v_sdlist_infor, v_sdlist_evenr, v_sdlist_evenk, v_sdlist_subdr,&
                  i_fail_save   ,&
                  event_type    , vale_ref    , nom_cham        , nom_cmp       ,&
                  crit_cmp      , pene_maxi   , resi_glob_maxi  ,&
                  action_type   , subd_methode, subd_auto       , subd_pas_mini ,&
                  subd_pas      , subd_niveau , pcent_iter_plus , coef_maxi     ,&
                  subd_inst     , subd_duree)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dfllvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
! aslint: disable=W1504
!
    real(kind=8), intent(in), pointer :: v_sdlist_infor(:)
    real(kind=8), intent(in), pointer :: v_sdlist_evenr(:)
    character(len=16), intent(in), pointer :: v_sdlist_evenk(:)
    real(kind=8), intent(in), pointer :: v_sdlist_subdr(:)
    integer, intent(in) :: i_fail_save
    character(len=16), intent(in) :: event_type
    real(kind=8), intent(in) :: vale_ref
    character(len=16), intent(in) :: nom_cham
    character(len=16), intent(in) :: nom_cmp
    character(len=16), intent(in) :: crit_cmp
    real(kind=8), intent(in) :: pene_maxi
    real(kind=8), intent(in) :: resi_glob_maxi
    character(len=16), intent(in) :: action_type
    character(len=16), intent(in) :: subd_methode
    real(kind=8), intent(in) :: subd_pas_mini
    integer, intent(in) :: subd_niveau
    integer, intent(in) :: subd_pas
    character(len=16), intent(in) :: subd_auto
    real(kind=8), intent(in) :: subd_inst
    real(kind=8), intent(in) :: subd_duree
    real(kind=8), intent(in) :: pcent_iter_plus
    real(kind=8), intent(in) :: coef_maxi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_LIST_INST
!
! Save parameters in datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  event_type       : type of event
! In  vale_ref         : value of VALE_REF for EVENEMENT=DELTA_GRANDEUR
! In  nom_cham         : value of NOM_CHAM for EVENEMENT=DELTA_GRANDEUR
! In  nom_cmp          : value of NOM_CMP for EVENEMENT=DELTA_GRANDEUR
! In  crit_cmp         : value of CRIT_CMP for EVENEMENT=DELTA_GRANDEUR
! In  pene_maxi        : value of PENE_MAXI for EVENEMENT=INTERPENETRATION
! In  resi_glob_maxi   : value of RESI_GLOB_MAXI for EVENEMENT=RESI_MAXI
! In  action_type      : type of action
! In  subd_methode     : value of SUBD_METHODE for ACTION=DECOUPE
! In  subd_pas_mini    : value of SUBD_PAS_MINI for ACTION=DECOUPE
! In  subd_niveau      : value of SUBD_NIVEAU for ACTION=DECOUPE
! In  subd_pas         : value of SUBD_PAS for ACTION=DECOUPE
! In  subd_auto        : value of SUBD_AUTO for ACTION=DECOUPE
! In  subd_inst        : value of SUBD_INST for ACTION=DECOUPE
! In  subd_duree       : value of SUBD_DUREE for ACTION=DECOUPE
! In  pcent_iter_plus  : value of PCENT_ITER_PLUS for ACTION=ITER_SUPPL
! In  coef_maxi        : value of COEF_MAXI for ACTION=ADAPT_COEF_PENA
!
! --------------------------------------------------------------------------------------------------
!
    integer :: leevr, leevk, lesur
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Get sizes of objects
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
!
! - Alarm for no-step cut
!
    if (event_type .eq. 'ERRE') then
        if (action_type .eq. 'ARRET') then
            call utmess('I', 'DISCRETISATION_9')
        endif
    endif
!
! - At least one ACTION=DECOUPE
!
    if (action_type .eq. 'DECOUPE') then
        v_sdlist_infor(7) = 1.d0
    endif
!
! - Type of event
!
    if (event_type .eq. 'ERRE') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+1) = 0.d0
    else if (event_type.eq.'DELTA_GRANDEUR') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+1) = 1.d0
    else if (event_type.eq.'COLLISION') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+1) = 2.d0
    else if (event_type.eq.'INTERPENETRATION') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+1) = 3.d0
    else if (event_type.eq.'DIVE_RESI') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+1) = 4.d0
    else if (event_type.eq.'INSTABILITE') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+1) = 5.d0
    else if (event_type.eq.'RESI_MAXI') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+1) = 6.d0
    else
        ASSERT(.false.)
    endif
!
! - Type of action
!
    if (action_type .eq. 'ARRET') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+2) = 0.d0
    else if (action_type.eq.'DECOUPE') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+2) = 2.d0
    else if (action_type.eq.'ITER_SUPPL') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+2) = 3.d0
    else if (action_type.eq.'AUTRE_PILOTAGE') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+2) = 4.d0
    else if (action_type.eq.'ADAPT_COEF_PENA') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+2) = 5.d0
    else if (action_type.eq.'CONTINUE') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+2) = 6.d0
    else
        ASSERT(.false.)
    endif
!
! - Parameters for EVENEMENT = 'DELTA_GRANDEUR'
!
    if (event_type .eq. 'DELTA_GRANDEUR') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+5) = vale_ref
        v_sdlist_evenk(leevk*(i_fail_save-1)+1) = nom_cham
        v_sdlist_evenk(leevk*(i_fail_save-1)+2) = nom_cmp
        v_sdlist_evenk(leevk*(i_fail_save-1)+3) = crit_cmp
    endif
!
! - Parameters for EVENEMENT = 'INTERPENETRATION'
!
    if (event_type .eq. 'INTERPENETRATION') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+6) = pene_maxi
    endif
!
! - Parameters for EVENEMENT = 'INTERPENETRATION'
!
    if (event_type .eq. 'RESI_MAXI') then
        v_sdlist_evenr(leevr*(i_fail_save-1)+7) = resi_glob_maxi
    endif
!
! - Parameters for ACTION = 'DECOUPE'
!
    if (subd_methode .eq. 'MANUEL') then
        v_sdlist_subdr(lesur*(i_fail_save-1)+1) = 1.d0
        v_sdlist_subdr(lesur*(i_fail_save-1)+2) = subd_pas
        v_sdlist_subdr(lesur*(i_fail_save-1)+3) = subd_pas_mini
        v_sdlist_subdr(lesur*(i_fail_save-1)+4) = subd_niveau
    else if (subd_methode.eq.'AUTO') then
        v_sdlist_subdr(lesur*(i_fail_save-1)+1) = 2.d0
        v_sdlist_subdr(lesur*(i_fail_save-1)+3) = subd_pas_mini
        v_sdlist_subdr(lesur*(i_fail_save-1)+5) = subd_inst
        v_sdlist_subdr(lesur*(i_fail_save-1)+6) = subd_duree
        if (subd_auto .eq. 'COLLISION') then
            v_sdlist_subdr(lesur*(i_fail_save-1)+10) = 1.d0
        else if (subd_auto.eq.'EXTRAPOLE') then
            v_sdlist_subdr(lesur*(i_fail_save-1)+10) = 2.d0
        else
            ASSERT(.false.)
        endif
    endif
!
! - Parameters for ACTION = 'ITER_SUPPL'
!
    if (action_type .eq. 'ITER_SUPPL') then
        v_sdlist_subdr(lesur*(i_fail_save-1)+7) = pcent_iter_plus
    endif
!
! - Parameters for ACTION = 'ADAPT_COEF_PENA'
!
    if (action_type .eq. 'ADAPT_COEF_PENA') then
        v_sdlist_subdr(lesur*(i_fail_save-1)+8) = coef_maxi
    endif
!
    call jedema()
!
end subroutine
