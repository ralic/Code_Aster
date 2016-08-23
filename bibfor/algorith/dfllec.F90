subroutine dfllec(sdlist, dtmin)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/dfdevn.h"
#include "asterfort/dfllac.h"
#include "asterfort/dfllne.h"
#include "asterfort/dfllpe.h"
#include "asterfort/dfllsv.h"
#include "asterfort/dfllvd.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    character(len=8), intent(in) :: sdlist
    real(kind=8), intent(in) :: dtmin
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_LIST_INST
!
! Read keywords
!
! --------------------------------------------------------------------------------------------------
!
! In  sdlist           : name of DEFI_LIST_INST datastructure
! In  dtmin            : minimum time increment in list of times
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_event = 7
    integer :: event_list(nb_event)
    character(len=16) :: keywf
    character(len=16) :: nom_cham, nom_cmp, crit_cmp
    real(kind=8) :: vale_ref, subd_pas_mini
    integer :: nb_fail_read, nb_fail
    integer :: i_fail, i_event, i_fail_save
    character(len=16) :: event_type, action_type, event_curr
    character(len=16) :: subd_methode, subd_auto
    integer :: subd_pas, subd_niveau
    real(kind=8) :: nivmax, nivear, pcent_iter_plus, pene_maxi, resi_glob_maxi
    real(kind=8) :: coef_maxi, subd_inst, subd_duree
    aster_logical :: l_required, l_save, l_fail_error
    integer :: i_last, iplus
    integer, pointer :: v_work(:) => null()
    integer :: leevr, leevk, lesur
    character(len=24) :: sdlist_evenr
    real(kind=8), pointer :: v_sdlist_evenr(:) => null()
    character(len=24) :: sdlist_evenk
    character(len=16), pointer :: v_sdlist_evenk(:) => null()
    character(len=24) :: sdlist_subdr
    real(kind=8), pointer :: v_sdlist_subdr(:) => null()
    character(len=24) :: sdlist_infor
    real(kind=8), pointer :: v_sdlist_infor(:) => null()
    character(len=16), parameter :: event_order(nb_event)  = (/'ERRE            ',&
                                                               'DELTA_GRANDEUR  ',&
                                                               'COLLISION       ',&
                                                               'INTERPENETRATION',&
                                                               'DIVE_RESI       ',&
                                                               'INSTABILITE     ',&
                                                               'RESI_MAXI       '/)
    aster_logical, parameter :: event_required(nb_event)  = (/.true._1 ,.false._1,&
                                                              .false._1,.false._1,&
                                                              .false._1,.false._1,&
                                                              .false._1/)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    keywf         = 'ECHEC'
    nb_fail_read  = 0
    nb_fail       = 0
    event_list(:) = 0
!
! - Get sizes of objects
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
!
! - Get number of failure keywords
!
    call dfllne(keywf, nb_fail_read, l_fail_error)
    nb_fail = nb_fail_read
!
! - ERROR failure is required
!
    if (.not. l_fail_error) then
        nb_fail = nb_fail + 1
    endif
!
! - Access to datastructures
!
    sdlist_infor = sdlist(1:8)//'.LIST.INFOR'
    call jeveuo(sdlist_infor, 'E', vr = v_sdlist_infor)
!
! - Create datastructure
!
    sdlist_evenr = sdlist(1:8)//'.ECHE.EVENR'
    sdlist_evenk = sdlist(1:8)//'.ECHE.EVENK'
    sdlist_subdr = sdlist(1:8)//'.ECHE.SUBDR'
    call wkvect(sdlist_evenr, 'G V R'  , nb_fail*leevr, vr   = v_sdlist_evenr)
    call wkvect(sdlist_evenk, 'G V K16', nb_fail*leevk, vk16 = v_sdlist_evenk)
    call wkvect(sdlist_subdr, 'G V R'  , nb_fail*lesur, vr   = v_sdlist_subdr)
    v_sdlist_infor(9) = nb_fail
!
! - Ordering list of events
!
    AS_ALLOCATE(vi=v_work, size=nb_fail)
    i_last = 1
    do i_event = 1, nb_event
        event_curr = event_order(i_event)
        l_required = event_required(i_event)
        do i_fail = 1, nb_fail_read
            call getvtx(keywf, 'EVENEMENT', iocc=i_fail, scal=event_type)
            if (l_required) then
                if (event_type .eq. 'ERREUR') then
                    event_list(i_event) = i_fail
                endif
            else
                if (event_type .eq. event_curr) then
                    event_list(i_event) = i_fail
                    if (event_type .eq. 'DELTA_GRANDEUR') then
                        v_work(i_last) = i_fail
                        i_last = i_last + 1
                    endif
                endif
            endif
        end do
    end do
!
! - List
!
    i_fail_save = 0
    iplus       = 0
    i_last      = 1
    do i_event = 1, nb_event
!
! ----- Current event
!
        i_fail     = event_list(i_event)
        event_curr = event_order(i_event)
        l_required = event_required(i_event)
!
! ----- Event to create
!
157     continue
        if (i_fail .eq. 0) then
! --------- This event doesn't exist but is required
            if (l_required) then
                event_type = event_curr
                iplus      = 0
                i_fail_save     = i_fail_save + 1
            endif
        else
            event_type = event_curr
            if (event_type .eq. 'DELTA_GRANDEUR') then
                iplus  = v_work(i_last)
                i_fail = iplus
                if (iplus .eq. 0) then
                    i_fail = 0
                else
                    i_fail_save = i_fail_save + 1
                    i_last = i_last + 1
                endif
            else
                i_fail_save = i_fail_save+ 1
            endif
        endif
!
! ----- Get parameters
!
        l_save = .false.
        if (i_fail .eq. 0) then
            if (l_required) then
! --------- Default value for this event
                call dfdevn(action_type, subd_methode, subd_pas_mini, subd_pas, subd_niveau)
                l_save = .true.
            endif
        else
!
! --------- Get parameters of EVENEMENT for current failure keyword
!
            call dfllpe(keywf    , i_fail        , event_type,&
                        vale_ref , nom_cham      , nom_cmp   , crit_cmp,&
                        pene_maxi, resi_glob_maxi)
!
! --------- Get parameters of ACTION for current failure keyword
!
            call dfllac(keywf          , i_fail       , dtmin     , event_type,&
                        action_type    ,&
                        subd_methode   , subd_pas_mini,&
                        subd_niveau    , subd_pas     ,&
                        subd_auto      , subd_inst    , subd_duree,&
                        pcent_iter_plus, coef_maxi    )
            l_save = .true.
        endif
!
! ----- Save parameters in datastructure
!
        if (l_save) then
            call dfllsv(v_sdlist_infor, v_sdlist_evenr, v_sdlist_evenk, v_sdlist_subdr,&
                        i_fail_save   ,&
                        event_type    , vale_ref    , nom_cham        , nom_cmp       ,&
                        crit_cmp      , pene_maxi   , resi_glob_maxi  ,&
                        action_type   , subd_methode, subd_auto       , subd_pas_mini ,&
                        subd_pas      , subd_niveau , pcent_iter_plus , coef_maxi     ,&
                        subd_inst     , subd_duree)
        endif
        if (iplus .ne. 0) then
            goto 157
        endif
    end do
!
! --- TRAITEMENT PARTICULIER DU MOT-CLE 'SUBD_NIVEAU' QUI EST EN FAIT
! --- UN MOT-CLE GLOBAL A TOUTE LES METHODES DE SOUS-DECOUPAGE
! --- ON PREND LE MAX SUR TOUS LES EVENEMENTS
!
    nivmax = 0.d0
    do i_fail = 1, nb_fail
        nivear = v_sdlist_subdr(lesur*(i_fail-1)+4)
        nivmax = max(nivear,nivmax)
    end do
!
! - Save NIVEAU_MAX
!
    do i_fail = 1, nb_fail
        v_sdlist_subdr(lesur*(i_fail-1)+4) = nivmax
    end do
!
    AS_DEALLOCATE(vi=v_work)
    call jedema()
end subroutine
