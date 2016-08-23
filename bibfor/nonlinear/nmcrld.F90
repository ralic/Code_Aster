subroutine nmcrld(sddisc)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/dfllsv.h"
#include "asterfort/dfllvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utdidt.h"
#include "asterfort/wkvect.h"
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
    character(len=19) :: sddisc
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - DISCRETISATION)
!
! CREATION EVENEMENTS ERREURS: ARRET
!
! --------------------------------------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_fail_save, nb_fail
    integer :: leevr, leevk, lesur
    character(len=24) :: sddisc_evenr
    real(kind=8), pointer :: v_sddisc_evenr(:) => null()
    character(len=24) :: sddisc_evenk
    character(len=16), pointer :: v_sddisc_evenk(:) => null()
    character(len=24) :: sddisc_subdr
    real(kind=8), pointer :: v_sddisc_subdr(:) => null()
    character(len=24) :: sddisc_infor
    real(kind=8), pointer :: v_sddisc_infor(:) => null()
    character(len=16) :: event_type, action_type
    real(kind=8) :: dtmin, vale_ref
    character(len=16) :: nom_cham, nom_cmp,crit_cmp
    real(kind=8) :: pene_maxi, resi_glob_maxi
    character(len=16) :: subd_methode
    real(kind=8) :: subd_pas_mini
    integer :: subd_niveau, subd_pas
    character(len=16) :: subd_auto
    real(kind=8) :: subd_inst, subd_duree, pcent_iter_plus, coef_maxi
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nb_fail     = 2
    i_fail_save = 1
    call utdidt('E', sddisc, 'LIST', 'NECHEC', vali_ = nb_fail)
    call utdidt('L', sddisc, 'LIST', 'DTMIN' , valr_ = dtmin)
!
! - Get sizes of objects
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
!
! - Create datastructure
!
    sddisc_infor = sddisc(1:19)//'.LINF'
    sddisc_evenr = sddisc(1:19)//'.EEVR'
    sddisc_evenk = sddisc(1:19)//'.EEVK'
    sddisc_subdr = sddisc(1:19)//'.ESUR'
    call wkvect(sddisc_evenr, 'V V R'  , nb_fail*leevr, vr   = v_sddisc_evenr)
    call wkvect(sddisc_evenk, 'V V K16', nb_fail*leevk, vk16 = v_sddisc_evenk)
    call wkvect(sddisc_subdr, 'V V R'  , nb_fail*lesur, vr   = v_sddisc_subdr)
!
! - Create reference error
!
    event_type      = 'ERRE'
    action_type     = 'ARRET'
    subd_methode    = ' '
    subd_auto       = ' '
    subd_pas_mini   = 0.d0
    subd_pas        = 0
    subd_niveau     = 0
    pcent_iter_plus = 0.d0
    coef_maxi       = 0.d0
    subd_inst       = 0.d0
    subd_duree      = 0.d0
    pene_maxi       = 0.d0
    crit_cmp        = ' '
    vale_ref        = 0.d0
    nom_cham        = ' '
    nom_cmp         = ' '
    resi_glob_maxi  = 0.d0
!
! - Create reference error
!
    call dfllsv(v_sddisc_infor, v_sddisc_evenr, v_sddisc_evenk, v_sddisc_subdr,&
                i_fail_save   ,&
                event_type    , vale_ref    , nom_cham        , nom_cmp       ,&
                crit_cmp      , pene_maxi   , resi_glob_maxi  ,&
                action_type   , subd_methode, subd_auto       , subd_pas_mini ,&
                subd_pas      , subd_niveau , pcent_iter_plus , coef_maxi     ,&
                subd_inst     , subd_duree)
!
    call jedema()
!
end subroutine
