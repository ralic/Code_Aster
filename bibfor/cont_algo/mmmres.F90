subroutine mmmres(mesh       , time_incr, ds_contact, disp_cumu_inst, sddisc, &
                  hval_veasse, cnsinr   , cnsper)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/detrsd.h"
#include "asterfort/iseven.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/mcopco.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mmmred.h"
#include "asterfort/mmmreg.h"
#include "asterfort/nmchex.h"
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/mmfield_prep.h"
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
    character(len=8), intent(in) :: mesh
    real(kind=8), intent(in) :: time_incr
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: disp_cumu_inst
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: hval_veasse(*)
    character(len=19), intent(in) :: cnsinr
    character(len=19), intent(in) :: cnsper
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Post-treatment
!
! Continue method - Prepare post-treatment fields
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  time_incr        : time increment
! In  ds_contact       : datastructure for contact management
! In  disp_cumu_inst   : displacement increment from beginning of current time
! In  sddisc           : datastructure for discretization
! In  hval_veasse      : hat-variable for vectors (node fields)
! In  cnsinr           : nodal field (CHAM_NO_S) for CONT_NOEU 
! In  cnsper           : nodal field (CHAM_NO_S) to save percussions
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8), parameter :: eps=1.d-6
    integer :: i_cont_poin, i_zone, i_elem_slav, i_poin_elem
    integer :: nb_dof, nb_poin_elem, nb_elem_slav, nb_cont_zone, model_ndim, nt_cont_poin
    integer :: ztabf, zresu, zperc
    integer :: node_slav_nume, elem_mast_nume, elem_slav_indx
    integer :: jdecme, indi_cont
    real(kind=8) :: gli, gli1, gli2
    real(kind=8) :: rn, rnx, rny, rnz
    real(kind=8) :: rtax, rtay, rtaz
    real(kind=8) :: rtgx, rtgy, rtgz
    real(kind=8) :: r, rx, ry, rz
    real(kind=8) :: imp, impx, impy, impz
    real(kind=8) :: node_status, lagsf
    real(kind=8) :: ksipr1, ksipr2, proj(3)
    character(len=19) :: disp_cumu_s
    character(len=19) :: cneltc_s, cneltf_s
    character(len=19) :: cneltc, cneltf
    character(len=19) :: newgeo
    character(len=24) :: sdcont_tabfin, sdcont_apjeu
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
    real(kind=8), pointer :: v_sdcont_apjeu(:) => null()
    real(kind=8) :: valras
    aster_logical :: l_frot_zone, l_veri, lcolli, laffle, l_frot
    real(kind=8), pointer :: v_cnsper_cnsv(:) => null()
    real(kind=8), pointer :: v_cnsinr_cnsv(:) => null()
    aster_logical, pointer :: v_cnsper_cnsl(:) => null()
    aster_logical, pointer :: v_cnsinr_cnsl(:) => null()
    real(kind=8), pointer :: v_cneltc(:) => null()
    real(kind=8), pointer :: v_cneltf(:) => null()
    real(kind=8), pointer :: v_slav_slide(:) => null()
    real(kind=8), pointer :: v_mast_slide(:) => null()
    real(kind=8), pointer :: v_disp_cumu(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    cneltc       = '&&MMMRES.CONT'
    cneltc_s     = '&&MMMRES.FCTCN'
    cneltf       = '&&MMMRES.FROT'
    cneltf_s     = '&&MMMRES.FROTCN'
    disp_cumu_s  = '&&MMMRES.DEPCN'
!
! - Get parameters
!
    l_frot       = cfdisl(ds_contact%sdcont_defi,'FROTTEMENT')
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO')
    nt_cont_poin = cfdisi(ds_contact%sdcont_defi,'NTPC' )
    model_ndim   = cfdisi(ds_contact%sdcont_defi,'NDIM')
!
! - Collision
!
    laffle = .false.
    valras = 1.d-3
    call iseven(sddisc, 'COLLISION', lcolli)
!
! - Acces to contact objects
!
    ztabf = cfmmvd('ZTABF')
    zperc = cfmmvd('ZPERC')
    zresu = cfmmvd('ZRESU')
    sdcont_tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    sdcont_apjeu  = ds_contact%sdcont_solv(1:14)//'.APJEU'
    call jeveuo(sdcont_tabfin, 'E', vr = v_sdcont_tabfin)
    call jeveuo(sdcont_apjeu , 'E', vr = v_sdcont_apjeu)
!
! - Geometric update
!
    newgeo = ds_contact%sdcont_solv(1:14)//'.NEWG'
!
! - Get fields
!
    call nmchex(hval_veasse, 'VEASSE', 'CNELTC', cneltc)
    call nmchex(hval_veasse, 'VEASSE', 'CNELTF', cneltf)
!
! - Prepare nodal fields (select right components)
!
    call mmmred(model_ndim, l_frot, disp_cumu_inst, disp_cumu_s, nb_dof)
    call mmfield_prep(cneltc, cneltc_s,&
                      l_sort_ = .true._1, nb_cmp_ = model_ndim,&
                      list_cmp_ = ['DX      ','DY      ','DZ      '])
    if (l_frot) then
        call mmfield_prep(cneltf, cneltf_s,&
                          l_sort_ = .true._1, nb_cmp_ = model_ndim,&
                          list_cmp_ = ['DX      ','DY      ','DZ      '])
    endif
!
! - Access to fields
!
    call jeveuo(disp_cumu_s(1:19)//'.CNSV', 'L', vr = v_disp_cumu)
    call jeveuo(cnsinr(1:19)//'.CNSV'     , 'E', vr = v_cnsinr_cnsv)
    call jeveuo(cnsinr(1:19)//'.CNSL'     , 'E', vl = v_cnsinr_cnsl)
    call jeveuo(cnsper(1:19)//'.CNSV'     , 'E', vr = v_cnsper_cnsv)
    call jeveuo(cnsper(1:19)//'.CNSL'     , 'E', vl = v_cnsper_cnsl)
    call jeveuo(cneltc_s(1:19)//'.CNSV', 'L', vr = v_cneltc)
    if (l_frot) then
        call jeveuo(cneltf_s(1:19)//'.CNSV', 'L', vr = v_cneltf)
    endif
!
! - Compute slide
!
    AS_ALLOCATE(vr = v_slav_slide, size = 2*nt_cont_poin)
    AS_ALLOCATE(vr = v_mast_slide, size = 2*nt_cont_poin)
    call mmmreg(mesh        , ds_contact  , v_disp_cumu, nb_dof,&
                v_slav_slide, v_mast_slide)
!
! - Loop on contact zones
!
    i_cont_poin = 1
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters of zone
!
        l_veri       = mminfl(ds_contact%sdcont_defi,'VERIF'          , i_zone)
        nb_elem_slav = mminfi(ds_contact%sdcont_defi,'NBMAE'          , i_zone)
        jdecme       = mminfi(ds_contact%sdcont_defi,'JDECME'         , i_zone)
        l_frot_zone  = mminfl(ds_contact%sdcont_defi,'FROTTEMENT_ZONE', i_zone)
!
! ----- No computation: no contact point
!
        if (l_veri) then
            goto 25
        endif
!
! ----- Loop on slave elements
!
        do i_elem_slav = 1, nb_elem_slav
!
! --------- Slave element index in contact datastructure
!
            elem_slav_indx = jdecme + i_elem_slav
!
! --------- Number of integration points on element
!
            call mminfm(elem_slav_indx, ds_contact%sdcont_defi, 'NPTM', nb_poin_elem)
!
! --------- Loop on integration points
!
            do i_poin_elem = 1, nb_poin_elem
!
                gli  = 0.d0
                gli1 = 0.d0
                gli2 = 0.d0
                rtax = 0.d0
                rtay = 0.d0
                rtaz = 0.d0
                rtgx = 0.d0
                rtgy = 0.d0
                rtgz = 0.d0
                rn   = 0.d0
                rnx  = 0.d0
                rny  = 0.d0
                rnz  = 0.d0
                node_status = 0.d0
!
! ------------- Get slave node index
!
                node_slav_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+25))
                if (node_slav_nume .le. 0) then
                    goto 99
                endif
!
! ------------- Contact status
!
                indi_cont      = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+23))
!
! ------------- Projection
!
                ksipr1         =      v_sdcont_tabfin(ztabf*(i_cont_poin-1)+6)
                ksipr2         =      v_sdcont_tabfin(ztabf*(i_cont_poin-1)+7)
                elem_mast_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+3))
                call mcopco(mesh  , newgeo, model_ndim, elem_mast_nume, ksipr1,&
                            ksipr2, proj)
!
! ------------- Get information for contact
!
                if (indi_cont .eq. 1) then
!
                    node_status = 1.d0
!
! ------------- Get nodal reaction for contact
!
                    if (model_ndim .eq. 3) then
                        rnx = v_cneltc(3*(node_slav_nume-1)+1)
                        rny = v_cneltc(3*(node_slav_nume-1)+2)
                        rnz = v_cneltc(3*(node_slav_nume-1)+3)
                        rn  = sqrt(rnx**2+rny**2+rnz**2)
                    else if (model_ndim.eq.2) then
                        rnx = v_cneltc(2*(node_slav_nume-1)+1)
                        rny = v_cneltc(2*(node_slav_nume-1)+2)
                        rn  = sqrt(rnx**2+rny**2)
                    else
                        ASSERT(.false.)
                    endif
!
! ----------------- Very near contact
!
                    if (rn .le. valras) then
                        laffle = .true.
                    endif
!
! ----------------- Friction
!
                    if (l_frot_zone) then
!
! --------------------- Compute slides
!
                        if (model_ndim .eq. 3) then
                            gli1 = v_slav_slide(2*(i_cont_poin-1)+1) -&
                                   v_mast_slide(2*(i_cont_poin-1)+1)
                            gli2 = v_slav_slide(2*(i_cont_poin-1)+2) -&
                                   v_mast_slide(2*(i_cont_poin-1)+2)
                            gli  = sqrt(gli1**2+gli2**2)
                        else if (model_ndim.eq.2) then
                            gli1 = v_slav_slide(i_cont_poin) -&
                                   v_mast_slide(i_cont_poin)
                            gli  = abs(gli1)
                        else
                            ASSERT(.false.)
                        endif
!
! --------------------- Friction Lagrange
!
                        if (model_ndim .eq. 3) then
                            lagsf = sqrt(v_disp_cumu(nb_dof*(node_slav_nume-1)+5)**2+&
                                         v_disp_cumu(nb_dof*(node_slav_nume-1)+6)**2)
                        else if (model_ndim.eq.2) then
                            lagsf = abs (v_disp_cumu(nb_dof*(node_slav_nume-1)+4))
                        else
                            ASSERT(.false.)
                        endif
!
! --------------------- Stick or slide ?
!
                        if (lagsf .ge. 0.999d0) then
                            node_status = 2.d0
                            if (model_ndim .eq. 3) then
                                rtgx = v_cneltf(3*(node_slav_nume-1)+1)
                                rtgy = v_cneltf(3*(node_slav_nume-1)+2)
                                rtgz = v_cneltf(3*(node_slav_nume-1)+3)
                            else if (model_ndim.eq.2) then
                                rtgx = v_cneltf(2*(node_slav_nume-1)+1)
                                rtgy = v_cneltf(2*(node_slav_nume-1)+2)
                            else
                                ASSERT(.false.)
                            endif
                        else
                            node_status = 1.d0
                            if (model_ndim .eq. 3) then
                                rtax = v_cneltf(3*(node_slav_nume-1)+1)
                                rtay = v_cneltf(3*(node_slav_nume-1)+2)
                                rtaz = v_cneltf(3*(node_slav_nume-1)+3)
                            else if (model_ndim.eq.2) then
                                rtax = v_cneltf(2*(node_slav_nume-1)+1)
                                rtay = v_cneltf(2*(node_slav_nume-1)+2)
                            else
                                ASSERT(.false.)
                            endif
                        endif
                    else
                        lagsf       = 0.d0
                        node_status = 2.d0
                    endif
                endif
!
! ------------- Total reaction
!
                rx = rnx + rtax + rtgx
                ry = rny + rtay + rtgy
                rz = rnz + rtaz + rtgz
                r  = sqrt(rx**2.d0+ry**2.d0+rz**2.d0)
!
! ------------- Percussion
!
                if (r .le. eps) then
                    imp  = 0.d0
                    impx = 0.d0
                    impy = 0.d0
                    impz = 0.d0
                else
                    imp  = v_cnsper_cnsv(zperc*(node_slav_nume-1)+1) + r*time_incr
                    impx = v_cnsper_cnsv(zperc*(node_slav_nume-1)+2) + rx* time_incr
                    impy = v_cnsper_cnsv(zperc*(node_slav_nume-1)+3) + ry* time_incr
                    impz = v_cnsper_cnsv(zperc*(node_slav_nume-1)+4) + rz* time_incr
                endif
!
! ------------- Save in CONT_NOEU field
!
                v_cnsinr_cnsv(zresu*(node_slav_nume-1)+1) = node_status
                v_cnsinr_cnsv(zresu*(node_slav_nume-1)+2) = -v_sdcont_apjeu(i_cont_poin)
                v_cnsinr_cnsl(zresu*(node_slav_nume-1)+1) = .true.
                v_cnsinr_cnsl(zresu*(node_slav_nume-1)+2) = .true.
                if (model_ndim .eq. 3) then
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+3 ) = rn
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+4 ) = rnx
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+5 ) = rny
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+6 ) = rnz
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+7 ) = gli1
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+8 ) = gli2
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+9 ) = gli
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+10) = rtax
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+11) = rtay
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+12) = rtaz
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+13) = rtgx
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+14) = rtgy
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+15) = rtgz
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+16) = rx
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+17) = ry
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+18) = rz
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+19) = r
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+21) = imp
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+22) = impx
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+23) = impy
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+24) = impz
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+28) = proj(1)
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+29) = proj(2)
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+30) = proj(3)
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+3 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+4 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+5 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+6 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+7 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+8 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+9 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+10) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+11) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+12) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+13) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+14) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+15) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+16) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+17) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+18) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+19) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+21) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+22) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+23) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+24) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+28) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+29) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+30) = .true.
                else if (model_ndim.eq.2) then
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+3 ) = rn
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+4 ) = rnx
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+5 ) = rny
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+7 ) = gli1
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+9 ) = gli
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+10) = rtax
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+11) = rtay
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+13) = rtgx
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+14) = rtgy
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+16) = rx
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+17) = ry
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+19) = r
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+21) = imp
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+22) = impx
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+23) = impy
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+28) = proj(1)
                    v_cnsinr_cnsv(zresu*(node_slav_nume-1)+29) = proj(2)
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+3 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+4 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+5 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+7 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+9 ) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+10) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+11) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+13) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+14) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+16) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+17) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+19) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+21) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+22) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+23) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+28) = .true.
                    v_cnsinr_cnsl(zresu*(node_slav_nume-1)+29) = .true.
                else
                    ASSERT(.false.)
                endif
!
! ------------- Save in percussion field
!
                v_cnsper_cnsv(zperc*(node_slav_nume-1)+1) = imp
                v_cnsper_cnsv(zperc*(node_slav_nume-1)+2) = impx
                v_cnsper_cnsv(zperc*(node_slav_nume-1)+3) = impy
                v_cnsper_cnsl(zperc*(node_slav_nume-1)+1) = .true.
                v_cnsper_cnsl(zperc*(node_slav_nume-1)+2) = .true.
                v_cnsper_cnsl(zperc*(node_slav_nume-1)+3) = .true.
!
                if (model_ndim .eq. 3) then
                    v_cnsper_cnsv(zperc*(node_slav_nume-1)+4) = impz
                    v_cnsper_cnsl(zperc*(node_slav_nume-1)+4) = .true.
                endif
 99             continue
!
! ------------- Next contact point
!
                i_cont_poin = i_cont_poin + 1
            end do
        end do
 25     continue
    end do
!
! - Clean
!
    call jedetr(cneltc)
    call detrsd('CHAMP', cneltc_s)
    call jedetr(cneltf)
    call detrsd('CHAMP', cneltf_s)
    call detrsd('CHAMP', disp_cumu_s)
    AS_DEALLOCATE(vr = v_slav_slide)
    AS_DEALLOCATE(vr = v_mast_slide)
!
! - Alarm for COLLISION
!
    if (laffle .and. lcolli) then
        call utmess('A', 'CONTACT3_98')
    endif
!
    call jedema()
end subroutine
