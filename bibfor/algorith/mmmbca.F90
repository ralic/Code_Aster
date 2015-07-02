subroutine mmmbca(mesh          , sddyna   , iter_newt, sdcont_defi, sdcont_solv   ,&
                  sdstat        , hval_incr, hval_algo, time_curr  , loop_cont_node,&
                  loop_cont_conv)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfnumm.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mcomce.h"
#include "asterfort/mmalgo.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mm_cycl_prop.h"
#include "asterfort/mm_cycl_stat.h"
#include "asterfort/mmeval_prep.h"
#include "asterfort/mmstac.h"
#include "asterfort/mmeven.h"
#include "asterfort/mmextm.h"
#include "asterfort/mmglis.h"
#include "asterfort/mmimp4.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mmstaf.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: sddyna
    integer, intent(in) :: iter_newt
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    real(kind=8), intent(in) :: time_curr
    aster_logical, intent(out) :: loop_cont_conv
    integer, intent(out) :: loop_cont_node
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Management of contact loop
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sddyna           : dynamic parameters datastructure
! In  iter_newt        : index of current Newton iteration
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
! In  sdstat           : datastructure for statistics
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  time_curr        : current time
! Out loop_cont_conv   : .true. if contact loop converged
! Out loop_cont_node   : number of contact state changing
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ztabf
    integer :: ifm, niv
    integer :: jdecme, elem_slav_indx, elem_slav_nume, elem_mast_nume
    integer :: indi_cont_curr, indi_cont_prev, indi_frot_prev, indi_frot_curr
    integer :: i_zone, i_elem_slav, i_cont_poin, i_poin_elem
    integer :: model_ndim, nb_cont_zone
    integer :: elem_slav_nbno, nb_poin_elem, nb_elem_slav
    integer :: indi_cont_eval, indi_frot_eval
    integer :: indi_cont_init, indi_frot_init
    real(kind=8) :: ksipr1, ksipr2, ksipc1, ksipc2
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: lagr_cont_node(9), lagr_fro1_node(9), lagr_fro2_node(9)
    real(kind=8) :: elem_slav_coor(27)
    real(kind=8) :: lagr_cont_poin
    real(kind=8) :: gap, gap_speed, gap_user
    real(kind=8) :: pres_frot(3), gap_user_frot(3)
    real(kind=8) :: coef_cont, coef_frot
    character(len=8) :: elem_slav_type
    character(len=19) :: cnscon, cnsfr1, cnsfr2
    character(len=24) :: sdcont_mdecol
    aster_logical, pointer :: v_sdcont_mdecol(:) => null()
    character(len=19) :: oldgeo, newgeo
    character(len=19) :: speed_field, chdepd
    character(len=19) :: depdel, depplu, vitplu
    aster_logical :: l_glis, l_speed, scotch
    aster_logical :: l_glis_init, l_veri, l_exis_glis, l_loop_cont, l_coef_adap
    aster_logical :: l_frot_zone, l_pena_frot, l_frot
    integer :: i_loop_geom, i_loop_frot, i_loop_cont
    character(len=24) :: sdcont_cychis, sdcont_cyccoe
    !real(kind=8), pointer :: v_sdcont_cychis(:) => null()
    !real(kind=8), pointer :: v_sdcont_cyccoe(:) => null()
    integer :: jcyhis, jcycoe
    character(len=24) :: sdcont_tabfin, sdcont_jsupco, sdcont_apjeu
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
    real(kind=8), pointer :: v_sdcont_jsupco(:) => null()
    real(kind=8), pointer :: v_sdcont_apjeu(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... ACTIVATION/DESACTIVATION'
    endif
!
! - Initializations
!
    loop_cont_conv = .true.
    loop_cont_node = 0
!
! - Parameters
!
    l_speed      = ndynlo(sddyna,'FORMUL_VITE')
    l_exis_glis  = cfdisl(sdcont_defi,'EXIS_GLISSIERE')
    l_loop_cont  = cfdisl(sdcont_defi,'CONT_BOUCLE')
    l_coef_adap  = cfdisl(sdcont_defi,'COEF_ADAPT')
    model_ndim   = cfdisi(sdcont_defi,'NDIM' )
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO')
    l_frot       = cfdisl(sdcont_defi,'FROTTEMENT')
!
! - Acces to contact objects
!
    ztabf = cfmmvd('ZTABF')
    sdcont_tabfin = sdcont_solv(1:14)//'.TABFIN'
    sdcont_jsupco = sdcont_solv(1:14)//'.JSUPCO'
    sdcont_apjeu  = sdcont_solv(1:14)//'.APJEU'
    sdcont_mdecol = sdcont_solv(1:14)//'.MDECOL'
    call jeveuo(sdcont_tabfin, 'E', vr = v_sdcont_tabfin)
    call jeveuo(sdcont_jsupco, 'E', vr = v_sdcont_jsupco)
    call jeveuo(sdcont_apjeu , 'E', vr = v_sdcont_apjeu)
    call jeveuo(sdcont_mdecol, 'E', vl = v_sdcont_mdecol)
!
! - Acces to cycling objects
!
    sdcont_cychis = sdcont_solv(1:14)//'.CYCHIS'
    sdcont_cyccoe = sdcont_solv(1:14)//'.CYCCOE'
    call jeveuo(sdcont_cychis, 'E', jcyhis)
    call jeveuo(sdcont_cyccoe, 'E', jcycoe)
!
! - Get hat variables
!
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
    call nmchex(hval_incr, 'VALINC', 'VITPLU', vitplu)
    call nmchex(hval_algo, 'SOLALG', 'DEPDEL', depdel)
!
! - Get off indicator for speed schemes
!
    scotch = v_sdcont_mdecol(1)
!
! - Geometric actualisation
!
    oldgeo = mesh//'.COORDO'
    newgeo = sdcont_solv(1:14)//'.NEWG'
    call mmfield_prep(oldgeo, newgeo,&
                      l_update_ = .true._1, field_update_ = depplu, alpha_ = 1.d0)
!
! - Create speed field
!    
    speed_field = '&&MMMBCA.ACTUVIT'
    if (l_speed) then
        call mmfield_prep(oldgeo, speed_field,&
                          l_update_ = .true._1, field_update_ = vitplu)
    endif
!
! - Prepare displacement field to get contact Lagrangien multiplier
!
    cnscon = '&&MMMBCA.CNSCON'
    call mmfield_prep(depplu, cnscon,&
                      l_sort_ = .true._1, nb_cmp_ = 1, list_cmp_ = ['LAGS_C  '])
!
! - Prepare displacement field to get friction Lagrangien multiplier
!
    chdepd = '&&MMMBCA.CHDEPD'
    cnsfr1 = '&&MMMBCA.CNSFR1'
    cnsfr2 = '&&MMMBCA.CNSFR2'
    if (l_frot) then
        call mmfield_prep(depdel, cnsfr1,&
                          l_sort_ = .true._1, nb_cmp_ = 1, list_cmp_ = ['LAGS_F1 '])
        if (model_ndim .eq. 3) then
            call mmfield_prep(depdel, cnsfr2,&
                              l_sort_ = .true._1, nb_cmp_ = 1, list_cmp_ = ['LAGS_F2 '])
        endif
        call mmfield_prep(oldgeo, chdepd,&
                          l_update_ = .true._1, field_update_ = depdel)
    endif
!
! - Loop on contact zones
!
    i_cont_poin = 1
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters of zone
!
        l_glis       = mminfl(sdcont_defi,'GLISSIERE_ZONE' , i_zone)
        l_veri       = mminfl(sdcont_defi,'VERIF'          , i_zone)
        nb_elem_slav = mminfi(sdcont_defi,'NBMAE'          , i_zone)
        jdecme       = mminfi(sdcont_defi,'JDECME'         , i_zone)
        l_frot_zone  = mminfl(sdcont_defi,'FROTTEMENT_ZONE', i_zone)
        l_pena_frot  = mminfl(sdcont_defi,'ALGO_FROT_PENA' , i_zone)
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
! --------- Informations about slave element
!
            call cfnumm(sdcont_defi, elem_slav_indx, elem_slav_nume)
!
! --------- Number of integration points on element
!
            call mminfm(elem_slav_indx, sdcont_defi, 'NPTM', nb_poin_elem)
!
! --------- Get coordinates of slave element
!
            call mcomce(mesh          , newgeo, elem_slav_nume, elem_slav_coor, elem_slav_type,&
                        elem_slav_nbno)
!
! --------- Get value of contact lagrangian multiplier at slave nodes
!
            call mmextm(sdcont_defi, cnscon, elem_slav_indx, lagr_cont_node)
!
! --------- Get value of friction lagrangian multipliers at slave nodes
!
            if (l_frot_zone) then
                call mmextm(sdcont_defi, cnsfr1, elem_slav_indx, lagr_fro1_node)
                if (model_ndim .eq. 3) then
                    call mmextm(sdcont_defi, cnsfr2, elem_slav_indx, lagr_fro2_node)
                endif
            endif       
!
! --------- Loop on integration points
!
            do i_poin_elem = 1, nb_poin_elem
!
! ------------- Current master element
!
                elem_mast_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+3))
!
! ------------- Get coordinates of the contact point 
!
                ksipc1 = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+4)
                ksipc2 = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+5)
!
! ------------- Get coordinates of the projection of contact point 
!
                ksipr1 = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+6)
                ksipr2 = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+7)
!
! ------------- Get local basis
!
                tau1(1) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+8)
                tau1(2) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+9)
                tau1(3) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+10)
                tau2(1) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+11)
                tau2(2) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+12)
                tau2(3) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+13)
!
! ------------- Compute gap and contact pressure
!
                call mmeval_prep(mesh   , time_curr  , model_ndim     , sdcont_defi , sdcont_solv,&
                                 l_speed, speed_field, i_zone         ,&
                                 ksipc1 , ksipc2     , ksipr1         , ksipr2     ,&
                                 tau1   , tau2       ,&
                                 elem_slav_indx, elem_slav_nume, elem_slav_nbno,&
                                 elem_slav_type, elem_slav_coor,&
                                 elem_mast_nume,&
                                 lagr_cont_node,&
                                 norm   , &
                                 gap    , gap_user, gap_speed, lagr_cont_poin)
!
! ------------- Previous status and coefficients
!
                indi_cont_init = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+23))
                if (l_frot_zone) then
                    indi_frot_init = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+24))
                endif
                coef_cont = zr(jcyhis-1+25*(i_cont_poin-1)+2)
                coef_frot = zr(jcyhis-1+25*(i_cont_poin-1)+6)
!
! ------------- Initial bilateral contact ?
!
                l_glis_init = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+18)).eq.1
!
! ------------- Total gap
!
                gap = gap+gap_user
!
! ------------- Save gaps
!
                v_sdcont_jsupco(i_cont_poin) = gap_user
                v_sdcont_apjeu(i_cont_poin)  = gap
!
! ------------- Excluded nodes => no contact !
!
                if (nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+19)) .eq. 1) then
                    indi_cont_curr = 0
                    goto 19
                endif
!
! ------------- Evaluate contact status
!
                call mmstac(gap, lagr_cont_poin, coef_cont, indi_cont_eval)
!
! ------------- Evaluate friction status
!
                if (l_frot_zone) then
                    call mmstaf(mesh, model_ndim, chdepd, coef_frot, l_pena_frot,&
                          elem_slav_nume, elem_slav_type, elem_slav_nbno, elem_mast_nume, ksipc1,&
                                ksipc2, ksipr1, ksipr2, lagr_fro1_node, lagr_fro2_node,&
                                tau1, tau2, norm, pres_frot, gap_user_frot,&
                                indi_frot_eval)
                endif
!
! ------------- Status treatment
!
                call mmalgo(sdcont_defi, sdcont_solv, l_loop_cont, l_frot_zone, l_speed,&
                            l_glis_init, l_coef_adap, i_zone, i_cont_poin, indi_cont_init,&
                            indi_cont_eval, indi_frot_eval, gap, gap_speed, lagr_cont_poin,&
                            gap_user_frot, pres_frot, zr(jcyhis), zr(jcycoe), indi_cont_curr,&
                            indi_frot_curr, loop_cont_node, loop_cont_conv, scotch)
!
 19             continue
!
! ------------- Save status
!
                v_sdcont_tabfin(ztabf*(i_cont_poin-1)+23) = indi_cont_curr
                if (l_frot_zone) then
                    v_sdcont_tabfin(ztabf*(i_cont_poin-1)+24) = indi_frot_curr
                endif
!
! ------------- Print status
!
                if (niv .ge. 2) then
                    call mmimp4(ifm, mesh, elem_slav_nume, i_poin_elem, indi_cont_prev,&
                                indi_cont_curr, indi_frot_prev, indi_frot_curr, l_frot, l_speed,&
                                l_glis, gap, gap_speed, lagr_cont_poin)
                endif
!
! ------------- Next contact point
!
                i_cont_poin = i_cont_poin + 1
            end do
        end do
 25     continue
    end do
!
! - Bilateral contact management
!
    if (loop_cont_conv .and. l_exis_glis) then
        call mmglis(sdcont_defi, sdcont_solv)
    endif
!
! - Statistics for cycling
!
    call mm_cycl_stat(sdstat, sdcont_defi, sdcont_solv)
!
! - Propagation of coefficient
!
    if (l_coef_adap) then
        call mm_cycl_prop(sdcont_defi, sdcont_solv, zr(jcyhis), zr(jcycoe))
    endif
!
! - Event management for impact
!
    call mmbouc(sdcont_solv, 'GEOM', 'READ', i_loop_geom)
    call mmbouc(sdcont_solv, 'FROT', 'READ', i_loop_frot)
    call mmbouc(sdcont_solv, 'CONT', 'READ', i_loop_cont)
    if ((iter_newt.eq.0) .and.&
        (i_loop_geom.eq.1) .and. (i_loop_frot.eq.1) .and. (i_loop_cont.eq.1)) then
        call mmeven('INI', sdcont_defi, sdcont_solv)
    else
        call mmeven('FIN', sdcont_defi, sdcont_solv)
    endif
!
! - Get off indicator for speed schemes
!
    v_sdcont_mdecol(1) = scotch
!
! - Cleaning
!
    call jedetr(newgeo)
    call jedetr(speed_field)
    call jedetr(chdepd)
    call detrsd('CHAM_NO_S', cnscon)
    call detrsd('CHAM_NO_S', cnsfr1)
    call detrsd('CHAM_NO_S', cnsfr2)
!
    call jedema()
end subroutine
