subroutine mmopti(mesh, sdcont_defi, sdcont_solv)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/detrsd.h"
#include "asterfort/apinfi.h"
#include "asterfort/apvect.h"
#include "asterfort/cfnumm.h"
#include "asterfort/mmelty.h"
#include "asterfort/armin.h"
#include "asterfort/infdbg.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsred.h"
#include "asterfort/mmextm.h"
#include "asterfort/mmvalp.h"
#include "asterfort/apinfr.h"
#include "asterfort/mminfm.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mminfl.h"
#include "asterfort/cfdisi.h"
#include "blas/ddot.h"
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
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Initial options (*_INIT)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: disp_init, cnsplu, cnscon
    real(kind=8) :: jeusgn, tau1(3), tau2(3), norm(3), noor
    real(kind=8) :: mlagc(9), pres_cont(1), flag_cont
    integer :: i_zone, i_elem_slav, i_poin_appa, i_poin_elem, i_cont_poin, ibid
    integer :: nb_cont_zone, nb_poin_elem, nb_elem_slav, model_ndim
    integer :: elem_slav_indx, elem_slav_nume, elem_slav_nbno
    real(kind=8) :: vectpm(3), seuil_init, epsint, armini, ksipr1, ksipr2
    aster_logical :: l_node_excl
    integer :: jdecme
    integer :: cont_init, type_inte, pair_type
    aster_logical :: l_veri, l_gliss, l_auto_seuil
    integer :: ndexfr
    character(len=8) :: elem_slav_type
    character(len=24) :: sdappa
    integer :: ztabf
    character(len=24) :: sdcont_tabfin
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> . Initial options'
    endif
!
! - Initializations
!
    cnsplu       = '&&MMOPTI.CNSPLU'
    cnscon       = '&&MMOPTI.CNSCON'
    i_poin_appa  = 1
    i_cont_poin  = 1
!
! - Parameters
!
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO')
    model_ndim   = cfdisi(sdcont_defi,'NDIM') 
!
! - Datastructure for contact solving
!
    sdcont_tabfin = sdcont_solv(1:14)//'.TABFIN'
    call jeveuo(sdcont_tabfin, 'E', vr = v_sdcont_tabfin)
    ztabf = cfmmvd('ZTABF')
!
! - Pairing datastructure
!
    sdappa = sdcont_solv(1:14)//'.APPA'
!
! - Tolerance for CONTACT_INIT
!
    armini = armin(mesh)
    epsint = 1.d-6*armini
!
! - Preparation for SEUIL_INIT
!
    do i_zone = 1, nb_cont_zone
        l_auto_seuil = mminfl(sdcont_defi,'SEUIL_AUTO', i_zone)
        if (l_auto_seuil) then
            disp_init =  sdcont_solv(1:14)//'.INIT'
            call cnocns(disp_init, 'V', cnsplu)
            call cnsred(cnsplu, 0, [0], 1, 'LAGS_C', 'V', cnscon)
            goto 30
        endif
    end do
 30 continue
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters of zone
!
        jdecme       = mminfi(sdcont_defi,'JDECME'        , i_zone)
        nb_elem_slav = mminfi(sdcont_defi,'NBMAE'         , i_zone)
        type_inte    = mminfi(sdcont_defi,'INTEGRATION'   , i_zone)
        l_gliss      = mminfl(sdcont_defi,'GLISSIERE_ZONE', i_zone)
        l_auto_seuil = mminfl(sdcont_defi,'SEUIL_AUTO'    , i_zone)
        seuil_init   = mminfr(sdcont_defi,'SEUIL_INIT'    , i_zone)
        seuil_init   = -abs(seuil_init)
        cont_init    = mminfi(sdcont_defi,'CONTACT_INIT'  , i_zone)
!
! ----- No computation: no contact point
!
        l_veri = mminfl(sdcont_defi,'VERIF', i_zone)
        if (l_veri) then
            nb_poin_elem = mminfi(sdcont_defi, 'NBPT', i_zone)
            i_poin_appa  = i_poin_appa + nb_poin_elem
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
            call mmelty(mesh, elem_slav_nume, elem_slav_type, elem_slav_nbno, ibid)
!
! --------- Number of integration points on element
!
            call mminfm(elem_slav_indx, sdcont_defi, 'NPTM'  , nb_poin_elem)
!
! --------- SANS_GROUP_NO_FR or SANS_NOEUD_FR ?
!
            call mminfm(elem_slav_indx, sdcont_defi, 'NDEXFR', ndexfr)
!
! --------- Loop on integration points
!
            do i_poin_elem = 1, nb_poin_elem
!
! ------------- Get pairing info
!
                call apinfr(sdappa, 'APPARI_PROJ_KSI1', i_poin_appa, ksipr1)
                call apinfr(sdappa, 'APPARI_PROJ_KSI2', i_poin_appa, ksipr2)
                call apinfi(sdappa, 'APPARI_TYPE'     , i_poin_appa, pair_type)
                call apvect(sdappa, 'APPARI_VECTPM'   , i_poin_appa, vectpm)   
!
! ------------- No nodal pairing !
!
                ASSERT(pair_type .ne. 1)
!
! ------------- Definition of local basis
!
                tau1(1) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+8)
                tau1(2) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+9)
                tau1(3) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+10)
                tau2(1) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+11)
                tau2(2) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+12)
                tau2(3) = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+13)
                call mmnorm(model_ndim, tau1, tau2, norm, noor)
!
! ------------- Excluded nodes
!
                l_node_excl = v_sdcont_tabfin(ztabf*(i_cont_poin-1)+19) .gt. 0.d0
!
! ------------- Signed gap
!
                jeusgn = ddot(model_ndim, norm, 1, vectpm, 1)
!
! ------------- Option: SEUIL_INIT
!
                if (l_auto_seuil) then
                    call mmextm(sdcont_defi, cnscon, elem_slav_indx, mlagc)
                    call mmvalp(model_ndim, elem_slav_type, elem_slav_nbno, 1, ksipr1,&
                                ksipr2    , mlagc         , pres_cont)
                    v_sdcont_tabfin(ztabf*(i_cont_poin-1)+17) = pres_cont(1)
                else
                    v_sdcont_tabfin(ztabf*(i_cont_poin-1)+17) = seuil_init
                endif
!
! ------------- Option: CONTACT_INIT
!
                flag_cont = 0.d0
                if (cont_init .eq. 2) then
! ----------------- Only interpenetrated points
                    if (jeusgn .le. epsint) then
                        flag_cont = 1.d0
                    endif
                else if (cont_init .eq. 1) then
! ----------------- All points
                    flag_cont = 1.d0
                else if (cont_init .eq. 0) then
! ----------------- No initial contact
                    flag_cont = 0.d0
                else
                    ASSERT(.false.)
                endif
!
! ------------- Option: GLISSIERE
!
                if (l_gliss) then
                    if (cont_init .eq. 1) then
                        v_sdcont_tabfin(ztabf*(i_cont_poin-1)+18) = 1.d0          
                    endif
                    if (cont_init .eq. 2 .and. (jeusgn .le. epsint)) then
                        v_sdcont_tabfin(ztabf*(i_cont_poin-1)+18) = 1.d0        
                    endif
                endif
!
! ------------- Excluded nodes => no contact !
!
                if (l_node_excl) then
                    flag_cont = 0.d0
                endif
!
! ------------- Save initial contact
!    
                v_sdcont_tabfin(ztabf*(i_cont_poin-1)+23) = flag_cont
!
! ------------- Next contact point
!
                i_poin_appa  = i_poin_appa + 1
                i_cont_poin  = i_cont_poin + 1
            end do
        end do
 25     continue
    end do
!
    call detrsd('CHAM_NO_S', cnsplu)
    call detrsd('CHAM_NO_S', cnscon)
!
end subroutine
