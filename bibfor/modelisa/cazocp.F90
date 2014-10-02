subroutine cazocp(sdcont)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: sdcont
!
! --------------------------------------------------------------------------------------------------
!
! Contact
!
! Read main parameters (not depending on contact zone)
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont         : name of contact datastructure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_defi

    integer :: nbreac, lgbloc, gcpmax, premax
    integer :: reacca, reacbs, reacbg
    character(len=16) :: rech, prec, reac, typcon, isto, elim_edge
    character(len=16) :: algoco, algofr, algoge
    integer :: noc
    real(kind=8) :: precis, coefrs
    real(kind=8) :: resige, resifr
    aster_logical :: l_cont_gcp, l_newt_fr
    aster_logical :: l_cont_disc, l_cont_cont, l_cont_xfem, l_frot, l_cont_mesh
    character(len=16) :: lissa, coef_adap
    character(len=24) :: sdcont_para_r
    real(kind=8), pointer :: v_para_r(:) => null()
    character(len=24) :: sdcont_para_i
    integer, pointer :: v_para_i(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    sdcont_defi  = sdcont(1:8)//'.CONTACT'
    elim_edge = 'DUAL'
    reac      = 'AUTOMATIQUE'
    algoco    = ' '
    algofr    = ' '
    algoge    = ' '
    nbreac    = 2
    lgbloc    = 10
    resige    = 1.d-2
    resifr    = 1.d-2
    l_newt_fr = .false.
!
! - Access to datastructure
!
    sdcont_para_r = sdcont_defi(1:16)//'.PARACR'
    sdcont_para_i = sdcont_defi(1:16)//'.PARACI'
    call jeveuo(sdcont_para_r, 'E', vr = v_para_r)
    call jeveuo(sdcont_para_i, 'E', vi = v_para_i)
!
! - Active functionnalites
!
    l_cont_disc = cfdisl(sdcont_defi,'FORMUL_DISCRETE')
    l_cont_cont = cfdisl(sdcont_defi,'FORMUL_CONTINUE')
    l_cont_mesh = l_cont_disc.or.l_cont_cont
    l_cont_xfem = cfdisl(sdcont_defi,'FORMUL_XFEM')
    l_cont_gcp  = cfdisl(sdcont_defi,'CONT_GCP' )
    l_frot      = cfdisl(sdcont_defi,'FROTTEMENT')
!
! - Geometric algorithm
!
    if (l_cont_cont) then
        call getvtx(' ', 'ALGO_RESO_GEOM', scal=algoge)
    else if (l_cont_xfem) then
        algoge = 'POINT_FIXE'
    else if (l_cont_disc) then
        algoge = 'POINT_FIXE'
    else
        ASSERT(.false.)
    endif
!
    if (algoge .eq. 'POINT_FIXE') then
        v_para_i(9) = 0
    else if (algoge.eq.'NEWTON') then
        v_para_i(9) = 1
    else
        ASSERT(.false.)
    endif
!
! - Geometric parameters
!
    if (algoge .eq. 'POINT_FIXE') then
        call getvtx(' ', 'REAC_GEOM', scal=reac)
        if (reac .eq. 'SANS') then
            v_para_i(1) = 0
            v_para_r(1) = resige
        else if (reac .eq. 'AUTOMATIQUE') then
            v_para_i(1) = -1
            call getvis(' ', 'ITER_GEOM_MAXI', scal=reacbg)
            v_para_i(6) = reacbg
            call getvr8(' ', 'RESI_GEOM', scal=resige)
            v_para_r(1) = resige
        else if (reac .eq. 'CONTROLE') then
            call getvis(' ', 'NB_ITER_GEOM', scal=nbreac)
            v_para_i(1) = nbreac
            v_para_r(1) = resige
        else
            ASSERT(.false.)
        endif
    else if (algoge .eq. 'NEWTON') then
        call getvr8(' ', 'RESI_GEOM', scal=resige)
        v_para_i(1) = 0
        v_para_r(1) = resige
    else
        ASSERT(.false.)
    endif
!
! - Friction algorithm
!
    if (l_frot) then
        if (l_cont_cont) then
            call getvtx(' ', 'ALGO_RESO_FROT', scal=algofr)
        else if (l_cont_xfem) then
            if (v_para_i(1) .eq. 0) then
                algofr = 'POINT_FIXE'
            else
                algofr = 'NEWTON'
            endif
        else if (l_cont_disc) then
            algofr = 'POINT_FIXE'
        else
            ASSERT(.false.)
        endif
    endif
!
    if (l_frot) then
        if (algofr .eq. 'POINT_FIXE') then
            v_para_i(28) = 0
        else if (algofr.eq.'NEWTON') then
            v_para_i(28) = 1
            l_newt_fr = .true.
        else
            ASSERT(.false.)
        endif
    endif
!
! - Friction parameters
!
    if (l_frot) then
        if (l_cont_cont) then
            if (algofr .eq. 'POINT_FIXE') then
                call getvis(' ', 'ITER_FROT_MAXI', scal=reacbs)
                v_para_i(7) = reacbs
                call getvr8(' ', 'RESI_FROT', scal=resifr)
                v_para_r(2) = resifr
            else
                call getvr8(' ', 'RESI_FROT', scal=resifr)
                v_para_r(2) = resifr
            endif
        else if (l_cont_xfem) then
            call getvis(' ', 'ITER_FROT_MAXI', scal=reacbs)
            v_para_i(7) = reacbs
            call getvr8(' ', 'RESI_FROT', scal=resifr)
            v_para_r(2) = resifr
        endif
    endif
!
! - Contact algorithm
!
    if (l_cont_cont) then
        call getvtx(' ', 'ALGO_RESO_CONT', scal=algoco)
    else if (l_cont_xfem) then
        algoco = 'POINT_FIXE'
    else if (l_cont_disc) then
        algoco = 'POINT_FIXE'
    else
        ASSERT(.false.)
    endif
!
    if (algoco .eq. 'POINT_FIXE') then
        v_para_i(27) = 0
    else if (algoco.eq.'NEWTON') then
        v_para_i(27) = 1
    else
        ASSERT(.false.)
    endif
!
! - Contact parameters
!
    if (algoco .eq. 'POINT_FIXE') then
        if (l_cont_xfem .or. l_cont_cont) then
            call getvis(' ', 'ITER_CONT_MULT', scal=reacca)
            call getvtx(' ', 'ITER_CONT_TYPE', scal=typcon)
            if (typcon .eq. 'MULT') then
                reacca = 4
                call getvis(' ', 'ITER_CONT_MULT', scal=reacca)
                v_para_i(5)  = reacca
                v_para_i(10) = -1
            else if (typcon.eq.'MAXI') then
                reacca = 30
                call getvis(' ', 'ITER_CONT_MAXI', scal=reacca)
                v_para_i(10) = reacca
                v_para_i(5)  = -1
            else
                ASSERT(.false.)
            endif
        else if (l_cont_disc) then
            call getvis(' ', 'ITER_CONT_MULT', scal=reacca)
            v_para_i(5)  = reacca
            v_para_i(10) = -1
        else
            ASSERT(.false.)
        endif
    else if (algoco.eq.'NEWTON') then
! ----- No parameters
    else
        ASSERT(.false.)
    endif
!
! - Discrete formulation
!
    if (l_cont_disc) then
        call getvtx(' ', 'STOP_SINGULIER', scal=isto)
        if (isto .eq. 'OUI') then
            v_para_i(2) = 0
        else if (isto .eq. 'NON') then
            v_para_i(2) = 1
        else
            ASSERT(.false.)
        endif
!
        call getvis(' ', 'NB_RESOL', scal=lgbloc)
        v_para_i(3) = lgbloc
! 
        if (l_cont_gcp) then
            call getvr8(' ', 'RESI_ABSO', scal=precis, nbret=noc)
            if (noc .eq. 0) then
                call utmess('F', 'CONTACT_4')
            endif
            v_para_r(4) = precis
!
            call getvis(' ', 'ITER_GCP_MAXI', scal=gcpmax)
            v_para_i(12) = gcpmax
!
            call getvtx(' ', 'PRE_COND', scal=prec)
            if (prec .eq. 'SANS') then
                v_para_i(13) = 0
            else if (prec.eq.'DIRICHLET') then
                v_para_i(13) = 1
                call getvr8(' ', 'COEF_RESI', scal=coefrs)
                v_para_r(5)  = coefrs
                call getvis(' ', 'ITER_PRE_MAXI', scal=premax)
                v_para_i(14) = premax
            else
                ASSERT(.false.)
            endif
!
            call getvtx(' ', 'RECH_LINEAIRE', scal=rech)
            if (rech .eq. 'ADMISSIBLE') then
                v_para_i(15) = 0
            else if (rech.eq.'NON_ADMISSIBLE') then
                v_para_i(15) = 1
            else
                ASSERT(.false.)
            endif
        endif
    endif
!
! - Smoothing
!
    if (l_cont_mesh) then
        call getvtx(' ', 'LISSAGE', scal=lissa)
        if (lissa(1:3) .eq. 'NON') then
            v_para_i(19) = 0
        else if (lissa(1:3) .eq. 'OUI') then
            v_para_i(19) = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! - Auto-adaptation 
!
    if (l_newt_fr .and. l_cont_cont) then
        call getvtx(' ', 'ADAPT_COEF', scal=coef_adap)
        if (coef_adap .eq. 'NON') then
            v_para_i(20) = 0
        else if (coef_adap .eq. 'OUI') then
            v_para_i(20) = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! - XFEM formulation
!
    if (l_cont_xfem) then
        call getvtx(' ', 'ELIM_ARETE', scal=elim_edge)
        if (elim_edge .eq. 'DUAL') then
            v_para_i(29) = 0
        else if (elim_edge .eq. 'ELIM') then
            v_para_i(29) = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! - Verification method
!
    if (l_cont_mesh) then
        call getvtx(' ', 'STOP_INTERP', scal=isto)
        if (isto .eq. 'OUI') then
            v_para_i(25) = 1
        else if (isto.eq.'NON') then
            v_para_i(25) = 0
        else
            ASSERT(.false.)
        endif
    endif
!
    call jedema()
end subroutine
