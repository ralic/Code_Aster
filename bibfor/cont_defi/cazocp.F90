subroutine cazocp(sdcont, model)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeveuo.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: model
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Get parameters (not depending on contact zones)
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont         : name of contact datastructure
! In  model          : name of model
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_defi
    integer :: geom_nbiter, nb_resol, gcp_maxi, gcp_pre_maxi
    integer :: cont_mult, frot_maxi, geom_maxi
    character(len=16) :: gcp_rech_line, gcp_precond, reac_geom, cont_type, stop_singular, elim_edge
    character(len=16) :: algo_reso_cont, algo_reso_frot, algo_reso_geom
    integer :: noc
    real(kind=8) :: resi_abso, gcp_coef_resi
    real(kind=8) :: geom_resi, frot_resi
    aster_logical :: l_cont_gcp, l_newt_fr
    aster_logical :: l_cont_disc, l_cont_cont, l_cont_xfem, l_frot, l_cont_lac
    aster_logical :: l_xfem_mortar
    character(len=16) :: lissage, coef_adap
    character(len=24) :: sdcont_paracr
    real(kind=8), pointer :: v_sdcont_paracr(:) => null()
    character(len=24) :: sdcont_paraci
    integer, pointer :: v_sdcont_paraci(:) => null()
    integer, pointer :: v_xfem_cont(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi    = sdcont(1:8)//'.CONTACT'
    elim_edge      = 'DUAL'
    reac_geom      = 'AUTOMATIQUE'
    algo_reso_cont = ' '
    algo_reso_frot = ' '
    algo_reso_geom = ' '
    geom_nbiter    = 2
    nb_resol       = 10
    geom_resi      = 1.d-2
    frot_resi      = 1.d-2
    l_newt_fr      = .false.
!
! - Access to datastructure
!
    sdcont_paracr = sdcont_defi(1:16)//'.PARACR'
    sdcont_paraci = sdcont_defi(1:16)//'.PARACI'
    call jeveuo(sdcont_paracr, 'E', vr = v_sdcont_paracr)
    call jeveuo(sdcont_paraci, 'E', vi = v_sdcont_paraci)
!
! - Active functionnalites
!
    l_cont_disc = cfdisl(sdcont_defi,'FORMUL_DISCRETE')
    l_cont_cont = cfdisl(sdcont_defi,'FORMUL_CONTINUE')
    l_cont_xfem = cfdisl(sdcont_defi,'FORMUL_XFEM')
    l_cont_lac  = cfdisl(sdcont_defi,'FORMUL_LAC')
    l_cont_gcp  = cfdisl(sdcont_defi,'CONT_GCP' )
    l_frot      = cfdisl(sdcont_defi,'FROTTEMENT')
!
! - Geometric algorithm
!
    if (l_cont_cont) then
        call getvtx(' ', 'ALGO_RESO_GEOM', scal=algo_reso_geom)
    else if (l_cont_xfem) then
        algo_reso_geom = 'POINT_FIXE'
    else if (l_cont_disc) then
        algo_reso_geom = 'POINT_FIXE'
    else if (l_cont_lac) then
        call getvtx(' ', 'ALGO_RESO_GEOM', scal=algo_reso_geom)
    else
        ASSERT(.false.)
    endif
!
    if (algo_reso_geom .eq. 'POINT_FIXE') then
        v_sdcont_paraci(9) = 0
    else if (algo_reso_geom.eq.'NEWTON') then
        v_sdcont_paraci(9) = 1
    else
        ASSERT(.false.)
    endif
!
! - Geometric parameters
!
    if (algo_reso_geom .eq. 'POINT_FIXE') then
        call getvtx(' ', 'REAC_GEOM', scal=reac_geom)
        if (reac_geom .eq. 'SANS') then
            v_sdcont_paraci(1) = 0
            v_sdcont_paracr(1) = geom_resi
        else if (reac_geom .eq. 'AUTOMATIQUE') then
            v_sdcont_paraci(1) = -1
            call getvis(' ', 'ITER_GEOM_MAXI', scal=geom_maxi)
            v_sdcont_paraci(6) = geom_maxi
            call getvr8(' ', 'RESI_GEOM', scal=geom_resi)
            v_sdcont_paracr(1) = geom_resi
        else if (reac_geom .eq. 'CONTROLE') then
            call getvis(' ', 'NB_ITER_GEOM', scal=geom_nbiter)
            v_sdcont_paraci(1) = geom_nbiter
            v_sdcont_paracr(1) = geom_resi
        else
            ASSERT(.false.)
        endif
        if (l_cont_lac) then
            call utmess('F', 'CONTACT4_1')
        endif
    else if (algo_reso_geom .eq. 'NEWTON') then
        call getvr8(' ', 'RESI_GEOM', scal=geom_resi)
        v_sdcont_paraci(1) = 0
        v_sdcont_paracr(1) = geom_resi
    else
        ASSERT(.false.)
    endif
!
! - Friction algorithm
!
    if (l_frot) then
        if (l_cont_cont) then
            call getvtx(' ', 'ALGO_RESO_FROT', scal=algo_reso_frot)
        else if (l_cont_xfem) then
            if (v_sdcont_paraci(1) .eq. 0) then
                algo_reso_frot = 'POINT_FIXE'
            else
                algo_reso_frot = 'NEWTON'
            endif
        else if (l_cont_disc) then
            algo_reso_frot = 'POINT_FIXE'
        else if (l_cont_lac) then
             call utmess('F', 'CONTACT4_4')
        else
            ASSERT(.false.)
        endif
    endif
!
    if (l_frot) then
        if (algo_reso_frot .eq. 'POINT_FIXE') then
            v_sdcont_paraci(28) = 0
        else if (algo_reso_frot.eq.'NEWTON') then
            v_sdcont_paraci(28) = 1
            l_newt_fr = .true.
        else
            ASSERT(.false.)
        endif
        ASSERT(.not.l_cont_lac)
    endif
!
! - Friction parameters
!
    if (l_frot) then
        if (l_cont_cont) then
            if (algo_reso_frot .eq. 'POINT_FIXE') then
                call getvis(' ', 'ITER_FROT_MAXI', scal=frot_maxi)
                v_sdcont_paraci(7) = frot_maxi
                call getvr8(' ', 'RESI_FROT', scal=frot_resi)
                v_sdcont_paracr(2) = frot_resi
            else
                call getvr8(' ', 'RESI_FROT', scal=frot_resi)
                v_sdcont_paracr(2) = frot_resi
            endif
        else if (l_cont_xfem) then
            call getvis(' ', 'ITER_FROT_MAXI', scal=frot_maxi)
            v_sdcont_paraci(7) = frot_maxi
            call getvr8(' ', 'RESI_FROT', scal=frot_resi)
            v_sdcont_paracr(2) = frot_resi
        endif
        ASSERT(.not.l_cont_lac)
    endif
!
! - Contact algorithm
!
    if (l_cont_cont) then
        call getvtx(' ', 'ALGO_RESO_CONT', scal=algo_reso_cont)
    else if (l_cont_xfem) then
        algo_reso_cont = 'POINT_FIXE'
    else if (l_cont_disc) then
        algo_reso_cont = 'POINT_FIXE'
    else if (l_cont_lac) then
        call getvtx(' ', 'ALGO_RESO_CONT', scal=algo_reso_cont)
    else
        ASSERT(.false.)
    endif
!
    if (algo_reso_cont .eq. 'POINT_FIXE') then
        v_sdcont_paraci(27) = 0
    else if (algo_reso_cont.eq.'NEWTON') then
        v_sdcont_paraci(27) = 1
    else
        ASSERT(.false.)
    endif
!
! - Contact parameters
!
    if (algo_reso_cont .eq. 'POINT_FIXE') then
        if (l_cont_xfem .or. l_cont_cont) then
            call getvtx(' ', 'ITER_CONT_TYPE', scal=cont_type)
            if (cont_type .eq. 'MULT') then
                cont_mult = 4
                call getvis(' ', 'ITER_CONT_MULT', scal=cont_mult)
                v_sdcont_paraci(5)  = cont_mult
                v_sdcont_paraci(10) = -1
            else if (cont_type.eq.'MAXI') then
                cont_mult = 30
                call getvis(' ', 'ITER_CONT_MAXI', scal=cont_mult)
                v_sdcont_paraci(10) = cont_mult
                v_sdcont_paraci(5)  = -1
            else
                ASSERT(.false.)
            endif
        else if (l_cont_disc) then
            call getvis(' ', 'ITER_CONT_MULT', scal=cont_mult)
            v_sdcont_paraci(5)  = cont_mult
            v_sdcont_paraci(10) = -1
        else if (l_cont_lac) then
            call utmess('F', 'CONTACT4_1')
        else
            ASSERT(.false.)
        endif
    else if (algo_reso_cont.eq.'NEWTON') then
! ----- No parameters
    else
        ASSERT(.false.)
    endif
!
! - Discrete formulation
!
    if (l_cont_disc) then
        call getvtx(' ', 'STOP_SINGULIER', scal=stop_singular)
        if (stop_singular .eq. 'OUI') then
            v_sdcont_paraci(2) = 0
        else if (stop_singular .eq. 'NON') then
            v_sdcont_paraci(2) = 1
        else
            ASSERT(.false.)
        endif
!
        call getvis(' ', 'NB_RESOL', scal=nb_resol)
        v_sdcont_paraci(3) = nb_resol
!
        if (l_cont_gcp) then
            call getvr8(' ', 'RESI_ABSO', scal=resi_abso, nbret=noc)
            if (noc .eq. 0) then
                call utmess('F', 'CONTACT_4')
            endif
            v_sdcont_paracr(4) = resi_abso
!
            call getvis(' ', 'ITER_GCP_MAXI', scal=gcp_maxi)
            v_sdcont_paraci(12) = gcp_maxi
!
            call getvtx(' ', 'PRE_COND', scal=gcp_precond)
            if (gcp_precond .eq. 'SANS') then
                v_sdcont_paraci(13) = 0
            else if (gcp_precond.eq.'DIRICHLET') then
                v_sdcont_paraci(13) = 1
                call getvr8(' ', 'COEF_RESI', scal=gcp_coef_resi)
                v_sdcont_paracr(5)  = gcp_coef_resi
                call getvis(' ', 'ITER_PRE_MAXI', scal=gcp_pre_maxi)
                v_sdcont_paraci(14) = gcp_pre_maxi
            else
                ASSERT(.false.)
            endif
!
            call getvtx(' ', 'RECH_LINEAIRE', scal=gcp_rech_line)
            if (gcp_rech_line .eq. 'ADMISSIBLE') then
                v_sdcont_paraci(15) = 0
            else if (gcp_rech_line.eq.'NON_ADMISSIBLE') then
                v_sdcont_paraci(15) = 1
            else
                ASSERT(.false.)
            endif
        endif
    endif
!
! - Smoothing
!
    if (l_cont_disc .or. l_cont_cont .or. l_cont_lac) then
        call getvtx(' ', 'LISSAGE', scal=lissage)
        if (lissage(1:3) .eq. 'NON') then
            v_sdcont_paraci(19) = 0
        else if (lissage(1:3) .eq. 'OUI') then
            v_sdcont_paraci(19) = 1
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
            v_sdcont_paraci(20) = 0
        else if (coef_adap .eq. 'OUI') then
            v_sdcont_paraci(20) = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! - XFEM formulation
!
    if (l_cont_xfem) then
        call jeveuo(model//'.XFEM_CONT', 'L', vi = v_xfem_cont)
        l_xfem_mortar = v_xfem_cont(1).eq.2
        call getvtx(' ', 'ELIM_ARETE', scal=elim_edge)
        if (elim_edge .eq. 'DUAL') then
            v_sdcont_paraci(29) = 0
        else if (elim_edge .eq. 'ELIM') then
            if (l_xfem_mortar) then
                call utmess('F', 'XFEM_62')
            endif
            v_sdcont_paraci(29) = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! - Verification method
!
    if (l_cont_disc.or.l_cont_cont) then
        call getvtx(' ', 'STOP_INTERP', scal=stop_singular)
        if (stop_singular .eq. 'OUI') then
            v_sdcont_paraci(25) = 1
        else if (stop_singular.eq.'NON') then
            v_sdcont_paraci(25) = 0
        else
            ASSERT(.false.)
        endif
    endif
!
end subroutine
