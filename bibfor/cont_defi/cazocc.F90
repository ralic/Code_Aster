subroutine cazocc(sdcont, keywf, i_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfl.h"
#include "asterfort/utmess.h"
#include "asterc/r8prem.h"
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
    integer, intent(in) :: i_zone
    character(len=16), intent(in) :: keywf
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Continue method - Get parameters of contact zone
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  i_zone           : index of contact zone
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_defi
    integer :: nb_frot_excl_1, nb_frot_excl_2
    integer :: nb_cont_excl_1, nb_cont_excl_2, nb_cont_excl_3, nb_cont_excl_4
    integer :: nb_dire_excl, noc
    character(len=16) :: s_cont_excl, s_frot_excl
    character(len=16) :: s_gliss, s_type_inte, s_cont_init, s_algo_cont, s_algo_frot
    real(kind=8) :: dire_excl_frot_i, dire_excl_frot(3)
    real(kind=8) :: coef_cont, coef_frot, seuil_init, coef_coul_frot
    real(kind=8) :: coef_augm_frot, coef_augm_cont
    real(kind=8) :: coef_pena_frot, coef_pena_cont
    real(kind=8) :: algo_cont, algo_frot
    real(kind=8) :: type_inte, cont_init, seuil_auto
    integer :: inte_order
    aster_logical :: l_inte_node, l_frot, l_node_excl, l_frot_excl, l_dire_excl_frot
    aster_logical :: l_gliss, l_newt_geom, l_newt_cont
    integer :: zcmcf, zexcl
    character(len=24) :: sdcont_caracf
    real(kind=8), pointer :: v_sdcont_caracf(:) => null()
    character(len=24) :: sdcont_exclfr
    real(kind=8), pointer :: v_sdcont_exclfr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    inte_order        = 0
    type_inte         = 0.d0
    algo_cont         = 0.d0
    coef_augm_cont    = 100.d0
    coef_pena_cont    = 100.d0
    coef_cont         = 100.d0
    algo_frot         = 0.d0
    coef_coul_frot    = 0.d0
    coef_augm_frot    = 100.d0
    coef_pena_frot    = 100.d0
    coef_frot         = 100.d0
    coef_coul_frot    = 0.d0
    seuil_init        = 0.d0
    seuil_auto        = 0.d0
    cont_init         = 0.d0
    dire_excl_frot_i  = 0.d0
    dire_excl_frot(1) = 0.d0
    dire_excl_frot(2) = 0.d0
    dire_excl_frot(3) = 0.d0
    l_inte_node       = .false.
    l_node_excl       = .false.
    l_frot_excl       = .false.
    l_gliss           = .false.
    l_dire_excl_frot  = .false.
    s_algo_frot       = ' '
!
! - Datastructure for contact
!
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
    sdcont_caracf = sdcont_defi(1:16)//'.CARACF'
    sdcont_exclfr = sdcont_defi(1:16)//'.EXCLFR'
    call jeveuo(sdcont_caracf, 'E', vr = v_sdcont_caracf)
    call jeveuo(sdcont_exclfr, 'E', vr = v_sdcont_exclfr)
    zcmcf = cfmmvd('ZCMCF')
    zexcl = cfmmvd('ZEXCL')
!
! - Parameters
!
    l_frot      = cfdisl(sdcont_defi,'FROTTEMENT')
    l_newt_geom = cfdisl(sdcont_defi,'GEOM_NEWTON')
    l_newt_cont = cfdisl(sdcont_defi,'CONT_NEWTON')
!
! - Integration scheme
!
    call getvtx(keywf, 'INTEGRATION', iocc=i_zone, scal=s_type_inte)
    if (s_type_inte .eq. 'AUTO') then
        l_inte_node = .true.
        type_inte   = 1.d0
    else if (s_type_inte .eq. 'GAUSS') then
        call getvis(keywf, 'ORDRE_INT', iocc=i_zone, scal=inte_order)
        type_inte = 10.d0*inte_order + 2.d0
    else if (s_type_inte .eq. 'SIMPSON') then
        call getvis(keywf, 'ORDRE_INT', iocc=i_zone, scal=inte_order)
        type_inte = 10.d0*inte_order + 3.d0
    else if (s_type_inte .eq. 'NCOTES') then
        call getvis(keywf, 'ORDRE_INT', iocc=i_zone, scal=inte_order)
        type_inte = 10.d0*inte_order + 4.d0
    else
        ASSERT(.false.)
    endif
    v_sdcont_caracf(zcmcf*(i_zone-1)+1) = type_inte
!
! - Contact method
!
    call getvtx(keywf, 'ALGO_CONT', iocc=i_zone, scal=s_algo_cont)
    if (s_algo_cont .eq. 'STANDARD') then
        call getvr8(keywf, 'COEF_CONT', iocc=i_zone, scal=coef_augm_cont)
        algo_cont = 1.d0
        coef_cont = coef_augm_cont
    else if (s_algo_cont .eq. 'PENALISATION') then
        call getvr8(keywf, 'COEF_PENA_CONT', iocc=i_zone, scal=coef_pena_cont)
        algo_cont = 3.d0
        coef_cont = coef_pena_cont
    else if (s_algo_cont .eq. 'LAC') then
        algo_cont = 5.d0
        coef_cont = coef_augm_cont
    else
        ASSERT(.false.)
    endif
    v_sdcont_caracf(zcmcf*(i_zone-1)+2) = coef_cont
    v_sdcont_caracf(zcmcf*(i_zone-1)+3) = algo_cont
!
! - Friction method
!
    if (l_frot) then
        call getvtx(keywf, 'ALGO_FROT', iocc=i_zone, scal=s_algo_frot)
        if (s_algo_frot .eq. 'STANDARD') then
            call getvr8(keywf, 'COEF_FROT', iocc=i_zone, scal=coef_augm_frot)
            algo_frot = 1.d0
            coef_frot = coef_augm_frot
        else if (s_algo_frot .eq. 'PENALISATION') then
            call getvr8(keywf, 'COEF_PENA_FROT', iocc=i_zone, scal=coef_pena_frot)
            algo_frot = 3.d0
            coef_frot = coef_pena_frot
        else
            ASSERT(.false.)
        endif
        if (s_algo_cont .ne. s_algo_frot) then
            call utmess('F', 'CONTACT_89')
        endif
    else
        coef_frot = 0.d0
        algo_frot = 0.d0
    endif
!
! - Get friction parameters
!
    if (l_frot) then
        call getvr8(keywf, 'COULOMB', iocc=i_zone, scal=coef_coul_frot)
        call getvr8(keywf, 'SEUIL_INIT', iocc=i_zone, scal=seuil_init, nbret=noc)
        if (noc .eq. 0) then
            seuil_auto = 1.d0
        endif
        if (coef_coul_frot .le. r8prem()) then
            coef_frot  = 0.d0
            algo_frot  = 0.d0
        endif
    endif
    v_sdcont_caracf(zcmcf*(i_zone-1)+4)  = coef_frot
    v_sdcont_caracf(zcmcf*(i_zone-1)+5)  = algo_frot
    v_sdcont_caracf(zcmcf*(i_zone-1)+6)  = coef_coul_frot
    v_sdcont_caracf(zcmcf*(i_zone-1)+7)  = seuil_init
    v_sdcont_caracf(zcmcf*(i_zone-1)+13) = seuil_auto
!
! - Check
!
    if (l_frot .and. (s_algo_cont.ne.s_algo_frot)) then
        call utmess('F', 'CONTACT_89')
    endif
    if ((s_algo_cont.eq.'PENALISATION') .and. l_newt_geom) then
        call utmess('A', 'CONTACT_21')
    endif
    if (l_newt_geom .and. (.not.l_newt_cont)) then
        call utmess('F', 'CONTACT_20')
    endif
!
! - Contact nodes excluded
!
    if (s_algo_cont .ne. 'LAC') then
        call getvtx(keywf, 'SANS_GROUP_NO', iocc=i_zone, scal=s_cont_excl, nbret=nb_cont_excl_1)
        call getvtx(keywf, 'SANS_NOEUD'   , iocc=i_zone, scal=s_cont_excl, nbret=nb_cont_excl_2)
        l_node_excl = (nb_cont_excl_1.ne.0) .or. (nb_cont_excl_2.ne.0)
        call getvtx(keywf, 'SANS_GROUP_MA', iocc=i_zone, scal=s_cont_excl, nbret=nb_cont_excl_3)
        call getvtx(keywf, 'SANS_MAILLE'  , iocc=i_zone, scal=s_cont_excl, nbret=nb_cont_excl_4)
        l_node_excl = l_node_excl.or.((nb_cont_excl_3.ne.0).or.(nb_cont_excl_4.ne.0))
    endif
!
! - Friction nodes excluded
!
    if (s_algo_cont .ne. 'LAC') then
        call getvtx(keywf, 'SANS_GROUP_NO_FR', iocc=i_zone, scal=s_frot_excl, nbret=nb_frot_excl_1)
        call getvtx(keywf, 'SANS_NOEUD_FR'   , iocc=i_zone, scal=s_frot_excl, nbret=nb_frot_excl_2)
        l_frot_excl = (nb_frot_excl_1.ne.0) .or. (nb_frot_excl_2.ne.0)
    endif
!
! - For friction direction to exclude (vector)
!
    if (l_frot_excl) then
        call getvr8(keywf, 'DIRE_EXCL_FROT', iocc=i_zone, nbval=3, vect=dire_excl_frot,&
                    nbret=nb_dire_excl)
        l_dire_excl_frot = (nb_dire_excl .ne. 0)
        if (.not.l_dire_excl_frot) then
! --------- All directions excluded
            dire_excl_frot_i  = 2.d0
            dire_excl_frot(1) = 0.d0
            dire_excl_frot(2) = 0.d0
            dire_excl_frot(3) = 0.d0
        else
! --------- Only one direction excluded
            dire_excl_frot_i = 1.d0
        endif
    else
        dire_excl_frot_i  = 0.d0
        dire_excl_frot(1) = 0.d0
        dire_excl_frot(2) = 0.d0
        dire_excl_frot(3) = 0.d0
    endif
!
! - Excluded: only node integration scheme
!
    if (.not.l_inte_node) then
        if (l_node_excl .or. l_frot_excl) then
            call utmess('F', 'CONTACT_97')
        endif
        if (.not.mminfl(sdcont_defi, 'MAIT', i_zone)) then
            call utmess('F', 'CONTACT_98')
        endif
    endif
!
! - Initial contact
!
    call getvtx(keywf, 'CONTACT_INIT', iocc=i_zone, scal=s_cont_init)
    if (s_cont_init .eq. 'OUI') then
        cont_init = 1.d0
    else if (s_cont_init .eq. 'INTERPENETRE') then
        cont_init = 2.d0
    else if (s_cont_init .eq. 'NON') then
        cont_init = 0.d0
    else
        ASSERT(.false.)
    endif
!
! - Bilateral contact
!
    if (s_algo_cont .ne. 'LAC') then
        call getvtx(keywf, 'GLISSIERE', iocc=i_zone, scal=s_gliss)
        if (s_gliss .eq. 'OUI') then
            l_gliss = .true.
        else if (s_gliss .eq. 'NON') then
            l_gliss = .false.
        else
            ASSERT(.false.)
        endif
    endif
!
    v_sdcont_caracf(zcmcf*(i_zone-1)+8) = cont_init
    if (l_gliss) then
       v_sdcont_caracf(zcmcf*(i_zone-1)+9) = 1.d0
    else
       v_sdcont_caracf(zcmcf*(i_zone-1)+9) = 0.d0
    endif
    if (l_node_excl) then
        v_sdcont_caracf(zcmcf*(i_zone-1)+10) = 1.d0
    else
        v_sdcont_caracf(zcmcf*(i_zone-1)+10) = 0.d0
    endif
    if (l_frot_excl) then
        v_sdcont_caracf(zcmcf*(i_zone-1)+11) = 1.d0
    else
        v_sdcont_caracf(zcmcf*(i_zone-1)+11) = 0.d0
    endif
    v_sdcont_caracf(zcmcf*(i_zone-1)+12) = dire_excl_frot_i
    v_sdcont_exclfr(zexcl*(i_zone-1)+1)  = dire_excl_frot(1)
    v_sdcont_exclfr(zexcl*(i_zone-1)+2)  = dire_excl_frot(2)
    v_sdcont_exclfr(zexcl*(i_zone-1)+3)  = dire_excl_frot(3)
!
end subroutine
