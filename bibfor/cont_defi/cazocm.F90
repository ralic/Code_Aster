subroutine cazocm(sdcont, keywf, i_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    character(len=8), intent(in) :: sdcont
    integer, intent(in) :: i_zone
    character(len=16), intent(in) :: keywf
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Meshed methods - Get parameters of contact zone
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  i_zone           : index of contact zone
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zmeth, zdirn, ztole
    integer :: noc, iret
    character(len=24) :: sdcont_defi
    character(len=16) :: type_pair, type_norm, type_appa_search, type_norm_mast, type_norm_slav
    character(len=16) :: type_jacobian, s_algo_cont
    character(len=16) :: dist_beam, dist_shell, cont_solv
    character(len=8) :: jeuf1, jeuf2
    real(kind=8) :: noor
    real(kind=8) :: norm_dire(3), dire_appa(3), tole_proj_ext, tole_appa, tole_interp
    aster_logical :: l_liss, l_calc
    character(len=24) :: sdcont_methco
    integer, pointer :: v_sdcont_methco(:) => null()
    character(len=24) :: sdcont_dirapp
    real(kind=8), pointer :: v_sdcont_dirapp(:) => null()
    character(len=24) :: sdcont_dirnor
    real(kind=8), pointer :: v_sdcont_dirnor(:) => null()
    character(len=24) :: sdcont_jeufo1
    character(len=8), pointer :: v_sdcont_jeufo1(:) => null()
    character(len=24) :: sdcont_jeufo2
    character(len=8), pointer :: v_sdcont_jeufo2(:) => null()
    character(len=24) :: sdcont_toleco
    real(kind=8), pointer :: v_sdcont_toleco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    tole_proj_ext    = 0.d0
    tole_appa        = 0.d0
    norm_dire(1:3)   = 0.d0
    dire_appa(1:3)   = 0.d0
    tole_interp      = 0.d0
    jeuf1            = ' '
    jeuf2            = ' '
    type_pair        = ' '
    type_norm        = ' '
    type_appa_search = ' '
    type_norm_mast   = ' '
    type_norm_slav   = ' '
    type_jacobian    = ' '
    s_algo_cont      = ' '
    dist_beam        = ' '
    dist_shell       = ' '
    cont_solv        = ' '
    l_calc           = .true.
    sdcont_defi      = sdcont(1:8)//'.CONTACT'
!
! - Parameters
!
    l_liss = cfdisl(sdcont_defi,'LISSAGE')
!
! - Datastructures for contact
!
    sdcont_methco = sdcont_defi(1:16)//'.METHCO'
    sdcont_toleco = sdcont_defi(1:16)//'.TOLECO'
    sdcont_dirapp = sdcont_defi(1:16)//'.DIRAPP'
    sdcont_dirnor = sdcont_defi(1:16)//'.DIRNOR'
    sdcont_jeufo1 = sdcont_defi(1:16)//'.JFO1CO'
    sdcont_jeufo2 = sdcont_defi(1:16)//'.JFO2CO'
    call jeveuo(sdcont_dirapp, 'E', vr = v_sdcont_dirapp)
    call jeveuo(sdcont_dirnor, 'E', vr = v_sdcont_dirnor)
    call jeveuo(sdcont_methco, 'E', vi = v_sdcont_methco)
    call jeveuo(sdcont_toleco, 'E', vr = v_sdcont_toleco)
    call jeveuo(sdcont_jeufo1, 'E', vk8 = v_sdcont_jeufo1)
    call jeveuo(sdcont_jeufo2, 'E', vk8 = v_sdcont_jeufo2)
    zmeth = cfmmvd('ZMETH')
    zdirn = cfmmvd('ZDIRN')
    ztole = cfmmvd('ZTOLE')
!
! - Type of pairing
!
    call getvtx(keywf, 'APPARIEMENT', iocc=i_zone, scal=type_pair)
    if (type_pair .eq. 'NODAL') then
        v_sdcont_methco(zmeth*(i_zone-1)+1) = 0
    else if (type_pair .eq. 'MAIT_ESCL') then
        v_sdcont_methco(zmeth*(i_zone-1)+1) = 1
    else if (type_pair .eq. 'MORTAR_LAC') then
         v_sdcont_methco(zmeth*(i_zone-1)+1) = 2
    else
        ASSERT(.false.)
    endif
!
! - Type of jacobian (MORTAR_LAC)
!
    call getvtx(keywf, 'ALGO_CONT', iocc=i_zone, scal=s_algo_cont, nbret = iret) 
    if (iret .eq. 0) then
        s_algo_cont = ' '
    endif
    if  (s_algo_cont .eq. 'LAC' ) then
        call getvtx(keywf, 'TYPE_JACOBIEN', iocc=i_zone, scal=type_jacobian)
        if (type_jacobian .eq. 'INITIAL') then
             v_sdcont_methco(zmeth*(i_zone-1)+23) = 0
        else if (type_jacobian .eq. 'ACTUALISE') then
             v_sdcont_methco(zmeth*(i_zone-1)+23) = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! - Get DIST_POUTRE/DIST_COQUE
!
    if (s_algo_cont .ne. 'LAC') then
        call getvtx(keywf, 'DIST_POUTRE', iocc=i_zone, scal=dist_beam)
        if (dist_beam .eq. 'OUI') then
            v_sdcont_methco(zmeth*(i_zone-1)+2) = 1
        endif
        call getvtx(keywf, 'DIST_COQUE', iocc=i_zone, scal=dist_shell)
        if (dist_shell .eq. 'OUI') then
            v_sdcont_methco(zmeth*(i_zone-1)+3) = 1
        endif
    endif
!
! - Type of normals
!
    if (s_algo_cont .ne. 'LAC') then
        call getvtx(keywf, 'NORMALE', iocc=i_zone, scal=type_norm)
        if (type_norm(1:4) .eq. 'MAIT') then
            if (type_norm(5:9) .eq. '_ESCL') then
                v_sdcont_methco(zmeth*(i_zone-1)+4) = 1
            else
                v_sdcont_methco(zmeth*(i_zone-1)+4) = 0
            endif
        else if (type_norm(1:4) .eq. 'ESCL') then
            v_sdcont_methco(zmeth*(i_zone-1)+4) = 2
        else
            ASSERT(.false.)
        endif
    endif
!
! - Type of normals
!
    if (s_algo_cont .ne. 'LAC') then
!
        call getvtx(keywf, 'VECT_MAIT', iocc=i_zone, scal=type_norm_mast)
!
        if (type_norm_mast .eq. 'AUTO') then
            v_sdcont_methco(zmeth*(i_zone-1)+5) = 0
        else if (type_norm_mast.eq.'FIXE') then
            if (type_norm .ne. 'MAIT') then
                call utmess('F', 'CONTACT3_50')
            endif
            if (l_liss) then
                call utmess('F', 'CONTACT3_54')
            endif
            v_sdcont_methco(zmeth*(i_zone-1)+5) = 1
            call getvr8(keywf, 'MAIT_FIXE', iocc=i_zone, nbval=3, vect=norm_dire,&
                        nbret=noc)
            ASSERT(noc.gt.0)
            call normev(norm_dire, noor)
            if (noor .le. r8prem()) then
                call utmess('F', 'CONTACT_15')
            endif
            v_sdcont_dirnor(zdirn*(i_zone-1)+1) = norm_dire(1)
            v_sdcont_dirnor(zdirn*(i_zone-1)+2) = norm_dire(2)
            v_sdcont_dirnor(zdirn*(i_zone-1)+3) = norm_dire(3)
        else if (type_norm_mast.eq.'VECT_Y') then
            if (type_norm .ne. 'MAIT') then
                call utmess('F', 'CONTACT3_51')
            endif
            if (l_liss) then
                call utmess('F', 'CONTACT3_54')
            endif
            v_sdcont_methco(zmeth*(i_zone-1)+5) = 2
            call getvr8(keywf, 'MAIT_VECT_Y', iocc=i_zone, nbval=3, vect=norm_dire,&
                        nbret=noc)
            ASSERT(noc.gt.0)
            call normev(norm_dire, noor)
            if (noor .le. r8prem()) then
                call utmess('F', 'CONTACT_16')
            endif
            v_sdcont_dirnor(zdirn*(i_zone-1)+1) = norm_dire(1)
            v_sdcont_dirnor(zdirn*(i_zone-1)+2) = norm_dire(2)
            v_sdcont_dirnor(zdirn*(i_zone-1)+3) = norm_dire(3)
        else
            ASSERT(.false.)
        endif
!
        call getvtx(keywf, 'VECT_ESCL', iocc=i_zone, scal=type_norm_slav)
!
        if (type_norm_slav .eq. 'AUTO') then
            v_sdcont_methco(zmeth*(i_zone-1)+6) = 0
        else if (type_norm_slav.eq.'FIXE') then
            if (type_norm .ne. 'ESCL') then
                call utmess('F', 'CONTACT3_52')
            endif
            if (l_liss) then
                call utmess('F', 'CONTACT3_54')
            endif
            v_sdcont_methco(zmeth*(i_zone-1)+6) = 1
            call getvr8(keywf, 'ESCL_FIXE', iocc=i_zone, nbval=3, vect=norm_dire,&
                        nbret=noc)
            ASSERT(noc.gt.0)
            call normev(norm_dire, noor)
            if (noor .le. r8prem()) then
                call utmess('F', 'CONTACT_15')
            endif
            v_sdcont_dirnor(zdirn*(i_zone-1)+4) = norm_dire(1)
            v_sdcont_dirnor(zdirn*(i_zone-1)+5) = norm_dire(2)
            v_sdcont_dirnor(zdirn*(i_zone-1)+6) = norm_dire(3)
        else if (type_norm_slav.eq.'VECT_Y') then
            if (type_norm .ne. 'ESCL') then
                call utmess('F', 'CONTACT3_53')
            endif
            if (l_liss) then
                call utmess('F', 'CONTACT3_54')
            endif
            v_sdcont_methco(zmeth*(i_zone-1)+6) = 2
            call getvr8(keywf, 'ESCL_VECT_Y', iocc=i_zone, nbval=3, vect=norm_dire,&
                        nbret=noc)
            ASSERT(noc.gt.0)
            call normev(norm_dire, noor)
            if (noor .le. r8prem()) then
                call utmess('F', 'CONTACT_16')
            endif
            v_sdcont_dirnor(zdirn*(i_zone-1)+4) = norm_dire(1)
            v_sdcont_dirnor(zdirn*(i_zone-1)+5) = norm_dire(2)
            v_sdcont_dirnor(zdirn*(i_zone-1)+6) = norm_dire(3)
        else
            ASSERT(.false.)
        endif
    endif
!
! - Pairing: search fixed direction - (DIRE_APPA)
!
    call getvtx(keywf, 'TYPE_APPA', iocc=i_zone, scal=type_appa_search)
    if (type_appa_search .eq. 'PROCHE') then
        v_sdcont_methco(zmeth*(i_zone-1)+7) = 0
    else if (type_appa_search(1:4) .eq. 'FIXE') then
        v_sdcont_methco(zmeth*(i_zone-1)+7) = 1
        call getvr8(keywf, 'DIRE_APPA', iocc=i_zone, nbval=3, vect=dire_appa,&
                    nbret=noc)
        ASSERT(noc.gt.0)
        call normev(dire_appa, noor)
        if (noor .le. r8prem()) then
            call utmess('F', 'CONTACT3_15')
        endif
        v_sdcont_dirapp(3*(i_zone-1)+1) = dire_appa(1)
        v_sdcont_dirapp(3*(i_zone-1)+2) = dire_appa(2)
        v_sdcont_dirapp(3*(i_zone-1)+3) = dire_appa(3)
    else if (type_appa_search  .eq.  'FORCEE') then
        v_sdcont_methco(zmeth*(i_zone-1)+7) = 2
    else if (type_appa_search  .eq.  'ROBUSTE') then
        v_sdcont_methco(zmeth*(i_zone-1)+7) = 3
    else if (type_appa_search  .eq.  'RAPIDE') then
        v_sdcont_methco(zmeth*(i_zone-1)+7) = 4
    else
        ASSERT(.false.)
    endif
!
! - Resolution of contact (VERIF mode) ?
!
    if (s_algo_cont .ne. 'LAC') then
        call getvtx(keywf, 'RESOLUTION', iocc=i_zone, scal=cont_solv)
        if (cont_solv .eq. 'OUI') then
            v_sdcont_methco(zmeth*(i_zone-1)+22) = 0
            l_calc = .true.
        else
            v_sdcont_methco(zmeth*(i_zone-1)+22) = 1
            l_calc = .false.
        endif
    endif
!
! - DIST_MAIT/DIST_ESCL
!
    if (s_algo_cont .ne. 'LAC') then
        call getvid(keywf, 'DIST_MAIT', iocc=i_zone, scal=jeuf1, nbret=noc)
        if (noc .ne. 0) then
            v_sdcont_jeufo1(i_zone) = jeuf1
        endif
        call getvid(keywf, 'DIST_ESCL', iocc=i_zone, scal=jeuf2, nbret=noc)
        if (noc .ne. 0) then
            v_sdcont_jeufo2(i_zone) = jeuf2
        endif
    endif
!
! - TOLE_PROJ_EXT
! --- TOLE_PROJ_EXT <0: disallow projection outside element
! --- TOLE_PROJ_EXT >0: allow projection outside element with TOLE_PROJ_EXT tolerance
!
    if (s_algo_cont .ne. 'LAC') then
        call getvr8(keywf, 'TOLE_PROJ_EXT', iocc=i_zone, scal=tole_proj_ext)
        if (tole_proj_ext .lt. 0.d0) then
            v_sdcont_toleco(ztole*(i_zone-1)+1) = -1.d0
        else
            v_sdcont_toleco(ztole*(i_zone-1)+1) = tole_proj_ext
        endif
    endif
!
! - TOLE_APPA
! --- TOLE_APPA <0: pairing for any distance
! --- TOLE_APPA >0: pairing only if distance(slave,master) < TOLE_APPA
!
    call getvr8(keywf, 'TOLE_APPA', iocc=i_zone, scal=tole_appa)
    if (tole_appa .lt. 0.d0) then
        v_sdcont_toleco(ztole*(i_zone-1)+2) = -1.d0
    else
        v_sdcont_toleco(ztole*(i_zone-1)+2) = tole_appa
    endif
!
! - Tolerance for non-computation mode
!
    if (.not.l_calc) then
        call getvr8(keywf, 'TOLE_INTERP', iocc=i_zone, scal=tole_interp)
        v_sdcont_toleco(ztole*(i_zone-1)+3) = tole_interp
    endif
!
end subroutine
