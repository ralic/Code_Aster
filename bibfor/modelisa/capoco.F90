subroutine capoco(sdcont, keywf)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: sdcont
    character(len=16), intent(in) :: keywf
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Get supplementary gap: beams
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, noc, i_beam_sect
    integer :: elem_slav_indx, elem_slav_nume
    integer :: jdecme
    integer :: i_zone, i_slav_elem
    integer :: nb_slav_elem, nb_cont_zone, nb_cont_elem, nb_para_maxi
    integer :: beam_tsec_indx, beam_r1_indx, beam_r2_indx, iad1, iad2
    real(kind=8) :: beam_radius_1, beam_radius_2, beam_radius
    aster_logical :: l_dist_exist
    character(len=8) :: cara_elem
    character(len=19) :: cara_elem_s
    aster_logical :: l_dist_beam
    real(kind=8), pointer :: v_caraelem_cesv(:) => null()
    character(len=8), pointer :: v_caraelem_cesc(:) => null()
    integer :: j_caraelem_cesd, j_caraelem_cesl
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
    character(len=24) :: sdcont_jeupou
    real(kind=8), pointer :: v_sdcont_jeupou(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    cara_elem_s = '&&CAPOCO.CARGEOPO'
!
! - Datastructure for contact definition
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
    sdcont_mailco = sdcont_defi(1:16)//'.MAILCO'
    sdcont_jeupou = sdcont_defi(1:16)//'.JEUPOU'
    call jeveuo(sdcont_mailco, 'L', vi = v_sdcont_mailco)
!
! - Parameters
!
    nb_cont_zone = cfdisi(sdcont_defi, 'NZOCO')
    nb_cont_elem = cfdisi(sdcont_defi, 'NMACO')
!
! - Create beam gap datastructure
!
    call wkvect(sdcont_jeupou, 'G V R', nb_cont_elem, vr = v_sdcont_jeupou)
!
! - Get elementary characteristics datastructure
!
    l_dist_exist = .false.
    do i_zone = 1, nb_cont_zone
        l_dist_beam = mminfl(sdcont_defi, 'DIST_POUTRE', i_zone)
        if (l_dist_beam) then
            l_dist_exist = .true.
            call getvid(keywf, 'CARA_ELEM', iocc=i_zone, scal=cara_elem, nbret=noc)
            ASSERT(noc.ne.0)
        endif
    end do
!
    if (.not. l_dist_exist) then
        goto 999
    endif
!
! - Access to elementary characteristics
!
    call carces(cara_elem//'.CARGEOPO', 'ELEM', ' ', 'V', cara_elem_s,&
                'A', iret)
    call jeveuo(cara_elem_s//'.CESC', 'L', vk8=v_caraelem_cesc)
    call jeveuo(cara_elem_s//'.CESD', 'L', j_caraelem_cesd)
    call jeveuo(cara_elem_s//'.CESL', 'L', j_caraelem_cesl)
    call jeveuo(cara_elem_s//'.CESV', 'L', vr=v_caraelem_cesv)
!
! - Get index for storing beam parameters
!
    nb_para_maxi   = zi(j_caraelem_cesd-1+2)
    beam_tsec_indx = indik8(v_caraelem_cesc,'TSEC    ',1,nb_para_maxi)
    beam_r1_indx   = indik8(v_caraelem_cesc,'R1      ',1,nb_para_maxi)
    beam_r2_indx   = indik8(v_caraelem_cesc,'R2      ',1,nb_para_maxi)
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
        l_dist_beam = mminfl(sdcont_defi, 'DIST_POUTRE', i_zone)
        if (l_dist_beam) then
            nb_slav_elem = mminfi(sdcont_defi, 'NBMAE' , i_zone)
            jdecme       = mminfi(sdcont_defi, 'JDECME', i_zone)
            do i_slav_elem = 1, nb_slav_elem
!
! ------------- Current element
!
                elem_slav_indx = jdecme+i_slav_elem
                elem_slav_nume = v_sdcont_mailco(elem_slav_indx)
!
! ------------- Beam section shape
!
                call cesexi('C', j_caraelem_cesd, j_caraelem_cesl, elem_slav_nume, 1,&
                            1, beam_tsec_indx, iad1)
                if (iad1 .gt. 0) then
                    i_beam_sect = nint(v_caraelem_cesv(abs(iad1)))
                else
                    i_beam_sect = 0
                endif
!
! ------------- Beam section shape is only circular !
!
                if (i_beam_sect .ne. 2) then
                    call utmess('F', 'CONTACT3_32')
                endif
!
! ------------- Get radius
!
                call cesexi('C', j_caraelem_cesd, j_caraelem_cesl, elem_slav_nume, 1,&
                            1, beam_r1_indx, iad1)
                ASSERT(iad1.gt.0)
                beam_radius_1 = v_caraelem_cesv(iad1)
                call cesexi('C', j_caraelem_cesd, j_caraelem_cesl, elem_slav_nume, 1,&
                            1, beam_r2_indx, iad2)
                ASSERT(iad2.gt.0)
                beam_radius_2 = v_caraelem_cesv(iad2)
!
! ------------- Different radius: mean value
!
                if (abs(beam_radius_1 - beam_radius_2).gt.r8prem()) then
                    call utmess('I', 'CONTACT3_37')
                endif
                beam_radius = (beam_radius_1+beam_radius_2)/2.d0
!
! ------------- Save value
!
                v_sdcont_jeupou(elem_slav_indx) = beam_radius
            end do
        endif
    end do
!
999 continue
!
    call detrsd('CHAM_ELEM_S', cara_elem_s)
!
end subroutine
