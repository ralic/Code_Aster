subroutine cacoco(sdcont, keywf, mesh)
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
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
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
    character(len=8), intent(in) :: mesh
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Get supplementary gap: shells
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  mesh             : name of mesh
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, noc
    integer :: nb_para_maxi, nb_cont_elem, nb_cont_zone, nb_slav_elem
    integer :: shell_ep_indx, shell_exc_indx, iad1
    integer :: elem_slav_indx, elem_slav_nume
    integer :: jdecme
    integer :: i_zone, i_slav_elem
    real(kind=8) :: shell_ep, shell_excent
    aster_logical :: l_dist_exist
    character(len=8) :: cara_elem, elem_slav_name
    character(len=19) :: cara_elem_s
    aster_logical :: l_dist_shell
    real(kind=8), pointer :: v_caraelem_cesv(:) => null()
    character(len=8), pointer :: v_caraelem_cesc(:) => null()
    integer :: j_caraelem_cesd, j_caraelem_cesl
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
    character(len=24) :: sdcont_jeucoq
    real(kind=8), pointer :: v_sdcont_jeucoq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    cara_elem_s = '&&CAPOCO.CARGEOPO'
!
! - Datastructure for contact definition
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
    sdcont_mailco = sdcont_defi(1:16)//'.MAILCO'
    sdcont_jeucoq = sdcont_defi(1:16)//'.JEUCOQ'
    call jeveuo(sdcont_mailco, 'L', vi = v_sdcont_mailco)
!
! - Parameters
!
    nb_cont_zone = cfdisi(sdcont_defi, 'NZOCO')
    nb_cont_elem = cfdisi(sdcont_defi, 'NMACO')
!
! - Create shell gap datastructure
!
    call wkvect(sdcont_jeucoq, 'G V R', nb_cont_elem, vr = v_sdcont_jeucoq)
!
! - Get elementary characteristics datastructure
!
    l_dist_exist = .false.
    do i_zone = 1, nb_cont_zone
        l_dist_shell = mminfl(sdcont_defi, 'DIST_COQUE', i_zone)
        if (l_dist_shell) then
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
    call carces(cara_elem//'.CARCOQUE', 'ELEM', ' ', 'V', cara_elem_s,&
                'A', iret)
    call jeveuo(cara_elem_s//'.CESC', 'L', vk8=v_caraelem_cesc)
    call jeveuo(cara_elem_s//'.CESD', 'L', j_caraelem_cesd)
    call jeveuo(cara_elem_s//'.CESL', 'L', j_caraelem_cesl)
    call jeveuo(cara_elem_s//'.CESV', 'L', vr=v_caraelem_cesv)
!
! - Get index for storing shell parameters
!
    nb_para_maxi   = zi(j_caraelem_cesd-1+2)
    shell_ep_indx  = indik8(v_caraelem_cesc,'EP      ',1,nb_para_maxi)
    shell_exc_indx = indik8(v_caraelem_cesc,'EXCENT  ',1,nb_para_maxi)
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
        l_dist_shell = mminfl(sdcont_defi, 'DIST_COQUE', i_zone)
        if (l_dist_shell) then
            nb_slav_elem = mminfi(sdcont_defi, 'NBMAE' , i_zone)
            jdecme       = mminfi(sdcont_defi, 'JDECME', i_zone)
            do i_slav_elem = 1, nb_slav_elem
!
! ------------- Current element
!
                elem_slav_indx = jdecme+i_slav_elem
                elem_slav_nume = v_sdcont_mailco(elem_slav_indx)
                call jenuno(jexnum(mesh//'.NOMMAI', elem_slav_nume), elem_slav_name)
!
! ------------- Get thickness
!
                call cesexi('C', j_caraelem_cesd, j_caraelem_cesl, elem_slav_nume, 1,&
                            1, shell_ep_indx, iad1)
                if (iad1 .gt. 0) then
                    shell_ep = v_caraelem_cesv(iad1)
                else
                    call utmess('F', 'CONTACT3_39', sk=elem_slav_name)
                endif
!
! ------------- Get excentricity
!
                call cesexi('C', j_caraelem_cesd, j_caraelem_cesl, elem_slav_nume, 1,&
                            1, shell_exc_indx, iad1)
                if (iad1 .gt. 0) then
                    shell_excent = v_caraelem_cesv(iad1)
                    if (shell_excent .ge. r8prem()) then
                        call utmess('F', 'CONTACT3_40', sk=elem_slav_name)
                    endif
                else
                    call utmess('F', 'CONTACT3_41', sk=elem_slav_name)
                endif
!
! ------------- Save
!
                v_sdcont_jeucoq(elem_slav_indx) = 0.5d0 * shell_ep
            end do
        endif
    end do
!
999 continue
!
    call detrsd('CHAM_ELEM_S', cara_elem_s)
!
end subroutine
