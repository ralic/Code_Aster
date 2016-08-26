subroutine comp_meca_uvar(compor_info, vari_link_base, vari_redu, nb_vari_redu, codret)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/wkvect.h"
#include "asterfort/jexnum.h"
#include "asterfort/codent.h"
#include "asterfort/jelira.h"
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
    character(len=19), intent(in) :: compor_info
    character(len=8), intent(in) :: vari_link_base
    character(len=19), intent(in) :: vari_redu
    integer, intent(out) :: nb_vari_redu
    integer, intent(out) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Create list of available internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  compor_info      : name of object for information about internal variables and comportement
! In  vari_link_base   : basename of object to link zone (CARTE) with available internal variables
! In  vari_redu        : list of available internal variables
! Out nb_vari_redu     : number of available internal variables
!
! VARI_REDU is the list of available internal variables on ALL mesh
! VARI_LINK is a object, by zone in COMPOR CARTE, to make link between internal variable in zone 
!           with available internal variable
!
! NB: "false" internal variable for elastic behaviour (named 'VIDE') is NOT in the list !
!
! --------------------------------------------------------------------------------------------------
!
    integer, pointer :: v_info(:) => null()
    integer, pointer :: v_zone(:) => null()
    integer, pointer :: v_vari_link(:) => null()
    character(len=16), pointer :: v_vari_redu(:) => null()
    character(len=16) :: vari_name
    character(len=8) :: saux08
    character(len=19) :: vari_link
    character(len=16), pointer :: v_vari(:) => null()
    integer :: nb_zone, nb_vari, nt_vari, nb_elem_zone, nb_elem_mesh
    integer :: i_zone, i_vari, i_vari_redu
!
! --------------------------------------------------------------------------------------------------
!
    codret       = 0
    nb_vari_redu = 0
!
! - Access to objects
!
    call jeveuo(compor_info(1:19)//'.ZONE', 'L', vi = v_zone)
    call jeveuo(compor_info(1:19)//'.INFO', 'L', vi = v_info)
    nb_elem_mesh = v_info(1)
    nb_zone      = v_info(2)
    nt_vari      = v_info(4)
!
! - Create list of available internal variables (too large)
!
    call wkvect(vari_redu, 'V V K16', nt_vari, vk16 = v_vari_redu)
!
! - Loop on CARTE of COMPOR
!
    do i_zone = 1, nb_zone
        nb_elem_zone = v_zone(i_zone)
        if (nb_elem_zone .ne. 0) then
            call jeveuo(jexnum(compor_info(1:19)//'.VARI', i_zone), 'L', vk16 = v_vari)
            call jelira(jexnum(compor_info(1:19)//'.VARI', i_zone), 'LONMAX', nb_vari)
            call codent(i_zone, 'G', saux08)
            vari_link = vari_link_base//saux08
            call wkvect(vari_link, 'V V I', nb_vari, vi = v_vari_link)
            do i_vari = 1, nb_vari
                vari_name = v_vari(i_vari)
                if (vari_name .eq. 'VIDE') then
                    i_vari_redu = 0
                    goto 50
                endif
                if (vari_name(1:2) .eq. '&&') then
                   if (vari_name.eq.'&&MULT_COMP') then
                        call utmess('I', 'COMPOR4_15')
                    else if (vari_name.eq.'&&EXTE_COMP') then
                        call utmess('I', 'COMPOR4_16')
                    else
                        ASSERT(.false.)
                    endif
                    codret      = 200
                    i_vari_redu = 0
                    goto 50
                endif
                do i_vari_redu = 1, nb_vari_redu
                    if (vari_name .eq. v_vari_redu(i_vari_redu)) then
                        goto 50
                    endif
                end do
                nb_vari_redu = nb_vari_redu + 1
                v_vari_redu(nb_vari_redu) = vari_name
                i_vari_redu  = nb_vari_redu
50              continue
                v_vari_link(i_vari) = i_vari_redu
            end do
        endif 
    end do
!
end subroutine
