subroutine nmetc0(model, cara_elem, compor, sd_inout)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: compor
    character(len=24), intent(in) :: sd_inout
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Compute initial field if necessary
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_inout         : datastructure for input/output parameters
! In  model            : name of model
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  compor           : name of <CARTE> COMPOR
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    character(len=24) :: field_type, field_name_init
    character(len=24) :: sief_init, vari_init, strx_init
    integer :: i_field, nb_field, zioch
    character(len=8) :: lpain(1), lpaout(2)
    character(len=24) :: lchin(1), lchout(2)
    character(len=19) :: ligrmo
    character(len=24) :: chgeom
    aster_logical :: l_sief, l_vari, l_strx
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Access to datastructure
!
    io_lcha = sd_inout(1:19)//'.LCHA'
    call jeveuo(io_lcha, 'L', vk24 = v_io_para)
    io_info = sd_inout(1:19)//'.INFO'
    call jeveuo(io_info, 'L', vi   = v_io_info)
!
    nb_field = v_io_info(1)
    zioch    = v_io_info(4)
!
! - Create initial fields or not ?
!
    l_sief = .false.
    l_vari = .false.
    l_strx = .false.
    do i_field = 1, nb_field
        field_type      = v_io_para(zioch*(i_field-1)+1 )
        field_name_init = v_io_para(zioch*(i_field-1)+2 )
        if (field_type .eq. 'SIEF_ELGA') then
            l_sief    = .true.
            sief_init = field_name_init
        endif
        if (field_type .eq. 'VARI_ELGA') then
            l_vari    = .true.
            vari_init = field_name_init
        endif
        if (field_type .eq. 'STRX_ELGA') then
            l_strx    = .true.
            strx_init = field_name_init
        endif
    end do
!
! - Initial fields: compute stress and internal variables
!
    if (l_vari .or. l_sief) then
        call dismoi('NOM_LIGREL', model, 'MODELE', repk=ligrmo)
        call copisd('CHAM_ELEM_S', 'V', compor, sief_init)
        call copisd('CHAM_ELEM_S', 'V', compor, vari_init)
        call megeom(model, chgeom)
        lpain(1)  = 'PGEOMER'
        lchin(1)  = chgeom
        lpaout(1) = 'PVARI_R'
        lchout(1) = vari_init
        lpaout(2) = 'PSIEF_R'
        lchout(2) = sief_init
        call calcul('S', 'TOU_INI_ELGA', ligrmo, 1, lchin,&
                    lpain, 2, lchout, lpaout, 'V',&
                    'OUI')
    endif
!
! - Initial fields: special multifibers field
!
    if (l_strx) then
        lpain(1)  = 'PCAORIE'
        lchin(1)  = cara_elem(1:8)//'.CARORIEN'
        lpaout(1) = 'PSTRX_R'
        lchout(1) = strx_init
        call calcul('S', 'INI_STRX', ligrmo, 1, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
    endif
!
    call jedema()
end subroutine
