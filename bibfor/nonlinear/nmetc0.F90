subroutine nmetc0(model, cara_elem, compor, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/alchml.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: compor
    type(NL_DS_InOut), intent(in) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Compute initial fields if necessary
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  compor           : name of <CARTE> COMPOR
! In  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: field_type, init_name
    character(len=24) :: sief_init, vari_init, strx_init
    integer :: i_field, nb_field, iret
    character(len=8) :: lpain(1), lpaout(2)
    character(len=24) :: lchin(1), lchout(2)
    character(len=19) :: ligrmo
    aster_logical :: l_sief, l_vari, l_strx, l_acti
!
! --------------------------------------------------------------------------------------------------
!
    nb_field = ds_inout%nb_field
!
! - Create initial fields or not ?
!
    l_sief = .false._1
    l_vari = .false._1
    l_strx = .false._1
    do i_field = 1, nb_field
        field_type = ds_inout%field(i_field)%type
        init_name  = ds_inout%field(i_field)%init_name
        l_acti     = ds_inout%l_field_acti(i_field)
        if (field_type .eq. 'SIEF_ELGA' .and. l_acti) then
            l_sief    = .true.
            sief_init = init_name
        endif
        if (field_type .eq. 'VARI_ELGA' .and. l_acti) then
            l_vari    = .true.
            vari_init = init_name
        endif
        if (field_type .eq. 'STRX_ELGA' .and. l_acti) then
            l_strx    = .true.
            strx_init = init_name
        endif
    end do
!
! - Initial fields: compute stress and internal variables
!
    if (l_vari .or. l_sief) then
        call dismoi('NOM_LIGREL', model, 'MODELE', repk=ligrmo)
        call alchml(ligrmo,'TOU_INI_ELGA','PSIEF_R','V',sief_init,iret,compor)
        call alchml(ligrmo,'TOU_INI_ELGA','PVARI_R','V',vari_init,iret,compor)
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
end subroutine
