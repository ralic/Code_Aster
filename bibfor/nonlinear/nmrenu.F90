subroutine nmrenu(modelz  , list_func_acti, list_load, ds_contact, nume_dof,&
                  l_renumber)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/numer3.h"
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
    character(len=*), intent(in) :: modelz
    character(len=24), intent(inout) :: nume_dof
    character(len=19), intent(in) :: list_load
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: list_func_acti(*)
    aster_logical, intent(out) :: l_renumber
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm
!
! Renumbering equations ?
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model datastructure
! In  list_load        : list of loads
! In  ds_contact       : datastructure for contact management
! In  list_func_acti   : list of active functionnalities
! Out l_renumber       : .true. if renumber
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_cont, l_cont_cont, l_cont_xfem, l_cont_elem, l_cont_xfem_gg
    character(len=24) :: sd_iden_rela
    character(len=24) :: crnudd
    aster_logical, pointer :: v_crnudd(:) => null()
    character(len=24) :: nosdco
    character(len=24), pointer :: v_nosdco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! - Initializations
!
    crnudd       = ds_contact%sdcont_solv(1:14)//'.NUDD'
    nosdco       = ds_contact%sdcont_solv(1:14)//'.NOSDCO'
    l_renumber   = .false.
    l_cont       = isfonc(list_func_acti,'CONTACT')
    if (.not.l_cont) then
        goto 999
    endif
!
! - Get identity relation datastructure
!
    call jeveuo(nosdco, 'L', vk24 = v_nosdco)
    sd_iden_rela = v_nosdco(4)
!
! - Contact method
!
    l_cont_elem = isfonc(list_func_acti,'ELT_CONTACT')
    l_cont_xfem = isfonc(list_func_acti,'CONT_XFEM')
    l_cont_cont = isfonc(list_func_acti,'CONT_CONTINU')
!
! - Numbering to change ?
!
    if (l_cont_elem) then
        if (l_cont_xfem) then
            l_cont_xfem_gg = cfdisl(ds_contact%sdcont_defi,'CONT_XFEM_GG')
            if (l_cont_xfem_gg) then
               l_renumber = .true.
            else
               l_renumber = .false.
            endif
        else
            call jeveuo(crnudd, 'E', vl = v_crnudd)
            l_renumber = v_crnudd(1)
            v_crnudd(1) = .false.
        endif
    endif
!
! - Re-numbering
!
    if (l_renumber) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ...... RE-CREATION DU NUME_DDL '
        endif
        call numer3(modelz, list_load, nume_dof, sd_iden_rela)
    endif
!
999 continue
!
end subroutine
