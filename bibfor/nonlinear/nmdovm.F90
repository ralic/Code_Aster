subroutine nmdovm(model       , l_affe_all, list_elem_affe, nb_elem_affe  , full_elem_s,&
                  rela_comp_py, type_cpla , l_auto_elas   , l_auto_deborst, l_comp_erre)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/lctest.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/teattr.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=8), intent(in) :: model
    character(len=24), intent(in) :: list_elem_affe
    aster_logical, intent(in) :: l_affe_all
    integer, intent(in) :: nb_elem_affe
    character(len=19), intent(in) :: full_elem_s
    character(len=16), intent(in) :: rela_comp_py
    character(len=16), intent(out) :: type_cpla
    aster_logical, intent(out) :: l_auto_elas
    aster_logical, intent(out) :: l_auto_deborst
    aster_logical, intent(out) :: l_comp_erre
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE>
!
! Check comportment/model with Comportement.py
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  full_elem_s    :  <CHELEM_S> of FULL_MECA option
! In  l_affe_all     : .true. if affect on all elements of model
! In  nb_elem_affe   : number of elements where comportment affected
! In  list_elem_affe : list of elements where comportment affected
! In  rela_comp_py   : comportement RELATION - Python coding
! Out type_cpla      : stress plane hypothesis (for Deborst)
! Out l_auto_elas    : .true. if at least one element use ELAS by default
! Out l_auto_deborst : .true. if at least one element swap to Deborst algorithm
! Out l_comp_erre    : .true. if at least one element use comportment on element doesn't support it
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: notype, type_elem, comp_rela_elem
    character(len=8) :: mesh
    integer :: nutyel
    integer :: j_cesd, j_cesl
    integer :: iret, irett, ielem
    integer :: iad
    integer :: j_elem_affe
    integer :: nb_elem_mesh, nb_elem
    integer :: nume_elem
    character(len=16), pointer :: cesv(:) => null()
    integer, pointer :: maille(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    type_cpla = 'ANALYTIQUE'
    l_auto_elas    = .false.
    l_auto_deborst = .false.
    l_comp_erre    = .false.
!
! - Access to model and mesh
!
    call jeveuo(model//'.MAILLE', 'L', vi=maille)
    call dismoi('NOM_MAILLA', model(1:8), 'MODELE', repk=mesh)
!
! - Access to <CHELEM_S> of FULL_MECA option
!
    call jeveuo(full_elem_s//'.CESD', 'L', j_cesd)
    call jeveuo(full_elem_s//'.CESL', 'L', j_cesl)
    call jeveuo(full_elem_s//'.CESV', 'L', vk16=cesv)
    nb_elem_mesh = zi(j_cesd-1+1)
!
! - Mesh affectation
!
    if (l_affe_all) then
        nb_elem = nb_elem_mesh
    else
        call jeveuo(list_elem_affe, 'L', j_elem_affe)
        nb_elem = nb_elem_affe
    endif
!
! - Loop on elements
!
    do ielem = 1, nb_elem
!
! ----- Current element
!
        if (l_affe_all) then
            nume_elem = ielem
        else
            nume_elem = zi(j_elem_affe-1+ielem)
        endif
!
! ----- <CARTE> access
!
        call cesexi('C', j_cesd, j_cesl, nume_elem, 1,&
                    1, 1, iad)
        if (iad .gt. 0) then
!
! --------- Comportment on element
!
            comp_rela_elem = cesv(iad)
            if (comp_rela_elem .eq. ' ') then
                l_auto_elas = .true.
            endif
!
! --------- Access to element type
!
            nutyel = maille(nume_elem)
            call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), notype)
!
! --------- Type of modelization
!
            call teattr('C', 'TYPMOD', type_elem, iret, typel=notype)
            if (iret .eq. 0) then
                if (type_elem(1:6) .eq. 'C_PLAN') then
                    call lctest(rela_comp_py, 'MODELISATION', 'C_PLAN', irett)
                    if (irett .eq. 0) then
                        l_auto_deborst = .true.
                        type_cpla = 'DEBORST'
                    endif
                else if (type_elem(1:6).eq.'COMP1D') then
                    call lctest(rela_comp_py, 'MODELISATION', '1D', irett)
                    if (irett .eq. 0) then
                        l_auto_deborst = .true.
                        type_cpla = 'DEBORST'
                    endif
                else if (type_elem(1:6).eq.'COMP3D') then
                    call lctest(rela_comp_py, 'MODELISATION', '3D', irett)
                    if (irett .eq. 0) then
                        l_comp_erre = .true.
                    endif
                else
                    call lctest(rela_comp_py, 'MODELISATION', type_elem, irett)
                    if (irett .eq. 0) then
                        l_comp_erre = .true.
                    endif
                endif
            endif
        endif
    enddo
!
    call jedema()
!
end subroutine
