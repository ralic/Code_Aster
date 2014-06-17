subroutine nmdovd(model, l_affe_all, list_elem_affe, nb_elem_affe, full_elem_s,&
                  defo_comp, defo_comp_py)
!
    implicit none
!
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
    logical, intent(in) :: l_affe_all
    integer, intent(in) :: nb_elem_affe
    character(len=19), intent(in) :: full_elem_s
    character(len=16), intent(in) :: defo_comp
    character(len=16), intent(in) :: defo_comp_py
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Check deformation with Comportement.py
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  full_elem_s    :  <CHELEM_S> of FULL_MECA option
! In  l_affe_all     : .true. if affect on all elements of model
! In  nb_elem_affe   : number of elements where comportment affected
! In  list_elem_affe : list of elements where comportment affected
! In  defo_comp      : comportement DEFORMATION
! In  defo_comp_py   : comportement DEFORMATION - Python coding
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: notype, texte(3), type_elem, type_elem2
    character(len=8) :: mesh, name_elem
    character(len=19) :: ligrmo
    integer :: nutyel
    integer :: j_cesd, j_cesl, j_cesv
    integer ::  iret, irett, ielem
    integer :: iad
    integer ::  j_grel, j_elem_affe
    integer :: nb_elem_mesh, nb_elem, nb_elem_grel
    integer :: nume_elem, nume_grel
    integer, pointer :: repe(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Access to model and mesh
!
    ligrmo = model//'.MODELE'
    call jeveuo(ligrmo(1:19)//'.REPE', 'L', vi=repe)
    call dismoi('NOM_MAILLA', model(1:8), 'MODELE', repk=mesh)
!
! - Access to <CHELEM_S> of FULL_MECA option
!
    call jeveuo(full_elem_s//'.CESD', 'L', j_cesd)
    call jeveuo(full_elem_s//'.CESL', 'L', j_cesl)
    call jeveuo(full_elem_s//'.CESV', 'L', j_cesv)
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
        call jenuno(jexnum(mesh(1:8)//'.NOMMAI', nume_elem), name_elem)
!
! ----- <CARTE> access
!
        call cesexi('C', j_cesd, j_cesl, nume_elem, 1,&
                    1, 1, iad)
        if (iad .gt. 0) then
!
! --------- Access to element type
!
            nume_grel = repe(2*(nume_elem-1)+1)
            call jeveuo(jexnum(ligrmo(1:19)//'.LIEL', nume_grel), 'L', j_grel)
            call jelira(jexnum(ligrmo(1:19)//'.LIEL', nume_grel), 'LONMAX', nb_elem_grel)
            nutyel = zi(j_grel-1+nb_elem_grel)
            call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), notype)
!
! --------- Type of modelization
!
            call teattr('C', 'TYPMOD', type_elem, iret, typel=notype)
            if (iret .eq. 0) then
                call teattr('C', 'TYPMOD2', type_elem2, iret, typel=notype)
                if (type_elem(1:6) .eq. 'COMP3D') then
                    call lctest(defo_comp_py, 'MODELISATION', '3D', irett)
                    if (irett .eq. 0) then
                        texte(1) = notype
                        texte(2) = name_elem
                        texte(3) = defo_comp
                        call utmess('F', 'COMPOR1_52', nk=3, valk=texte)
                    endif
                else if (type_elem(1:6).eq.'COMP1D') then
                    if (type_elem2 .eq. 'PMF') then
                        call lctest(defo_comp_py, 'MODELISATION', 'PMF', irett)
                        if (irett .eq. 0) then
                            texte(1) = notype
                            texte(2) = name_elem
                            texte(3) = defo_comp
                            call utmess('F', 'COMPOR1_52', nk=3, valk=texte)
                        endif
                    else
                        call lctest(defo_comp_py, 'MODELISATION', '1D', irett)
                        if (irett .eq. 0) then
                            texte(1) = notype
                            texte(2) = name_elem
                            texte(3) = defo_comp
                            call utmess('F', 'COMPOR1_52', nk=3, valk=texte)
                        endif
                    endif
                else
                    call lctest(defo_comp_py, 'MODELISATION', type_elem, irett)
                    if (irett .eq. 0) then
                        texte(1) = notype
                        texte(2) = name_elem
                        texte(3) = defo_comp
                        call utmess('F', 'COMPOR1_52', nk=3, valk=texte)
                    endif
                endif
            endif
        endif
    end do
!
    call jedema()
!
end subroutine
