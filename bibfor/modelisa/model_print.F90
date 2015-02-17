subroutine model_print(model)
!
    implicit none
!
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
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
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=8), intent(in) :: model
!
! --------------------------------------------------------------------------------------------------
!
! Print model informations
!
! --------------------------------------------------------------------------------------------------
!
! In  model        : name of the model
!
! --------------------------------------------------------------------------------------------------
!
    integer ifm, niv
    integer :: numvec, nbgrel, igrel, long_grel, nume_elem, nume_node
    integer :: nume_type_poi1, jc, j,k, ibegin
    integer :: nume_type_elem, nume_type_geom
    integer :: iexi, nb_type_elem
    integer :: nb_elem_grel
    character(len=8) :: type_geom, name_entity, tabmai(8)
    character(len=19) :: ligrel_model
    character(len=8) :: mesh
    character(len=16) :: type_elem, modelisa, valk(8)
    character(len=32) :: phemod
    integer, pointer :: p_mesh_typmai(:) => null()
    integer, pointer :: p_nb_elem(:) => null()
    character(len=24) :: model_liel
    integer, pointer :: p_model_liel(:) => null()
    character(len=24) :: model_nema
    integer, pointer :: p_model_nema(:) => null()
    character(len=8), pointer :: p_type_geom(:) => null()
    character(len=16), pointer :: p_modeli(:) => null()
    character(len=16), pointer :: p_type_elem(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    numvec   = 1
    modelisa = ' '
!
! - Access to mesh
!
    call dismoi('NOM_MAILLA', model, 'MODELE', repk = mesh)
    call jeveuo(mesh//'.TYPMAIL', 'L', vi = p_mesh_typmai)
!
! - Access to catalogs
!
    call jelira('&CATA.TE.NOMTE', 'NOMMAX', nb_type_elem)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), nume_type_poi1)
!
! - Access to model
!
    ligrel_model = model//'.MODELE'
    model_liel   = model//'.MODELE    .LIEL'
    model_nema   = model//'.MODELE    .NEMA'
!
! - Allocate
!
    AS_ALLOCATE(vi   = p_nb_elem  , size = nb_type_elem)
    AS_ALLOCATE(vk16 = p_modeli   , size = nb_type_elem)
    AS_ALLOCATE(vk8  = p_type_geom, size = nb_type_elem)
    AS_ALLOCATE(vk16 = p_type_elem, size = nb_type_elem)
!
! - Head
!
    call utmess('I', 'MODELE1_20')
!
! - Access to LIGREL
!
    call jeexin(ligrel_model//'.LIEL', iexi)
    if (iexi .ne. 0) then
        call jeveuo(model_liel, 'L', vi = p_model_liel)
        call jelira(model_liel, 'NMAXOC', nbgrel)
    endif
!
! - Access to "late" elements
!
    call jeexin(model_nema, iexi)
    if (iexi .gt. 0) then
        call jeveuo(model_nema, 'L', vi = p_model_nema)
    endif
!
! - Each type: counting
!
    do igrel = 1, nbgrel
        call jelira(jexnum(ligrel_model//'.LIEL', igrel), 'LONMAX', long_grel)
        nb_elem_grel = long_grel - 1
        if (nb_elem_grel .ne. 0) then
            nume_type_elem = p_model_liel(numvec+nb_elem_grel)
            if (nume_type_elem .eq. 0) then
                goto 10
            endif
            ASSERT(nume_type_elem.gt.0 .and. nume_type_elem.le.nb_type_elem)
            p_nb_elem(nume_type_elem) = p_nb_elem(nume_type_elem) + nb_elem_grel
            if (p_modeli(nume_type_elem) .eq. ' ') then
                call jenuno(jexnum('&CATA.TE.NOMTE', nume_type_elem), type_elem)
                if (type_elem .eq. 'MECA_HEXS8') then
                    call utmess('A', 'MODELE1_7')
                endif
                nume_elem = p_model_liel(numvec)
                if (nume_elem .lt. 0) then
                    nume_type_geom = nume_type_poi1
                else
                    nume_type_geom = p_mesh_typmai(nume_elem)
                endif
                call jenuno(jexnum('&CATA.TM.NOMTM', nume_type_geom), type_geom)
                call dismoi('PHEN_MODE', type_elem, 'TYPE_ELEM', repk = phemod)
                modelisa = phemod(17:32)
                if (phemod(1:10) .eq. '#PLUSIEURS') then
                    modelisa = ' '
                endif
                p_type_elem(nume_type_elem) = type_elem
                p_modeli(nume_type_elem)    = modelisa
                p_type_geom(nume_type_elem) = type_geom
            endif
        endif
        numvec = numvec+long_grel
 10     continue
    end do
!
! - Each type: printing
!
    do igrel = 1, nb_type_elem
        nb_elem_grel = p_nb_elem(igrel)
        if (nb_elem_grel .ne. 0) then
            valk(1) = p_modeli(igrel)
            valk(2) = p_type_geom(igrel)
            valk(3) = p_type_elem(igrel)
            call utmess('I', 'MODELE1_21', nk = 3, valk = valk, si = nb_elem_grel)
        endif
    end do
!
! - Level 2 printing
!
    numvec   = 1
    modelisa = ' '
    if (niv .eq. 2) then
        do igrel = 1, nbgrel
            call jelira(jexnum(ligrel_model//'.LIEL', igrel), 'LONMAX', long_grel)
            nb_elem_grel = long_grel - 1
            if (nb_elem_grel .ne. 0) then
                nume_elem = p_model_liel(numvec)
                if (nume_elem .lt. 0) then
                    nume_type_geom = nume_type_poi1
                else
                    nume_type_geom = p_mesh_typmai(nume_elem)
                endif
                nume_type_elem = p_model_liel(numvec+nb_elem_grel)
                call jenuno(jexnum('&CATA.TM.NOMTM', nume_type_geom), type_geom)
                call jenuno(jexnum('&CATA.TE.NOMTE', nume_type_elem), type_elem)
                call dismoi('PHEN_MODE', type_elem, 'TYPE_ELEM', repk=phemod)
                modelisa = phemod(17:32)
                if (phemod(1:10) .eq. '#PLUSIEURS') then
                    modelisa = ' '
                endif
                valk(1) = modelisa
                valk(2) = type_geom
                valk(3) = type_elem
                if (nume_elem .lt. 0) then
                    call utmess('I', 'MODELE1_8')
                else
                    call utmess('I', 'MODELE1_9')
                endif
                call utmess('I', 'MODELE1_21', nk = 3, valk = valk, si = long_grel-1)
                jc = 0
                ibegin = numvec
                do k = 1,nb_elem_grel
                    j=ibegin-1+k
                    jc = jc+1
                    nume_elem = p_model_liel(j)
                    if (nume_elem .lt. 0) then
                        ASSERT(p_model_nema(1).ne.0)
                        nume_node = p_model_nema((-nume_elem*2-1))
                        call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_entity)
                    else
                        call jenuno(jexnum(mesh//'.NOMMAI', nume_elem), name_entity)
                    endif
                    if (jc .le. 8) then
                        tabmai(jc) = name_entity
                    end if
                    if (jc .eq.8) then
                        call utmess('I', 'MODELE1_38', nk = 8, valk = tabmai)
                        jc         = 0
                    endif
                    if (k .eq. nb_elem_grel) then
                        valk(1:8) = ' '
                        if (jc.le.7) then
                            valk(1:jc) = tabmai(1:jc)
                        else
                            ASSERT(.false.)
                        endif
                        call utmess('I', 'MODELE1_38', nk = 8, valk = valk)
                    endif
                end do
            endif
            numvec = numvec+long_grel
        end do
    endif
!
! - Deallocate
!
    AS_DEALLOCATE(vi   = p_nb_elem  )
    AS_DEALLOCATE(vk16 = p_modeli   )
    AS_DEALLOCATE(vk8  = p_type_geom)
    AS_DEALLOCATE(vk16 = p_type_elem)
!
end subroutine
