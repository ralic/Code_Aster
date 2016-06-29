subroutine comp_meca_mod(keywordfact, iocc, model, ndime_model, nom_mod_mfront)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/comp_read_mesh.h"
#include "asterfort/teattr.h"
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
! person_in_charge: nicolas.sellenet at edf.fr
!
    character(len=16), intent(in) :: keywordfact
    integer :: iocc, ndime_model
    character(len=8), intent(in) :: model
    character(len=16), intent(out) :: nom_mod_mfront
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Recherche de l'hypothese de modelisation associe a une occurence
! du mot cle facteur keywordfact
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact    : nom du mot cle facteur
! In  iocc           : occurence du mot cle facteur
! In  model          : nom du modele
! In  mesh           : nom du maillage
! Out nom_mod_mfront : type de modelisation
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_elem_affe, nb_elem, ielem, nume_elem
    integer :: nutyel, iret
    aster_logical :: l_affe_all
    character(len=24) :: list_elem_affe
    character(len=16) :: notype, type_elem, type_elem_save, principal
    character(len=8) :: mesh
    character(len=1) :: d1
    integer, pointer :: maille(:) => null()
    integer, pointer :: v_elem_affe(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jeveuo(model//'.MAILLE', 'L', vi=maille)
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
    list_elem_affe = '&&COMPMECASAVE.LIST'
!
!
! - Get list of elements where comportment is defined
!
    call comp_read_mesh(mesh          , keywordfact, iocc        ,&
                        list_elem_affe, l_affe_all , nb_elem_affe)
!
    if (l_affe_all) then
        call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi=nb_elem)
    else
        call jeveuo(list_elem_affe, 'L', vi = v_elem_affe)
        nb_elem = nb_elem_affe
    endif
!
    type_elem_save = ' '
    do ielem = 1, nb_elem
!
! ----- Element courant
!
        if (l_affe_all) then
            nume_elem = ielem
        else
            nume_elem = v_elem_affe(ielem)
        endif
!
! --------- Recherche du type d'element
!
        nutyel = maille(nume_elem)
        if (nutyel.ne.0) then
            call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), notype)
!
! --------- Type de modelisation
!
            call teattr('C', 'TYPMOD', type_elem, iret, typel=notype)
            call teattr('C', 'PRINCIPAL', principal, iret, typel=notype)
            call teattr('C', 'DIM_TOPO_MODELI', d1, iret, typel=notype)
            read(d1,'(I1)') ndime_model

            if (principal(1:3) .eq. 'OUI') then
!
                if ( type_elem_save.eq.' ' ) type_elem_save = type_elem

                if (type_elem_save.ne.type_elem) then
                  call utmess('F','COMPOR4_13', si=iocc, nk=2, &
                              valk=[type_elem_save, type_elem])
                endif
!
                if ( type_elem.eq.'COMP3D' ) then
                    nom_mod_mfront = '_Tridimensional'
                elseif ( type_elem.eq.'C_PLAN' ) then
                    nom_mod_mfront = '_PlaneStress'
                elseif ( type_elem.eq.'D_PLAN' ) then
                    nom_mod_mfront = '_PlaneStrain'
                elseif ( type_elem.eq.'AXIS' ) then
                    nom_mod_mfront = '_Axisymmetrical'
                elseif ( type_elem.eq.'NON_DEFINI' ) then
! dans phenomene_modelisation.cata, il manque l'attribut TYPMOD pour la THM
                    nom_mod_mfront = '_Tridimensional'
                else
                    ASSERT(.false.)
                endif
            endif

        endif
!
    enddo
!
!
end subroutine
