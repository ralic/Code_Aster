subroutine dfc_read_disc(sdcont      , keywf       , mesh        , model        , model_ndim,&
                         nb_cont_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/dfc_read_zone.h"
#include "asterfort/cfnodb.h"
#include "asterfort/cfbord.h"
#include "asterfort/chckco.h"
#include "asterfort/dimeco.h"
#include "asterfort/dimecz.h"
#include "asterfort/quadco.h"
#include "asterfort/elimcq.h"
#include "asterfort/tablco.h"
#include "asterfort/cacoco.h"
#include "asterfort/cacoeq.h"
#include "asterfort/capoco.h"
#include "asterfort/elimco.h"
#include "asterfort/sansco.h"
#include "asterfort/typeco.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: model_ndim
    integer, intent(in) :: nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Discrete method - Read contact data
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  mesh             : name of mesh
! In  model            : name of model
! In  model_ndim       : dimension of model
! In  nb_cont_zone     : number of zones of contact
! Out nb_cont_surf     : number of surfaces of contact
! Out nb_cont_elem     : number of elements of contact
! Out nb_cont_node     : number of nodes of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cont_surf, nb_cont_elem, nb_cont_node
    integer :: cont_form, indqua
!
! --------------------------------------------------------------------------------------------------
!
    cont_form = 1
!
! - Quadratic elements (indqua = 0 if need linearization, see CACOEQ)
!
    call quadco(sdcont, indqua)
!
! - Read zone: nodes and elements
!
    call dfc_read_zone(sdcont      , keywf       , mesh        , model        , nb_cont_zone,&
                       nb_cont_surf, nb_cont_elem, nb_cont_node)
!
! - Cleaning nodes and elements
!
    call elimco(sdcont      , mesh        , model        , indqua,  nb_cont_zone,&
                nb_cont_surf, nb_cont_elem, nb_cont_node )
!
! - Suppress middle nodes from QUAD8
!
    if (indqua.eq.0) then
        call elimcq(sdcont      , mesh, indqua, nb_cont_zone, nb_cont_surf,&
                    nb_cont_node)
    endif
!
! - Inverse connectivities
!
    call tablco(sdcont, mesh, nb_cont_surf, nb_cont_elem, nb_cont_node)
!
! - Information by zone
!
    call dimecz(sdcont, mesh, nb_cont_zone, cont_form)
!
! - Information for vectors length
!
    call dimeco(sdcont      , model_ndim, nb_cont_zone, nb_cont_surf, nb_cont_elem,&
                nb_cont_node)
!
! - Keyword SANS_GROUP_NO
!
    call sansco(sdcont, keywf, mesh)
!
! - Elements and nodes parameters
!
    call typeco(sdcont, mesh)
!
! - Check common nodes
!
    call cfnodb(sdcont)
!
! - Check dimension of elements versus model dimension
!
    call cfbord(sdcont, mesh)
!
! - Check normals/tangents
!
    call chckco(sdcont, mesh, model_ndim)
!
! - Gap for beams
!
    call capoco(sdcont, keywf)
!
! - Gap for shells
!
    call cacoco(sdcont, keywf, mesh)
!
! - Linear relations for quadratic elements
!
    if (indqua.eq.0) then
        call cacoeq(sdcont, mesh)
    endif
!
end subroutine
