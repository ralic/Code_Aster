subroutine dfc_read_cont(sdcont, keywf       , mesh, model, model_ndim,&
                         ligret, nb_cont_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/dfc_read_zone.h"
#include "asterfort/dfc_save_dime.h"
#include "asterfort/dfc_chck.h"
#include "asterfort/tablco.h"
#include "asterfort/utmess.h"
#include "asterfort/cacoco.h"
#include "asterfort/capoco.h"
#include "asterfort/caraxi.h"
#include "asterfort/elimco.h"
#include "asterfort/sansco.h"
#include "asterfort/sanscc.h"
#include "asterfort/typeco.h"
#include "asterfort/mmprel.h"
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
    character(len=19), intent(in) :: ligret
    integer, intent(in) :: nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Continue method - Read contact data
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  mesh             : name of mesh
! In  model            : name of model
! In  ligret           : name of special LIGREL for slave elements (CONTINUE formulation)
! In  model_ndim       : dimension of model
! In  nb_cont_zone     : number of zones of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cont_surf, nb_cont_elem, nb_cont_node, nb_node_coq3d
    aster_logical :: l_elim_coq3d
!
! --------------------------------------------------------------------------------------------------
!
    l_elim_coq3d = .false.
!
! - Read zone: nodes and elements
!
    call dfc_read_zone(sdcont      , keywf       , mesh        , model, nb_cont_zone,&
                       nb_cont_surf, nb_cont_elem, nb_cont_node)
!
! - Cleaning nodes and elements
!
    call elimco(sdcont      , mesh        , model  , nb_cont_surf,&
                nb_cont_elem, nb_cont_node, l_elim_coq3d, nb_node_coq3d_ = nb_node_coq3d)
    if (nb_node_coq3d.ne.0) then
        call utmess('F','CONTACT_94')
    endif
!
! - Inverse connectivities
!
    call tablco(sdcont, mesh, nb_cont_surf, nb_cont_elem, nb_cont_node)
!
! - Save contact counters
!
    call dfc_save_dime(sdcont      , mesh        , model_ndim, nb_cont_zone, nb_cont_surf,&
                       nb_cont_elem, nb_cont_node)
!
! - Keyword SANS_GROUP_NO
!
    call sansco(sdcont, keywf, mesh)
!
! - Special keywords for CONTINUE method
!
    call sanscc(sdcont, keywf, mesh)
!
! - Elements and nodes parameters
!
    call typeco(sdcont, mesh)
!
! - Some checks
!
    call dfc_chck(sdcont, mesh, model_ndim)
!
! - Gap for beams
!
    call capoco(sdcont, keywf)
!
! - Gap for shells
!
    call cacoco(sdcont, keywf, mesh)
!
! - Check if axi-symmetric
!
    call caraxi(sdcont, model, mesh, model_ndim)
!
! - Create slave elements in model
!
    call mmprel(sdcont, mesh, model, ligret)
!
end subroutine
