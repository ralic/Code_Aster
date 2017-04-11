subroutine ddr_read(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvis.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/romBaseRead.h"
#include "asterfort/getnode.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_ParaDDR), intent(inout) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_DOMAINE_REDUIT - Initializations
!
! Read parameters
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_layer_ma = 0, nocc, nb_node = 0
    aster_logical :: l_corr_ef = .false._1
    type(ROM_DS_Empi) :: empi_prim, empi_dual
    character(len=8)  :: base_prim = ' ', base_dual = ' ', mesh = ' '
    character(len=16) :: k16bid = ' ', answer, keywf
    character(len=24) :: grelem_rid  = ' ', grnode_int  = ' ', grnode_sub = ' '
    character(len=24) :: list_node
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_10')
    endif
!
! - Output datastructure
!
    call getres(mesh, k16bid, k16bid)
!
! - Get parameters
!
    call getvtx(' ', 'NOM_DOMAINE'    , scal = grelem_rid)
    call getvis(' ', 'NB_COUCHE_SUPPL', scal = nb_layer_ma)
    call getvtx(' ', 'NOM_INTERFACE'  , scal = grnode_int)
    call getvtx(' ', 'CORR_COMPLET'   , scal = answer)
    l_corr_ef = answer .eq. 'OUI'
    if (l_corr_ef) then
        call getvtx(' ', 'NOM_ENCASTRE', scal = grnode_sub)
    endif
    keywf = 'DOMAINE_INCLUS'
    call getfac(keywf, nocc)
    ASSERT(nocc .le. 1)
    if (nocc .eq. 1) then
        list_node = '&&OP0050.LIST_NODE'
        call getnode(mesh   , keywf, 1, ' ', list_node,&
                     nb_node)
        call jeveuo(list_node, 'L', vi = ds_para%v_rid_mini)
        ds_para%nb_rid_mini = nb_node
    endif
!
! - Get informations about bases - Primal
!
    call getvid(' ', 'BASE_PRIMAL', scal = base_prim)
    call romBaseRead(base_prim, empi_prim)
!
! - Get informations about bases - Dual
!
    call getvid(' ', 'BASE_DUAL', scal = base_dual)
    call romBaseRead(base_dual, empi_dual)
!
! - Save parameters in datastructure
!
    ds_para%mesh          = mesh
    ds_para%grelem_rid    = grelem_rid
    ds_para%nb_layer_ma   = nb_layer_ma
    ds_para%grnode_int    = grnode_int
    ds_para%l_corr_ef     = l_corr_ef
    ds_para%grnode_sub    = grnode_sub
    ds_para%ds_empi_prim  = empi_prim
    ds_para%ds_empi_dual  = empi_dual
!
end subroutine
