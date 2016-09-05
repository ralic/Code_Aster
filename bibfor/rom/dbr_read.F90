subroutine dbr_read(ds_para, result)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/romSnapRead.h"
#include "asterfort/rs_getfirst.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/rsexch.h"
#include "asterfort/dismoi.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(ROM_DS_ParaDBR), intent(inout) :: ds_para
    character(len=8), intent(out) :: result
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Read parameters
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_para          : datastructure for parameters
! Out result           : results from empirical base is constructed
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nocc, iret, nume_first
    integer :: nb_mode_maxi = 0, nb_equa = 0, nb_node = 0
    real(kind=8) :: tole_svd = 0.d0
    character(len=8)  :: base = ' ', model = ' ', mesh = ' '
    character(len=16) :: k16bid
    character(len=16) :: field_type = ' '
    character(len=8)  :: axe_line = ' '
    character(len=8)  :: surf_num = ' '
    character(len=8)  :: base_type = ' '
    type(ROM_DS_Snap) :: ds_snap
    type(ROM_DS_Empi) :: ds_empi
    character(len=24) :: field_refe = '&&ROM_COMP.FIELD'
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
    call getres(base, k16bid, k16bid)
!
! - Get parameters - Results to process
!
    result = ' '
    call getvid(' ', 'RESULTAT', scal = result)
    call getvtx(' ', 'NOM_CHAM', scal = field_type, nbret = nocc)
    ASSERT(nocc .eq. 1)
    call dismoi('NOM_MODELE', result, 'RESULTAT', repk = model)
!
! - Get parameters - Base type to numeration
!
    call getvtx(' ', 'TYPE_BASE', scal = base_type)
    if (base_type .eq. 'LINEIQUE') then
        call getvtx(' ', 'AXE', scal = axe_line, nbret = nocc)
        ASSERT(nocc .eq. 1)
        call getvtx(' ', 'SECTION', scal = surf_num, nbret = nocc)
        ASSERT(nocc .eq.1 )
    endif
!
! - Get parameters - For SVD selection
!
    call getvr8(' ', 'TOLE_SVD', scal = tole_svd)
    call getvis(' ', 'NB_MODE' , scal = nb_mode_maxi, nbret = nocc)
    if (nocc .eq. 0) then
        nb_mode_maxi = 0
    endif
!
! - Get informations about fields
!
    call rs_getfirst(result, nume_first)
    call rsexch(' ', result, field_type, nume_first, field_refe, iret)
    if (iret .ne. 0) then
        call utmess('F', 'ROM5_11', sk = field_type)
    endif
    call dismoi('NB_EQUA'     , field_refe, 'CHAM_NO' , repi = nb_equa) 
    call dismoi('NOM_MAILLA'  , field_refe, 'CHAM_NO' , repk = mesh)
    call dismoi('NB_NO_MAILLA', mesh      , 'MAILLAGE', repi = nb_node)
    ds_empi%base         = base
    ds_empi%field_type   = field_type
    ds_empi%field_refe   = field_refe
    ds_empi%mesh         = mesh
    ds_empi%model        = model
    ds_empi%base_type    = base_type
    ds_empi%axe_line     = axe_line
    ds_empi%surf_num     = surf_num
    ds_empi%nb_node      = nb_node
    ds_empi%nb_mode      = 0
    ds_empi%nb_equa      = nb_equa
    ds_empi%nb_cmp       = nb_equa/nb_node
!
! - Read parameters for snapshot selection
!
    ds_snap = ds_para%ds_snap
    call romSnapRead(result, ds_snap)
!
! - Save parameters in datastructure
!
    ds_para%nb_mode_maxi = nb_mode_maxi
    ds_para%ds_snap      = ds_snap
    ds_para%tole_svd     = tole_svd
    ds_para%ds_empi      = ds_empi
!
end subroutine
