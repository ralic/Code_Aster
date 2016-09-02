subroutine romBaseRead(base, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/romBaseInfo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rs_getfirst.h"
#include "asterfort/rs_get_liststore.h"
#include "asterfort/rsexch.h"
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
    character(len=8), intent(in)     :: base
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Read empiric modes base
!
! --------------------------------------------------------------------------------------------------
!
! In  base             : name of empiric base
! IO  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: iret, nume_first, jv_para, nume_pl
    integer :: nb_equa = 0, nb_mode = 0, nb_node = 0, nb_cmp = 0
    character(len=8)  :: mesh = ' ', model = ' ', base_type = ' ', axe_line = ' '
    character(len=24) :: surf_num = ' ', field_refe = ' ', field_type = ' '
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_2', sk = base)
    endif
!
! - Get informations about empiric modes - Parameters
!
    call rs_get_liststore(base, nb_mode)
    call rsadpa(base, 'L', 1, 'NOM_CHAM', 1, 0, sjv = jv_para)
    field_type   = zk24(jv_para)
    call rsadpa(base, 'L', 1, 'MODELE'  , 1, 0, sjv = jv_para)
    model        = zk8(jv_para)
    call rsadpa(base, 'L', 1, 'NUME_PLAN', 1, 0, sjv = jv_para)
    nume_pl      = zi(jv_para)
    base_type    = ' '
    if (nume_pl .ne. 0) then
        base_type = 'LINEIQUE'
    endif
!
! - Get informations about empiric modes - Field
!
    call rs_getfirst(base, nume_first)
    field_refe = base(1:8)//'FIELD_REFE'
    call rsexch(' ', base, field_type, nume_first, field_refe, iret)
    ASSERT(iret.eq.0)
!
! - Get informations about empiric modes - Others
!
    call dismoi('NB_EQUA'     , field_refe, 'CHAM_NO' , repi = nb_equa)
    call dismoi('NOM_MAILLA'  , model     , 'MODELE'  , repk = mesh)
    call dismoi('NB_NO_MAILLA', mesh      , 'MAILLAGE', repi = nb_node)
    nb_cmp = nb_equa/nb_node
!
! - Save informations about empiric modes
!
    ds_empi%base       = base
    ds_empi%field_type = field_type
    ds_empi%field_refe = field_refe
    ds_empi%mesh       = mesh
    ds_empi%model      = model
    ds_empi%base_type  = base_type
    ds_empi%axe_line   = axe_line
    ds_empi%surf_num   = surf_num
    ds_empi%nb_equa    = nb_equa
    ds_empi%nb_node    = nb_node
    ds_empi%nb_cmp     = nb_cmp
    ds_empi%nb_mode    = nb_mode
!
! - Print
!
    if (niv .ge. 2) then
       call romBaseInfo(ds_empi)
    endif
!
end subroutine
