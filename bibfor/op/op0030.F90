subroutine op0030()
!
    implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/cagene.h"
#include "asterfort/calico.h"
#include "asterfort/caliun.h"
#include "asterfort/cfdisl.h"
#include "asterfort/chveno.h"
#include "asterfort/copisd.h"
#include "asterfort/cormgi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/infmaj.h"
#include "asterfort/initel.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/lgtlgr.h"
#include "asterfort/wkvect.h"
#include "asterfort/utmess.h"
#include "asterfort/exixfe.h"
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
!
!
! ----------------------------------------------------------------------
!
! COMMANDE:  DEFI_CONTACT
!
! ----------------------------------------------------------------------
!
    integer :: iret, noc, nb_dim
    character(len=4) :: vale_type
    character(len=8) :: mesh, model, load
    character(len=16) :: k16dummy, command
    character(len=16) :: formul
    character(len=19) :: ligrmo, ligret, ligrel, ligrch
    integer :: iform
    aster_logical :: lallv
    character(len=24) :: sdcont_defi
    character(len=8), pointer :: p_load_type(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
! - Initializations
!
    formul = ' '
    iform = 0
    ligret = '&&OP0030.LIGRET'
    ligrel = '&&OP0030.LIGREL'
    vale_type = 'REEL'
!
! - Which command ?
!
    call getres(load, k16dummy, command)
!
! - Mesh, Ligrel for model, dimension of model
!
    sdcont_defi = load(1:8)//'.CONTACT'
!
! - Mesh, Ligrel for model, dimension of model
!
    call cagene(load, command, ligrmo, mesh, nb_dim)
    model = ligrmo(1:8)
!
! - Load type
!
    ligrch = load//'.CHME.LIGRE'
    call wkvect(load//'.TYPE', 'G V K8', 1, vk8 = p_load_type)
    p_load_type(1) = 'MECA_RE'
!
! - Contact formulation
!
    call getvtx(' ', 'FORMULATION', scal=formul, nbret=noc)
    ASSERT(noc.ne.0)
!
    if (formul .eq. 'DISCRETE') then
        iform = 1
    else if (formul.eq.'CONTINUE') then
        iform = 2
    else if (formul.eq.'XFEM') then
        iform = 3
    else if (formul.eq.'LIAISON_UNIL') then
        iform = 4
    else
        ASSERT(.false.)
    endif
!
! - Check model
!
    if (formul.eq.'XFEM') then
        call exixfe(model, iret)
        if (iret .eq. 0) then
            call utmess('F', 'XFEM2_8', sk=model)
        endif
    endif
!
! - Read data
!
    if (iform .eq. 4) then
        call caliun(load, mesh, model)
    else
        call calico(load, mesh, model, nb_dim, iform,&
                    ligret)
    endif
!
! - New <LIGREL>
!
    lallv = cfdisl(sdcont_defi,'ALL_VERIF')
    if (iform .eq. 2) then
        if (.not.lallv) then
            call lgtlgr('V', ligret, ligrel)
            call detrsd('LIGRET', ligret)
            call copisd('LIGREL', 'G', ligrel, ligrch)
            call detrsd('LIGREL', ligrel)
        endif
    endif
!
! - Update loads <LIGREL>
!
    call jeexin(ligrch//'.LGRF', iret)
    if (iret .ne. 0) then
        call adalig(ligrch)
        call cormgi('G', ligrch)
        call jeecra(ligrch//'.LGRF', 'DOCU', cval='MECA')
        call initel(ligrch)
    endif
!
! - Check mesh orientation (normals)
!
    if ((iform.eq.1) .or. (iform.eq.2)) then
        call chveno(vale_type, mesh, model)
    endif
!
    call jedema()
!
end subroutine
