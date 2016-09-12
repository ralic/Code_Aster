subroutine op0030()
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/caform.h"
#include "asterfort/cagene.h"
#include "asterfort/calico.h"
#include "asterfort/caliun.h"
#include "asterfort/cfdisl.h"
#include "asterfort/chveno.h"
#include "asterfort/copisd.h"
#include "asterfort/cormgi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/infmaj.h"
#include "asterfort/initel.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/lgtlgr.h"
#include "asterfort/check_model.h"
#include "asterfort/wkvect.h"
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
!
! --------------------------------------------------------------------------------------------------
!
! COMMANDE:  DEFI_CONTACT
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, nb_dim
    character(len=4) :: vale_type
    character(len=8) :: mesh, model, sdcont
    character(len=16) :: k16dummy, command
    character(len=19) :: ligrmo, ligret, ligrel, ligrch
    integer :: cont_form
    aster_logical :: lallv
    character(len=24) :: sdcont_defi
    character(len=8), pointer :: p_sdcont_type(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
! - Initializations
!
    cont_form = 0
    ligret    = '&&OP0030.LIGRET'
    ligrel    = '&&OP0030.LIGREL'
    vale_type = 'REEL'
!
! - Which command ?
!
    call getres(sdcont, k16dummy, command)
!
! - Main datastructure fon contact definition
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
!
! - Mesh, Ligrel for model, dimension of model
!
    call cagene(sdcont, command, ligrmo, mesh, nb_dim)
    model = ligrmo(1:8)
!
! - Load type
!
    ligrch = sdcont//'.CHME.LIGRE'
    call wkvect(sdcont//'.TYPE', 'G V K8', 1, vk8 = p_sdcont_type)
    p_sdcont_type(1) = 'MECA_RE'
!
! - Get contact formulation
!
    call caform(cont_form)
!
! - Check model/mesh
!
    call check_model(mesh, model, cont_form) 
!
! - Read and create datastructures 
!
    if (cont_form .eq. 4) then
        call caliun(sdcont, mesh, model)
    else
        call calico(sdcont, mesh, model, nb_dim, cont_form,&
                    ligret)
    endif
!
! - New <LIGREL>
!
    lallv = cfdisl(sdcont_defi,'ALL_VERIF')
    if (cont_form .eq. 2 .or. cont_form .eq. 5) then
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
    if ((cont_form.eq.1) .or. (cont_form.eq.2)) then
        call chveno(vale_type, mesh, model)
    endif
!
    call jedema()
!
end subroutine
