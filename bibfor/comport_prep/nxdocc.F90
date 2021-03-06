subroutine nxdocc(model, compor)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/comp_init.h"
#include "asterfort/comp_ther_read.h"
#include "asterfort/comp_ther_save.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
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
!
    character(len=8), intent(in) :: model
    character(len=19), intent(out) :: compor
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (thermics)
!
! Get parameters from COMPORTEMENT keyword and prepare COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  model       : name of model
! In  compor      : name of <CARTE> COMPOR
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cmp
    character(len=8) :: mesh
    character(len=19) :: list_vale
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    compor = '&&NXDOCC.COMPOR'
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
    list_vale = '&&LIST_VALE'
!
! - Read informations from command file
!
    call comp_ther_read(list_vale)
!
! - Create COMPOR <CARTE>
!
    call comp_init(mesh, compor, 'V', nb_cmp)
!
! - Save informations in COMPOR <CARTE>
!
    call comp_ther_save(mesh, compor, nb_cmp, list_vale)
!
! - Clean it
!
    call jedetr(list_vale)
!
    call jedema()
!
end subroutine
