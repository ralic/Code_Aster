subroutine nmdocr(model, carcri)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/carc_init.h"
#include "asterfort/carc_read.h"
#include "asterfort/carc_save.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
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
    character(len=8), intent(in) :: model
    character(len=24), intent(out) :: carcri
!
! --------------------------------------------------------------------------------------------------
!
! SAISIE ET STOCKAGE DES PARAMETRES LOCAUX DE COMPORTEMENT
!
! --------------------------------------------------------------------------------------------------
!
! In  model       : name of model
! Out carcri      : name of <CARTE> CARCRI
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_vale
    character(len=8) :: mesh
    integer :: ibid, iret
    integer :: nb_cmp, nbocc
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
    carcri    = '&&NMDOCR.CARCRI'
    list_vale = '&&NMDOCR.LIST_VALE'
!
! - Create CARCRI <CARTE>
!
    call carc_init(mesh, carcri, nb_cmp)
!
! - Read informations from command file
!
    call carc_read(list_vale, nbocc)
    if (nbocc.eq.0) goto 99
!
! - Save and check informations in CARCRI <CARTE>
!
    call carc_save(model, mesh, carcri, nb_cmp, list_vale)
!
    call jedetr(list_vale(1:19)//'.VALR')
    call jedetr(list_vale(1:19)//'.VALK')
!
 99 continue
!
    call jedema()
end subroutine

