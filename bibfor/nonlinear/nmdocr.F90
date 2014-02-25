subroutine nmdocr(model, carcri)
!
    implicit none
!
#include "asterfort/as_deallocate.h"
#include "asterfort/carc_info.h"
#include "asterfort/carc_init.h"
#include "asterfort/carc_read.h"
#include "asterfort/carc_save.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/nocart.h"
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
    character(len=8) :: mesh
    integer :: nb_cmp, nbocc_carcri
    character(len=16), pointer :: p_info_carc_valk(:) => null()
    real(kind=8)     , pointer :: p_info_carc_valr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
    carcri = '&&NMDOCR.CARCRI'
!
! - Create carcri informations objects
!
    call carc_info(p_info_carc_valk, p_info_carc_valr, nbocc_carcri)
!
! - Create CARCRI <CARTE>
!
    call carc_init(mesh, carcri, nb_cmp)
!
! - Default CARCRI <CARTE> on all mesh
!
    call nocart(carcri, 1, nb_cmp)
!
! - Read informations from command file
!
    call carc_read(p_info_carc_valk, p_info_carc_valr)
!
! - Save and check informations in CARCRI <CARTE>
!
    call carc_save(model           , mesh            , carcri, nb_cmp, &
                   p_info_carc_valk, p_info_carc_valr)
!
    AS_DEALLOCATE(vk16 = p_info_carc_valk)
    AS_DEALLOCATE(vr   = p_info_carc_valr)
!
end subroutine
