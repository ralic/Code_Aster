subroutine nmdocr(model, carcri)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/carc_info.h"
#include "asterfort/carc_init.h"
#include "asterfort/carc_read.h"
#include "asterfort/carc_save.h"
#include "asterfort/dismoi.h"
#include "asterfort/nocart.h"
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
! aslint: disable=W1003
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in)   :: model
    character(len=24), intent(out) :: carcri
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Get parameters from COMPORTEMENT keyword and prepare CARCRI <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! Out carcri           : name of <CARTE> CARCRI
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: mesh
    integer :: nb_cmp
    type(NL_DS_ComporParaPrep) :: ds_compor_para
!
! --------------------------------------------------------------------------------------------------
!
    carcri = '&&NMDOCR.CARCRI'
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
! - Create carcri informations objects
!
    call carc_info(ds_compor_para)
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
    call carc_read(ds_compor_para, model)
!
! - Save and check informations in CARCRI <CARTE>
!
    call carc_save(model, mesh, carcri, nb_cmp, ds_compor_para)
!
! - Cleaning
!
    deallocate(ds_compor_para%v_para)
!
end subroutine
