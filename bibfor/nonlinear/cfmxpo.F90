subroutine cfmxpo(mesh  , model_   , ds_contact, nume_inst  , sddisc, &
                  sdstat, hval_algo, hval_incr , hval_veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdeco.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmxre.h"
#include "asterfort/cfverl.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmdeco.h"
#include "asterfort/xmdeco.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24), intent(in) :: sdstat
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: hval_algo(*)
    character(len=19), intent(in) :: hval_veasse(*) 
    character(len=19), intent(in) :: hval_incr(*)
    character(len=*), intent(in) :: model_
    integer, intent(in) :: nume_inst
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (POST_TRAITEMENT)
!
! POST_TRAITEMENT DU CONTACT (TOUTES METHODES)
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  model            : name of model
! In  mesh             : name of mesh
! In  nume_inst        : index of current step time
! In  sddisc           : datastructure for time discretization
! In  sdstat           : datastructure for statistics
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  hval_veasse      : hat-variable for vectors (node fields)
!
! ----------------------------------------------------------------------
!
    aster_logical :: l_cont_disc, l_cont_cont, l_all_verif, l_cont_xfem
    character(len=8) :: model
!
! ----------------------------------------------------------------------
!
    model = model_
!
! --- TYPE DE CONTACT
!
    l_cont_cont = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    l_all_verif = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
    l_cont_xfem = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
!
! --- GESTION DE LA  DECOUPE
!
    if (.not.l_all_verif) then
        if (l_cont_disc) then
            call cfdeco(ds_contact)
        else if (l_cont_cont) then
            call mmdeco(ds_contact)
        else if (l_cont_xfem) then
            call xmdeco(ds_contact)
        endif
    endif
!
! --- VERIFICATION FACETTISATION
!
    if (l_cont_disc .or. l_cont_cont) then
        call cfverl(ds_contact)
    endif
!
! --- REMPLISSAGE DU CHAM_NO CONT_NOEU ET PERCUSSION
!
    call cfmxre(mesh, model, sdstat, ds_contact,nume_inst,&
                sddisc, hval_algo, hval_incr, hval_veasse)
!
end subroutine
