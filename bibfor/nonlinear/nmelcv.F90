subroutine nmelcv(phase    , mesh     , model    , mate     , ds_contact    ,&
                  disp_prev, vite_prev, acce_prev, vite_curr, disp_cumu_inst,&
                  vect_elem)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/cfdisl.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmelco_prep.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
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
    character(len=4), intent(in) :: phase
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: disp_prev
    character(len=19), intent(in) :: vite_prev
    character(len=19), intent(in) :: acce_prev
    character(len=19), intent(in) :: vite_curr
    character(len=19), intent(in) :: disp_cumu_inst
    character(len=19), intent(out) :: vect_elem
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue/XFEM/LAC methods - Compute elementary vectors for contact
!
! --------------------------------------------------------------------------------------------------
!
! In  phase            : phase (contact or friction)
! In  mesh             : name of mesh
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  ds_contact       : datastructure for contact management
! In  disp_prev        : displacement at beginning of current time
! In  vite_prev        : speed at beginning of current time
! In  vite_curr        : speed at current time
! In  acce_prev        : acceleration at beginning of current time
! In  disp_cumu_inst   : displacement increment from beginning of current time
! Out vect_elem        : elementary vectors
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer, parameter :: nbout = 3
    integer, parameter :: nbin  = 30
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
    character(len=1) :: base
    character(len=19) :: ligrel
    character(len=16) :: option
    aster_logical :: l_cont_cont, l_cont_xfem, l_cont_xfem_gg, l_cont_lac
    aster_logical :: l_all_verif
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! - Initializations
!
    base = 'V'
!
! - Get contact parameters
!
    l_cont_cont    = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    l_cont_xfem    = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    !l_cont_lac     = cfdisl(ds_contact%sdcont_defi,'FORMUL_LAC')
    l_cont_lac     = .false._1
    l_cont_xfem_gg = cfdisl(ds_contact%sdcont_defi,'CONT_XFEM_GG')
    l_all_verif    = cfdisl(ds_contact%sdcont_defi, 'ALL_VERIF')
!
! --- TYPE DE CONTACT
!
    if (.not.l_all_verif) then
!
! ----- Print
!
        if (niv .ge. 2) then
            write (ifm,*) '<CONTACT> CALCUL DES SECDS MEMBRES ELEMENTAIRES'
        endif
!
! ----- Init fields
!
        call inical(nbin, lpain, lchin, nbout, lpaout,&
                    lchout)
!
! ----- Prepare input fields
!
        call nmelco_prep(phase    , 'VECT'   ,&
                         mesh     , model    , mate     , ds_contact,&
                         disp_prev, vite_prev, acce_prev, vite_curr , disp_cumu_inst,&
                         nbin     , lpain    , lchin    ,&
                         option)
!
! ----- <LIGREL> for contact elements
!
        ligrel = ds_contact%ligrel_elem_cont
!
! ----- Preparation of elementary vector
!
        call detrsd('VECT_ELEM', vect_elem)
        call memare('V', vect_elem, model, ' ', ' ',&
                    'CHAR_MECA')
!
! ----- Prepare output fields
!
        lpaout(1) = 'PVECTUR'
        lchout(1) = vect_elem
!
! ----- Computation
!
        call calcul('S', option, ligrel, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, base,&
                    'OUI')
!
! ----- Copy output fields
!
        call reajre(vect_elem, lchout(1), base)
    endif
!
    call jedema()
!
end subroutine
