subroutine nmelcm(phase    , mesh     , model    , mate     , ds_contact    ,&
                  disp_prev, vite_prev, acce_prev, vite_curr, disp_cumu_inst,&
                  matr_elem)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/cfdisl.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/nmelco_prep.h"
#include "asterfort/reajre.h"
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
    character(len=19), intent(out) :: matr_elem
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue/XFEM/LAC methods - Compute elementary matrix for contact
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
! Out matr_elem        : elementary matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer, parameter :: nbout = 3
    integer, parameter :: nbin  = 28
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
    character(len=1) :: base
    character(len=19) :: ligrel
    character(len=19) :: xcohes, ccohes
    character(len=16) :: option
    aster_logical :: l_cont_cont, l_cont_xfem, l_cont_xfem_gg, l_cont_lac
    aster_logical :: l_cont_pena, l_all_verif, l_xfem_czm
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
    l_cont_pena    = cfdisl(ds_contact%sdcont_defi,'EXIS_PENA')
    l_xfem_czm     = cfdisl(ds_contact%sdcont_defi,'EXIS_XFEM_CZM')
    l_all_verif    = cfdisl(ds_contact%sdcont_defi, 'ALL_VERIF')
    
    if (.not.l_all_verif) then
!
! ----- Print
!
        if (niv .ge. 2) then
            write (ifm,*) '<CONTACT> CALCUL MATRICES ELEMENTAIRES'
        endif
!
! ----- Init fields
!
        call inical(nbin, lpain, lchin, nbout, lpaout,&
                    lchout)
!
! ----- Prepare input fields
!
        call nmelco_prep(phase    , 'MATR'   ,&
                         mesh     , model    , mate     , ds_contact,&
                         disp_prev, vite_prev, acce_prev, vite_curr , disp_cumu_inst,&
                         nbin     , lpain    , lchin    ,&
                         option   , ccohes   , xcohes)
!
! ----- <LIGREL> for contact elements
!
        ligrel = ds_contact%ligrel_elem_cont
!
! ----- Preparation of elementary matrix
!
        call detrsd('MATR_ELEM', matr_elem)
        call memare('V', matr_elem, model, ' ', ' ',&
                    'RIGI_MECA')
!
! ----- Prepare output fields
!
        if (l_cont_pena) then
            lpaout(1) = 'PMATUNS'
            lchout(1) = matr_elem(1:15)//'.M01'
        else
            lpaout(1) = 'PMATUUR'
            lchout(1) = matr_elem(1:15)//'.M01'
            if (phase .eq. 'FROT') then
                lpaout(2) = 'PMATUNS'
                lchout(2) = matr_elem(1:15)//'.M02'
            else if (phase .eq. 'CONT' .and. l_xfem_czm) then
                lpaout(3) = 'PCOHESO'
                lchout(3) = ccohes
            endif
        endif
!
! ----- Computation
!
        call calcul('S', option, ligrel, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, base,&
                    'OUI')
!
! ----- Copy output fields
!
        call reajre(matr_elem, lchout(1), base)
        if ((phase .eq. 'FROT') .and. (l_xfem_czm.or.(.not.l_cont_pena))) then
            call reajre(matr_elem, lchout(2), base)
        endif
        if (l_xfem_czm .and. phase .eq. 'CONT') then
            call copisd('CHAMP_GD', 'V', lchout(3), xcohes)
        endif
    endif
!
    call jedema()
!
end subroutine
