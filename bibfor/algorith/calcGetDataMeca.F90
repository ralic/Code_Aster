subroutine calcGetDataMeca(list_load      , model         , mate     , cara_elem,&
                           disp_prev      , disp_cumu_inst, vari_prev, sigm_prev,&
                           ds_constitutive, l_elem_nonl, nume_harm)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/nmlect.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/nonlinDSConstitutiveCreate.h"
#include "asterfort/nmdorc.h"
#include "asterfort/nonlinDSConstitutiveInit.h"
#include "asterfort/dismoi.h"
#include "asterfort/isOptionPossible.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(out) :: list_load
    character(len=24), intent(out) :: model
    character(len=24), intent(out) :: mate
    character(len=24), intent(out) :: cara_elem
    character(len=19), intent(out) :: disp_prev
    character(len=19), intent(out) :: disp_cumu_inst
    character(len=19), intent(out) :: vari_prev
    character(len=19), intent(out) :: sigm_prev
    type(NL_DS_Constitutive), intent(out) :: ds_constitutive
    aster_logical, intent(out) :: l_elem_nonl
    integer, intent(out) :: nume_harm
!
! --------------------------------------------------------------------------------------------------
!
! Command CALCUL
!
! Get data for mechanics
!
! --------------------------------------------------------------------------------------------------
!
! Out list_load        : name of datastructure for list of loads
! Out model            : name of model
! Out mate             : name of material characteristics (field)
! Out cara_elem        : name of elementary characteristics (field)
! Out disp_prev        : displacement at beginning of step
! Out disp_cumu_inst   : displacement increment from beginning of step
! Out vari_prev        : internal variables at beginning of step
! Out sigm_prev        : stress at beginning of step
! Out ds_constitutive  : datastructure for constitutive laws management
! Out l_elem_nonl      : .true. if all elements can compute non-linear options
! Out nume_harm        : Fourier harmonic number 
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
    aster_logical :: l_etat_init
    integer :: nocc
    character(len=19) :: ligrmo
!
! --------------------------------------------------------------------------------------------------
!
    list_load      = '&&OP0026.LISCHA'
    cara_elem      = '&&OP0026.CARELE'
    model          = ' '
    mate           = ' '
    vari_prev      = ' '
    sigm_prev      = ' '
    disp_prev      = ' '
    disp_cumu_inst = ' '
    l_elem_nonl    = .false._1
!
! - Get parameters from command file
!
    call nmlect(result, model, mate, cara_elem, list_load)
!
! - Can have internal variables ?
!
    call dismoi('NOM_LIGREL', model, 'MODELE', repk=ligrmo)
    call isOptionPossible(ligrmo, 'TOU_INI_ELGA', 'PVARI_R', l_some_ = l_elem_nonl)
! - Does option FULL_MECA exist
    if (l_elem_nonl) then
        call isOptionPossible(ligrmo, 'FULL_MECA', 'PDEPLPR', l_some_ = l_elem_nonl)
    endif
!
! - Get displacements
!
    call getvid(' ', 'DEPL', scal=disp_prev, nbret = nocc)
    if (nocc .eq. 0) then
        disp_prev = ' '
    endif
    call getvid(' ', 'INCR_DEPL', scal=disp_cumu_inst, nbret = nocc)
    if (nocc .eq. 0) then
        disp_cumu_inst = ' '
    endif
!
! - Get stresses
!
    call getvid(' ', 'SIGM', scal=sigm_prev, nbret=nocc)
    l_etat_init = nocc .ne. 0
    if (nocc .eq. 0) then
        sigm_prev = ' '
    endif
!
! - Get internal variables
!
    call getvid(' ', 'VARI', scal=vari_prev, nbret=nocc)
    if (nocc .eq. 0) then
        vari_prev = ' '
    endif
    if (vari_prev .ne. ' ' .and. .not. l_elem_nonl) then
        call utmess('I', 'CALCUL1_7')
    endif
!
! - Get Fourier Mode
!
    call getvis(' ', 'MODE_FOURIER', scal=nume_harm, nbret=nocc)
    if (nocc .eq. 0) nume_harm = 0
!
! - Prepare constitutive laws management datastructure
!
    if (l_elem_nonl) then
        call nonlinDSConstitutiveCreate(ds_constitutive)
        call nmdorc(model, mate, l_etat_init,&
                    ds_constitutive%compor, ds_constitutive%carcri, ds_constitutive%mult_comp,&
                    l_implex_ = .false._1)
        call nonlinDSConstitutiveInit(model, cara_elem, ds_constitutive)
    endif
!
end subroutine
