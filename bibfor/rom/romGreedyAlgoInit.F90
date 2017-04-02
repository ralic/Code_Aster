subroutine romGreedyAlgoInit(syst_type , nb_mode   , nb_vari_coef,&
                             vect_refe , ds_para_rb)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/infniv.h"
#include "asterfort/copisd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=1), intent(in) :: syst_type
    integer, intent(in) :: nb_mode
    integer, intent(in) :: nb_vari_coef
    character(len=19), intent(in) :: vect_refe
    type(ROM_DS_ParaDBR_RB), intent(inout) :: ds_para_rb
!
! --------------------------------------------------------------------------------------------------
!
! Greedy algorithm
!
! Init algorithm
!
! --------------------------------------------------------------------------------------------------
!
! In  syst_type        : global type of system (real or complex)
! In  nb_mode          : number of empirical modes
! In  nb_vari_coef     : number of coefficients to vary
! In  vect_refe        : reference vector to create residual vector
! IO  ds_para_rb       : datastructure for parameters (RB)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: jv_dummy
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_42')
    endif
!
    ds_para_rb%coef_redu  = '&&OP0053.COEF_REDU'
    ds_para_rb%resi_type  = syst_type
    ds_para_rb%resi_vect  = '&&OP0053.RESI_VECT'
    call wkvect(ds_para_rb%coef_redu, 'V V '//syst_type, nb_mode * nb_vari_coef, jv_dummy)
    call copisd('CHAMP_GD', 'V', vect_refe, ds_para_rb%resi_vect)
    call copisd('CHAMP_GD', 'V', vect_refe, ds_para_rb%vect_2mbr_init)
    AS_ALLOCATE(vr = ds_para_rb%resi_norm, size = nb_vari_coef+1)
!
end subroutine
