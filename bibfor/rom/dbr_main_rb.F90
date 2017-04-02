subroutine dbr_main_rb(ds_para_rb, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/preres.h"
#include "asterfort/vtcrem.h"
#include "asterfort/copisd.h"
#include "asterfort/resoud.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/dbr_calcrb_prep.h"
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
    type(ROM_DS_ParaDBR_RB), intent(in) :: ds_para_rb
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Main subroutine to compute empiric modes - For RB methods
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para_rb       : datastructure for parameters (RB)
! IO  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: icode, ibid, nb_equa, iret, i_equa, jv_para
    character(len=19) :: maprec, solver, vect_zero, syst_solu, crgc, syst_2mbr, syst_matr
    complex(kind=8), pointer :: v_soluti(:) => null()
    complex(kind=8) :: c16bid = (0.d0, 0.d0)
    character(len=8) :: base, model
    character(len=24) :: field_type, field_save
    complex(kind=8), pointer :: v_field_save(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    vect_zero      = ds_para_rb%vect_zero
    maprec         = '&&OP0053.MAPREC'
    syst_solu      = ds_para_rb%syst_solu
    crgc           = '&&OP0053.GCPC'
    solver         = ds_para_rb%solver
    syst_matr      = ds_para_rb%syst_matr
    syst_2mbr      = ds_para_rb%syst_2mbr
    nb_equa        = ds_empi%nb_equa
    base           = ds_empi%base
    field_type     = ds_empi%field_type
    model          = ds_empi%model
!
! - Prepare matrix and second member
!
    call dbr_calcrb_prep(ds_para_rb)
!
! - Factor matrix
!
    call preres(solver, 'V', icode, maprec, syst_matr,&
                ibid, -9999)
    if ((icode .eq. 1) .or. (icode .eq. 2)) then
        call utmess('I', 'DYNAMIQUE_14', sr=0.d0)
    endif
!
! - Solve system
!
    call resoud(syst_matr, maprec    , solver, vect_zero, 0       ,&
                syst_2mbr, syst_solu , 'V'   , [0.d0]   , [c16bid],&
                crgc     , .true._1  , 0     , iret)
    call jeveuo(syst_solu(1:19)//'.VALE', 'L', vc=v_soluti)
!
! - Create output datastructure
!
    call rscrsd('G', base, 'MODE_EMPI', 1)
!
! - Save solution
!
    call rsexch(' ', base, field_type, 1, field_save, iret)
    ASSERT(iret .eq. 100 .or. iret .eq. 0)
    if (iret .eq. 100) then
        call copisd('CHAMP_GD', 'G', syst_2mbr, field_save)
    endif
    call jeveuo(field_save(1:19)//'.VALE', 'E', vc = v_field_save)
    do i_equa = 1, nb_equa
        v_field_save(i_equa) = v_soluti(i_equa)
    end do
    call rsnoch(base, field_type, 1)
    call rsadpa(base, 'E', 1, 'FREQ', 1, 0, sjv=jv_para)
    zr(jv_para)  = 1.d0
    call rsadpa(base, 'E', 1, 'MODELE', 1, 0, sjv=jv_para)
    zk8(jv_para) = model
    call rsadpa(base, 'E', 1, 'NOM_CHAM', 1, 0, sjv=jv_para)
    zk24(jv_para) = field_type
    call rsadpa(base, 'E', 1, 'NUME_PLAN', 1, 0, sjv=jv_para)
    zi(jv_para) = 0
    call rsadpa(base, 'E', 1, 'NUME_MODE', 1, 0, sjv=jv_para)
    zi(jv_para) = 1
!
end subroutine
