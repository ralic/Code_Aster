subroutine dbr_calcrb_solv(ds_para_rb)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(ROM_DS_ParaDBR_RB), intent(in) :: ds_para_rb
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Solve linear system
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para_rb       : datastructure for parameters (RB)
!
! --------------------------------------------------------------------------------------------------
!
    complex(kind=8), parameter :: c16bid =(0.d0,0.d0)
    integer :: icode, ibid
    character(len=19) :: maprec, solver, crgc, vect_zero
    character(len=19)  :: syst_matr, syst_2mbr, syst_solu
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
!
! - Factor matrix
!
    call preres(solver, 'V', icode, maprec, syst_matr,&
                ibid, -9999)
    if ((icode .eq. 1) .or. (icode .eq. 2)) then
        call utmess('I', 'ROM2_18')
    endif
!
! - Solve system
!
    call resoud(syst_matr, maprec    , solver, vect_zero, 0       ,&
                syst_2mbr, syst_solu , 'V'   , [0.d0]   , [c16bid],&
                crgc     , .true._1  , 0     , icode)
!
end subroutine
