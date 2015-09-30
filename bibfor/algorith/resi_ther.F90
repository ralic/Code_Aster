subroutine resi_ther(model    , mate     , time     , compor    , temp_prev,&
                     temp_iter, hydr_prev, hydr_curr, dry_prev  , dry_curr ,&
                     varc_curr, vect_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/gcnco2.h"
#include "asterfort/megeom.h"
#include "asterfort/reajre.h"
#include "asterfort/inical.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: time
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: temp_prev
    character(len=24), intent(in) :: temp_iter
    character(len=24), intent(in) :: hydr_prev   
    character(len=24), intent(in) :: hydr_curr
    character(len=24), intent(in) :: dry_prev   
    character(len=24), intent(in) :: dry_curr
    character(len=24), intent(in) :: compor
    character(len=19), intent(in) :: varc_curr    
    character(len=24), intent(inout) :: vect_elem
!
! --------------------------------------------------------------------------------------------------
!
! Thermic
! 
! Residuals from non-linear laws 
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  time             : time (<CARTE>)
! In  temp_prev        : previous temperature
! In  temp_iter        : temperature field at current Newton iteration
! In  hydr_prev        : previous hydratation
! In  hydr_curr        : current hydratation
! In  dry_prev         : previous drying
! In  dry_curr         : current drying
! In  compor           : name of comportment definition (field)
! In  varc_curr        : command variable for current time
! IO  vect_elem        : name of vect_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbin, nbout
    parameter (nbin = 10, nbout = 2)
    character(len=8) :: lpain(nbin), lpaout(nbout)
    character(len=19) :: lchin(nbin), lchout(nbout)
!
    character(len=1) :: base, stop_calc
    character(len=8) :: newnom
    character(len=16) :: option
    character(len=19) :: resu_elem
    character(len=24) :: ligrel_model
    character(len=24) :: chgeom
    integer :: ibid
!
! --------------------------------------------------------------------------------------------------
!
    resu_elem    = '&&HYDRES.0000000'
    stop_calc    = 'S'
    base         = 'V'
    option       = 'RESI_RIGI_MASS'
    ligrel_model = model(1:8)//'.MODELE'
!
! - Init fields
!
    call inical(nbin  , lpain, lchin, nbout, lpaout,&
                lchout)    
!
! - Geometry field
!
    call megeom(model, chgeom)
!
! - Input fields
!
    lpain(1)  = 'PGEOMER'
    lchin(1)  = chgeom(1:19)
    lpain(2)  = 'PMATERC'
    lchin(2)  = mate(1:19)
    lpain(3)  = 'PTEMPSR'
    lchin(3)  = time(1:19)
    lpain(4)  = 'PTEMPEI'
    lchin(4)  = temp_iter(1:19)
    lpain(5)  = 'PHYDRPM'
    lchin(5)  = hydr_prev(1:19)
    lpain(6)  = 'PCOMPOR'
    lchin(6)  = compor(1:19)
    lpain(7)  = 'PTEMPER'
    lchin(7)  = temp_prev(1:19)
    lpain(8)  = 'PTMPCHI'
    lchin(8)  = dry_prev(1:19)
    lpain(9)  = 'PTMPCHF'
    lchin(9)  = dry_curr(1:19)
    lpain(10) = 'PVARCPR'
    lchin(10) = varc_curr(1:19)
!
! - Output fields
!
    lpaout(1) = 'PRESIDU'
    lchout(1) = '&&HYDRES.???????'
    lpaout(2) = 'PHYDRPP'
    lchout(2) = hydr_curr(1:19)
!
! - Generate new RESU_ELEM name
!
    newnom = resu_elem(10:16)
    call gcnco2(newnom)
    lchout(1) (10:16) = newnom(2:8)
    call corich('E', lchout(1), -1, ibid)
!
! - Number of fields
!
    call calcul(stop_calc, option, ligrel_model, nbin  , lchin,&
                lpain    , nbout , lchout      , lpaout, base ,&
                'OUI')
!
! - Add RESU_ELEM in vect_elem
!
    call reajre(vect_elem, lchout(1), base)
!
end subroutine
