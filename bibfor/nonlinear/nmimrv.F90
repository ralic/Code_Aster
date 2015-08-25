subroutine nmimrv(ds_print, list_func_acti, iter_newt, line_search_coef, line_search_iter,&
                  eta)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
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
    type(NL_DS_Print), intent(inout) :: ds_print
    integer, intent(in) :: list_func_acti(*)
    integer, intent(in) :: iter_newt
    real(kind=8), intent(in) :: line_search_coef
    integer, intent(in) :: line_search_iter
    real(kind=8), intent(in) :: eta
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Set value of informations in convergence table (residuals are in nmimre)
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_print         : datastructure for printing parameters
! In  list_func_acti   : list of active functionnalities
! In  iter_newt        : index of current Newton iteration
! In  line_search_coef : coefficient for line search
! In  line_search_iter : number of iterations for line search
! In  eta              : coefficient for pilotage (continuation)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_line_search, l_pilo, l_deborst
!
! --------------------------------------------------------------------------------------------------
!
    l_deborst     = isfonc(list_func_acti,'DEBORST')
    l_pilo        = isfonc(list_func_acti,'PILOTAGE')
    l_line_search = isfonc(list_func_acti,'RECH_LINE')
!
! - Set values for line search
!
    if (l_line_search .and. (iter_newt.ne.0)) then
        call nmimci(ds_print, 'RELI_NBIT', line_search_iter, .true._1)
        call nmimcr(ds_print, 'RELI_COEF', line_search_coef, .true._1)
    endif
!
! - Set value for pilotage
!
    if (l_pilo) then
        call nmimcr(ds_print, 'PILO_COEF', eta, .true._1)
    endif
!
! - Set value for De Borst method (plane stress)
!
    if (l_deborst) then
        call nmimck(ds_print, 'DEBORST  ', 'DE BORST...', .true._1)
    endif
!
end subroutine
