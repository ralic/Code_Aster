subroutine mm_cycl_print(ds_print, ds_measure)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/nmrvai.h"
#include "asterfort/nmimci.h"
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
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Informations printing in convergence table
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_measure       : datastructure for measure and statistics management
! IO  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: cycl_nb(4),cycl_nb_tot
!
! --------------------------------------------------------------------------------------------------
!
    call nmrvai(ds_measure, 'Cont_Cycl1', phasis = 'N', output_count = cycl_nb(1))
    call nmrvai(ds_measure, 'Cont_Cycl2', phasis = 'N', output_count = cycl_nb(2))
    call nmrvai(ds_measure, 'Cont_Cycl3', phasis = 'N', output_count = cycl_nb(3))
    call nmrvai(ds_measure, 'Cont_Cycl4', phasis = 'N', output_count = cycl_nb(4))
    cycl_nb_tot = cycl_nb(1) + cycl_nb(2) + cycl_nb(3) + cycl_nb(4)
    call nmimci(ds_print, 'CTCC_CYCL', cycl_nb_tot, .true._1)

end subroutine
