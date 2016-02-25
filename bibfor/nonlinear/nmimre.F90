subroutine nmimre(ds_conv, ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
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
    type(NL_DS_Conv), intent(in) :: ds_conv
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Set value of residuals informations in convergence table
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_conv          : datastructure for convergence management
! IO  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_resi, nb_resi
    real(kind=8) :: vale_calc
    character(len=16) :: locus_calc
    character(len=24) :: col_name, col_name_locus
!
! --------------------------------------------------------------------------------------------------
!
    nb_resi = ds_conv%nb_resi
!
! - Loop on residuals
!
    do i_resi = 1, nb_resi
        vale_calc       = ds_conv%list_resi(i_resi)%vale_calc
        locus_calc      = ds_conv%list_resi(i_resi)%locus_calc
        col_name        = ds_conv%list_resi(i_resi)%col_name
        col_name_locus  = ds_conv%list_resi(i_resi)%col_name_locus
        call nmimcr(ds_print, col_name      , vale_calc , l_affe = .true._1)
        call nmimck(ds_print, col_name_locus, locus_calc, l_affe = .true._1)
    end do
!
end subroutine
