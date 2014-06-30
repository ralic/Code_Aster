subroutine mm_cycl_print(sd_impr, sd_stat)
!
    implicit     none
!
#include "asterfort/nmrvai.h"
#include "asterfort/nmimci.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sd_stat
    character(len=24), intent(in) :: sd_impr
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Informations printing in convergence table
!
! --------------------------------------------------------------------------------------------------
!
!
! In  sd_stat      : data structure for non-linear stats
! In  sd_impr      : data structure for non-linear printing
!
! --------------------------------------------------------------------------------------------------
!
    integer :: cycl_nb(4),cycl_nb_tot
!
! --------------------------------------------------------------------------------------------------
!
    call nmrvai(sd_stat, 'CTCC_CYCL_1', 'N', cycl_nb(1))
    call nmrvai(sd_stat, 'CTCC_CYCL_2', 'N', cycl_nb(2))
    call nmrvai(sd_stat, 'CTCC_CYCL_3', 'N', cycl_nb(3))
    call nmrvai(sd_stat, 'CTCC_CYCL_4', 'N', cycl_nb(4))
    cycl_nb_tot = cycl_nb(1) + cycl_nb(2) + cycl_nb(3) + cycl_nb(4)
    call nmimci(sd_impr, 'CTCC_CYCL', cycl_nb_tot, .true._1)

end subroutine
