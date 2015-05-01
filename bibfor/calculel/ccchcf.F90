subroutine ccchcf(name_form, nb_val_in, val_in, cmp_in, nb_cmp_out,&
                  val_out, ichk)
!
    implicit none
!
#include "asterfort/fointe.h"
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
! person_in_charge: mathieu.courtois at edf.fr
!
    integer, intent(in) :: nb_cmp_out
    character(len=8), intent(in) :: name_form(nb_cmp_out)
    integer, intent(in) :: nb_val_in
    real(kind=8), intent(in) :: val_in(nb_val_in)
    character(len=8), intent(in) :: cmp_in(nb_val_in)
    real(kind=8), intent(out) :: val_out(nb_cmp_out)
    integer, intent(out) :: ichk
!
! --------------------------------------------------------------------------------------------------
!
! CALC_CHAMP - CHAM_UTIL
!
! Compute FORMULE
!
! --------------------------------------------------------------------------------------------------
!
! In  name_form  : names of formulas
! In  nb_val_in  : number of input values
! In  val_in     : input values
! In  cmp_in     : name of input components
! In  nb_cmp_out : number of output values
! Out val_out    : output values
! Out ichk       : 0 if OK
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i
    character(len=1) :: codmes
    character(len=8) :: nomf
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initializations
!
    ichk = 1
!     METTRE 'A' POUR DEBUG, ' ' EN PROD
    codmes = 'A'
    if (nb_val_in .eq. 0) goto 999
!
! - Evaluate formulas
!
    do i = 1, nb_cmp_out
        nomf = name_form(i)
        call fointe(codmes, nomf, nb_val_in, cmp_in, val_in,&
                    val_out(i), ichk)
        if (ichk .ne. 0) goto 999
    end do
!
999 continue
!
end subroutine
