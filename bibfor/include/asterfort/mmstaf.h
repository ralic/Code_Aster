!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! aslint: disable=W1504
interface
    subroutine mmstaf(noma, ndim, chdepd, coef_frot, lpenaf,&
                      nummae, aliase, nne, nummam, ksipc1,&
                      ksipc2, ksipr1, ksipr2, mult_lagr_f1, mult_lagr_f2,&
                      tang_1, tang_2, norm, indi_frot_eval ,pres_frot,&
                      dist_frot)
        character(len=8), intent(in) :: noma, aliase
        integer, intent(in) :: ndim, nne
        real(kind=8), intent(in) :: ksipc1, ksipc2
        real(kind=8), intent(in) :: ksipr1, ksipr2
        integer, intent(in) :: nummae, nummam
        character(len=19), intent(in) :: chdepd
        real(kind=8), intent(in) :: tang_1(3), tang_2(3), norm(3)
        real(kind=8), intent(in) :: coef_frot
        logical, intent(in) :: lpenaf
        real(kind=8), intent(in) :: mult_lagr_f1(9), mult_lagr_f2(9)
        integer, intent(out) :: indi_frot_eval
        real(kind=8), intent(out) :: pres_frot(3), dist_frot(3)
    end subroutine mmstaf
end interface
