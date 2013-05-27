function pacou2(x, fvec, vecr1, vecr2, typflu,&
                vecr3, amor, masg, vecr4, vecr5,&
                veci1, vg, indic, nbm, nmode,&
                n)
    implicit none
! ---------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
    include 'asterfort/pacouf.h'
    integer :: n
    real(kind=8) :: x(*), fvec(*), amor(*), vg, masg(*)
    real(kind=8) :: vecr1(*), vecr2(*), vecr3(*), vecr4(*), vecr5(*)
    integer :: veci1(*)
    character(len=8) :: typflu
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, indic, nbm, nmode
    real(kind=8) :: pacou2, sum
!-----------------------------------------------------------------------
    call pacouf(x, fvec, vecr1, vecr2, typflu,&
                vecr3, amor, masg, vecr4, vecr5,&
                veci1, vg, indic, nbm, nmode)
    sum = 0.0d0
    do 11 i = 1, n
        sum = sum + fvec(i)*fvec(i)
11  end do
    pacou2 = 0.5d0*sum
!
end function
