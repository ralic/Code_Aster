subroutine pacou3(xold, fold, g, p, x,&
                  f, fvec, stpmax, check, tolx,&
                  vecr1, vecr2, typflu, vecr3, amor,&
                  masg, vecr4, vecr5, veci1, vg,&
                  indic, nbm, nmode, n)
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
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
!
! ARGUMENTS
! ---------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/pacou2.h"
    real(kind=8) :: f, fold, stpmax, tolx, amor(*)
    real(kind=8) :: g(*), p(*), x(*), xold(*), fvec(*), vg, masg(*)
    real(kind=8) :: vecr1(*), vecr2(*), vecr3(*), vecr4(*), vecr5(*)
    integer :: veci1(*)
    aster_logical :: check, first
    character(len=8) :: typflu
!
!
! ALF ASSURE UNE DECROISSANCE SUFFISANTE DE LA VALEUR DE LA FONCTION.
!-----------------------------------------------------------------------
    integer :: i, indic, n, nbm, nmode
    real(kind=8) :: a, alam, alam2, alamin, alf, b, disc
    real(kind=8) :: f2, fold2, rhs1, rhs2, slope, sum, temp
    real(kind=8) :: test, tmplam
!-----------------------------------------------------------------------
    parameter (alf=1.0d-4)
! ******************   DEBUT DU CODE EXECUTABLE   **********************
!
    check = .false.
!
    sum = 0.00d0
    do 11 i = 1, n
        sum = sum + p(i)*p(i)
 11 end do
    sum = sqrt(sum)
!
    if (sum .gt. stpmax) then
        do 12 i = 1, n
            p(i) = p(i)*stpmax/sum
 12     continue
    endif
!
    slope = 0.0d0
    do 13 i = 1, n
        slope = slope + g(i)*p(i)
 13 end do
!
    test = 0.0d0
    do 14 i = 1, n
        temp = abs(p(i))/max(abs(xold(i)),1.0d0)
        if (temp .gt. test) test = temp
 14 end do
!
    alamin = tolx/test
    alam = 1.0d0
    first = .true.
!
  1 continue
    do 15 i = 1, n
        x(i) = xold(i) + alam*p(i)
 15 end do
!
    f = pacou2(&
        x, fvec, vecr1, vecr2, typflu, vecr3, amor, masg, vecr4, vecr5, veci1, vg, indic, nbm,&
        nmode, n&
        )
    if (alam .lt. alamin) then
        do 16 i = 1, n
            x(i) = xold(i)
 16     continue
        check = .true.
        goto 9999
!
    else if (f.le.fold+alf*alam*slope) then
        goto 9999
!
    else
        if (first) then
            tmplam = -slope/ (2.0d0* (f-fold-slope))
            first = .false.
!
        else
            rhs1 = f - fold - alam*slope
            rhs2 = f2 - fold2 - alam2*slope
            a = (rhs1/alam**2-rhs2/alam**2)/ (alam-alam2)
            b = (-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/ (alam-alam2)
            if (abs(a) .le. 1.0d-30) then
                tmplam = -slope/ (2.0d0*b)
!
            else
                disc = b*b - 3.0d0*a*slope
                tmplam = (-b+sqrt(disc))/ (3.0d0*a)
            endif
!
            if (tmplam .gt. 0.5d0*alam) tmplam = 0.5d0*alam
        endif
!
    endif
!
    alam2 = alam
    f2 = f
    fold2 = fold
    alam = max(tmplam,0.1d0*alam)
    goto 1
!
9999 continue
end subroutine
