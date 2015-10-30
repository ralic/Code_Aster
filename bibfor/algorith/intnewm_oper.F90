subroutine intnewm_oper(nbequ, par, mgen, kgen, agen, &
                        ktilda, ftild1, ftild2, ftild3)
    implicit none
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
! person_in_charge: hassan.berro at edf.fr
!
! intnewm_oper : Calculate (or update) the operators for a Newmark integration
!
!       --- ktilda, ftild1, ftild2, and ftild3 ---
!           kt (i,j) = a1*m(i,j) + k(i,j) + a2*c(i,j)
!           ft1(i,j) = a3*m(i,j) + a5*c(i,j)
!           ft2(i,j) = a1*m(i,j) + a2*c(i,j)
!           ft3(i,j) = a4*m(i,j) + a6*c(i,j) 
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/trlds.h"
!
!   -0.1- Input/output arguments
    integer     , intent(in)           :: nbequ
    real(kind=8)                       :: par(:)
    real(kind=8), pointer, intent(in)  :: mgen(:), kgen(:), agen(:)
    real(kind=8), pointer, intent(out) :: ktilda(:), ftild1(:), ftild2(:), ftild3(:)
!
!   -0.2- Local variables
    integer           :: i, j, iret

!   --------------------------------------------------------------------------
#define a(k) par(k)
#define norm_coef par(9) 
#define beta par(10) 
#define gamma par(11)
#define mdiag_r par(12)
#define kdiag_r par(13)
#define cdiag_r par(14)

#define mdiag (nint(mdiag_r).eq.1)
#define kdiag (nint(kdiag_r).eq.1)
#define cdiag (nint(cdiag_r).eq.1)

#define m(row,col) mgen((row-1)*nbequ+col) 
#define k(row,col) kgen((row-1)*nbequ+col) 
#define c(row,col) agen((row-1)*nbequ+col) 

#define kt(row,col) ktilda((row-1)*nbequ+col) 
#define ft1(row,col) ftild1((row-1)*nbequ+col) 
#define ft2(row,col) ftild2((row-1)*nbequ+col) 
#define ft3(row,col) ftild3((row-1)*nbequ+col) 

!       --- M is diagonal
    if (mdiag) then
        if (kdiag) then
            if (cdiag) then
                do i = 1, nbequ
                    ftild2(i) = a(1)*mgen(i) + a(2)*agen(i)*mgen(i)
                    ktilda(i) = ftild2(i) + kgen(i)
                    ftild1(i) = a(3)*mgen(i) + a(5)*agen(i)*mgen(i)
                    ftild3(i) = a(4)*mgen(i) + a(6)*agen(i)*mgen(i)                           
                end do
            else
                do i = 1, nbequ
                    ft2(i,i) = a(1)*mgen(i) + a(2)*c(i,i)
                    kt (i,i) = ft2(i,i) + kgen(i)
                    ft1(i,i) = a(3)*mgen(i) + a(5)*c(i,i)
                    ft3(i,i) = a(4)*mgen(i) + a(6)*c(i,i)
                    do j = i+1, nbequ
                        kt (i,j) = a(2)*c(i,j)
                        kt (j,i) = a(2)*c(j,i)
                        ft1(i,j) = a(5)*c(i,j)
                        ft1(j,i) = a(5)*c(j,i)
                        ft2(i,j) = kt(i,j)
                        ft2(j,i) = kt(j,i)
                        ft3(i,j) = a(6)*c(i,j)
                        ft3(j,i) = a(6)*c(j,i)
                    end do
                end do
            end if
        else
            if (cdiag) then
                do i = 1, nbequ
                    ftild2(i) = a(1)*mgen(i) + a(2)*agen(i)*mgen(i)
                    kt  (i,i) = ftild2(i) + k(i,i)
                    ftild1(i) = a(3)*mgen(i) + a(5)*agen(i)*mgen(i)
                    ftild3(i) = a(4)*mgen(i) + a(6)*agen(i)*mgen(i) 
                    do j = i+1, nbequ
                        kt (i,j) = k(i,j)
                        kt (j,i) = k(j,i)
                    end do
                end do
            else
                do i = 1, nbequ
                    ft2(i,i) = a(1)*mgen(i) + a(2)*c(i,i)
                    kt (i,i) = ft2(i,i) + k(i,i)
                    ft1(i,i) = a(3)*mgen(i) + a(5)*c(i,i)
                    ft3(i,i) = a(4)*mgen(i) + a(6)*c(i,i)
                    do j = i+1, nbequ
                        ft2(i,j) = a(2)*c(i,j)
                        ft2(j,i) = a(2)*c(j,i)
                        kt (i,j) = ft2(i,j) + k(i,j)
                        kt (j,i) = ft2(j,i) + k(j,i)
                        ft1(i,j) = a(5)*c(i,j)
                        ft1(j,i) = a(5)*c(j,i)
                        ft3(i,j) = a(6)*c(i,j)
                        ft3(j,i) = a(6)*c(j,i)
                    end do
                end do
            end if
        endif

!   --- M is not diagonal, K is supposed to be full as well
    else
        if (cdiag) then
            do i = 1, nbequ
                ft2(i,i) = a(1)*m(i,i) + a(2)*agen(i)*m(i,i)
                kt (i,i) = ft2(i,i) + k(i,i)
                ft1(i,i) = a(3)*m(i,i) + a(5)*agen(i)*m(i,i)
                ft3(i,i) = a(4)*m(i,i) + a(6)*agen(i)*m(i,i) 
                do j = i+1, nbequ
                    ft2(i,j) = a(1)*m(i,j)
                    ft2(j,i) = a(1)*m(j,i)
                    kt (i,j) = ft2(i,j) + k(i,j)
                    kt (j,i) = ft2(i,j) + k(j,i)
                    ft1(i,j) = a(3)*m(i,j)
                    ft1(j,i) = a(3)*m(j,i)
                    ft3(i,j) = a(4)*m(i,j)
                    ft3(j,i) = a(4)*m(j,i)
                end do
            end do
        else
            do i = 1, nbequ
                ft2(i,i) = a(1)*m(i,i) + a(2)*c(i,i)
                kt (i,i) = ft2(i,i) + k(i,i)
                ft1(i,i) = a(3)*m(i,i) + a(5)*c(i,i)
                ft3(i,i) = a(4)*m(i,i) + a(6)*c(i,i) 
                do j = i+1, nbequ
                    ft2(i,j) = a(1)*m(i,j) + a(2)*c(i,j)
                    ft2(j,i) = a(1)*m(j,i) + a(2)*c(j,i)
                    kt (i,j) = ft2(i,j) + k(i,j)
                    kt (j,i) = ft2(j,i) + k(j,i)
                    ft1(i,j) = a(3)*m(i,j) + a(5)*c(i,j)
                    ft1(j,i) = a(3)*m(j,i) + a(5)*c(j,i)
                    ft3(i,j) = a(4)*m(i,j) + a(6)*c(i,j)
                    ft3(j,i) = a(4)*m(j,i) + a(6)*c(j,i)
                end do
            end do
        endif
    endif

    norm_coef = -1.d25
    do i = 1, size(ktilda)
        if (abs(ktilda(i)).gt.norm_coef) norm_coef = abs(ktilda(i))
    end do

    ASSERT(norm_coef.gt.1.d-25)
    do i=1, size(ftild1)
        ftild1(i) = ftild1(i)/norm_coef
        ftild2(i) = ftild2(i)/norm_coef
        ftild3(i) = ftild3(i)/norm_coef
    end do

    do i=1, size(ktilda)
        ktilda(i) = ktilda(i)/norm_coef
    end do

!   --- Factorize ktilda if needed for later resolution for displacement
    if (size(ktilda).gt.nbequ) then
        call trlds(ktilda, nbequ, nbequ, iret)
    endif

end subroutine