subroutine intdevo_oper(nbequ, par, mgen, kgen, agen, &
                        dt, invm_c, op_h1, op_h2, invm_k)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: hassan.berro at edf.fr
!
! intnewm_oper : Calculate (or update) the operators for a Devogelaere integration
!
!
#include "jeveux.h"
#include "asterfort/rrlds.h"
#include "asterfort/trlds.h"
#include "blas/dcopy.h"

!
!   -0.1- Input/output arguments
    integer     , intent(in)           :: nbequ
    real(kind=8), intent(in)           :: par(:)
    real(kind=8), pointer, intent(in)  :: mgen(:), kgen(:), agen(:)
    real(kind=8), intent(in)           :: dt
    real(kind=8), pointer, intent(out) :: invm_c(:), op_h1(:), op_h2(:), invm_k(:)
!
!   -0.2- Local variables
    integer           :: i, j, iret
    real(kind=8)      :: invm
!   --------------------------------------------------------------------------
#define mdiag (nint(par(1)).eq.1)
#define kdiag (nint(par(2)).eq.1)
#define cdiag (nint(par(3)).eq.1)
#define tol par(4)
#define alpha par(5)
#define dtmin par(6)
#define dtmax par(7)
#define deltadt par(8)
#define nbnlsav par(9)
#define nbsavnl nint(par(9))

#define m(row,col) mgen((row-1)*nbequ+col) 
#define k(row,col) kgen((row-1)*nbequ+col) 
#define c(row,col) agen((row-1)*nbequ+col) 

#define im_c(row,col) invm_c((row-1)*nbequ+col)
#define im_k(row,col) invm_k((row-1)*nbequ+col)
#define h1(row,col) op_h1((row-1)*nbequ+col)
#define h2(row,col) op_h2((row-1)*nbequ+col)
#define k(row,col) kgen((row-1)*nbequ+col)
#define c(row,col) agen((row-1)*nbequ+col) 

!   --------------------------------------------------------------------------------
!   --- Calculation of the operators invm_c, h0, h1, and h2
    if (cdiag) then
!       --- C is diagonal, thus given is the modal damping agen = (M^-1)*C
        do i = 1, nbequ
            invm_c(i)= agen(i)
            op_h1(i) = 4.d0 + dt*invm_c(i)
            op_h2(i) = 6.d0 + dt*invm_c(i)
        end do            
    else
!       --- C is a full matrix
        if (mdiag) then
            do i = 1, nbequ
                invm = (1.d0/mgen(i))
                im_c(i,i)= invm*c(i,i)
                h1(i,i)  = 4.d0 + dt*im_c(i,i)
                h2(i,i)  = 6.d0 + dt*im_c(i,i)
                do j = i+1, nbequ
                    im_c(i,j)= invm*c(i,j)
                    h1(i,j)  =  dt*im_c(i,j)
                    h2(i,j)  =  dt*im_c(i,j)
                    im_c(j,i)= invm*c(j,i)
                    h1(j,i)  =  dt*im_c(j,i)
                    h2(j,i)  =  dt*im_c(j,i)
                end do
            end do            
        else
            call dcopy(nbequ*nbequ, agen, 1, invm_c, 1)
            call rrlds(mgen, nbequ, nbequ, invm_c, nbequ)
            do i = 1, nbequ
                h1(i,i)  = 4.d0 + dt*im_c(i,i)
                h2(i,i)  = 6.d0 + dt*im_c(i,i)
                do j = i+1, nbequ
                    h1(i,j)  =  dt*im_c(i,j)
                    h2(i,j)  =  dt*im_c(i,j)
                    h1(j,i)  =  dt*im_c(j,i)
                    h2(j,i)  =  dt*im_c(j,i)
                end do
            end do 
        end if
        call trlds(op_h1, nbequ, nbequ, iret)
        call trlds(op_h2, nbequ, nbequ, iret)
    end if

!   --------------------------------------------------------------------------------
!   --- Calculation of the operator invm_k
    if (mdiag) then
        if (kdiag) then
            do i = 1, nbequ
                invm_k(i) = kgen(i)/mgen(i)
            end do 
        else
            do i = 1, nbequ
                invm = 1.d0/mgen(i)
                im_k(i,i) = invm*k(i,i)
                do j = i+1, nbequ
                    im_k(i,j) = invm*k(i,j)
                    im_k(j,i) = invm*k(j,i)
                end do
            end do 
        end if
    else
        do i = 1, nbequ
            if (kdiag) then
                im_k(i,i) = kgen(i)
                do j = i+1, nbequ
                    im_k(i,j) = 0.d0
                    im_k(j,i) = 0.d0
                end do
            else
                im_k(i,i) = k(i,i)
                do j = 1, nbequ
                    im_k(i,j) = k(i,j)
                    im_k(j,i) = k(j,i)
                end do
            end if
        end do 
        call rrlds(mgen, nbequ, nbequ, invm_k, nbequ)
    end if

end subroutine