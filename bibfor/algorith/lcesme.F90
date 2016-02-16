subroutine lcesme(tns,eig,fct,prec,val,drv)
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
    implicit none
#include "asterfort/lcdpdt.h"
#include "asterfort/assert.h"

    interface
    subroutine fct(x,v,d)
        real(kind=8),intent(in) :: x
        real(kind=8),intent(out):: v,d
    end subroutine fct
    end interface

    real(kind=8),intent(in) :: tns(6),eig(3),prec
    real(kind=8),intent(out):: val(6),drv(6,6)
! --------------------------------------------------------------------------------------------------
!   Value and derivative (tangent matrix) of a tensorial function of the eigenvalues
!     Ref. Carlson D.E. and Hoger A. (1984)
! --------------------------------------------------------------------------------------------------
! tns : symmetric argument tensor in symmetric format
! eig : eigenvalues of tensor tns in decreasing order
! fct : scalar function of the eigenvalues. Return a vector(/value, derivative/)
! prec: precision for distinguishing close eigenvalues
! val : value of the tensorial function
! drv : derivative of the tensorial function
! --------------------------------------------------------------------------------------------------
    integer     :: nbeig,i,j,k
    real(kind=8):: x(3),f(3),d(3),g(3),s(3),c(6)
    real(kind=8):: tns2(6),p(3)
    real(kind=8),dimension(6,6):: x2tx2,x2tx,x2t,xtx,xt
! --------------------------------------------------------------------------------------------------
    real(kind=8),dimension(6),parameter  :: kr=(/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
    real(kind=8),dimension(6,6)          :: id 
    integer,dimension(5),parameter       :: perm=(/1,2,3,1,2/)
! --------------------------------------------------------------------------------------------------

    ASSERT(size(tns).eq.6)
    id=0.d0
    forall (i=1:6) id(i,i)=1.d0



! Number of distinct eigenvalues and initialisation

    if (eig(1)-eig(3) .le. 2*prec) then
        nbeig = 1
        x(1)  = 0.5d0*(eig(1)+eig(3))
    else if (eig(1)-eig(2) .le. prec) then
        nbeig = 2
        x(1) = eig(3)
        x(2) = 0.5d0*(eig(1)+eig(2))
    else if (eig(2)-eig(3) .le. prec) then
        nbeig = 2
        x(1) = eig(1)
        x(2) = 0.5d0*(eig(2)+eig(3))
    else
        nbeig = 3
        x = eig
    end if



! Tensoriel value and derivative

    select case (nbeig)

    case (1)
!       Triple eigenvalue

        call fct(x(1),f(1),d(1))
        val = f(1)*kr
        drv = d(1)*id

    case (2)
!       Double eigenvalue

        do i = 1,2
            call fct(x(i),f(i),d(i))
        end do

        c(1) = 0.5d0 * ((x(1)-x(2))*(d(1)+d(2))-2*(f(1)-f(2)))
        c(2) = -(x(1)-x(2))*(x(2)*d(1)+x(1)*d(2)) + (x(1)+x(2))*(f(1)-f(2))
        c(3) = (x(1)-x(2))*(d(1)*x(2)**2 + d(2)*x(1)**2) - 2*x(1)*x(2)*(f(1)-f(2))
        xt  = lcdpdt(tns,kr)
        xtx = lcdpdt(tns,tns)
        val = ( (f(1)-f(2))*tns - (x(2)*f(1)-x(1)*f(2))*kr ) / (x(1)-x(2))
        drv = (c(1)*xtx+c(2)*xt+c(3)*id)/(x(1)-x(2))**3


    case (3)
!       distinct eigenvalues

        do i = 1,3
            j = perm(i+1)
            k = perm(i+2)
            call fct(x(i),f(i),d(i))
            g(i) = f(i) / ((x(i)-x(j)) * (x(i)-x(k)))
        end do

        c = 0.d0
        p = 0.d0
        do i = 1,3
            j = perm(i+1)
            k = perm(i+2)
            s(i) = (d(i)-(x(i)-x(j))*(g(i)+g(k))-(x(i)-x(k))*(g(i)+g(j))) &
                 / ((x(i)-x(j))**2 * (x(i)-x(k))**2)

            c(1) = c(1) + 0.5d0*s(i)
            c(2) = c(2) - s(i)*(x(j)+x(k))
            c(3) = c(3) + s(i)*x(j)*x(k)
            c(4) = c(4) + 0.5d0*s(i)*(x(j)+x(k))**2
            c(5) = c(5) + g(i) - s(i)*x(j)*x(k)*(x(j)+x(k))
            c(6) = c(6) + s(i)*(x(j)*x(k))**2 - g(i)*(x(j)+x(k))

            p(1) = p(1) + g(i)
            p(2) = p(2) - (x(j)+x(k))*g(i)
            p(3) = p(3) + x(j)*x(k)*g(i)
        end do

        xt    = lcdpdt(tns,kr)
        xtx   = lcdpdt(tns,tns)
        tns2  = matmul(0.5d0*xtx,kr)
        x2t   = lcdpdt(tns2,kr)
        x2tx  = lcdpdt(tns2,tns)
        x2tx2 = lcdpdt(tns2,tns2)

        val = p(1)*tns2 + p(2)*tns + p(3)*kr
        drv = c(1)*x2tx2 + c(2)*x2tx + c(3)*x2t + c(4)*xtx + c(5)*xt + c(6)*id

    case default
        ASSERT(.false.)

    end select

        
end subroutine lcesme
