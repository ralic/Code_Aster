subroutine interf(mater, kfonc1, kfonc2, normf, x0,&
                  xrac)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/cdnfo2.h"
#include "asterfort/rcvale.h"
#include "asterfort/utmess.h"
    character(len=8) :: kfonc1, kfonc2, mater, k8b
    real(kind=8) :: normf, x0
    real(kind=8) :: xrac
    integer :: ier1, ier2, iter, itermx
    real(kind=8) :: fx1(1), fx2(1), fx, dfx1, dfx2, dfx, tole, err
    character(len=16) :: phenom
    integer :: icodr2(1)
!
    phenom = 'GLRC_DAMAGE'
    iter=0
    itermx=100
    xrac=x0
!
    k8b = 'X '
    call rcvale(mater, phenom, 1, k8b, [xrac],&
                1, kfonc1, fx1(1), icodr2(1), 1)
    call rcvale(mater, phenom, 1, k8b, [xrac],&
                1, kfonc2, fx2(1), icodr2(1), 1)
!
    fx=fx1(1)-fx2(1)
    err=abs(fx)
    tole=1.d-8*normf
!
    do 9, iter = 1,itermx
    if (err .le. tole) goto 10
    call cdnfo2(mater, kfonc1, xrac, 1, dfx1,&
                ier1)
    call cdnfo2(mater, kfonc2, xrac, 1, dfx2,&
                ier2)
    dfx=dfx1-dfx2
!
    if ((abs(dfx) .lt. 1.d-12) .or. (ier1 .gt. 0) .or. (ier2 .gt. 0)) then
        call utmess('F', 'ELEMENTS2_27')
    endif
!
    xrac=xrac-fx/dfx
    call rcvale(mater, phenom, 1, k8b, [xrac],&
                1, kfonc1, fx1(1), icodr2(1), 1)
    call rcvale(mater, phenom, 1, k8b, [xrac],&
                1, kfonc2, fx2(1), icodr2(1), 1)
    fx=fx1(1)-fx2(1)
    err=abs(fx)
 9  continue
    call utmess('F', 'ELEMENTS2_27')
!
10  continue
!
end subroutine
