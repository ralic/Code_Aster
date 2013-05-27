subroutine usvect(coef, alphad, alpham, alphaf, prof,&
                  ndim, vect)
    implicit   none
    include 'asterfort/assert.h'
    integer :: ndim
    real(kind=8) :: coef, vect(*), alphad, alpham, alphaf, prof
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
    integer :: i, ind
    real(kind=8) :: thet0, theta, ai1, ai2, bi1, bi2, p, pm
!-----------------------------------------------------------------------
!
    ind = 0
    do 10 i = 1, ndim
        if (vect(2*i-1) .ge. alphad) then
            ind = i
            goto 12
        endif
10  end do
    call assert(.false.)
12  continue
!
    ai1 = prof / ( alpham - alphad )
    bi1 = ai1*alphad
    ai2 = prof / ( alphaf - alpham )
    bi2 = ai2*alpham
    pm = 0.9d0 * ( ai1*alpham - bi1 )
!
    do 20 i = ind, ndim
!
        theta = vect(2*i-1)
!
        if (theta .gt. alphaf) goto 9999
!
        if (theta .lt. alpham) then
            p = ai1*theta - bi1
        else
            p = prof + bi2 - ai2*theta
        endif
        p = min ( p , pm )
        vect(2*i) = vect(2*i) + coef*p
!
20  end do
!
    do 30 i = 1, ndim
!
        thet0 = vect(2*i-1)
        theta = 360.d0 + thet0
!
        if (theta .gt. alphaf) goto 9999
!
        if (theta .lt. alpham) then
            p = ai1*theta - bi1
        else
            p = prof + bi2 - ai2*theta
        endif
        p = min ( p , pm )
        vect(2*i) = vect(2*i) + coef*p
!
30  end do
!
9999  continue
!
end subroutine
