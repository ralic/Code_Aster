subroutine usenco(ai1, bi1, alphad, alphaf, ndim,&
                  vect)
    implicit   none
    include 'asterfort/assert.h'
    integer :: ndim
    real(kind=8) :: ai1, bi1, vect(*), alphad, alphaf
!-----------------------------------------------------------------------
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
    real(kind=8) :: theta, p
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
    do 20 i = ind, ndim
!
        theta = vect(2*i-1)
!
        if (theta .gt. alphaf) goto 9999
!
        p = ai1*theta - bi1
!
        vect(2*i) = vect(2*i) + p
!
20  end do
!
9999  continue
!
end subroutine
