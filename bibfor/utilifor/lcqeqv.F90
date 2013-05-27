function lcqeqv(x, y)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       EGALITE DE 2 VECTEURS  X =? Y
!       IN  X      :  VECTEUR
!       IN  Y      :  VECTEUR
!       OUT LCQEQV :  REPONSE = 'OUI' OU 'NON'
!       ----------------------------------------------------------------
    real(kind=8) :: epsi
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    parameter       ( epsi = 1.d-9 )
    integer :: n, nd
    real(kind=8) :: x(6), y(6)
    character(len=3) :: lcqeqv
    common /tdim/   n , nd
!       ----------------------------------------------------------------
    do 1 i = 1, n
        if (abs (x(i) - y(i)) .gt. epsi) then
            lcqeqv = 'NON'
            goto 9999
        endif
 1  continue
    lcqeqv = 'OUI'
!
9999  continue
end function
