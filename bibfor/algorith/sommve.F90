subroutine sommve(np, vec1, n1, vec2, n2,&
                  vecres)
!
! ********************************************************************
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ********************************************************************
! DESCRIPTION : SOMME DE DEUX VECTEURS
! ------------
!
! ****************** DECLARATION DES VARIABLES ***********************
!
    implicit none
!
! ARGUMENTS
! ---------
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    integer :: np, n1, n2
    real(kind=8) :: vec1(*), vec2(*), vecres(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ier
!
! ****************** DEBUT DU CODE EXECUTABLE ************************
!
    ier = 0
    call vecini(np, 0.d0, vecres)
!
    if (n1 .gt. np .or. n2 .gt. np .or. n1 .ne. n2) then
        ier = 1
        call u2mess('F', 'ALGORITH10_60')
    endif
!
    if (ier .eq. 0) then
        do 10 i = 1, n1
            vecres(i) = vec1(i) + vec2(i)
10      end do
    endif
!
end subroutine
