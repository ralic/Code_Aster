subroutine projvd(testc, np1, nb1, nb2, mat,&
                  u, v)
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
! DESCRIPTION : PROJECTION DE LA MATRICE MAT SUR LE VECTEUR U
! ------------
!
! ****************** DECLARATION DES VARIABLES ***********************
!
    implicit none
!
! ARGUMENTS
! ---------
#include "asterfort/prmave.h"
#include "asterfort/u2mess.h"
    integer :: testc, np1, nb1, nb2
    real(kind=8) :: mat(np1, *), u(*), v(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ier
!
! ****************** DEBUT DU CODE EXECUTABLE ************************
!
    if (testc .eq. 1) then
!
        ier = 0
        call prmave(0, mat, np1, nb1, nb2,&
                    u, nb2, v, nb1, ier)
        if (ier .ne. 0) then
            call u2mess('F', 'ALGORITH10_10')
        endif
!
    else
!
        if (nb1 .eq. nb2) then
            do 100 i = 1, nb1
                v(i) = u(i)
100          continue
!
        else if (nb1 .lt. nb2) then
            do 200 i = 1, nb1
                v(i) = u(i)
200          continue
            do 300 i = nb1+1, nb2
                v(i) = 0.0d0
300          continue
!
        else if (nb2 .lt. nb1) then
            do 400 i = 1, nb2
                v(i) = u(i)
400          continue
            do 500 i = nb2+1, nb1
                v(i) = 0.0d0
500          continue
        endif
!
    endif
!
end subroutine
