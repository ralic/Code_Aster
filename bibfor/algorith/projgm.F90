subroutine projgm(np1, np2, ic, nbm, phii,&
                  floc, fmod)
!
! ********************************************************************
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
! ********************************************************************
! DESCRIPTION : CONVERTIT EN BASE MODALE LES FORCES LOCALISEES
! ------------
!
! ****************** DECLARATION DES VARIABLES ***********************
!
    implicit none
!
! ARGUMENTS
! ---------
    integer :: np1, np2
    integer :: ic, nbm
    real(kind=8) :: phii(np2, np1, 3), floc(*), fmod(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i
!
! ****************** DEBUT DU CODE EXECUTABLE ************************
!
    do 10 i = 1, nbm
        fmod(i) = 0.0d0
        fmod(i) = fmod(i) + phii(ic,i,1) * floc(1)
        fmod(i) = fmod(i) + phii(ic,i,2) * floc(2)
        fmod(i) = fmod(i) + phii(ic,i,3) * floc(3)
10  end do
!
end subroutine
