subroutine zconju(zin, prea, pima)
    implicit none
!-----------------------------------------------------------------------
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
!
!  BUT:  RENDRE LES PARTIES REELLE, IMAGINAIRE D'UN COMPLEXE*16
!   ROUTINE PLUS PRECISE QUE LES REAL ET IMAG FORTRAN
!
!  ATTENTION PREA ET PIMA DOIVENT BIEN ETRE DES REAL*8 DANS LA
!   SUBROUTINE APPELANTE
!
!-----------------------------------------------------------------------
!
! ZIN      /I/: COMPLEXE A DECORTIQUER
! PREA     /O/: PARTIE REELLE CORRESPONDANTE
! PIMA     /O/: PARTIE IMAGINAIRE CORRESPONDANTE
!
!-----------------------------------------------------------------------
    complex(kind=8) :: zin
    real(kind=8) :: prea, pima
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    prea = dble(zin)
    pima = dimag(zin)
!
end subroutine
