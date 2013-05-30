subroutine calfft(np1, np4, nbm, n, dtext,&
                  fext, omegaf, aa, bb)
!
!----------------------------------------------------------------------
! TOLE CRS_1404
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!----------------------------------------------------------------------
! DESCRIPTION : CALCULE LES COEFFICIENTS DE FOURIER ASSOCIES A
! -----------   L'EXCITATION TEMPORELLE FEXT(T), AA ET BB, AINSI QUE
!               PULSATIONS DE FOURIER OMEGAF
!
! ******************   DECLARATION DES VARIABLES   *********************
!
    implicit none
!
! ARGUMENTS
! ---------
    include 'asterc/r8depi.h'
    include 'asterfort/fft.h'
    include 'asterfort/matini.h'
    include 'asterfort/vecini.h'
    integer :: np1, np4, nbm, n
    real(kind=8) :: dtext
    real(kind=8) :: fext(np4, *), omegaf(*), aa(np4, *), bb(np4, *)
!
! VARIABLES LOCALES
! -----------------
!
    integer :: i, j
    integer :: n2m1
    real(kind=8) :: df
    complex(kind=8) :: fcext(n)
!
! ******************   DEBUT DU CODE EXECUTABLE   **********************
!
    n2m1 = (n/2) - 1
    call matini(np4, np1, 0.d0, aa)
    call matini(np4, np1, 0.d0, bb)
    call vecini(np4, 0.d0, omegaf)
!
    df = 1.0d0 / (dble(n)*dtext)
!
    do 1 j = 1, nbm
        do 2 i = 1, n
            fcext(i) = dcmplx(fext(i,j),0.0d0)
 2      continue
        call fft(fcext, n, 0)
        do 5 i = 1, n2m1
            aa(i,j) = 2.0d0 * dble(fcext(i+1))
            bb(i,j) = -2.0d0 * dimag(fcext(i+1))
 5      continue
 1  end do
!
    do 6 i = 1, n
        omegaf(i) = r8depi() * df * dble(i)
 6  end do
!
end subroutine
