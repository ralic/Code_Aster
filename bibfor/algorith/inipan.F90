subroutine inipan(np1, nbm, cmod0, kmod0, cmod,&
                  kmod, amor0, puls0, amor, puls,&
                  fnlmod, fexmod, fmod00)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : INITIALISATION DES PARAMETRES POUR ITERATIONS NEWTON
! -----------
!               APPELANT : NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: np1, nbm
    real(kind=8) :: cmod0(np1, *), kmod0(np1, *), cmod(np1, *), kmod(np1, *)
    real(kind=8) :: amor0(*), puls0(*), amor(*), puls(*), fnlmod(*), fexmod(*)
    real(kind=8) :: fmod00(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    do 10 i = 1, nbm
        amor0(i) = amor(i)
10  end do
!
    do 20 i = 1, nbm
        puls0(i) = puls(i)
20  end do
!
    do 30 i = 1, nbm
        fmod00(i) = fnlmod(i) + fexmod(i)
30  end do
!
    do 40 j = 1, nbm
        do 41 i = 1, nbm
            kmod0(i,j) = kmod(i,j)
41      continue
40  end do
!
    do 50 j = 1, nbm
        do 51 i = 1, nbm
            cmod0(i,j) = cmod(i,j)
51      continue
50  end do
!
! --- FIN DE INIPAN.
end subroutine
