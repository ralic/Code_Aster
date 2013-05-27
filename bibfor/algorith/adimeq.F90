subroutine adimeq(nbm, nbmc, cmod, kmod, fmod,&
                  masgi, amori, pulsi, cmodc, kmodc,&
                  fmoda)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
! DESCRIPTION : ADIMENSIONNALISATION DE L'EQUATION DE LA MECANIQUE
! -----------
!               APPELANT : CALFNL
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: nbm, nbmc
    real(kind=8) :: cmod(nbm, *), kmod(nbm, *), fmod(*), masgi(*), amori(*)
    real(kind=8) :: pulsi(*), cmodc(nbm, *), kmodc(nbm, *), fmoda(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    do 10 i = 1, nbmc
        fmoda(i) = fmod(i)/masgi(i)
10  end do
!
!
    do 20 j = 1, nbmc
        do 21 i = 1, nbmc
            cmodc(i,j) = - cmod(i,j)/masgi(i)
            kmodc(i,j) = - kmod(i,j)/masgi(i)
21      continue
20  end do
!
    do 30 i = 1, nbmc
        cmodc(i,i) = cmodc(i,i) + (amori(i)/masgi(i))
        kmodc(i,i) = kmodc(i,i) + pulsi(i)*pulsi(i)
30  end do
!
! --- FIN DE ADIMEQ.
end subroutine
