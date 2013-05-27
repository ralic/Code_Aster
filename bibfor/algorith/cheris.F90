subroutine cheris(nb, ivec, i, iran)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 19/02/91
!-----------------------------------------------------------------------
!  BUT:  CHERCHER LE RANG D'UN ENTIER DANS UNE LISTE D'ENTIER
!
!-----------------------------------------------------------------------
!
! NB       /I/: NOMBRE D'ENTIER DE LA LISTE
! IVEC     /I/: VECTEUR DES ENTIERS
! I        /I/: ENTIER A TROUVER
! IRAN     /O/: RANG DE L'ENTIER
!
!-----------------------------------------------------------------------
!
    integer :: ivec(nb)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iran, j, nb
!-----------------------------------------------------------------------
    iran=0
    do 10 j = 1, nb
        if (ivec(j) .eq. i) iran=j
10  end do
!
end subroutine
