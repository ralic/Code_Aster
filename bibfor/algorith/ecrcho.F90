subroutine ecrcho(iordre, nbnl, old, depbut, vitbut,&
                  forbut)
    implicit none
!-----------------------------------------------------------------------
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
! DESCRIPTION : ARCHIVAGE DES RESULTATS
! -----------  (GRANDEURS PHYSIQUES AUX NOEUDS DE CHOC)
!
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: iordre, nbnl
    real(kind=8) :: old(9, *), depbut(nbnl, 3, *), vitbut(nbnl, 3, *)
    real(kind=8) :: forbut(nbnl, 3, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: ic, nbr
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    nbr = iordre + 1
!
!  1. FORCES DE CONTACT
!     -----------------
    do 10 ic = 1, nbnl
        forbut(ic,1,nbr) = old(8,ic)
        forbut(ic,2,nbr) = old(3,ic)
        forbut(ic,3,nbr) = old(4,ic)
10  end do
!
!  2. DEPLACEMENTS DES NOEUDS DE CHOC
!     -------------------------------
    do 20 ic = 1, nbnl
        depbut(ic,1,nbr) = old(5,ic)
        depbut(ic,2,nbr) = old(6,ic)
        depbut(ic,3,nbr) = old(7,ic)
20  end do
!
!  3. VITESSES DES NOEUDS DE CHOC
!     ---------------------------
    do 30 ic = 1, nbnl
        vitbut(ic,1,nbr) = old(9,ic)
        vitbut(ic,2,nbr) = old(1,ic)
        vitbut(ic,3,nbr) = old(2,ic)
30  end do
!
! --- FIN DE ECRCHO.
end subroutine
