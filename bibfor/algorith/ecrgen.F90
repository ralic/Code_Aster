subroutine ecrgen(iordre, nbmode, tc, dt, depg,&
                  vitg, accg, depgen, vitgen, accgen,&
                  temps, jordre, ptemps)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : ARCHIVAGE DES RESULTATS (GRANDEURS GENERALISEES)
! -----------
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: iordre, nbmode
    real(kind=8) :: tc, depg(*), vitg(*), accg(*), dt, depgen(nbmode, *)
    real(kind=8) :: vitgen(nbmode, *), accgen(nbmode, *), temps(*), ptemps(*)
    integer :: jordre(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, nbr
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    nbr = iordre + 1
!
!  0. VALEUR DE L'INSTANT ET NUMERO D'ORDRE DE L'INSTANT
!     --------------------------------------------------
    temps(nbr) = tc
    jordre(nbr) = iordre
    ptemps(nbr) = dt
!
!  1. DEPLACEMENTS GENERALISES
!     ------------------------
    do 10 i = 1, nbmode
        depgen(i,nbr) = depg(i)
10  end do
!
!  2. VITESSES GENERALISEES
!     ---------------------
    do 20 i = 1, nbmode
        vitgen(i,nbr) = vitg(i)
20  end do
!
!  3. ACCELERATIONS GENERALISEES
!     --------------------------
    do 30 i = 1, nbmode
        accgen(i,nbr) = accg(i)
30  end do
!
! --- FIN DE ECRGEN.
end subroutine
