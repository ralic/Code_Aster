subroutine vecnuv(ipre, ider, gamma, phinit, dphi,&
                  n, k, dim, vectn, vectu,&
                  vectv)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean.angles at edf.fr
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: ipre, ider, n, k, dim
    real(kind=8) :: gamma, phinit, dphi, vectn(dim), vectu(dim)
    real(kind=8) :: vectv(dim)
! ----------------------------------------------------------------------
! BUT: DETERMINER LES COORDONNEES (X,Y,Z) DES VECTEURS NORMAUX.
! ----------------------------------------------------------------------
! ARGUMENTS :
!  IPRE     IN   I  : PREMIERE VALEUR DU POINTEUR.
!  IDER     IN   I  : DERNIERE VALEUR DU POINTEUR.
!  GAMMA    IN   R  : VALEUR DE GAMMA.
!  PHINIT   IN   R  : VALEUR INITIALE DE L'ANGLE PHI.
!  DPHI     IN   R  : INCREMENT DE PHI.
!  N        IN   I  : COMPTEUR.
!  K        IN   I  : VALEUR DU DECALAGE.
!  DIM      IN   I  : DIMENSION DES VECTEUR.
!  VECTN    OUT  R  : COORDONNEES DES VECTEURS NORMAUX.
!  VECTU    OUT  R  : COORDONNEES DES VECTEURS TANGENTS, COMPOSANTES U.
!  VECTV    OUT  R  : COORDONNEES DES VECTEURS TANGENTS, COMPOSANTES V.
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: i
    real(kind=8) :: phi
!     ------------------------------------------------------------------
!
!234567                                                              012
!
    call jemarq()
!
    do 10 i = ipre, ider
        n = n + 1
        phi = phinit + (i-k)*dphi
!
        vectn((n-1)*3 + 1) = sin(gamma)*cos(phi)
        vectn((n-1)*3 + 2) = sin(gamma)*sin(phi)
        vectn((n-1)*3 + 3) = cos(gamma)
!
        vectu((n-1)*3 + 1) = -sin(phi)
        vectu((n-1)*3 + 2) = cos(phi)
        vectu((n-1)*3 + 3) = 0.0d0
!
        vectv((n-1)*3 + 1) = -cos(gamma)*cos(phi)
        vectv((n-1)*3 + 2) = -cos(gamma)*sin(phi)
        vectv((n-1)*3 + 3) = sin(gamma)
!
10  end do
!
    call jedema()
!
end subroutine
