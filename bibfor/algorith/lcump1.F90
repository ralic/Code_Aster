subroutine lcump1(cmat, nmat, ambda1, ambda2, x1,&
                  x2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
! ROUTINE APPELE DANS LCUMFS
! LCUMP1     SOURCE    BENBOU   01/03/26
!
!_______________________________________________________________________
!
! CALCUL DE PARAMETRE DE LA LOI DE FLUAGE PROPRE SPHERIQUE
! IN  CMAT     : VECTEUR DE PARAMETRES (MATERIAU ET AUTRE)
! IN  NMAT     : DIMENSION DE CMAT
! OUT AMBDA1   : PARAMETRE MATERIAU LAMBDA1
! OUT AMBDA2   : PARAMETRE MATERIAU LAMBDA2
! OUT X1       : PARAMETRE MATERIAU X1
! OUT X2       : PARAMETRE MATERIAU X2
!_______________________________________________________________________
!
!     IMPLICIT REAL*8(A-H,O-Z)
    implicit none
    integer :: nmat
    real(kind=8) :: cmat(nmat)
    real(kind=8) :: ambda1, ambda2, delta, risp, rrsp, uii, uri
    real(kind=8) :: urr, visp, vrsp, x1, x2
!
! RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
!
    rrsp = cmat(3)
    vrsp = cmat(4)
    risp = cmat(5)
    visp = cmat(6)
!
! PARAMETRES INTERMEDIAIRES => EQUATION (3.10-1)
!
    urr = rrsp/vrsp
    uii = risp/visp
    uri = rrsp/visp
!
! PARAMETRE INTERMEDIAIRE => EQUATION (3.10-2)
!
    delta = (urr - uii)**2 + 8*uri*(urr + uii) + 16*uri**2
!
! INVERSE DES TEMPS CARACTERISTIQUES => EQUATION (3.10-4)
!
    ambda1 = - (urr + 4*uri + uii + sqrt(delta))/2
    ambda2 = - (urr + 4*uri + uii - sqrt(delta))/2
!
! PARAMETRES INTERMEDIAIRES => EQUATION (3.10-3)
!
    x1 = (ambda1 + uii)/(2*uri)
    x2 = 2*uri/(ambda2 + uii)
!
end subroutine
