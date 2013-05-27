subroutine nmcb2(eps, beta1, eb, d1, sigf1,&
                 beta2, d2, sigma)
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
! --- ------------------------------------------------------------------
!     LABORDERIE 1D : CALCUL DE SIGMA DANS LE CAS 2
!
!     EPS   : IN     : DEFORMATION
!     EB    : IN     : MODULE BETON
!     BETA1 : IN     : COEFFICIENT BETA1 DE LA LOI
!     BETA2 : IN     : COEFFICIENT BETA2 DE LA LOI
!     D1    : IN     : ENDOMMAGEMENT
!     D2    : IN     : ENDOMMAGEMENT
!     SIGF1 : IN     : CONTRAINTE DE REFERMETURE DES FISSURES
!     SIGMA : OUT    : CONTRAINTE
!
! --- ------------------------------------------------------------------
    real(kind=8) :: eps, beta1, eb, d1, sigf1, beta2, d2, sigma
    real(kind=8) :: fprim, titi, toto, un, esec
    parameter  (un=1.0d0)
!
!     MODULE SECANT DE COMPRESSION
    esec = eb*(un-d2)
!
    titi = sigf1 + beta1*d1*(un-d2)/(un-d1)
    toto = beta1*d1*(un-d2)/(un-d1)
    fprim = (esec*eps-beta2*d2+sigf1)/titi
! --- CONTRAINTE
    sigma = esec*eps - beta2*d2 - toto*fprim
end subroutine
