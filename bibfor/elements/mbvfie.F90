subroutine mbvfie(nno,kpg,dff,sigpk2,ipoids,h,covadef,vecfie)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/r8inir.h"
#include "jeveux.h"
!
    integer :: nno, kpg
    integer :: ipoids
    real(kind=8) :: h
    real(kind=8) :: sigpk2(2, 2)
    real(kind=8) :: dff(2, nno), covadef(3,3)
    real(kind=8) :: vecfie(3*nno)
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DU VECTEUR FORCE INTERNE ELEMENTAIRE
!                          POUR LES MEMBRANES EN GRANDES DEFORMATIONS
! ----------------------------------------------------------------------
! IN  NNO          NOMBRE DE NOEUDS
!     KPG          INCREMENT SUR LA BOUCLE DES PTS DE GAUSS
!     DFF          DERIVEE DES F. DE FORME
!     SIGPK2       CONTRAINTES DE PIOLA KIRCHHOFF
!     IPOIDS       ADRESSE DANS ZR DU TABLEAU POIDS
!     H            EPAISSEUR DE LA MEMBRANES
!     COVADEF      BASE COVARIANTE SUR LA CONFIGURATION DEFORMEE
!
! OUT VECFIE       VECTEUR FORCE INTERNE
! ----------------------------------------------------------------------
!
    integer :: a, p, i, alpha, gamma
    
    call r8inir(3*nno, 0.d0, vecfie, 1)
    
    do a = 1, nno
        do p = 1, 3
            i = 3*(a-1) + p
            
            do alpha = 1, 2
                do gamma = 1, 2
                    vecfie(i) = vecfie(i) + dff(alpha,a)*covadef(p,gamma)*&
                                sigpk2(gamma,alpha)*h*zr(ipoids+kpg-1)
                end do
            end do
        end do
    end do
    
end subroutine
