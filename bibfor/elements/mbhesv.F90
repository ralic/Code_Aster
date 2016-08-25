subroutine mbhesv(imate,kpg,fami,aini,metrini,metrdef,sigpk2,dsigpk2)
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
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/fctlam.h"
#include "jeveux.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    character(len=4) :: fami
    integer :: kpg, imate
    real(kind=8) :: aini(2, 2),metrini(2, 2)
    real(kind=8) :: metrdef(2, 2)
    real(kind=8) :: sigpk2(2, 2), dsigpk2(2, 2, 2, 2)
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DE LA LOI DE COMPORTEMENT REDUITE HYPERELASTIQUE
!                          EN ST VENANT-KIRCHHOFF POUR LES MEMBRANES
!    - HYPOTHESES : MATERIAU ISOTROPE
!                   CONTRAINTES PLANES
! ----------------------------------------------------------------------
! IN  IMATE             ADRESSE DANS ZI DU TABLEAU PMATERC
!     KPG               NUMERO DU POINT DE GAUSS DANS LA BOUCLE
!     FAMI              NOM DE LA FAMILLE DE POINT DE GAUSS ('RIGI', 'MASS', ...)
!     AINI,ADEF         METRIQUE CONTRAVARIANTE                   (RESP. ETAT INITIAL/DEFORME)
!     COVAINI,COVADEF   COORD DES VECTEURS DE LA BASE COVARIANTES (RESP. ETAT INITIAL/DEFORME)
!     METRINI, METRDEF  METRIQUE COVARIANTE                       (RESP. ETAT INITIAL/DEFORME)
!     JACINI, JACDEF    JACOBIEN DE LA METRIQUE COVARIANTE        (RESP. ETAT INITIAL/DEFORME)
!
! OUT SIGPK2            CONTRAINTES DE PIOLA KIRCHHOFF II
!     DSIGPK2           TENSEUR TANGENT MATERIEL = d(SIGPK2)/d(E) (E : TENSEUR GREEN LAGRANGE)
! ----------------------------------------------------------------------
!
    character(len=16) :: nomres(26)
    character(len=32) :: phenom
    integer :: icodre(26)
    integer :: nbv
    integer :: alpha, beta, gamma, delta
    real(kind=8) :: valres(26)
    real(kind=8) :: young, nu, lambda, mu
    real(kind=8) :: factor0, factor1
!
! DEFINITION DES DONNES MATERIAU
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
    if (phenom .eq. 'ELAS') then
        nbv=2
        nomres(1)='E'
        nomres(2)='NU'
        
        call rcvalb(fami, kpg, 1, '+', zi(imate),' ', phenom, 0, '', &
                    [0.d0],nbv, nomres, valres, icodre, 1)
                    
        young = valres(1)
        nu = valres(2)
    else
        call utmess('F', 'MEMBRANE_4')
    endif
    
! --- COEFFICIENTS DE LAME
    lambda=young*nu/((1+nu)*(1-2*nu))
    mu=young/(2*(1+nu))
! 
! CALCUL DES CONTRAINTES DE PIOLA KIRCHOFF II (LOI DE COMPORTEMENT REDUITE)
!
    factor0 = young/(1-nu*nu)
    
    call r8inir(2*2, 0.d0, sigpk2, 1)
    
    do alpha = 1, 2
        do beta = 1, 2
            do gamma = 1, 2
                do delta = 1, 2 
                    sigpk2(alpha,beta) = sigpk2(alpha,beta) +     &
                           factor0*(0.5*(1-nu)*(                  &
                           aini(alpha,gamma)*aini(beta,delta)  +  &
                           aini(alpha,delta)*aini(beta,gamma)) +  &
                           nu*aini(alpha,beta)*aini(gamma,delta)  &
                           )*0.5*(metrdef(delta,gamma)-metrini(delta,gamma))
                end do
            end do
        end do
    end do
!
! CALCUL DU TENSEUR TANGENT MATERIEL d(sigPK2)/dE
!    
    
    factor1 = 2*lambda*mu/(lambda+2*mu)
    
    do alpha = 1, 2
        do beta = 1, 2
            do gamma = 1, 2
                do delta = 1, 2
                    dsigpk2(alpha,beta,gamma,delta) = mu*(  &
                    aini(alpha,gamma)*aini(beta,delta)  +   &
                    aini(alpha,delta)*aini(beta,gamma)) +   &
                    factor1*aini(alpha,beta)*aini(gamma,delta)
                end do
            end do
        end do
    end do
    
end subroutine
