subroutine srhtet(nbmat, mater, rcos3t, r0c, rtheta)

!
! ===================================================================================
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
! ===================================================================================

!!!
!!! MODELE LKR : CALCUL DE H(THETA)
!!!

! ===================================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES MATERIAU
!     : MATER  : COEFFICIENTS MATERIAU
!                    MATER(*,1) = CARACTERISTIQUES ELASTIQUES
!                    MATER(*,2) = CARACTERISTIQUES PLASTIQUES
!     : RCOS3T : COS(3THETA)
! OUT : H0E    : PARAMETRE UTILISES DANS LE CRITERE
!     : H0C    : PARAMETRE UTILISES DANS LE CRITERE
!     : HTHETA : H(THETA)
! ===================================================================================

    implicit none

#include "asterc/r8pi.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat
    real(kind=8) :: mater(nbmat,2),rcos3t,r0c,rtheta
    
    !!!
    !!! Variables locales
    !!!
    
    real(kind=8) :: gamma,pi,beta
    
    !!!
    !!! Recuperation des parametres du modele
    !!!
    
    pi=r8pi()
    beta=mater(4,2)
    gamma=mater(5,2)
    
    !!!
    !!! Calcul de h0c et h(theta) (r0c et r(theta))
    !!!
    
    r0c=cos(beta*pi/6.d0-1.d0/3.d0*acos(gamma))
    rtheta=cos(beta*pi/6.d0-1.d0/3.d0*acos(gamma*rcos3t))
    
end subroutine
