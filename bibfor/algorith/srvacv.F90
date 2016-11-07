subroutine srvacv(nbmat,mater,paravi,varvi)

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
!!! MODELE LKR : CALCUL DES FONCTIONS A^VP, B^VP, D^VP ET K^VP
!!!

! ===================================================================================
! IN  : NBMAT          : NOMBRE DE PARAMETRES DU MODELE
!     : MATER(NBMAT,2) : PARAMETRES DU MODELE
!     : PARAVI(3)      : PARAMETRES D'ECROUISSAGE VISCO.
!                          PARAVI(1) = AXIV
!                          PARAVI(2) = SXIV
!                          PARAVI(3) = MXIV
! OUT : VARVI(4)       : (AVXIV, BVXIV, DVXIV, KXIV)
! ===================================================================================

    implicit      none

#include "asterc/r8pi.h"

    !!!
    !!! Variables globales
    !!!
   
    integer :: nbmat
    real(kind=8) :: mater(nbmat,2),paravi(3),varvi(4)
    
    !!!
    !!! Variables locales
    !!!
    
    real(kind=8) :: sigc,gamma,beta,r0c,avxiv,bvxiv,kvxiv,dvxiv
    real(kind=8) :: pi
    
    !!!
    !!! Recuperation des parametres du modele
    !!!
    
    sigc=mater(3,2)
    beta=mater(4,2)
    gamma=mater(5,2)
    pi=r8pi()
    
    !!!
    !!! Calcul de k, a, b et d
    !!!
    
    r0c=cos(beta*pi/6.d0-1.d0/3.d0*acos(gamma))
    
    kvxiv=(2.d0/3.d0)**(1.d0/2.d0/paravi(1))
    avxiv=-paravi(3)*kvxiv/sqrt(6.d0)/sigc/r0c
    bvxiv=paravi(3)*kvxiv/3.d0/sigc
    dvxiv=paravi(2)*kvxiv

    !!!
    !!! Stockage
    !!!
    
    varvi(1)=avxiv
    varvi(2)=bvxiv
    varvi(3)=dvxiv
    varvi(4)=kvxiv

end subroutine
