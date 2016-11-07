subroutine srvacp(nbmat, mater, paraep, varpl)

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
!!! MODELE LKR : CALCUL DES FONCTIONS A^P, B^P, D^P ET K^P
!!!

! ===================================================================================
! IN  : NBMAT          : NOMBRE DE PARAMETRES DU MODELE
!     : MATER(NBMAT,2) : PARAMETRES DU MODELE
!     : PARAEP(3)      : VARIABLE D'ECROUISSAGE
!                          PARAEP(1) = AXIP
!                          PARAEP(2) = SXIP
!                          PARAEP(3) = MXIP
! OUT : VARPL(4)       : (ADXIP, BDXIP, DDXIP, KDXIP)
! ===================================================================================

    implicit      none
    
#include "asterc/r8pi.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat
    real(kind=8) :: paraep(3),mater(nbmat,2),varpl(4)
    
    !!!
    !!! Variables locales
    !!!
    
    real(kind=8) :: sigc,gamma,beta,r0c,pi
    real(kind=8) :: adxip,bdxip,ddxip,kdxip
    
    !!!
    !!! Recuperation de parametres materiaux
    !!!
    
    sigc=mater(3,2)
    beta=mater(4,2)
    gamma=mater(5,2)
    pi=r8pi()
    
    !!!
    !!! Calcul de k, a, b et d
    !!!
    
    r0c=cos(beta*pi/6.d0-1.d0/3.d0*acos(gamma))
    
    kdxip=(2.d0/3.d0)**(1.d0/2.d0/paraep(1))
    adxip=-paraep(3)*kdxip/sqrt(6.d0)/sigc/r0c
    bdxip=paraep(3)*kdxip/3.d0/sigc
    ddxip=paraep(2)*kdxip
    
    !!!
    !!! Stockage
    !!!
    
    varpl(1)=adxip
    varpl(2)=bdxip
    varpl(3)=ddxip
    varpl(4)=kdxip

end subroutine
