subroutine srdhds(nbmat, mater, s, dhds, retcom)

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
!!! MODELE LKR : CALCUL DE LA DERIVEE DH(THETA)/DS (H MINUSCULE)
!!!

! ===================================================================================
! IN  : NBMAT          :  NOMBRE DE PARAMETRES MATERIAU
!     : MATER(NBMAT,2) :  COEFFICIENTS MATERIAU A T+DT
!                            MATER(*,1) = CARACTERISTIQUES ELASTIQUES
!                            MATER(*,2) = CARACTERISTIQUES PLASTIQUES
!     : INVAR          : INVARIANT DES CONTRAINTES
!     : S(6)           : DEVIATEUR DES CONTRAINTES
! OUT : DHDS(6)        : DH(THETA)/DS
!     : RETCOM         : CODE RETOUR POUR REDECOUPAGE
! ===================================================================================

    implicit none

#include "asterc/r8miem.h"
#include "asterfort/cjst.h"
#include "asterfort/cos3t.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcsove.h"
#include "asterc/r8pi.h"
#include "asterfort/utmess.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat,retcom
    real(kind=8) :: mater(nbmat,2),s(6),dhds(6)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndt,ndi
    real(kind=8) :: gamma,beta,pref,t(6),sii,rcos3t,dets,ptit
    real(kind=8) :: fact1(6),fact2(6),r54,pi,drdcos,dcosds(6)
    common /tdim/ ndt, ndi
    
    !!!
    !!! Recuperation des parametres du modele
    !!!
    
    beta=mater(4,2)
    gamma=mater(5,2)
    pref=mater(1,2)
    r54=sqrt(54.d0)
    pi=r8pi()
    
    !!!
    !!! Calcul du deviateur des contraintes
    !!!
    
    retcom=0
    ptit=r8miem()
    call lcprsc(s,s,sii)
    sii=sqrt(sii)
    
    !!! on verifie si sii n'est pas nul car division par sii dans la routine
    if (sii.lt.ptit) then
        call utmess('A','COMPOR1_92')
        retcom=1
        goto 1000
    endif
    
    !!!
    !!! Calcul de cos(3theta), det(s) et d(det(s))/d(s)
    !!!
    
    rcos3t=cos3t(s,pref,1.d-8)
    dets=(sii**3.d0)*rcos3t/r54
    
    call cjst(s,t)
    
    !!!
    !!! Calcul de d(r(theta))/d(cos(3theta))
    !!!
    
    drdcos=-r54*gamma*sin(beta*pi/6.d0-1.d0/3.d0*acos(gamma*rcos3t))/&
            3.d0/sqrt(1.d0-(gamma*rcos3t)**2.d0)
    
    !!!
    !!! Calcul de d(cos(3theta))/d(s)
    !!!
    
    call lcprsv(1.d0/sii**3.d0,t,fact1)
    call lcprsv(-3.d0*dets/sii**5.d0,s,fact2)
    call lcsove(fact1,fact2,dcosds)
    
    !!!
    !!! Calcul final
    !!!
    call lcprsv(drdcos,dcosds,dhds)
    
1000  continue

end subroutine
