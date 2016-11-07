subroutine srdfdt(nbmat,mater,ucrip,invar,s,paraep,varpl,dpdt,dfdt)

!
! ===================================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG             
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
!!! MODELE LKR : CALCUL DE DF/DT
!!!

! ===================================================================================
! IN  : NBMAT          : NOMBRE DE PARAMETRES DU MODELE
!     : MATER(NBMAT,2) : PARAMETRES DU MODELE
!     : UCRIP          : PARTIE SOUS LA PUISSANCE DANS LE CRITERE
!     : INVAR          : INVARIANT TCONTRAINTES
!     : S(6)           : DEVIATEUR DES CONTRAINTES
!     : PARAEP(3)      : PARAMETRES D'ECROUISSAGE
!                           PARAEP(1) = AXIP
!                           PARAEP(2) = SXIP
!                           PARAEP(3) = MXIP
!     : VARPL(4)       : VARPL(1) = ADXIP
!                        VARPL(2) = BDXIP
!                        VARPL(3) = DDXIP
!                        VARPL(4) = KDXIP
!     : DPDT(3)        : DPDT(1) = DADT
!                        DPDT(2) = DSDT
!                        DPDT(3) = DMDT
! OUT : DFDT           : DF/DT
! ===================================================================================

    implicit      none

#include "asterfort/cos3t.h"
#include "asterfort/lcprsc.h"
#include "asterfort/srhtet.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat
    real(kind=8) :: mater(nbmat,2),ucrip,s(6),paraep(3),varpl(4),dpdt(3),dfdt,invar
    
    !!!
    !!! Variables locales
    !!!
    
    real(kind=8) :: pref,sigc,rcos3t,r0c,rtheta,sii
    real(kind=8) :: dfdad,dfdsd,dfdmd,fact1,fact3,fact4,fact5
    
    !!!
    !!! Recuperation des parametres du modele
    !!!
    
    sigc=mater(3,2)
    pref=mater(1,2)
    
    !!!
    !!! Calcul de sii et recuperation de h(theta) et h0c
    !!!
    call lcprsc(s,s,sii)
    sii=sqrt(sii)
    
    rcos3t=cos3t(s,pref,1.d-8)
    call srhtet(nbmat,mater,rcos3t,r0c,rtheta)
    
    !!!
    !!! Calcul de df*/ds*
    !!!
    
    fact1=-paraep(1)*varpl(4)*sigc*r0c
    if (ucrip.gt.0.d0) then
        
        dfdsd=fact1*ucrip**(paraep(1)-1.d0)
        
    else
        
        dfdsd=0.d0
        
    endif
    
    !!!
    !!! Calcul de df*/dm*
    !!!
    
    if (ucrip.gt.0.d0) then
        
        fact3=-paraep(1)*sigc*r0c
        fact4=varpl(1)*sii*rtheta/paraep(3)
        fact5=varpl(2)*invar/paraep(3)
        dfdmd=fact3*(fact4+fact5)*ucrip**(paraep(1)-1.d0)
        
    else
        
        dfdmd=0.d0
        
    endif
    
    !!!
    !!! Calcul de df*/da*
    !!!
    
    if (ucrip.gt.0.d0) then
        
        dfdad=-sigc*r0c*log(ucrip/varpl(4))*ucrip**paraep(1)
        
    else
    
        dfdad=0.d0
        
    endif
    
    !!!
    !!! Assemblage
    !!!
    
    dfdt=dpdt(1)*dfdad+dpdt(2)*dfdsd+dpdt(3)*dfdmd

end subroutine
