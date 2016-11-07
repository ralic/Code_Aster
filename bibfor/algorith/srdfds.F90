subroutine srdfds(nbmat,mater,para,var,ds2hds,ucri,dfdsig)

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
!!! MODELE LKR : CALCUL DE DF/DSIG
!!!

! ===================================================================================
! IN  : NBMAT          : NOMBRE DE PARAMETRES DU MODELE
!     : MATER(NBMAT,2) : PARAMETRES DU MODELE
!     : S(6)           : TENSEUR DU DEVIATEUR DES CONTRAINTES
!     : PARA(3)        : VARIABLE D'ECROUISSAGE
!                           PARA(1) = AXI
!                           PARA(2) = SXI
!                           PARA(3) = MXI
!     : VAR(4)         : (ADXI, BDXI, DDXI, KDXI)
!     : DS2HDS(6)      : D(SII*H(THETA))/DSIG
!     : UCRI           : TERME SOUS LA PUISSANCE DANS LE CRITERE
! OUT : DFDSIG(6)      : DF/DSIG
! ===================================================================================

    implicit      none

#include "asterc/r8prem.h"
#include "asterfort/r8inir.h"
#include "asterc/r8pi.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat
    real(kind=8) :: mater(nbmat,2),para(3),var(4),ucri,ds2hds(6),dfdsig(6)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndi,ndt,i
    real(kind=8) :: sigc,gamma,beta,r0c,pi,fact1
    real(kind=8) :: a(6),kron(6),fact3
    common /tdim/ ndt, ndi
    
    data kron /1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    
    !!!
    !!! Recuperation de parametres du modele
    !!!
    
    sigc=mater(3,2)
    beta=mater(4,2)
    gamma=mater(5,2)
    pi=r8pi()
    r0c=cos(beta*pi/6.d0-1.d0/3.d0*acos(gamma))
    
    !!!
    !!! Termes intermediaires
    !!!
    
    fact1=para(1)*sigc*r0c
    fact3=para(1)-1.d0
    
    !!!
    !!! Resultat final
    !!!
    
    call r8inir(6,0.d0,a,1)
    call r8inir(6,0.d0,dfdsig,1)
    
    do i=1,ndt
        a(i)=var(1)*ds2hds(i)+var(2)*kron(i)
    end do
    
    do i=1,ndt
        if (ucri.le.r8prem()) then
            dfdsig(i)=ds2hds(i)
        else
            dfdsig(i)=ds2hds(i)-fact1*a(i)*(ucri**fact3)
        endif
    end do

end subroutine
