subroutine srds2h(nbmat, mater, s, dhds, ds2hds, retcom)

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
!!! MODELE LKR : CALCUL DES DERIVEES D(SII*H(THETA))/DSIG
!!!

! ===================================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE
!     : MATER  : PARAMETRES DU MODELE
!     : INVAR  :  INVARIANT DES CONTRAINTES
!     : S      :  DEVIATEUR DES CONTRAINTES
!     : DHDS   : Dh(THETA)/DS
! OUT : DS2HDS : D(SII*H(THETA))/DSIG
! ===================================================================================

    implicit none

#include "asterc/r8miem.h"
#include "asterfort/cos3t.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lctrma.h"
#include "asterfort/srhtet.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat,retcom
    real(kind=8) :: mater(nbmat,2),s(6),dhds(6),ds2hds(6)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndt,ndi,i,k
    real(kind=8) :: pref,r0c,rtheta
    real(kind=8) :: kron(6),iden6(6,6)
    real(kind=8) :: a(6),b(6,6),bt(6,6)
    real(kind=8) :: sii,rcos3t,ptit
    common /tdim/ ndt, ndi
    
    data kron /1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    
    data iden6 /1.d0,0.d0,0.d0,0.d0,0.d0,0.d0,&
               &0.d0,1.d0,0.d0,0.d0,0.d0,0.d0,&
               &0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,&
               &0.d0,0.d0,0.d0,1.d0,0.d0,0.d0,&
               &0.d0,0.d0,0.d0,0.d0,1.d0,0.d0,&
               &0.d0,0.d0,0.d0,0.d0,0.d0,1.d0/

    !!!
    !!! Recuperation des parametres du modele
    !!!
    
    pref=mater(1,2)
    
    !!!
    !!! Calcul du deviateur et verification qu'il n'est pas nul
    !!!
    
    retcom=0
    ptit=r8miem()
    
    call lcprsc(s,s,sii)
    
    sii=sqrt (sii)
    
    if (sii.lt.ptit) then
        call utmess('A', 'COMPOR1_93')
        retcom=1
        goto 1000
    endif
    
    !!!
    !!! Recuperation de r(theta) et r0c
    !!!
    
    rcos3t=cos3t(s,pref,1.0d-8)
    call srhtet(nbmat,mater,rcos3t,r0c,rtheta)
    
    !!!
    !!! Calcul du premier terme
    !!!
    
    call r8inir(6,0.d0,a,1)
    do i=1,ndt
        a(i)=dhds(i)*sii+rtheta*s(i)/sii
    end do

    !!!
    !!! Calcul du second terme
    !!!
    
    call r8inir(6*6,0.d0,b,1)
    
    do i=1,ndt
        do k=1,ndt
            b(i,k)=iden6(i,k)-kron(i)*kron(k)/3.d0
        end do
    end do
    
    !!!
    !!! Resultat final
    !!!
    
    call r8inir(6,0.d0,ds2hds,1)
    call lctrma(b,bt)
    call lcprmv(bt,a,ds2hds)
    
1000  continue

end subroutine
