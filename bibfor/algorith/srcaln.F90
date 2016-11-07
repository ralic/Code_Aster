subroutine srcaln(s,b,vecn,retcom)

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
!!! MODELE LKR : CALCUL DE N
!!!

! ===================================================================================
! IN  : S(6)    : DEVIATEUR DES CONTRAINTES
!     : BPRIME  : PARAMETRE BPRIME
! OUT : VECN(6) : N = (BPRIME*S/SII-KRON)/SQRT(BPRIME**2+3)
! ===================================================================================
    
    implicit none
#include "asterc/r8miem.h"
#include "asterfort/lcprsc.h"
#include "asterfort/utmess.h"

    !!!
    !!! Variables globales
    !!!

    integer :: retcom
    real(kind=8) :: b,s(6),vecn(6)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: i,ndt,ndi
    real(kind=8) :: sii,racine,kron(6),ptit
    common /tdim/ ndt, ndi
    
    data kron /1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    
    !!!
    !!! Calcul de sii et verif. qu'il n'est pas nul
    !!!
    
    retcom=0
    ptit=r8miem()

    call lcprsc(s, s, sii)
    sii=sqrt(sii)

    if (sii.lt.ptit) then
        call utmess('A', 'COMPOR1_94')
        retcom=1
        goto 100
    endif
    
    !!!
    !!! Calcul de n
    !!!
    
    racine=sqrt(b*b+3.d0)
    
    do i=1,ndt
        vecn(i)=(b*s(i)/sii-kron(i))/racine
    end do
    
100  continue

end subroutine
