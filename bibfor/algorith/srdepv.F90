subroutine srdepv(depsv, ddepsv, dgamv, ddgamv)

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
!!! MODELE LKR : DERIVEE DE LA DEF. VISCO. ET DU PARAMETRE D ECROUISSAGE VISCO.
!!!

! ===================================================================================
! IN  : DT     : PAS DE TEMPS
!     : DEPSV  : DEFORMATIONS VISCO.
! OUT : DDEPSV : DEFORMATIONS DEVIATORIQUES VISQUEUSES
!     : DGAMV  : PARAMETRE D ECROUISSAGE VISQUEUX
!     : DDGAMV : DERIVEE DU PARAMETRE D ECROUISSAGE VISQUEUX PAR  RAPPORT A DEPS
! ===================================================================================

    implicit    none

#include "asterfort/lcdevi.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lctrma.h"
#include "asterfort/r8inir.h"

    !!!
    !!! Variables globales
    !!!
    
    real(kind=8) :: depsv(6), ddepsv(6)
    real(kind=8) :: dgamv, ddgamv(6)
    
    !!!
    !!! Variables locales 
    !!!
    
    integer :: i, k, ndi, ndt
    real(kind=8) :: devia(6,6), deviat(6,6)
    common /tdim/   ndt , ndi
    
    !!!
    !!! Deviateur du tenseur des def. visco.
    !!!
    
    call lcdevi(depsv, ddepsv)

    !!!
    !!! Calcul de dgamv
    !!!
    
    dgamv=0.d0
    
    do i=1, ndt
        dgamv=dgamv+ddepsv(i)**2
    end do
    
    dgamv=sqrt(2.d0/3.d0*dgamv)
    
    !!!
    !!! Matrice de projection dev.
    !!!
    
    call r8inir(6*6, 0.d0, devia, 1)
    
    do i=1, 3
        do k=1, 3
            devia(i,k)=-1.d0/3.d0
        end do
    end do
    
    do i=1, ndt
        devia(i,i)=devia(i,i)+1.d0
    end do
    
    call lctrma(devia, deviat)
    
    !!!
    !!! Calcul de dgamv/deps
    !!!
    
    call r8inir(6, 0.d0, ddgamv, 1)
    
    if (dgamv.le.0.d0) then
        do i=1, ndt
            ddgamv(i)=0.d0
        end do
    else
        call lcprmv(deviat, ddepsv, ddgamv)
        do i = 1, ndt
            ddgamv(i)=2.d0/3.d0*ddgamv(i)/dgamv
        end do
    endif

end subroutine
