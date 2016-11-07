subroutine srd2de(devsig, d2dets)

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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ===================================================================================

!!!
!!! MODELE LKR : CALCUL DE DERIVEE 2NDE DE DET(S) PAR RAPPORT A DEVIATEUR SIGMA
!!!

! ===================================================================================
! IN  : DEVSIG(6)   : DEVIATEUR DES CONTRAINTES
! OUT : D2DETS(6,6) : DERIVEE 2NDE DET(S) PAR RAPPORT A SIGMA (NDT X NDT)
! ===================================================================================

    implicit   none

#include "asterfort/lcinma.h"

    !!!
    !!! Variables globales
    !!!
    
    real(kind=8) :: devsig(6), d2dets(6, 6)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndi, ndt
    real(kind=8) :: r2
    common /tdim/   ndt,ndi
    
    !!!
    !!! Calcul de d2dets
    !!!
    
    call lcinma(0.d0, d2dets)
    
    r2 = sqrt(2.d0)
    
    if (ndt .eq. 6) then
        
        d2dets(1,2)=devsig(3)
        d2dets(1,3)=devsig(2)
        d2dets(1,6)=-devsig(6)
        d2dets(2,1)=devsig(3)
        d2dets(2,3)=devsig(1)
        d2dets(2,5)=-devsig(5)
        d2dets(3,1)=devsig(2)
        d2dets(3,2)=devsig(1)
        d2dets(3,4)=-devsig(4)
        d2dets(4,3)=-devsig(4)
        d2dets(4,4)=-devsig(3)
        d2dets(4,5)=devsig(6)/r2
        d2dets(4,6)=devsig(5)/r2
        d2dets(5,2)=-devsig(5)
        d2dets(5,4)=devsig(6)/r2
        d2dets(5,5)=-devsig(2)
        d2dets(5,6)=devsig(4)/r2
        d2dets(6,1)=-devsig(6)
        d2dets(6,4)=devsig(5)/r2
        d2dets(6,5)=devsig(4)/r2
        d2dets(6,6)=-devsig(1)
        
    else if (ndt.eq.4) then
        
        d2dets(1,2)=devsig(3)
        d2dets(2,1)=devsig(3)
        d2dets(1,3)=devsig(2)
        d2dets(3,1)=devsig(2)
        d2dets(2,3)=devsig(1)
        d2dets(3,2)=devsig(1)
        d2dets(4,4)=-devsig(3)
        d2dets(4,3)=-devsig(4)
        d2dets(3,4)=-devsig(4)
        
    endif

end subroutine
