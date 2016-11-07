subroutine srd2hs(nmat,materf,devsig,sii,rcos3t,d2hds2)

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
!!! MODELE LKR : CALCUL DE LA DERIVEE 2NDE DE H PAR RAPPORT A DEVIATEUR SIGMA
!!!

! ===================================================================================
! IN  : NMAT           : DIMENSION TABLE DES PARAMETRES MATERIAU
!     : MATERF(NMAT,2) : PARAMETRES MATERIAU A T+DT
!     : DEVSIG(6)      : DEVIATEUR DES CONTRAINTES
!     : SII            : 2EME INVARIANT DU DEVIATEUR
!     : RCOS3T         : COS(3THETA) = SQRT(54)*DET(DEVSIG)/SII**3
!     : DHDS(6)        : DERIVEE DE H PAR RAPPORT A DEVSIG
! OUT : D2HDS2(6,6)    :  DERIVEE 2NDE H PAR RAPPORT A SIGMA (NDT X NDT)
! ===================================================================================

    implicit   none

#include "asterc/r8pi.h"
#include "asterfort/cjst.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcprte.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcsove.h"
#include "asterfort/lcprsm.h"
#include "asterfort/srd2de.h"
#include "asterfort/lcsoma.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nmat
    real(kind=8) :: devsig(6),rcos3t,sii,d2hds2(6,6),materf(nmat,2)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndi,ndt,i
    real(kind=8) :: mident(6,6),gamma,beta,pdcds(6,6),drdcos,sxs(6,6)
    real(kind=8) :: dcosds(6)
    real(kind=8) :: ddetds(6),fact4,dcds1(6),dcds2(6)
    real(kind=8) :: d2dets(6,6),r54,dets,fact3,fact10(6,6),fact9(6,6)
    real(kind=8) :: pi,denom,fact1,fact2,d2rdc2
    real(kind=8) :: fact5(6,6),fact6(6,6),fact7(6,6),fact8(6,6),fact56(6,6)
    real(kind=8) :: fact78(6,6),fact5678(6,6),d2cds2(6,6)
    common /tdim/ ndt,ndi
    
    !!!
    !!! Recup. des para. mater
    !!!
    
    beta=materf(4,2)
    gamma=materf(5,2)
    pi=r8pi()
    r54=sqrt(54.d0)
    
    !!!
    !!! Construction de d**2r/dcos(3t)**2
    !!!
    
    denom=9.d0*(1.d0-(gamma*rcos3t)**2.d0)**(3.d0/2.d0)
    fact1=-gamma**2.d0*sqrt(1.d0-(gamma*rcos3t)**2.d0)*&
          cos(beta*pi/6.d0-1.d0/3.d0*acos(gamma*rcos3t))
    fact2=-3.d0*(gamma**3.d0)*rcos3t*&
          sin(beta*pi/6.d0-1.d0/3.d0*acos(gamma*rcos3t))
    d2rdc2=(fact1+fact2)/denom
    
    !!!
    !!! Construction de d cos3t / ds x d cos3t / ds
    !!!
    
    dets=sii**3.d0*rcos3t/r54
    fact3=r54/sii**3.d0
    fact4=-3.d0*r54*dets/sii**5.d0
    call cjst(devsig,ddetds)
    call lcprsv(fact3,ddetds,dcds1)
    call lcprsv(fact4,devsig,dcds2)
    call lcsove(dcds1,dcds2,dcosds)
    call lcprte(dcosds,dcosds,pdcds)
    
    !!!
    !!! Construction de d(r)/d(cos3t)
    !!!
    
    drdcos=-gamma*sin(beta*pi/6.d0-1.d0/3.d0*acos(gamma*rcos3t))/&
            3.d0/sqrt(1.d0-(gamma*rcos3t)**2.d0)
    
    !!!
    !!! Construction de d**2 cos3t / ds**2
    !!!
    
    call lcprte(devsig,ddetds,fact5)
    call lcprsm(-1.d0*2.d0,fact5,fact5)
    call lcprte(devsig,devsig,sxs)
    call lcprsm(5.d0*dets/sii**2.d0,sxs,fact6)
    call srd2de(devsig,d2dets)
    call lcprsm(sii**2.d0/3.d0,d2dets,fact7)
    call lcinma(0.d0,mident)
    
    do i=1,ndt
        mident(i,i)=1.d0
    end do
    
    call lcprsm(-1.d0*dets,mident,fact8)
    call lcsoma(fact5,fact6,fact56)
    call lcsoma(fact7,fact8,fact78)
    call lcsoma(fact56,fact78,fact5678)
    call lcprsm(3.d0*r54/sii**5.d0,fact5678,d2cds2)
    
    !!!
    !!! Assemblage final
    !!!
    
    call lcprsm(d2rdc2,pdcds,fact9)
    call lcprsm(drdcos,d2cds2,fact10)
    call lcsoma(fact9,fact10,d2hds2)

end subroutine
