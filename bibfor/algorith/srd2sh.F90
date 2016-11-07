subroutine srd2sh(nmat,materf,varh,dhds,devsig,rcos3t,d2shds)

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
!!! MODELE LKR : CALCUL DE DERIVEE 2NDE DE SII*H PAR RAPPORT A SIGMA
!!!

! ===================================================================================
! IN  : NMAT           : DIMENSION TABLE DES PARAMETRES MATERIAU
!     : MATERF(NMAT,2) : PARAMETRES MATERIAU A T+DT
!     : VARH           : VECTEUR CONTENANT H0E,H0C ET HTHETA
!     : DHDS(6)        : DERIVEE DE HTHETA PAR RAPPORT A SIGMA
!     : DEVSIG(6)      : DEIATEUR DES CONTRAINTES
!     : RCOS3T         : COS(3THETA) = SQRT(54)*DET(DEVISG)/SII**3
! OUT : D2SHDS(6,6)    :  DERIVEE 2NDE SII*H PAR RAPPORT A SIGMA (NDT X NDT)
!     : IRET           :  CODE RETOUR
! ===================================================================================
    
    implicit   none

#include "asterfort/lcinma.h"
#include "asterfort/lcprmm.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprte.h"
#include "asterfort/lcsoma.h"
#include "asterfort/lceqma.h"
#include "asterfort/srd2hs.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nmat
    real(kind=8) :: materf(nmat,2),varh(2),d2shds(6,6),dhds(6),devsig(6),rcos3t
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndi,ndt,i,j
    real(kind=8) :: sii,dikdjl(6,6),dijdkl(6,6)
    real(kind=8) :: dsiids(6),dsdsig(6,6),mat1(6,6),d2hds2(6,6)
    real(kind=8) :: mat2(6,6),mat3(6,6),dhtds(6),mat4(6,6),mat5(6,6)
    real(kind=8) :: d2hdsi(6,6)
    common /tdim/ ndt,ndi
    
    !!!
    !!! Construction de sii
    !!!
    
    call lcprsc(devsig, devsig, sii)
    sii=sqrt(sii)
    
    !!!
    !!! initialisation matrice d_ik x d_jl
    !!!
    
    call lcinma(0.d0,dikdjl)
    
    do i=1,ndt
        dikdjl(i,i)=1.d0
    end do
    
    !!!
    !!! initialisation matrice d_ij x d_kl
    !!!
    
    call lcinma(0.d0,dijdkl)
    
    do i=1,ndi
        do j=1,ndi
            dijdkl(i,j)=1.d0/3.d0
        end do
    end do
    
    !!!
    !!! Calcul de d(sii)/d(sigma)
    !!!
    
    do i=1,ndt
        dsiids(i)=0.d0
        do j=1,ndt
            dsdsig(j,i)=dikdjl(j,i)-dijdkl(j,i)
            dsiids(i)=dsiids(i)+devsig(j)*dsdsig(j,i)/sii
        end do
    end do
    
    !!! Calcul de dh/ds*dsii/dsig = mat1
    call lcprte(dhds,dsiids,mat1)
    
    !!! Calcul de d2h/ds2
    call srd2hs(nmat,materf,devsig,sii,rcos3t,d2hds2)
    
    !!! Calcul de d2h/ds2
    call lcprmm(d2hds2,dsdsig,d2hdsi)
    
    !!! Construction de sii*d2h/dsigma = mat2
    call lcprsm(sii,d2hdsi,mat2)
    
    !!! mat2 + mat1 = mat3
    call lcsoma(mat1,mat2,mat3)
    
    !!! mat2 = coefh*mat3
    call lceqma(mat3,mat2)
    
    !!! Construction de dh/dsigma = (dh/ds)*(ds/dsigma)
    call lceqma(dsdsig,mat1)
    
    do i=1,ndt
        dhtds(i)=0.d0
        do j=1,ndt
            dhtds(i)=dhtds(i)+dhds(j)*mat1(j,i)/sii
        end do
    end do
    
    !!! Construction de mat1 = s x dh/dsigma
    call lcprte(devsig,dhtds,mat1)
    
    !!! Calcul de mat3 ) h/sii*(ds/dsig)
    call lcprsm(varh(2)/sii,dsdsig,mat3)
    
    !!! Calcul de mat5 = h*s*(dsii/ds)/sii**2
    call lcprte(devsig,dsiids,mat4)
    call lcprsm(varh(2)/sii**2.d0,mat4,mat5)
    
    !!! mat4 = mat2+mat1+mat3-mat5
    do i=1,ndt
        do j=1,ndt
            mat4(i,j)=mat2(i,j)+mat1(i,j)+mat3(i,j)-mat5(i,j)
        end do
    end do
    
    !!! d2sh/ds = ds/dsig * mat4
    call lcprmm(dsdsig, mat4, d2shds)

end subroutine
