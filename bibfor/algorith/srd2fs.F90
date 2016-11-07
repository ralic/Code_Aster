subroutine srd2fs(nmat,materf,para,vara,varh,i1,devsig,ds2hds,d2shds,d2fds2)

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
!!! MODELE LKR : CALCUL DE DERIVEE 2NDE DE F PAR RAPPORT A SIGMA
!!!

! ===================================================================================
! IN  : NMAT           : DIMENSION TABLE DES PARAMETRES MATERIAU
!     : MATERF(NMAT,2) : PARAMETRES MATERIAU A T+DT
!     : I1             : TRACE DES CONTRAINTES
!     : DEVSIG(6)      : DEVIATEUR DES CONTRAINTES
!     : PARA(3)        : PARAMETRES AXI, SXI, MXI
!     : VARA(4)        : PARAMTERES ADXI,BDXI,DDXI,KDXI
!     : VARH(3)        : VECTEUR CONTENANT H0E,H0C ET HTHETA
!     : DS2HDS(6)      : DERIVEE DE SII*H PAR RAPPORT A SIGMA
!     : D2SHDS(6,6)    : DERIVVE 2NDE DE SII*H PAR RAPPORT A SIGMA
! OUT : D2FDS2(6,6)    : DERIVEE 2NDE F PAR RAPPORT A SIGMA (NDT X NDT)
!     : IRET           : CODE RETOUR
! ===================================================================================

    implicit   none

#include "asterfort/lcdima.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprte.h"

    !!!
    !!! Variable globale
    !!!
    
    integer :: nmat
    real(kind=8) :: d2fds2(6,6),para(3),vara(4),materf(nmat,2)
    real(kind=8) :: devsig(6),i1,ds2hds(6),varh(2),d2shds(6,6)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndi,ndt,i
    real(kind=8) :: sigc,sii,coef1,coef2,vident(6),vect1(6)
    real(kind=8) :: mat1(6,6),mat2(6,6),mat3(6,6),ucri
    common /tdim/ ndt,ndi
    
    !!! Recup. de sigma_c
    sigc=materf(3,2)
    
    !!!
    !!! Construction de sii
    !!!
    
    call lcprsc(devsig,devsig,sii)
    sii=sqrt(sii)
    
    !!!
    !!! Construction coef1 = a*sigc*h0c*(a-1)*(ad*sii*h + b*i1 + d)**(a-2)
    !!! Construction coef1 = a*sigc*h0c*(ad*sii*h + b*i1 + d)**(a-1)
    !!!
    
    ucri=vara(1)*sii*varh(2)+vara(2)*i1+vara(3)
    
    if (ucri.le.0.d0) then
        ucri=0.d0
        coef1=0.d0
        coef2=1.d0
    else
        coef1=para(1)*sigc*varh(1)*(para(1)-1.d0)*ucri**(para(1)-2.d0)
        coef2=1.d0-(vara(1)*para(1)*sigc*varh(1)*ucri**(para(1)-1.d0))
    endif
    
    !!!
    !!! Constuction du vecteur identite
    !!!
    
    call lcinve(0.d0,vident)
    
    do i=1,ndi
        vident(i)=1.d0
    end do
    
    !!!
    !!! Construction de a*ds2hds + b*vident
    !!!
    
    do i=1,ndt
        vect1(i)=vara(1)*ds2hds(i)+vara(2)*vident(i)
    end do
    
    !!!
    !!! Produit tensoriel de coef1*(vect1 x vect1)
    !!!
    
    call lcprte(vect1,vect1,mat1)
    call lcprsm(coef1,mat1,mat2)
    
    !!!
    !!! Produit coef2 * d2shds
    !!!

    call lcprsm(coef2,d2shds,mat3)
    
    !!!
    !!! Construction mat3 - mat2 = d2fds2
    !!!
    
    call lcdima(mat3, mat2, d2fds2)

end subroutine
