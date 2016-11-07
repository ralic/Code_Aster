subroutine srdgds(nmat,materf,para,vara,devsig,&
                  i1,val,ds2hds,vecn,dfds,&
                  bprimp,nvi,vint,dhds,tmp,dgds)

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
!!! MODELE LKR : CALCUL DE DERIVEE DE G PAR RAPPORT A SIGMA
!!!

! ===================================================================================
! IN  ::: NMAT           : DIMENSION TABLE PARAMETRES MATERIAU
!     ::: MATERF(NMAT,2) : TABLE DES PARAMETRES MATERIAU A T+DT
!     ::: PARA(3)        : VECTEUR CONTENANT AXI, SXI ET MXI
!     ::: VARA(4)        : VECTEUR CONTENANT ADXI, BDXI, DDXI ET KDXI
!     ::: DEVSIG(6)      : DEVIATEUR DES CONTRAINTES
!     ::: I1             : TRACE DES CONTRAINTES
!     ::: VAL            : BOOLEEN PRECISANT DILATANCE EN PRE(0) OU POST-PIC(1)
!     ::: VECN(6)        : VECTEUR UNITAIRE DE DILATANCE
!     ::: DFDS(6)        : DERIVEE SEUIL PAR RAPPORT A SIGMA
!     ::: DS2HDS(6)      : DERIVEE DE SII*H PAR RAPPORT A SIGMA
!     ::: BPRIMP         : PARAMETRE DE DILATANCE FCTN SIGMA
!     ::: NVI            : NOMBRE DE VARIABLES INTERNES
!     ::: VINT(NVI)      : VARIABLES INTERNES
!     ::: DHDS(6)        : DERIVEE DE H(THETA) PAR RAPPORT A SIGMA
! OUT ::: DGDS(6,6)      : DERIVEE DU POTENTIEL G PAR RAPPORT A SIGMA (NDTXNDT)
!     ::: IRET           : CODE RETOUR
! ===================================================================================
    
    implicit   none

#include "asterfort/cos3t.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprte.h"
#include "asterfort/srd2fs.h"
#include "asterfort/srd2sh.h"
#include "asterfort/srdnds.h"
#include "asterfort/srhtet.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nmat,nvi,val
    real(kind=8) :: materf(nmat,2),dgds(6,6),vecn(6),dfds(6),vint(nvi)
    real(kind=8) :: para(3),vara(4),ds2hds(6),devsig(6),i1,bprimp
    real(kind=8) :: dhds(6),tmp
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: i,j,ndi,ndt
    real(kind=8) :: d2fds2(6,6),d2fdsn(6)
    real(kind=8) :: dfdnpn(6,6),dfpndn(6,6)
    real(kind=8) :: dfdsdn(6),dfdsvn,d2shds(6,6),varh(2)
    real(kind=8) :: r0c,rtheta,rcos3t,patm
    real(kind=8) :: dndsig(6,6),d2fn2(6,6)
    
    common /tdim/ ndt,ndi
    
    !!! Recup. de patm    
    patm=materf(1,2)
    
    !!!
    !!! Recup. de h0c et h(theta)
    !!!
    
    rcos3t=cos3t(devsig,patm,1.d-8)
    call srhtet(nmat,materf,rcos3t,r0c,rtheta)
    
    varh(1)=r0c
    varh(2)=rtheta
    
    !!!
    !!! Construction de d2f/ds2
    !!!
    
    call srd2sh(nmat,materf,varh,dhds,devsig,rcos3t,d2shds)
    call srd2fs(nmat,materf,para,vara,varh,i1,devsig,ds2hds,d2shds,d2fds2)
    
    !!!
    !!! Construction de d(n)/d(sig)
    !!!
    
    call srdnds(nmat,materf,i1,devsig,bprimp,nvi,vint,val,para,tmp,dndsig)
    
    !!!
    !!! Construction de (d2(f)/ds(2):n)n
    !!!
    
    do i=1, ndt
        d2fdsn(i)=0.d0
        do j=1,ndt
            d2fdsn(i)=d2fdsn(i)+vecn(j)*d2fds2(j,i)
        end do
    end do
    
    call lcprte(vecn,d2fdsn, d2fn2)
    
    !!!
    !!! Construction de (d(f)/d(s)d(n)/d(sig))n
    !!!
    
    do i=1,ndt
        dfdsdn(i)=0.d0
        do j=1,ndt
            dfdsdn(i)=dfdsdn(i)+dfds(j)*dndsig(j,i)
        end do
    end do
    
    call lcprte(vecn,dfdsdn,dfdnpn)
    
    !!!
    !!! Construction de (d(f)/d(s):n)d(n)/d(sig)
    !!!
    
    call lcprsc(dfds,vecn,dfdsvn)
    call lcprsm(dfdsvn,dndsig,dfpndn)
    
    !!!
    !!! Assemblage
    !!!
    
    do i=1,ndt
        do j=1,ndt
            dgds(i,j)=d2fds2(i,j)-d2fn2(i,j)-dfdnpn(i,j)-dfpndn(i,j)
        end do
    end do
    
end subroutine
