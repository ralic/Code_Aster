subroutine srijpl(nmat,mater,sigf,nr,drdy,dsde)

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
!!! MODELE LKR : MATRICE COHERENTE DE LKR A T+DT
!!!

! ===================================================================================
! IN  : NMAT          :  DIMENSION MATER
!     : MATER(NMAT,2) :  COEFFICIENTS MATERIAU
!     : SIGF(6)       :  ETAT DE CONTRAINTES A T+DT
!     : NR            :  DIMENSION MATRICE JACOBIENNE
!     : DRDY(NR,NR)   :  MATRICE JACOBIENNE (NR*NR)
! OUT : DSDE(6,6)     :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
! ===================================================================================

    implicit none

#include "asterc/r8prem.h"
#include "asterfort/lcdima.h"
#include "asterfort/lceqma.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmm.h"
#include "asterfort/lcprsm.h"
#include "asterfort/mgauss.h"
#include "asterfort/prmama.h"
#include "asterfort/r8inir.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nmat,nr
    real(kind=8) :: dsde(6,6),mater(nmat,2)
    real(kind=8) :: drdy(nr,nr),sigf(6)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: i,j,iret,ier,ndt,ndi
    real(kind=8) :: jss(6,6),jsz(6,3),jzs(3,6),jzz(3,3)
    real(kind=8) :: hook(6,6),hooknl(6,6),i1,coefnl
    real(kind=8) :: patm,nelas,invjzz(3,3),j3x6(3,6)
    real(kind=8) :: det,j6x6(6,6),dijaco(6,6),invdij(6,6)
    real(kind=8) :: maxi,mini,mue,mu
    common /tdim/   ndt  , ndi
    
    !!!
    !!! Initialisation de matrices a zero
    !!!
    
    call lcinma(0.d0,jss)
    call r8inir(18,0.d0,jsz,1)
    call r8inir(18,0.d0,jzs,1)
    call r8inir(9,0.d0,jzz,1)
    
    !!!
    !!! Recherche du aximum de drdy
    !!!
    
    maxi=0.d0
    do i=1,nr
        do j=1,nr
            if(abs(drdy(i,j)).gt.maxi) maxi = abs(drdy(i,j))
        end do
    end do
    
    !!!
    !!! Dimensionnement a r8prem()
    !!!
    
    mini=r8prem()*maxi
    
    do i=1,nr
        do j=1,nr
            if(abs(drdy(i,j)).lt.mini)drdy(i,j)=0.d0
        end do
    end do
    
    !!!
    !!! Separation des termes du jacobien
    !!!
    
    do i=1,ndt
        do j=1,ndt
            jss(i,j)=drdy(i,j)
        end do
    end do
    
    do i=1,3
        do j=1,ndt
            jsz(j,i)=drdy(j,ndt+i)
            jzs(i,j)=drdy(ndt+i,j)
        end do
    end do
    
    do i=1,3
        do j=1,3
            jzz(i,j)=drdy(ndt+i,ndt+j)
        end do
    end do
    
    call lcopli('ISOTROPE', '3D      ', mater, hook)
    
    !!!
    !!! Prise en compte du terme non lineaire E(i1) avec i1 = -trace(sig) 
    !!! car convention meca. sol
    !!!
    
    i1=-(sigf(1)+sigf(2)+sigf(3))
    patm=mater(1,2)
    nelas=mater(2,2)
    coefnl=(i1/(3.d0*patm))**nelas
    mue=mater(4,1)
    mu=-mue*coefnl
    
    !!!
    !!! Mise a l'echelle
    !!!
    
    coefnl=coefnl/mu
    call lcprsm(coefnl,hook,hooknl)
    
    !!!
    !!! Construction du tenseur tangent
    !!!
    
    !!! Inversion du terme jzz
    call r8inir(9,0.d0,invjzz,1)
    
    do i=1,3
        invjzz(i,i)=1.d0
    end do
    
    call mgauss('NCVP',jzz,invjzz,3,3,3,det,iret)
    
    if (iret.gt.0) call r8inir(9, 0.d0, invjzz, 1)
    
    !!!! Produit du terme jzz**-1 * jzs = j3x6
    call prmama(1,invjzz,3,3,3,jzs,3,3,ndt,j3x6,3,3,ndt,ier)
    
    if (ier.gt.0) write(6,*)'ECHEC AVEC PRMAMA 1'
    
    !!! Produit du terme jsz*(jzz)**-1 * jzs = jsz*j3x6 = j6x6
    call prmama(1,jsz,6,ndt,3,j3x6,3,3,ndt,j6x6,6,ndt,ndt,ier)
    
    if (ier.gt.0) write(6,*)'ECHEC AVEC PRMAMA 2'
    
    !!! Difference de matrice (jss - j6x6) = dijaco
    call lcdima(jss,j6x6,dijaco)
    
    !!! Inversion du terme dijaco
    call lcinma(0.d0,invdij)
    
    do i=1,ndt
        invdij(i,i)=1.d0
    end do
    
    call mgauss('NCVP',dijaco,invdij,6,ndt,ndt,det,iret)
    
    if (iret.gt.1) call lceqma(hook,dsde)
    
    !!! Construction de dsde = indiv*hooknl
    call lcprmm(invdij,hooknl,dsde)

end subroutine
