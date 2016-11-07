subroutine srdlam(varv,nbmat,mater,deps,depsv,dgamv,im,sm,vinm,nvi,de,&
                  ucrip,seuilp,gp,devgii,paraep,varpl,dfdsp,dlam)

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
!!! MODELE LKR : VALEUR DE L ACCROISSEMENT DU MULTIPLICATEUR PLASTIQUE
!!!

! ===================================================================================
! IN  : VARV   : INDICATEUR CONTRACTANCE OU DILATANCE
!     : NBMAT  : NOMBRE DE PARAMETRES MATERIAU
!     : MATER  : COEFFICIENTS MATERIAU A T+DT
!                   MATER(*,1) = CARACTERISTIQUES ELASTIQUES
!                   MATER(*,2) = CARACTERISTIQUES PLASTIQUES
!     : DEPS   : ACCROISSEMENT DES DEFORMATIONS A T
!     : DEPSV  : ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUE A T
!     : DGAMV  : ACCROISSEMENT DE GAMMA VISCOPLASTIQUE
!     : IM     : INVARIANT DES CONTRAINTES A T
!     : SM     : DEVIATEUR DES CONTRAINTES A T
!     : VINM   : VARIABLES INTERNES
!     : DE     : MATRICE ELASTIQUE
!     : UCRIP  : VALEUR DE U POUR LES CONTRAINTES A L INSTANT MOINS
!     : SEUILP : VALEUR DU SEUIL PLASTIAQUE A L INSTANT MOINS
! OUT : DLAM   : VALEUR DE DELTA LAMBDA ELASTOPLASTIQUE
! ===================================================================================

    implicit   none

#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/srdepp.h"
#include "asterfort/srdfdx.h"
#include "asterfort/srdpdt.h"
#include "asterfort/srdfdt.h"
#include "asterfort/r8inir.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: varv,nbmat,nvi
    real(kind=8) :: sm(6),deps(6),depsv(6),mater(nbmat,2),coupl
    real(kind=8) :: dgamv,gp(6),devgii,paraep(3),varpl(4),dfdsp(6)
    real(kind=8) :: vinm(nvi),dlam,de(6,6),ucrip,seuilp,im
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndi,ndt,i
    real(kind=8) :: degp(6),dfdegp,derpar(3),dfdxip,dfdsig,kron(6),tdfdsp,tft
    real(kind=8) :: sigint(6),sigv(6),sigt(6),dpdt(3),dfdt,alpha,dtemp,kk
    common /tdim/ ndt,ndi
    
    data kron /1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    
    !!!
    !!! Cotnrainte intermediaire
    !!!
    
    call lcprmv(de,deps,sigt)
    call r8inir(6,0.d0,sigint,1)
    call lcprmv(de,depsv,sigv)
  
    do i=1,ndt
        sigint(i)=sigt(i)-sigv(i)
    end do
    
    !!!
    !!! Recuperation de dfp/dxip
    !!!
    
    call srdepp(vinm,nvi,nbmat,mater,paraep,derpar)
    call srdfdx(nbmat,mater,ucrip,im,sm,paraep,varpl,derpar,dfdxip)
    
    !!!
    !!! Produit de:gp
    !!!
    
    call r8inir(6,0.d0,degp,1)
    call lcprmv(de,gp,degp)
    
    !!!
    !!! Produit (dfp/dsig):(de:gp)
    !!!
    
    call lcprsc(dfdsp,degp,dfdegp)
    
    !!!
    !!! Produit (dfp/dsig):(de:(deps-depsv))
    !!!
    
    call lcprsc(dfdsp,sigint,dfdsig)
    
    !!!
    !!! Terme dependant de T
    !!!
    
    !!! df/dT
    call srdpdt(vinm,nvi,nbmat,mater,paraep,dpdt)
    call srdfdt(nbmat,mater,ucrip,im,sm,paraep,varpl,dpdt,dfdt)
    
    
    kk=mater(12,1)
    alpha=mater(3,1)
    dtemp=mater(11,1)
    
    !!! Produit de df/dsig par kron
    call lcprsc(dfdsp,kron,tdfdsp)
    
    !!! Assemblage des termes dependant de T
    tft=(dfdt+3.d0*kk*alpha*tdfdsp)*dtemp
    
    !!!
    !!! Calcul de dlambda
    !!!
    
    coupl=mater(28,2)
    
    if ((varv.eq.1).and.(coupl.ge.1.d0/2.d0)) then
        dlam=(seuilp+dfdsig+dfdxip*dgamv+tft)/(dfdegp-dfdxip*sqrt(2.d0/3.d0)*devgii)
    else
        dlam=(seuilp+dfdsig+tft)/(dfdegp-dfdxip*sqrt(2.d0/3.d0)*devgii)
    endif
    
    if (dlam.lt.0.d0) then
        dlam=0.d0
    endif

end subroutine
