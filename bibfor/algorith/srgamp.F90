subroutine srgamp(val, varv, im, sm, ucrip,&
                  seuilp, vinm, nvi, nbmat, mater, de,&
                  deps, depsv, dgamv, depsp, dgamp, retcom)

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
!!! MODELE LKR : CALCUL DE DEPSP ET DE DGAMP
!!!

! ===================================================================================
! IN  : VAL            : INDICATEUR POUR DISTINGUER LES LOIS DE DILATANCE
!     : VARV           : INDICATEUR CONTRACTANCE OU DILATANCE
!     : IM             : INVARIANT DES CONTRAINTES A T
!     : SM(6)          : DEVIATEUR DES CONTRAINTES A T
!     : UCRIP          : VALEUR DE U POUR LES CONTRAINTES A L INSTANT MOINS
!     : SEUILP         : VALEUR DU SEUIL PLASTIQUE A L INSTANT MOINS
!     : VINM(NVI)      : VARIABLES INTERNES
!     : NVI            : NOMBRE DE VARIABLES INTERNES
!     : NBMAT          : NOMBRE DE PARAMETRES MATERIAU
!     : MATER(NBMAT,2) : COEFFICIENTS MATERIAU A T + DT
!                           MATER(*,1) = CARACTERISTIQUES ELASTIQUES
!                           MATER(*,2) = CARACTERISTIQUES PLASTIQUES
!     : DE(6,6)        : MATRICE ELASTIQUE
!     : DEPS(6)        : INCREMENT DEFORMATIONS TOTALES
!     : DEPSV(6)       : ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUES A T
!     : DGAMV          : ACCROISSEMENT DE GAMMVP
! OUT : DEPSP(6)       : DEFORMATIONS PLASTIQUES
!     : DGAMP          : VARIABLE D ECROUISSAGE PLASTIQUES
!     : RETCOM         : CODE RETOUR POUR REDECOUPAGE DU PAS DE TEMPS
! ===================================================================================

    implicit    none

#include "asterfort/lcdevi.h"
#include "asterfort/srbpri.h"
#include "asterfort/srcalg.h"
#include "asterfort/srcaln.h"
#include "asterfort/srdfds.h"
#include "asterfort/srdhds.h"
#include "asterfort/srdlam.h"
#include "asterfort/srds2h.h"
#include "asterfort/srvacp.h"
#include "asterfort/srvarp.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat,val,varv,retcom,nvi
    real(kind=8) :: im,sm(6),mater(nbmat,2),vinm(nvi),depsp(6),deps(6),depsv(6)
    real(kind=8) :: dgamp,dgamv,de(6,6),ucrip,seuilp
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: i,ndi,ndt
    real(kind=8) :: paraep(3),varpl(4),dhds(6),ds2hds(6), dfdsp(6), ddepsp(6)
    real(kind=8) :: vecnp(6),gp(6),bprimp,dlam,devgii,tmm
    common /tdim/ ndt,ndi
    
    !!!
    !!! Recuperation des temperatures et des increments
    !!!
    
    tmm=mater(6,1)
    
    !!!
    !!! Calcul de dfp/dsig
    !!!
    
    call srdhds(nbmat,mater,sm,dhds,retcom)
    call srds2h(nbmat,mater,sm,dhds,ds2hds,retcom)
    call srvarp(vinm,nvi,nbmat,mater,tmm,paraep)
    call srvacp(nbmat,mater,paraep,varpl)
    call srdfds(nbmat,mater,paraep,varpl,ds2hds,ucrip,dfdsp)
    
    !!!
    !!! Calcul de gp
    !!!
    
    bprimp=srbpri(val,vinm,nvi,nbmat,mater,paraep,im,sm,tmm)
    call srcaln(sm,bprimp,vecnp,retcom)
    call srcalg(dfdsp,vecnp,gp,devgii)
    
    !!!
    !!! Calcul de dlambda
    !!!
    
    call srdlam(varv,nbmat,mater,deps,depsv,dgamv,im,sm,vinm,nvi,de,&
                ucrip,seuilp,gp,devgii,paraep,varpl,dfdsp,dlam)

    !!!
    !!! Calcul de dgamp
    !!!
    
    do i=1,ndt
        depsp(i)=dlam*gp(i)
    end do
    
    call lcdevi(depsp,ddepsp)
    
    dgamp=0.d0
    
    do i=1, ndt
        dgamp=dgamp+ddepsp(i)**2.d0
    end do
    
    dgamp=sqrt(2.d0/3.d0*dgamp)

end subroutine
