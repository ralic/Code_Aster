subroutine srdgde(val, vintr, dt, seuive, ucrim,&
                  im, sm, vinm, nvi, nbmat, mater,&
                  tmp, depsv, dgamv, retcom)

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
!!! MODELE LKR : DEF. DE LA DEFORMATION VISCO. ET DU PARAMETRE D'ECROUISSAGE VISCO.
!!!

! ===================================================================================
! IN  : VAL            : INDICATEUR POUR LES LOIS DE DILATANCE
!     : VINTR          : INDICATEUR CONTRACTANCE OU  DILATANCE
!     : DT             : PAS DE TEMPS
!     : SEUIVE         : SEUIL VISCO. EN FONCTION DE LA PREDICITION
!     : UCRIM          : EN FONCTION DES CONTRAINTES A LINSTANT MOINS
!     : IM             : INVARIANT DES CONTRAINTES A L INSTANT MOINS
!     : SM(6)          : DEVIATEUR DES CONTRAINTES A L INSTANT MOINS
!     : VINM(NVI)      : VARIABLES INTERNES
!     : NVI            : NOMBRE DE VARIABLES INTERNES
!     : NBMAT          : NOMBRE DE PARAMETRES MATERIAU
!     : MATER(NBMAT,2) : COEFFICIENTS MATERIAU A T + DT
!                           MATER(*,1) = CARACTERISTIQUES ELASTIQUES
!                           MATER(*,2) = CARACTERISTIQUES PLASTIQUES
!     : TMP            : TEMPERATURE A L'INSTANT - OU +
! OUT : DEPSV(6)       : DEFORMATIONS VISCO.
!     : DGAMV          : VARIABLES D ECROUISSAGE VISQUEUX
!     : RETCOM         : CODE RETOUR POUR REDECOUPAGE DU PAS DE TEMPS
! ===================================================================================

    implicit    none

#include "asterfort/lcdevi.h"
#include "asterfort/srbpri.h"
#include "asterfort/srcalg.h"
#include "asterfort/srcaln.h"
#include "asterfort/srdfds.h"
#include "asterfort/srdhds.h"
#include "asterfort/srds2h.h"
#include "asterfort/srvacv.h"
#include "asterfort/srvarv.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat,retcom,val,nvi
    real(kind=8) :: seuive,ucrim,im,sm(6),vintr
    real(kind=8) :: mater(nbmat,2),vinm(nvi),depsv(6),dgamv,dt,tmp
    
    !!!
    !!! Variable locales
    !!!
    
    integer :: i, ndi,ndt
    real(kind=8) :: a,n,pa,bidon,paravi(3),varvi(4)
    real(kind=8) :: dhds(6),ds2hds(6),dfdsv(6),bprime,vecnv(6),gv(6),ddepsv(6)
    real(kind=8) :: tpp,trr,av0,r,z
    common /tdim/ ndt,ndi
    
    !!!
    !!! Recuperation des temperatures
    !!!
    
    tpp=mater(7,1)
    trr=mater(8,1)
    
    !!!
    !!! Recuperation des parametres materiaux
    !!!
    
    !!! parametres a T0
    pa=mater(1,2)
    av0=mater(16,2)
    n=mater(17,2)
    r=8.3144621d0
    z=mater(27,2)
    
    !!! parametres a T
    !!! if obligatoire car en meca. pure Tp et Tref sont nulles sonc / 0
    if ((tpp.ge.trr).and.(trr.gt.0.d0)) then
        a=av0*exp(-z/r/tpp*(1.d0-tpp/trr))
    else
        a=av0
    endif
    
    !!!
    !!! Calcul de dfvp/dsig
    !!!
    
    call srdhds(nbmat,mater,sm,dhds,retcom)
    call srds2h(nbmat,mater,sm,dhds,ds2hds,retcom)
    call srvarv(vintr,nbmat,mater,tmp,paravi)
    call srvacv(nbmat,mater,paravi,varvi)
    call srdfds(nbmat,mater,paravi,varvi,ds2hds,ucrim,dfdsv)
    
    !!!
    !!! Calcul de n
    !!!
    
    bprime=srbpri(val,vinm,nvi,nbmat,mater,paravi,im,sm,tmp)
    call srcaln(sm,bprime,vecnv,retcom)
    
    !!!
    !!! Calcul de gvp
    !!!
    
    call srcalg(dfdsv,vecnv,gv,bidon)
    
    !!!
    !!! Calcul de dgamv
    !!!
    
    do i=1,ndt
        if (seuive.le.0.d0) then
            depsv(i)=0.d0
        else
            depsv(i)=a*((seuive/pa)**n)*gv(i)*dt
        endif
    end do
    
    call lcdevi(depsv,ddepsv)
    
    dgamv=0.d0
    
    do i = 1, ndt
        dgamv=dgamv+ddepsv(i)**2.d0
    end do
    
    dgamv=sqrt(2.d0/3.d0*dgamv)

end subroutine
