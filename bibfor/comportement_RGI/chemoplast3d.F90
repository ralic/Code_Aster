subroutine chemoplast3d(xmat, nmat, var0, varf, nvari,&
                        dt, depst, nstrs, sigf, mfr,&
                        errb3d, teta1, teta2, fl3d, ifour,&
                        istep)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      sous programme de calcul des contraintes par drucker prager
!       non associee avec hydratation et ecrouissage lineaire
!=====================================================================
    implicit none
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/hydr_xmat.h"
#include "asterfort/hydr_vari.h"
#include "asterfort/transpos1.h"
#include "asterfort/b3d_l3.h"
#include "asterfort/x33x6.h"
#include "asterfort/elas_iso_3d.h"
#include "asterfort/cohe_3d.h"
#include "asterfort/df_3d.h"
#include "asterfort/dps_3d.h"
#include "asterfort/dev_3d.h"
#include "asterfort/dp_3d.h"
#include "asterfort/j2d_i1_3d.h"
#include "asterfort/utmess.h"
!      declaration externe
    integer :: nstrs
    integer :: nvari
    integer :: nmat
    real(kind=8) :: xmat(nmat)
    real(kind=8) :: var0(nvari)
    real(kind=8) :: varf(nvari)
    real(kind=8) :: dt
    real(kind=8) :: depst(nstrs)
    real(kind=8) :: sigf(nstrs)
    integer :: mfr
    integer :: errb3d
    real(kind=8) :: teta1
    real(kind=8) :: teta2
    aster_logical :: fl3d
    integer :: ifour
    integer :: istep
!
!   ***********************************************************************
!     declaration locale
    integer :: nmelast, erreur, i, iter1
!     deformation totale   (rem les depst castem sont des gammas)    
    real(kind=8) :: depst6(6), dsige6(6), sig6(6), dev6(6), dps06(6), dsigp6(6)
    real(kind=8) :: dps16(6), depspl6(6), dspl6(6), deps16(6)
    real(kind=8) :: e0, xnu0, cohe0, delta0, h0, beta0, hydra1, hydra0, hydras, e1, cohe1
    real(kind=8) :: cohemin1, cohe2, delta1, h1, h2, beta1, beta2, xnu1, epse0
    real(kind=8) :: epse2, epse1, delta3, beta3, delta2, cohe3, h3, xi1, f0, cohe4, delta4, beta4
    real(kind=8) :: h4, df1, dcde, coeff4, denom4, dlambda, depse, xj2d
!     elasticite isotrope
    parameter (nmelast=4)
!   ***********************************************************************
!      print*,xmat
!     young
    e0=xmat(1)
!     poisson
    xnu0=xmat(2)
    if (xnu0 .gt. 0.49) then
        print*,'Coeff de Poisson trop grand dans endo13d'
        errb3d=1
        call utmess('F', 'COMPOR1_90')
    end if
!     cohesion
    cohe0=xmat(nmelast+1)
!     angle Drucker Prager critere
    delta0=xmat(nmelast+2)
!     Module d ecrouissage
    h0=xmat(nmelast+3)
!     Parametre de dilatance
    beta0=xmat(nmelast+4)
!     caracteristiques de l hydratation en fin de pas
    hydra1=xmat(nmelast+5)
!     recuperation de l hydratation debut de pas
    hydra0=var0(14)
!     actualisation hydratation fin de pas
    varf(14)=hydra1      
!     seuil d hydratation
    hydras=xmat(nmelast+6)    
!   ***********************************************************************
!     prise en compte de l hydratation sur les variables materiau
!     recuperation des caracteristiques de l hydratation
!     effet de l hydratation sur Young
!      print*,'Av hydr_xmat endo3d',E00,e0,hydra1,hydras,0.66d0,erreur
    erreur=0
    call hydr_xmat(e0, e1, hydra1, hydras, 0.5d0,&
                   erreur)
    call hydr_xmat(cohe0, cohe1, hydra1, hydras, 1.d0,&
                   erreur)
!     initialisation cohesion minimale      
    cohemin1=cohe1/1000.d0 
    cohe2=cohe1      
    call hydr_xmat(delta0, delta1, hydra1, hydras, 1.d0,&
                   erreur)
    delta2=delta1      
    call hydr_xmat(h0, h1, hydra1, hydras, 1.d0,&
                   erreur)
    h2=h1      
    call hydr_xmat(beta0, beta1, hydra1, hydras, 1.d0,&
                   erreur)
    beta2=beta1      
!     effet de l hydratation sur Poisson neglige (comme dans cendo3d car
!     significatif que avant le seuil
    xnu1=xnu0
!     recuperation de la deformation plastique equivalente
    epse0=var0(7)
!     actualisation par l hydratation
    call hydr_vari(epse0, epse1, hydra0, hydra1, hydras,&
                   erreur)
!     initialisation def plast cumulee
    epse2=epse1      
!     test pb hydratation      
    if (erreur .ne. 0) then
        print*,'pb hydratation dans endo3d'
        read*
        call utmess('F', 'COMPOR1_90')
    end if
!     ***********************************************************************
!      recuperation des increments de deformations
    if (mfr .ne. 33) then
!       on est pas en formulation poreux
!       print*,'increment de deformation'
        if (ifour .eq. 2) then
!        massif 3d non poreux
            do i = 1, 6
                depst6(i)=depst(i)
!         print*,'depst',i,'=',depst(i)
            end do
        else
!        deformation plane ou axisymetrique non poreux
            do i = 1, nstrs
                depst6(i)=depst(i)
            end do
            if (nstrs .lt. 6) then
                do i = (nstrs+1), 6
                    depst6(i)=0.d0
                end do
            end if
        end if
    else
        print*,'formulation poreux non achevee ds endo3d...'
        errb3d=1
        call utmess('F', 'COMPOR1_90')
    end if
!   ***********************************************************************
!      tir elastique
    call elas_iso_3d(depst6, e1, xnu1, dsige6)
    do i = 1, nstrs
        sig6(i)=var0(7+i)+dsige6(i)
!        print*,'sigf interface3d av ecoulement=',sig6(i)
    end do 
!      test du critere
!      cohesion actuelle si ecrouissage non nul
    call cohe_3d(delta2, beta2, cohe2, h2, cohemin1,&
                 epse2, delta3, beta3, cohe3, h3)
!      calcul des invariants
    call j2d_i1_3d(sig6, xj2d, xi1)
!      test de franchissement du critère de Drucker Prager 
!      et modif de la cohesion et des angles si necessaire
    call dp_3d(xj2d, xi1, cohe3, delta3, beta3,&
               h3, cohemin1, f0, cohe4, delta4,&
               beta4, h4)
!       print*, 'invariant', sqrt(xj2d),xi1,'critere',f0
!      ********************************************************************
!      iteration pour retourner sur la surface de charge
    iter1=1       
    if (f0 .gt. 0.d0) then
!       ecoulement plastique necessaire       
 10     continue 
!       calcul du deviateur
        call dev_3d(sig6, dev6, xi1)
!       calcul de df/dsigma pour le critere
        call dps_3d(dev6, xj2d, xi1, delta4, dps06)
!       calcul de la direction de l ecoulement plastique
        call dps_3d(dev6, xj2d, xi1, beta4, dps16)
!       calcul de l increment de contrainte plastique norme
!       passage des composantes hors digonales en gama
        do i = 1, 6
            if (i .le. 3) then
                deps16(i)=dps16(i)
            else
                deps16(i)=2.d0*dps16(i)
            end if
        end do
        call elas_iso_3d(deps16, e1, xnu1, dsigp6)
!       calcul de l increment plastique normé de la fonction de charge
        call df_3d(dps06, dsigp6, df1)
!       ********calcul de l ecrouissage*************************** 
        dcde=h4
        coeff4=sqrt(0.5d0+(beta4**2)/3.d0)
!       **********************************************************        
!       calcul du multiplicateur plastique
!       verification de la condition de positivite du multiplicateur plastique
        denom4=dcde*coeff4+df1
        if (denom4 .gt. 0.d0) then
            dlambda=f0/denom4
        else
            print*,'Ecrouissage negatif trop fort pour interface3d'
            erreur=1
            goto 20
        end if 
!        print*,'ecoulement plastique iter',iter1,'dlambda',dlambda        
!        print*,'dlambda',dlambda
!       calcul de l increment et de la deformation plastique
!       de l increment de la contrainte plastique
!       de la nouvelle contrainte totale
!       print*,'nouvelle contrainte', ' def plast'
        do i = 1, 6
            depspl6(i)=dlambda*dps16(i)
            dspl6(i)=dlambda*dsigp6(i)
            sig6(i)=sig6(i)-dspl6(i)
!          print*,sig6(i),depspl6(i)
        end do
        depse=dlambda*coeff4
!       calcul de la nouvelle deformation plastique cumulee        
        epse2=epse2+depse 
!       calcul de la nouvelle cohesion
        call cohe_3d(delta2, beta2, cohe2, h2, cohemin1,&
                     epse2, delta3, beta3, cohe3, h3)
!       calcul des invariants         
!       verif du critere apres ecoulement
        call j2d_i1_3d(sig6, xj2d, xi1)
!       test de franchissement du critère de Drucker Prager 
!       et modif de la cohesion et des angles si necessaire
!        print*,'f avant ecoulement',f0
        call dp_3d(xj2d, xi1, cohe3, delta3, beta3,&
                   h3, cohemin1, f0, cohe4, delta4,&
                   beta4, h4)
!        print*,' f apres ecoulement',f0
!       test de convergence        
        if (abs(f0) .gt. (1d-8*cohemin1)) then
            iter1=iter1+1
!        print*,'sous iteration',iter1
!        test nbr max d iterations         
            if (iter1 .lt. 100) then
                goto 10
            else
                print*,'nbre max d iter atteint ds interface3d :',100
            end if
        end if
    end if 
!
!     ********************************************************************
!     stockage de la deformation plastique actualisee
    varf(7)=epse2
!     affectation des contraintes dans le vecteur de sortie
    do i = 1, nstrs
        sigf(i)=sig6(i)
        varf(7+i)=sigf(i)
!        print*,'sigf interface3d=',sigf(i)
    end do
!      read*
    20    if (erreur.ne.0) then
    print*,'erreur dans endo3d'
    errb3d=1
end if
!****************************************************************************
!     recuperation des varibles internes en cas de calcul non local
    if (istep .eq. 1) then
!       on recopie les var0 ds les varf sauf celle a moyenner
        do i = 1, nvari
            if (i .ne. 7) then
                if (i .ne. 7) then
                    varf(i)=var0(i)
                else
                    var0(i)=varf(7)
                end if
            end if
        end do
    end if
end subroutine
