subroutine b3d_srf(sigal6,vss33,spl6,long3,eps23,wfeff3,wfm6,&
     vss33t,sigaf6,xnu0,e0,dtpic,rc,wref,tanphi,d66,erreur,dlt3,&
     ssl6,ept0,sigel6,sref,fl3d,eve6,vve6,sve6,y1sy,tau1,tau2,cc2,&
     teta1,eafluage,dt,vm6,maj0,depst6,sigef06,xloc)
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
!=====================================================================
!     calcul de la la contrainte de refermeture de fissure de traction
!     et de l'endommagement du contact à utiliser pour l'endo
!     en cisaillement et Poisson ds dt66
!     spl6 est la contrainte plastique de refermeture

!     attention a ne pas confondre siga (apparente) et sigat
!     (complementaire aux apparentes)
!     la notation est differente de celle de endo 3d

!     attention siga6 doit etre apparente aus sens de lendo
!     mais effective au sens de la poro mecanique
!=====================================================================
        implicit none
#include "asterfort/b3d_actherm.h"
#include "asterfort/x6x33.h"
#include "asterfort/b3d_chrep.h"
#include "asterfort/indice1.h"
#include "asterfort/x33x6.h"
#include "asterfort/b3d_flu1d.h"
#include "asterfort/b3d_relax1.h"
#include "asterf_types.h"
!     declaration externes
      real(kind=8) :: sigal6(6),vss33(3,3),spl6(6),dlt3(3),long3(3),eps23(3)
      real(kind=8) :: wfeff3(3),wfm6(6),vss33t(3,3),sigaf6(6),d66(6,6)
      real(kind=8) :: ssl6(6),sigel6(6),eve6(6),sve6(6),vve6(6),vm6(6)
      real(kind=8) :: depst6(6),sigef06(6),coeff1,veps1f,veps2f,eps11,deps_lim,sref,sref2
      real(kind=8) :: xnu0,e0,dtpic,rc,wref,tanphi,ept0,eafluage
      integer :: erreur
      aster_logical :: fl3d,maj0
!     declaration locales
      real(kind=8) :: x33(3,3),sigal33(3,3),sigef33(3,3),spl33(3,3)
      real(kind=8) :: sigaf33(3,3),wfm33(3,3),ssl33(3,3)
      real(kind=8) :: sigel33(3,3),eve33(3,3),sve33(3,3),vve33(3,3),vm33(3,3)
      integer :: i,j,k,l
      real(kind=8) :: s1,srfmin,tmax
      real(kind=8) :: sigmapic, xloc,deps2,deps1,deps0,tkl
      real(kind=8) ::tau2,tau1,eta1,eta2,xx1,s0,dsigma,dt,vsigma,eps10,veps10,veps20
      real(kind=8) :: referm3(3),dw3(3),deps6(6),deps33(3,3),sigef033(3,3)
      real(kind=8) :: e1,y1sy,teta1,cc2,tfiss,dmax,tlk,spl1,spl0,sref1,xx2
!***********************************************************************
!     expression des variables dans la base principale actuelle
!***********************************************************************
!     passage des contraintes apparentes(endo) effective (poro) en base prin d endo
      call x6x33(sigal6,x33)
      call b3d_chrep(sigal33,x33,vss33)
!     passage de la part non endo de la contrainte effective
!     en base prin d endo
      call x6x33(sigel6,x33)
      call b3d_chrep(sigel33,x33,vss33)
!     passage de la contrainte avant endo de Poisson
!     (ayant servi pour le fluage et l elasticite dans la zone non endo)
      call x6x33(sigef06,x33)
      call b3d_chrep(sigef033,x33,vss33)
!     passage de la deformation visco elastique de l etage 1 dans la base princ d endo
      if(fl3d) then
         call x6x33(eve6,x33)
         call b3d_chrep(eve33,x33,vss33)
         call x6x33(vve6,x33)
         call b3d_chrep(vve33,x33,vss33)
!        passage des vitesses de fluage de maxwell dans la fissure
         call x6x33(vm6,x33)
         call b3d_chrep(vm33,x33,vss33)
      end if
!     passage des contraintes de localisation precedente dans la base prin actuelle
      call x6x33(sve6,x33)
      call b3d_chrep(sve33,x33,vss33)
!     passage des contrainte plastique dans la fissure ds la base prin
!     actuelle
      call x6x33(spl6,x33)
      call b3d_chrep(spl33,x33,vss33)
!     passage des ouvertures precedentes ds la base prin d endo
      call x6x33(wfm6,x33)
      call b3d_chrep(wfm33,x33,vss33)
!     passage des seuils d endo localise dans la base prin d endo diffus
      call x6x33(ssl6,x33)
      call b3d_chrep(ssl33,x33,vss33)
!     expression des deformation reversibles pour les fissures localisees
!     comme depst6 contient des gama il faut dabord les repasser en epsilon
      do i=1,6
         if (i.le.3) then
             deps6(i)=depst6(i)
         else
             deps6(i)=depst6(i)*0.5d0
         end if
      end do
!     passage des increments de def en base princ d endo
      call x6x33(deps6,x33)
      call b3d_chrep(deps33,x33,vss33)
!***********************************************************************
!     initialisation des parametres
!***********************************************************************
!     contrainte de refermeture
      sref=max(sref,1.d-3*rc)
!     contrainte effective au pic de traction (debut de la localisation)
      sigmapic=e0*ept0
!     contrainte de reference pour prise en compte effet de Poisson
!dans les fissure refermees
      sref2=min(sref,sigmapic)
!***********************************************************************
!     calcul des ouvertures normales des fissures en visco
!elasticite non vieillissante
!***********************************************************************
      if(fl3d) then
!       dans le cas du fluage
        E1=y1sy*e0
!       prise en compte de l effet thermique sur les viscosités
!        print*,'cc',cc2
        call b3d_actherm(teta1,eafluage,E1,(cc2*e0*tau2/tau1),tau1,&
       eta1,eta2)
!       print*,'eta1',eta1,'eta2',eta2
!       read*
      end if
!*******************on boucle sur les 3 directions de fissuration ******
      do i=1,3
!     ******************************************************************
!      part de la contrainte de refermeture dans l effet de Poisson
!     ******************************************************************
       if(sigel33(i,i).ge.sigmapic) then
!          il faut attribuer tout l effet de Poisson des contrainte negative
!a ce qui est dans la fissure
           referm3(i)=1.d0
       else
           if(sigel33(i,i).le.-sref2) then
!             l effet de Poisson des contraintes negative est deja considere
!dans la contrainteeffective
              referm3(i)=0.d0
           else
!             on fait une interpolation lineaire pour recuperer
!             l effet de Poisson au fur et a mesure de la refermeture
              referm3(i)= (sigel33(i,i)+sref2)/(sigmapic+sref2)
           end if
       end if
!      desactivation du couplage de Poisson dans les fissures
       referm3(i)=0.d0
!      *****************************************************************
!      prise en compte de la part de deformation attribuable a la localisation
!      *****************************************************************
!      que si Dt->1 cad xx1=1
       xx1=max((dlt3(i)-dtpic),0.d0)/(1.d0-dtpic)

!      *****************************************************************
!      contrainte effective de localisation du pas precedent
!      attention pour b3d_relax1 s1 et s0 doivent être égales aux
!     contraintes effectives dans le materiau sain
       s0=sve33(i,i)
       s1=sigel33(i,i)
       if(maj0) then
!        1er passage dans endo3d on suppose s0=s1 pour tenir compte
!        d un eventuel chargement initial
         s0=s1
       end if
!      mise a jour de la contrainte de calcul d ouverture de fissure pour le pas suivant
       sve33(i,i)=s1
!      calcul de l increment de deformation totale du a l increment de contrainte
!  estimation du fluage dans la zone non endommagée tendue
       if(s1.lt.0.) then
             if(s0.gt.0.) then
                dsigma=-s0
             else
                dsigma=0.d0
             end if
       else
             if(s0.lt.0.) then
                 dsigma=s1
             else
                 dsigma=s1-s0
             end if
       end if
       if(fl3d) then
!         on ne calcule les increments d ouveture de fissure que si
!         la fissure est ouverte ou la contrainte s1 positive
          if(dt.ne.0.) then
             vsigma=dsigma/dt
          else
             vsigma=0.d0
          end if
!         calcul des increments de deformation pour les 3 etages
!         ATTENTION evolution hydratation negligee sur le pas
          eps10=eve33(i,i)
          if(s0.gt.0.) then
             veps10=vve33(i,i)
          else
             if(eps10.gt.0.) then
                 if (vve33(i,i).gt.0.) then
                     veps10=0.d0
                 else
                     veps10=vve33(i,i)
                 end if
             else
                 veps10=0.d0
             end if
          end if
          if(s0.gt.0.)then
             veps20=vm33(i,i)
          else
             veps20=0.d0
          end if
          call b3d_flu1d(vsigma,e0,dt,eps10,veps10,E1,eta1,eta2,&
         veps20,deps0,deps1,deps2,veps1f,veps2f)
!         print*,e0,E1,eta1,eta2,dt,s0,s1,eve33(i,i)
!         print*,'eps11',eps11
!         print*,'deps0,deps1,deps2',deps0,deps1,deps2
!         read*
       else
!         pas de fluage, seul l increment elastique est a considerer
          deps0=dsigma/e0
          deps1=0.d0
          deps2=0.d0
          eps11=0.d0
          veps1f=0.d0
          veps2f=0.d0
       end if
!      *****************************************************************
!      calcul de l increment d ouverture pour l element  totalement endommage
!      de taille unitaire
!      ouverture pour un element unitaire totalement fissuree
       dw3(i)=deps0+deps1+deps2
       goto 41
!      *****************************************************************
!      limitation ouverture pour une element unitaire partiellement fisssure
!      l increment d ouverture ne provient de la fissuration
!      que si Dt->1 cad xx=1
       deps_lim=deps33(i,i)
       if (deps_lim.ge.0.d0) then
!        cas ou l increment de deformation est positif
         if (dw3(i).gt.deps_lim) then
!          l increment d'ouverture ne peut pas être superieur a
!          l increment de deplacement sur l element s il s agit d une extension
!           print*,'limitation de l ouverture de fissure dans b3d_srf'
!           print*, 'dw/l',dw3(i),' eps lim',deps_lim,' direction',i
           coeff1=deps_lim/dw3(i)
         else
           if(dw3(i).le.0.d0) then
!            si l element s allonge la fissure ne peut pas se refermer
!             print*,'refermeture interdite'
             coeff1=0.d0
           else
             coeff1=1.d0
!             print*,'evolution  normale'
           end if
         end if
       else
!        cas ou l increment de deformation est negatif
         if (dw3(i).lt.deps_lim) then
!          l increment d'ouverture ne peut pas être superieur a
!          l increment de deplcement sur l element s il s agit d une refermeture
!           print*,'limitation de la refermeture de fissure dans b3d_srf'
           coeff1=deps_lim/dw3(i)
         else
           if(dw3(i).gt.0.d0) then
!            si l element se raccourci la fissure ne peut pas s ouvrir
             coeff1=0.d0
           else
             coeff1=1.d0
           end if
        end if
       end if
!      affectation de la limitation eventuelle
       dw3(i)=dw3(i)*coeff1
       deps0=deps0*coeff1
       deps2=deps2*coeff1
       deps1=deps1*coeff1
       veps1f=veps1f*coeff1
       veps2f=veps2f*coeff1
41     continue
!      *****************************************************************
!      limitation de la refermeture a zero
!      on garde l increment douverture s'il conduit à une ouverture positive
!       wfm33(i,i)=wfm33(i,i)+dw3(i)
!       goto 42
       if(wfm33(i,i)+dw3(i).lt.0.d0) then
         if(wfm33(i,i).le.0.d0) then
           dw3(i)=0.d0
           wfm33(i,i)=0.d0
           deps0=0.d0
           deps2=0.d0
           deps1=0.d0
           veps1f=0.d0
           veps2f=0.d0
          else
!          on ajuste l increment pour refermer la fissure
!          et on met les vitesse a zero pour le pas suivant
           if(dw3(i).ne.0.d0) then
            coeff1=-(wfm33(i,i)/dw3(i))
           else
            coeff1=0.d0
           end if
           dw3(i)=dw3(i)*coeff1
           wfm33(i,i)=0.d0
           deps0=deps0*coeff1
           deps2=deps2*coeff1
           deps1=deps1*coeff1
           veps1f=0.d0
           veps2f=0.d0
          end if
       else
!          mise a jour de l'ouverture de fissure localisee maxi si post pic et endo a un
!          pour un element de taille unitaire
           wfm33(i,i)=wfm33(i,i)+dw3(i)
       end if
!      *****************************************************************
!      maj des variables internes pour la fissuration
!      mise a jour de la deformation de l etage de Kelvin voigt dans les variables internes
       eve33(i,i)=eve33(i,i)+deps1
!      mise a jour des vitesses de deformation visqueuses pour Kelvin et Maxwell
       do j=1,3
         if (i.eq.j) then
             vve33(j,i)= veps1f
             vm33(j,i) = veps2f
          else
             vve33(j,i)=0.d0
             vm33(j,i) =0.d0
          end if
       end do
!      *****************************************************************
!      ouverture de fissure actuelle
!      *****************************************************************
!      stockage de l ouverture dans la direction principale actuelle des contraintes
!      pour l etat d endo actuel et la direction principale  actuelle avec
!      prise en compte de la longueur de l element
       wfeff3(i)=max(wfm33(i,i),0.d0)*long3(i)*xx1

!      *****************************************************************
!      calcul des contraintes normales dans la fissures si neccessaire
!      *****************************************************************

       if (ssl33(i,i).ge.sigmapic) then
!        actualisation de l indicateur de localisation pour le calcul non local
         xloc=1.d0
!        contrainte de refermeture  de reference
!        forme exponetielle
!         xx2=exp(-wfeff3(i)/wref)
!        forme hyperbolique
         xx2=wref/(wfeff3(i)+wref*sref/e0)
         sref1=sref*xx2
!         goto 43
!        calcul de la contrainte dans la fissure avec la contrainte plastique precedente
         if(fl3d) then
!          relaxation de la contrainte plastique dans la fissure refermee
           spl0=spl33(i,i)
!           if((spl0.gt.0.).and.((s1-spl0).lt.0.)) then
            call b3d_relax1(spl0,s0,s1,vsigma,eta1,E1,eta2,e0,&
           dt,sref1,spl1,dw3(i),sref,wref,wfeff3(i),long3(i),&
           xx1,dlt3(i),dtpic)
!           else
!            spl1=spl0
!            print*, 'pas de relaxation'
!           end if
!***********************************************************************
!    on debranche la refermeture si le fluage est active
           spl1=0.d0
!    la contrainte dans la fissure n est negative que si le squelette solide
!    est comprimee
           sref1=min(sigel33(i,i),0.d0)
!************************************************************************
           spl33(i,i) = max(spl1,0.d0)
!           print*,'spl0 ds b3d_srf dirrection',i,spl0,' spl1',spl1
!           print*,spl0,s0,vsigma,eta1,E1,eta2,e0,dt,spl1
         end if
         sigef33(i,i)=sigel33(i,i)-spl33(i,i)
!        sref1=0.d0
!        cas ou la contrainte en zone non fissuree est de traction
         if(sigel33(i,i).gt.0.d0) then
!          ecoulement plastique si necessaire
           if (sigef33(i,i).lt.0.d0) then
!            compression autorisee si la vitesse de deformation est negative
!            compression autorisee limitee par la contrainte de refermeture
             if(sigel33(i,i).gt.0.d0) then
                srfmin=-sref1
             else
!           print*,'seff localise <0 ds b3d_srf'
                srfmin=sigel33(i,i)-sref1
             endif
             if(sigef33(i,i).lt.srfmin) then
!               la contrainte est limitée par la fonction de refermeture
                sigef33(i,i)=srfmin
                spl33(i,i)=sigel33(i,i)-srfmin
                if(spl33(i,i).lt.0.d0) then
!                   la fissure est totalement refermee
                    sigef33(i,i)=sigel33(i,i)
                    print*,'on met spl a 0 ds b3d_srf'
                    spl33(i,i)=0.d0
                end if
             end if
           else
!            traction interdite dans la fissure
             spl33(i,i)=sigel33(i,i)
             sigef33(i,i)=0.d0
           end if
         else
!          on a passe le pic de traction mais on est revenu en compression dans cette direction
!          la contrainte plastique n evolue plus si la fissure est totalement refermee
!          calcul de la contrainte dans la fissure avec la contrainte plastique precedente
           srfmin=sigel33(i,i)-sref1
           if(sigef33(i,i).lt.srfmin) then
!               la contrainte est limitée par la fonction de refermeture
                sigef33(i,i)=srfmin
                spl33(i,i)=sigel33(i,i)-srfmin
                if(spl33(i,i).lt.0.d0) then
!                   la fissure est totalement refermee
                    sigef33(i,i)=sigel33(i,i)
                    spl33(i,i)=0.d0
                end if
           end if
         end if
       else
!       il n y a jamais eu de localisation sur cette facette
        if(sigel33(i,i).le.0.d0) then
          sigef33(i,i)=sigel33(i,i)
          spl33(i,i)=0.d0
        else
          sigef33(i,i)=0.d0
          spl33(i,i)=sigef33(i,i)
        end if
       end if
       if(spl33(i,i).lt.0.d0) then
!        print*,'spl33 ds b3d_srf', spl33(i,i)
        spl33(i,i)=0.d0
        sigef33(i,i)=sigel33(i,i)
       end if
      end do
!***********************************************************************
!     stockage des variables internes pour le calcul d ouverture en base fixe
!***********************************************************************
!     retour de l ouverture de fissure en base fixe
      call b3d_chrep(x33,wfm33,vss33t)
      call x33x6(x33,wfm6)
!     retour de la contrainte de calcul d ouverture en base fixe
      call b3d_chrep(x33,sve33,vss33t)
      call x33x6(x33,sve6)
!     retour des deformations et vitesses visco plastique au droit des
!fissures en base fixe
      if(fl3d) then
!        retour de la deformation visco elastique en base fixe
         call b3d_chrep(x33,eve33,vss33t)
         call x33x6(x33,eve6)
!        retour de la vitesse de fluage de kelvin en base fixe
         call b3d_chrep(x33,vve33,vss33t)
         call x33x6(x33,vve6)
!        retour de la vitesse de fluage de maxwell en base fixe
         call b3d_chrep(x33,vm33,vss33t)
         call x33x6(x33,vm6)
      end if
!***********************************************************************
!     calcul des contraintes normales apparentes et effets de Poisson des refermetures
!     DESACTIVE
!***********************************************************************
      do i=1,3
        call indice1(i,k,l)
        sigaf33(i,i)=d66(i,i)*sigef33(i,i)&
       +xnu0*(sigef33(k,k)*d66(k,k)*referm3(k)+&
       sigef33(l,l)*d66(l,l)*referm3(l))
      end do
!***********************************************************************
!     calcul des contraintes tangentes et ecoulements associes
!***********************************************************************
!     recuperation des contraintes tangentes effectives dans
!     les fissures
!     si elles sont refermees et correction en fonction du frottement
!     admissible
      do i=1,3
       call indice1(i,k,l)
!      valeur de la contrainte tangente avant ecoulement plastique
       sigef33(k,l)=sigel33(k,l)-spl33(k,l)
!      condition de frottement (incluant interlocking)
!      contraintes tangentes totales admissibles
       tkl=abs(sigel33(k,l))*(1.d0-d66(k,k))+&
!     #     d66(k,k)*abs(min(sigef33(k,k),0.d0))*tanphi
          abs(min(sigaf33(k,k),0.d0))*tanphi
       tlk=abs(sigel33(k,l))*(1.d0-d66(l,l))+&
!     #     d66(l,l)*abs(min(sigef33(l,l),0.d0))*tanphi
          abs(min(sigaf33(l,l),0.d0))*tanphi
       dmax=max(d66(k,k),d66(l,l))
!      contrainte admissible sur la facette la plus defavorisee
       tmax=min(tkl,tlk)
!      contrainte admissible correspondante sur la facette la plus fissuree
       if(dmax.gt.1d-5) then
        tfiss=(tmax-abs(sigel33(k,l))*(1.d0-dmax))/dmax
!       comparaison avec la contrainte dans la fissure
        if(sigef33(k,l).gt.0.d0) then
          if (sigef33(k,l).gt.tfiss) then
               sigef33(k,l)=tfiss
               spl33(k,l)=sigel33(k,l)-tfiss
          end if
        else
          if (sigef33(k,l).lt.(-tfiss)) then
               sigef33(k,l)=-tfiss
               spl33(k,l)=sigel33(k,l)+tfiss
          end if
        end if
       end if
!      symetrisation du tenseur des contraintes de refermeture de
!      fissure
       sigef33(l,k)=sigef33(k,l)
       spl33(l,k)=spl33(k,l)
!       print*,'tmax',tmax,'t el',sigel33(k,l),' tfiss',sigef33(l,k),
!     # 'sn',sigef33(i,i),'dmax',dmax
!      contrainte tangentes apparentes fonction de l'endo maxi
       sigaf33(k,l)=sigef33(k,l)*dmax
       sigaf33(l,k)=sigaf33(k,l)
      end do
!***********************************************************************
!     retour des contraintes de refermetures et plastiques en base fixe
!***********************************************************************
!     retour  des contraintes dans la fissure en base fixe
      call x33x6(sigaf33,sigaf6)
      call x6x33(sigaf6,x33)
      call b3d_chrep(sigaf33,x33,vss33t)
      call x33x6(sigaf33,sigaf6)
!     retour de la contraintes plastiques dans la fissures en base fixe
      call b3d_chrep(x33,spl33,vss33t)
      call x33x6(x33,spl6)
end subroutine
