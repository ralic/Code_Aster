subroutine endo3d(xmat, nmat, var0, varf, nvari,&
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
!      sous programme de calcul des contraintes endommagees
!      calcule l increment elastique si cela n a pas deja
!      ete fait dans le module de fluage (fl3d a vrai)
!=====================================================================
    implicit none
#include "asterf_types.h"
#include "asterfort/hydr_xmat.h"
#include "asterfort/hydr_vari.h"
#include "asterfort/b3d_dth.h"
#include "asterfort/b3d_actherm.h"
#include "asterfort/b3d_pm.h"
#include "asterfort/b3d_elas.h"
#include "asterfort/b3d_sigd.h"
#include "asterfort/b3d_stock_tail.h"
#include "asterfort/b3d_l3.h"
#include "asterfort/b3d_nosnap.h"
#include "asterfort/b3d_partition.h"
#include "asterfort/b3d_sst.h"
#include "asterfort/b3d_sd.h"
#include "asterfort/b3d_d66.h"
#include "asterfort/x6x33.h"
#include "asterfort/b3d_chrep.h"
#include "asterfort/x33x6.h"
#include "asterfort/b3d_sigapp.h"
#include "asterfort/b3d_srf.h"
#include "asterfort/b3d_dc.h"
#include "asterfort/b3d_covs.h"
#include "asterfort/utmess.h"
!***********************************************************************
!      declaration externe
    integer :: nmat, nvari, nstrs, mfr, ifour, istep,i
    real(kind=8) :: depst(nstrs)
    real(kind=8) :: xmat(nmat), var0(nvari), varf(nvari), sigf(nstrs)
    real(kind=8) :: dt
    integer :: errb3d
    real(kind=8) :: teta1, teta2
    aster_logical :: fl3d
!***********************************************************************
!     dimension des tableaux d echange avec le modele d endommagement
!     cas des materiaux
!     dimension reelle
    integer :: nmelast, nmfluag, nmendo,nmhydra
!     cas des variables internes (cf idvar4)
!     dimensions reelles
    integer :: nvendo, nvfluag, nvhydra, nvtail
!     nbre de composantes materiau
!     pour plus de details cf idvisc
!     -elasticite isotrope
    parameter (nmelast=4)
!     -commune
    parameter (nmhydra=24)
!     -fluage
    parameter (nmfluag=5)
!     -endommagement
    parameter (nmendo=10)
!     nombre de composantes de varibles internes
!     -nombre de variable interne pour le modele de fluage
    parameter (nvfluag=48)
!     -nbre de variables internes communes
    parameter (nvhydra=29)
!     -nombre de variables internes pour le modele d endommagement
    parameter (nvendo=77)
!     -le stockage des vecteurs propres de la matrice de taille
    parameter (nvtail=10)
!***********************************************************************
!     declarations locale pour cendo3d
    real(kind=8) :: x6(6), x33(3, 3)
!     increments de deformation
    real(kind=8) :: depst6(6)
    real(kind=8) :: dgamd6(6), sigef6(6)
!     contraintes effectives
    real(kind=8) :: sigef3(3), vsigef33(3, 3), vsigef33t(3, 3), sigeft6(6)
    real(kind=8) :: sigefc6(6)
    real(kind=8) :: sigefc3(3), sigeft3(3), sigef33(3, 3)
    integer :: erreur
!     seuil diffus
    real(kind=8) :: ssg6(6)
!     taille des elements
    real(kind=8) :: t33(3, 3), n33(3, 3),regl0,fr2,fr10,xtaumin,fminvref
    real(kind=8) :: xk00,xmu00,e00,xx1,hydras,rc00,rt00,epc0,ept0,dt80,gft00
    real(kind=8) :: gfc00,wref0,tphi0,xktmp,beta1,gama1,xnu00,hydra1,hydra0
    real(kind=8) :: sref0,xs1,coefft,cg0,delta,e0,sref1,rt0,gft0
    real(kind=8) :: rc0,ept00,cnlt1,dpic0,coeff2,epsplg0,xwnsat0, poro0,xwnsat
    real(kind=8) :: delta0,aleas0,xktemp,gfc0,biot00,umbiot00,vw1,vw0
    real(kind=8) :: xwsat,xmsrt0,xmsrt1,sfld,vg0,vp0,taug0,xmg0, eafluage
    real(kind=8) :: xnu0,umbiot0,biot0,dflu0,dth0,pw1,pg1,bw1,etag1,etaw1,xumdft
    real(kind=8) :: pw0,dpw,epsvpw,bg1,xk0,xmu0,disolocal, aux3,rtw1,gftw1,aux4
    real(kind=8) ::xb,xwb0,coeffgf0,veq1,depsv,xvef,xli,dpict00,rtw2,errmesh
    real(kind=8) ::gftw2,eptw2,rteff,errmesh2,coeffgf2,suw1,sigmax,tref
    real(kind=8) :: tau0,tau1,dtaueq,tequ1,xtau2,xtau1,cc2,teta12,xloc
    real(kind=8) ::sigseuil, dsigtmp0,taueq1,y1sy,tau2,dvfiss0,dpw1,suc0,dc0,dcf,umd0
    real(kind=8) ::dsigtmp1
!     fin des tabelaux de fluage et hydratation
    integer :: nmat0, nvar0
!     fin des tableaux fluage hydratation et taille
    integer :: nmat1, nvar1
!     traitement de la ocalisation
    aster_logical :: local
    aster_logical :: vrai
!     vecteur propre des tailles des element
    real(kind=8) :: vt33(3, 3)
!     contraintes a utiliser pour actualiser les seuils
!     taille dans les directions principales d endommagement
    real(kind=8) :: l3(3)
!     gestion du seuil de fissuration et de la densite de fissuration
    real(kind=8) :: d03(3), df3(3), st3(3), vss33(3, 3), vss33t(3, 3)
!     localisation et vitesse d endo, def a rupture, resist residuelle
    real(kind=8) :: e23(3), rapp6(6)
!     nombre de fissure par metre en fissuration diffuse
    real(kind=8) :: nfid1
!     controle de la recuperation de la resistance,
!     et controle de compatibilite de la resistance
    aster_logical :: rrr, dldd
!     seuil pour l'endo des coeffs de Poisson
    real(kind=8) :: ssp6(6)
!     matrice d endommagement
    real(kind=8) :: d66(6, 6)
!     contrainte apparente
    real(kind=8) :: siga6(6)
!     contrainte effective apres endo du coeff de Poisson
    real(kind=8) :: sigep6(6)
!     contrainte totale non endommagee
    real(kind=8) :: siget6(6)
!     contrainte effective apres attenuation des coeffs de Poisson
!     seuils de traction pour l endo localise des modules
    real(kind=8) :: ssl6(6)
!     contrainte plastique dans les fissures localisees
    real(kind=8) :: spl6(6)
!     ouvertures de fissure actuelles dans les directions principales
    real(kind=8) :: wfeff3(3)
!     ouverture maximales
    real(kind=8) :: wfm6(6)
!     contrainte effective reprise par les fissures refermees
    real(kind=8) :: sigaf6(6)
!     contrainte apparente de traction en base fixe
    real(kind=8) :: sigat6(6)
!     diagonalisation des contraintes effective endommage par la tractio
    real(kind=8) :: siga3(3), vsiga33(3, 3), vsiga33t(3, 3)
    real(kind=8) :: sigac6(6), sigac3(3), sigat3(3)
!     longueur des elements pour l'endo de compression
    real(kind=8) :: long3(3)
!     endo localise de traction calcule avec contraintes en poisson deco
    real(kind=8) :: dfl3(3), e2l3(3), dl66(6, 6)
!     contrainte effective endo localise
    real(kind=8) ::  sigal33(3, 3)
!     varaiation de volume
    real(kind=8) :: dv0
!     part de la contrainte totale due aux pressions
    real(kind=8) ::  dsw6(6), dsw33(3, 3)
!     deformation plastique dues au gel
    real(kind=8) :: epsplg6(6), epsplfg6(6)
!     deformation plastique dues a l eau
    real(kind=8) :: ssw6(6)
!     passage base prin de plasticite
    real(kind=8) :: vplg33(3, 3), vplg33t(3, 3)
!     passage base prin de plasticite
    real(kind=8) :: vssw33(3, 3), vssw33t(3, 3)
!     endommagements diffus hydriques et de gel
    real(kind=8) :: dw3(3), dg3(3)
!     Weibull
    real(kind=8) :: veq, xweibull, xwb1
    real(kind=8) :: vref0, vmax0, weib0, coeffGf
!     deformation visco elastique (kelvin voigt au droit des fissures)
    real(kind=8) :: eve6(6), vve6(6), sve6(6)
!     contrainte dans la fissure
    real(kind=8) :: sigarf6(6)
!     vitesse de maxwell dans les fissures de traction
    real(kind=8) :: vm6(6)
!     varaible logique pour savoir si on doit considerer s1 comme s0
!     dans les fissures pour le suivi de la contrainte viscoelastique
    aster_logical :: maj0
!     coeff d amplification maxi temporel
    real(kind=8) :: coefftmax0
!     tableaux pour stocker les contraintes modifiees par les autocontra
    real(kind=8) :: sigi6(6), sigit3(3), sigic3(3)
!
!     ******************************************************************
!     parametres fixes
!     valeur de de sigma residuelle / rt pour s>E*e2 (e2=def a rupture
!      post pic)
    parameter(beta1=5.d-3)
!     valeur mini de e2=gama1*e1(e1=rt/E)(distance relative / snap back)
    parameter(gama1=1.1d0)
!     coeff multiplicateur de la deformation au pic a partir duquel
!     l approximation pre pic passe de bilineaire à Weibull
    parameter(regl0=1.05d0)
!     passage de la parabole concave a la parabole convexe pour
!     eps=epic+(e2-epic)/reg10 (pour la compression)
    parameter(fr2=2.d0)
!     passage de la parabole concave a la parabole convexe pour
!     eps=epic+(e2-epic)/fr10 (pour la traction)
    parameter(fr10=100.d0)
!     valeur du taux de chargement minimal pour le calcul de le effet
!     d echelle probabiliste
    parameter(xtaumin=1.d-6)
!     valeur maxi du coeff d amplification temporel
    parameter(coefftmax0=100.d0)
!     Veq/vref minimale
    parameter(fminvref=1.e-6)
!***********************************************************************
!     initialisation des positions initiales des xmat et var0 pour
!     la lecture et l ecriture des variables du modele d endommagement
!     INDICE 0 = fin du fluage (debut endommagement-1)
!      print*,'fl3d',fl3d
      if (fl3d) then
       nmat0=nmelast+nmhydra+nmfluag
       nvar0=nvfluag+nvhydra
      else
       nmat0=nmelast+nmhydra
       nvar0=nvhydra
      end if
!     INDICE 1 = fin (fluage et endommagement) (debut taille-1)
      nmat1=nmat0+nmendo
      nvar1=nvar0+nvendo
!     initilialisation de la variable d erreur
      erreur=0
!     ******************************************************************
!     chargement des parametres du modele et prise en compte de l
!     l evolution de l hydratation sur les caracteristiques materiau
!     coeffs d elasticite isotrope pour le materiau effectif elastique
!     young
!***********************************************************************
!     affichage des parametres materiaux
!      print*,'on arrive dans endo3d'
!      do i=1,nmat
!       print*,'xmat(',i,')=', xmat(i)
!      end do
!     controle des entiers de position memoire
!      print*,'nmat0, nmat1, nvar0, nvar1'
!      print*, nmat0, nmat1, nvar0, nvar1
!     affichage des variables internes en debut de pas
!      print*,'nvari',nvari,'nstrs',nstrs,'mfr',mfr
!      do i=1,nvari
!        print*,'var0(',i,')=',var0(i),' varf(',i,')=',varf(i)
!      end do
!      read*
!***********************************************************************

!     recuperation du coeff de compressibilite draine
      xk00=xmat(nmelast+19)
      if(xk00.ne.0.) then
!      print*,'coeff elastique recalcules a partir de k et mu endo3d'
!      coefficient de cisaillement
       xmu00=xmat(nmelast+20)
!      recalcul des coeffs E et nu compatibles avec k et mu
       e00=9.d0*xk00/(1.d0+3.d0*xk00/xmu00)
       xx1=(2.d0*xmu00)/(3.d0*xk00)
       xnu00=(1.d0-xx1)/(2.d0+xx1)
      else
!       print*,'on adopte les coeff du modele elastique dans endo3d'
!      Young
       e00=xmat(1)
!      poisson
       xnu00=xmat(2)
      end if
!     test coeff de Poisson
      if (xnu00 .gt. 0.49) then
        print*,'Coeff de Poisson trop grand dans endo13d'
        errb3d=1
      call utmess('F','COMPOR1_90')
      end if
!     caracteristiques de l hydratation en fin de pas
      hydra1=xmat(nmelast+1)
!     stockage hydratation en variable interne pour le pas suivant
      varf(1)=hydra1
!     recuperation de l hydratation du pas precedent
      hydra0=var0(1)
!     seuil d hydratation
      hydras=xmat(nmelast+2)
!     resistance en compression
      rc00=xmat(nmelast+3)
!     resistance en traction
      rt00=xmat(nmelast+4)
!     deformation au pic de compression
      epc0=xmat(nmelast+5)
!     deformation au pic de traction
      ept0=xmat(nmelast+6)
!     endo thermique a 80°C
      dt80=xmat(nmelast+21)
!     energie de fissuration de traction
      gft00=xmat(nmat0+1)
!     energie de fissuration en compression uniaxiale
      gfc00=xmat(nmat0+2)
!     ouverture residuelle de reference pour les fissures de traction
!     sigma=sref/2e si w = wref
      wref0=xmat(nmat0+3)
!     coeff de frottement sur les bords de fissure
      tphi0=xmat(nmat0+4)
!     coeff delta de Drucker Pragger a partir de sin(phim)
      delta0=xmat(nmat0+5)
!     contrainte de refermeture complete de la fissure
      sref0=xmat(nmat0+6)
!     volume  de reference pour l essai de traction
      vref0=xmat(nmat0+7)
!     volume  dintegration maximale pour le non local
      vmax0=xmat(nmat0+8)
!     exposant de Weibull
      weib0=xmat(nmat0+9)
!     aleas induit par les contraintes de compression dans les heterogenites      )
      aleas0=xmat(nmat0+10)
!     exposant de leffet d echelle temporel
      xktemp=xmat(nmelast+23)
      if(xktemp.eq.0.) then
!         print*,'il manque KTMP dans ENDO3D, on prend 2.5'
         xktmp=2.5d0
      end if
!     seuil de l effet d echelle temporel
      xs1=xmat(nmelast+22)
      if (xs1.ge.1.) then
!         on ignore l effet dechelle temporel
          coefft=1.d0
      end if
!     adaptation des donnees determinites pour le calcul probabiliste
!     cf fichier maple drucker prager probabiliste
      if(aleas0.ne.0.) then
!         aleas0=min(aleas0,0.49)
         aleas0=max(aleas0,0.)
!        on peut forcer le critere de compression a ignorer l aleas
!        en mettant cg0 à 0.
         cg0=0.d0
         delta=delta0
!         cg =(-sqrt(0.3D1+0.6D1 * cg0 * sqrt(0.2D1)+0.9D1* cg0 ** 2 -
!     #   0.6D1 * cg0 - 0.6D1 * cg0 ** 2 * sqrt(0.2D1))+sqrt(0.3D1) -
!     #   (2. * delta)) / (-0.2D1 + cg0 * sqrt(0.2D1) + 0.2D1 * cg0)
!         delta0=cg
      else
         cg0=0.d0
      end if

!***********************************************************************
!     prise en compte de l hydratation sur les variables materiau
!     recuperation des caracteristiques de l hydratation
!     effet de l hydratation sur Young
!      print*,'Av hydr_xmat endo3d',e00,e0,hydra1,hydras,0.66d0,erreur
      call hydr_xmat(e00,e0,hydra1,hydras,0.33d0,erreur)
!     prise en compte de l hydratation sur la contrainte de refermeture
      sref1=sref0*e00/e0
!      print*,'e0 ds endo3d', e0
!      read*
      if (erreur.ne.0) then
       print*,'pb hydratation dans endo3d'
       call utmess('F','COMPOR1_90')
      end if
!     effet de l hydratation sur Poisson neglige (comme dans cendo3d car
!     significatif que avant le seuil)
      xnu0=xnu00
!     ( a modifier si retrait considere avant le seuil)
      call hydr_xmat(rt00,rt0,hydra1,hydras,0.4d0,erreur)
!     effet de l hydratation sur l energie de fissuration de traction
      call hydr_xmat(gft00,gft0,hydra1,hydras,0.33d0,erreur)
!     effet de l hydratation sur la resistance a la compression
      call hydr_xmat(rc00,rc0,hydra1,hydras,0.8d0,erreur)
!     effet de l hydratation sur l energie de fissuration en compression
      call hydr_xmat(gfc00,gfc0,hydra1,hydras,0.33d0,erreur)

!***********************************************************************
!     precaution pour avoir ept0>=rt/E
      ept00=max(ept0,rt00/e00)
      ept0=max(ept0,rt0/e0)
!     coeff de non linearite pre-pic en traction
      cnlt1=ept0/(rt0/e0)
!     precaution au cas ou epc0 non fourni
      epc0=max(epc0,rc0/e0)
!     adaptation des données pour le critere en contrainte effective
!     avec attenuation du coeff de Poisson
      dpic0=1.d0-rt0/e0/ept0
      coeff2=(1.d0+xnu0*(1.d0-dpic0)*2./3.)
      gft0=gft0*coeff2
      gft00=gft00*coeff2
      rt0=rt0*coeff2
      rt00=rt00*coeff2
!**********************************************************************
!     recuperation des donnees poro mecaniques
!     deformation caracteristique viscoplastique pour le gel
      epsplg0=xmat(nmelast+7)
!     module equivalent eau non sat
      xwnsat0=xmat(nmelast+18)
      call hydr_xmat(xwnsat0,xwnsat,hydra1,hydras,4.d0,erreur)
!     poro totale
      poro0=xmat(nmelast+15)
      if(mfr.ne.33) then
!      formulation non poreuse
!      coeff de Biot
       biot00=xmat(nmelast+8)
       umbiot00=1.d0-biot00
!      effet de l' hydratation sur le coeff de Biot
       call hydr_xmat(umbiot00,umbiot0,hydra1,hydras,0.5d0,erreur)
       biot0=1.d0-umbiot0
!      volume d eau actuel
       vw1=xmat(nmelast+9)
!      actualisation de l apport de masse d eau
       varf(nvar0+44)=vw1-poro0
!      volume d eau initial rapporte a la poro
       vw0=poro0+var0(nvar0+44)
!      compressibilite de l eau
       xwsat=xmat(nmelast+10)
      else
!      on est en formulation poreux
!      on adopte la valeur de la poro-meca couplee
!      si biot depend de l hydratation ce nest pas considere ici
       biot0=xmat(nmat-1)
!      module de Biot du fluide en sature et en non sature
       xwsat=xmat(nmat)
       xwnsat=xwsat
!      le volume d eau est calcule a partir de l apport de masse
!      fluide normalise
!      l apport de masse fluide est egalement disponible sigf(nstrs)=msr0
       xmsrt0=var0(nvar0+44)
!       xmsrt1=xmsrt0+sigf(nstrs)
       xmsrt1=sigf(nstrs)
       varf(nvar0+44)=xmsrt1
       vw0=poro0+xmsrt0
       vw1=poro0+xmsrt1
      end if
!      print*,'dans endo3d'
!      print*,'biot',biot0,' mob',xwsat,' vw0', vw0
!      read*
!     heterogeneite contrainte hydrique
      sfld=xmat(nmelast+11)
!     volume de gel
      vg0=xmat(nmelast+12)
!     compressibilite du gel
      xmg0=xmat(nmelast+13)
!     poro accessible au gel
      vp0=xmat(nmelast+14)
!     temps caracteristique ecoulement du gel
      taug0=xmat(nmelast+16)
!     energie d activation des viscosites
      eafluage=xmat(nmelast+17)
!***********************************************************************
!     initialisation de la variable d erreur
!      print*,'mfr1,mfr',mfr1,mfr
!     read*
      errb3d=0
!***********************************************************************
!      recuperation des increments de deformations
       if (mfr.ne.33) then
!       on est pas en formulation poreux
!       print*,'increment de deformation'
        if (ifour.eq.2) then
!        massif 3d non poreux
         do i=1,6
          depst6(i)=depst(i)
!         print*,'depst',i,'=',depst(i)
         end do
        else
!        deformation plane ou axisymetrique non poreux
         do i=1,nstrs
          depst6(i)=depst(i)
         end do
         if(nstrs.lt.6) then
          do i=(nstrs+1),6
          depst6(i)=0.d0
          end do
         end if
        end if
       else
!       on est  en formulation poreux
!       print*,'increment de deformation'
        if (ifour.eq.2) then
!        massif 3d non poreux
         do i=1,6
          depst6(i)=depst(i)
!         print*,'depst',i,'=',depst(i)
         end do
        else
!        deformation plane ou axisymetrique poreux
         do i=1,(nstrs-1)
          depst6(i)=depst(i)
         end do
         if((nstrs-1).lt.6) then
          do i=nstrs,6
           depst6(i)=0.d0
          end do
         end if
        end if
       end if
!***********************************************************************
!      recuperation ou calcul des contraintes effectives  ds la matrice
!      et des endommagements diffus
       if (fl3d) then
!       les contraintes effectives sont issues du modele de fluage
!       ****************************************************************
!       on les charge sans les recalculer, la position de ces
!       variables est definie dans le modele de fluage et dans idvar4
!       print*,'chargement des calculs effectues par fluage 3d'
        do i=1,6
!        contrainte effective dans le squelette solide
         sigef6(i)=varf(nvhydra+37+i)
         varf(nvar0+i)=sigef6(i)
!        contrainte seuil dans le critere RGI
         ssg6(i)=varf(1+i)
!        deforlations anelastiques RGI
         epsplg6(i)=varf(7+i)
!        contrainte seuil critere hydrique (le cas echeant dans b3d_sigd)
         ssw6(i)=varf(18+i)
!        print*,'sigef6(',i,')=',sigef6(i)
!        print*,'ssg6(',i,')=',ssg6(i)
!        print*,'epsplg6(',i,')=',epsplg6(i)
!        print*,'ssw6(',i,')=',ssw6(i)
         if (i.le.3) then
!         recuperation des endommagements diffus calcules avec le fluage
!         endommagement du aux RGI
          dg3(i)=varf(13+i)
!         endommagement due a lhydrique (le cas echeant)
          dw3(i)=varf(24+i)
!         print*,'dg3(',i,')=',dg3(i)
!         print*,'dw3(',i,')=',dw3(i)
         end if
        end do
!       recuperation de la variation volumique visco elastique cumulee
!       calculee par le modele de fluage
        dv0=varf(nvhydra+1)+varf(nvhydra+2)+varf(nvhydra+3)
!       print*,'variation volumique visco elastique',dv0
        varf(nvar0+42)=dv0
!       recuperation de l endo de fluage
        dflu0=varf(nvhydra+47)
!       print*,'dflu debut endo3d',dflu0
!       recuperation de l endo thermique
        dth0=varf(28)
!       recuperation des pressions calculees lors du fluage
!       pression de l eau
        pw1=varf(17)
!       pression RGI
        pg1=varf(18)
!       recalcul du coeff de Biot reel de l eau tel que fait
!       dans b3d_bwpw appele dans fluag3d
        if(mfr.ne.33) then
         if (pw1.lt.0.d0) then
!            juin 2013 biot constant tout est dans pw
!             bw1=biot0*vw0/poro0
!            pour etre compatible avec version juin 2013 de b3d_bwpw
             bw1=biot0
         else
             bw1=biot0
         end if
        else
         bw1=biot0
        end if
!       recuperation des contraintes apparentes issues du fluage
!       calcul des contraintes apparentes non affectees par le fluage
!       depuis juin 2013 dflu n affecte plus les contraintes effectives
!       il est integre dans l etage de Maxwell (cf calage S.Multon)
!        xumdft=((1.d0-dflu0)*(1.d0-dth0))
!       dflu0 est repporté sur rc et non sur E
        xumdft=(1.d0-dth0)
        if(mfr.ne.33) then
!        formulation non poreuse
         do i=1,nstrs
          sigat6(i)=sigf(i)
!         recuperation de la contrainte effective sans endo localisee
          if (pw1.ge.0.d0) then
!            cas du milieu sature : cf b3d_sigd pour operation inverse
!            on endommage avec les contrainte effective poro mecaniques
             if (i.le.3) then
              sigaf6(i)=(sigat6(i)+bw1*pw1)/xumdft
             else
              sigaf6(i)=sigat6(i)/xumdft
             end if
           else
!            cas du milieu non sature en eau : on endommage avec les contraintes
!            totales non saturees
             sigaf6(i)=sigat6(i)/xumdft
           end if
!          print*,'sigat init(',i,')=',sigat6(i)
!          print*,'sigaf init(',i,')=',sigaf6(i)
         end do
         if (nstrs.lt.6) then
          do i=(nstrs+1),6
             sigat6(i)=0.d0
             sigaf6(i)=0.d0
          end do
         end if
        else
!        formulation poreuse avec fluage
         do i=1,(nstrs-1)
          sigat6(i)=sigf(i)
!         recuperation de la contrainte effective sans endo localisee
          if (pw1.lt.0.d0) then
!            cas du milieu non sature en eau : on endommage avec les contraintes
!            totales non saturees mais comme en a enlever les contraintes hydriques
!            des contraintes effectives pour que le code considere nos totales
!            et non les siennes, il faut les remettre pour retrouver les totales
!            vraies, on les reenlevera a la fin
!            hydriques on les remet
             if (i.le.3) then
              sigaf6(i)=(sigat6(i)-bw1*pw1)/xumdft
             else
              sigaf6(i)=sigat6(i)/xumdft
             end if
           else
!            cas du milieu sature en surpression : cf b3d_sigd pour operation inverse
!            on endommage avec les contrainte effective poro mecaniques qui
!            en formulation poreuse sont celles issues du fluage, si la pression est positive
!            elle ne contiennent pas de pression, il s agit donc bien des contraintes
!            non endommagee dans le solide
             sigaf6(i)=sigat6(i)/xumdft
           end if
!          print*,'sigat init(',i,')=',sigat6(i)
!          print*,'sigaf init(',i,')=',sigaf6(i)
         end do
         if ((nstrs-1).lt.6) then
          do i=nstrs,6
             sigat6(i)=0.d0
             sigaf6(i)=0.d0
          end do
         end if
        end if
!       on aura besoin des increments de deformation pour limiter
!       l increment d ouverture de fissure, on les recalcule
!       on enleve les fissures remplies de gel du systeme paralelle
!       pour les mettre en serie
!       les increment de deformation anelastiques ont déja été calculés
!       avec le fluage
        do i=1,6
         epsplfg6(i)=varf(7+i)
         epsplg6(i)=var0(7+i)
!        les termes hors diagonale contiennent des gama
         if (i.le.3) then
          depst6(i)=depst6(i)-(epsplfg6(i)-epsplg6(i))
         else
          depst6(i)=depst6(i)-2.d0*(epsplfg6(i)-epsplg6(i))
         end if
        end do
!       *** fin traitement si fluage prealable *************************
       else
!       **** on est pas passé par le fluage avant l endo ***************
!       les contraintes effectives doivent être calculees ici
!       ****************************************************************

!       ***endo de fluage***********************************************
!       pas de fluage prealable
        dflu0=0.d0
!       print*,'dflu',dflu0

!       *** endo thermique *********************************************
!       on calcule l endo thermique
!       recuperation de l endo thermique et effet de l'hydratation
        call hydr_vari(var0(28),dth0,hydra0,hydra1,hydras,erreur)
        call b3d_dth(teta2,dth0,dt80)
!       mise a jour de l endo thermique
        varf(28)=dth0

!       *** contraintes effectives *************************************
!       les contraintes effectives ne sont pas connues, on les calcule
!       actualisation de la deformation viscoplastique et de la pression de gel
!       recuperation des parametres d endo et de la def vpl
        do i=1,6
          call hydr_vari(var0(1+i),ssg6(i),hydra0,hydra1,&
        hydras,erreur)
          call hydr_vari(var0(7+i),epsplg6(i),hydra0,hydra1,&
         hydras,erreur)
          call hydr_vari(var0(18+i),ssw6(i),hydra0,&
         hydra1,hydras,erreur)
          if (i.le.3) then
             dg3(i)=var0(13+i)
             dw3(i)=var0(24+i)
          end if
        end do
!       volume visco elastique intial
        dv0=var0(nvar0+42)
!       calcul des viscosités de gel en fonction du temps caracteristique
!       et des raideurs des fluides
        call b3d_actherm(teta1,eafluage,xmg0,xwsat,taug0,&
       etag1,etaw1)
!       ****************************************************************
!       calculs poro_mecanique (pw,pg,epl,dpl...) et gonflement du au gel
!       print*,'on fait le calcul poro meca 1'
        pw0=var0(17)
        if(mfr.eq.33) then
!        on est en poreux  l increment de la pression a deja été calculee
         dpw=depst(nstrs)
!        l apport de masse fluide est egalement disponible sig(nstrs)=msr0
!         print*,'ds endo3d apports de masse fluide',vw0,vw1
!        b3d pm ne calcule que la pression de gel
        end if
!       prise en compte des microfissures hydriques eventuelle
        epsvpw=0.d0
!       calcul des pressions
        call b3d_pm(vg0,vp0,dv0,depst6,ssg6,dg3,epsplg6,epsplfg6,&
      etag1,bg1,xmg0,pg1,dt,epsplg0,vw0,poro0,biot0,xwsat,pw1,bw1,&
      epsvpw,vplg33,vplg33t,e0,rt0,ept0,erreur,xwnsat,mfr,pw0,dpw,vw1)
!       stockage des pressions actualisees apres increment et
!       ecoulement viscoplastique
        varf(17)=pw1
        varf(18)=pg1
!       mise a jour des variables internes de def plastique
        do i=1,6
         varf(7+i)=epsplfg6(i)
         if (i.le.3) then
          varf(13+i)=dg3(i)
         end if
        end do

!       ****************************************************************
!       mise a jour des deformations elastiques
!       on enleve les fissures remplies de gel du systeme paralelle
!       pour les mettre en serie
        do i=1,6
         if (i.le.3) then
          depst6(i)=depst6(i)-(epsplfg6(i)-epsplg6(i))
         else
          depst6(i)=depst6(i)-2.d0*(epsplfg6(i)-epsplg6(i))
         end if
        end do
!       partition des increments de deformations en spherique
!       et deviatorique
!       increment de la deformation volumique
        depsv=0.d0
        do i=1,3
         depsv=depsv+depst6(i)
        end do
!       mise a jour variation volumique elastique
        varf(nvar0+42)=var0(nvar0+42)+depsv
!       print*,'variation volumique totale',dv0
!       increment des gammas
        do i=1,3
         dgamd6(i)=(depst6(i)-depsv/3.d0)*2.d0
        end do
        do i=4,6
         dgamd6(i)=depst6(i)
        end do
        xk0=e0/3.d0/(1.d0-2.d0*xnu0)
        xmu0=e0/2.d0/(1.d0+xnu0)
!       calcul des contraintes effectives dans le squelette solide
        call b3d_elas(var0,nvari,nvar0,depsv,dgamd6,&
       xk0,xmu0,sigef6,varf,hydra0,hydra1)
!        print*,'contrainte effective dans le solide'
!        do i=1,6
!         print*,'sig0(',i,')=',var0(nvar0+i),' sigf(',i,')=',sigef6(i)
!        end do

!       *****************************************************************
!       prise en compte de l'endo viscoplastique  diffus
!       maj des fonctions seuils cible de la viscoplasticite
!       evaluation des contraintes totales affectes par l endo diffus
!       print*,'on fait le calcul poro meca2'
        call b3d_sigd(bg1,pg1,bw1,pw1,sfld,ssg6,e0,rt0,ept0,&
       mfr,erreur,dg3,dw3,xnu0,sigaf6,dflu0,sigef6,xmg0,&
       sigat6,vplg33,vplg33t,vssw33,vssw33t,ssw6,dth0)
        do i=1,6
!         print*,'sigat6 apres sigd(',i,')=',sigat6(i)
          varf(1+i)=ssg6(i)
          varf(18+i)=ssw6(i)
          if (i.le.3) then
            varf(24+i)=dw3(i)
          end if
        end do

!       ****************************************************************
!       calcul des contraintes apparentes non affectees par
!       l endo de fluage et l endo thermique
!       juin 2013 modif propose par S.Multon dflu n affecte que Maxwell
!        xumdft=((1.d0-dflu0)*(1.d0-dth0))
!       (1.d0-dflu0) repporté sur rc et non directement sur E
        xumdft=(1.d0-dth0)
        if(mfr.ne.33) then
!        formulation non poreuse
         do i=1,nstrs
!         recuperation de la contrainte effective sans endo localisee
          if (pw1.ge.0.d0) then
!            cas du milieu sature : cf b3d_sigd pour operation inverse
!            on endommage avec les contrainte effective poro mecaniques
             if (i.le.3) then
              sigaf6(i)=(sigat6(i)+bw1*pw1)/xumdft
             else
              sigaf6(i)=sigat6(i)/xumdft
             end if
           else
!            cas du milieu non sature en eau : on endommage avec les contraintes
!            totales non saturees
             sigaf6(i)=sigat6(i)/xumdft
           end if
!          print*,'sigat init(',i,')=',sigat6(i)
!          print*,'sigaf init(',i,')=',sigaf6(i)
         end do
         if (nstrs.lt.6) then
          do i=(nstrs+1),6
             sigat6(i)=0.d0
             sigaf6(i)=0.d0
          end do
         end if
        else
!        formulation poreuse
         do i=1,(nstrs-1)
!         recuperation de la contrainte effective sans endo localisee
          if (pw1.lt.0.d0) then
!            cas du milieu non sature en eau : on endommage avec les contraintes
!            totales non saturees mais comme on a enlever les contraintes hydriques
!            des contraintes  effectives pour que castem considere nos totales et non les siennes
!            il faut les remettre pour retrouver les totales vraies, on les reenlevera a la fin
!            hydriques on les remet
             if (i.le.3) then
              sigaf6(i)=(sigat6(i)-bw1*pw1)/xumdft
             else
              sigaf6(i)=sigat6(i)/xumdft
             end if
           else
!            cas du milieu sature en surpression : cf b3d_sigd pour operation inverse
!            on endommage avec les contrainte effective poro mecaniques qui
!            en formulation poreuse sont celles issues du fluage sans les surpression
             sigaf6(i)=sigat6(i)/xumdft
           end if
!          print*,'sigat init(',i,')=',sigat6(i)
!          print*,'sigaf init(',i,')=',sigaf6(i)
         end do
         if ((nstrs-1).lt.6) then
          do i=nstrs,6
             sigat6(i)=0.d0
             sigaf6(i)=0.d0
          end do
         end if
        end if
!       fin de la zone de traitement si pas de fluage prealable
      end if
!***********************************************************************
!      modification des resistances en fonction des endommagements diffus
!      dus au gel et des tailles des zones chargees
!***********************************************************************
!      on suppose la resistance residuelle isotrope est controlee par
!      l endo diffus le plus grand (sauf le fluage et la thermique)
!      (car on est effectif / fluage)
!      disolocal=max(dg3(1),dg3(2),dg3(3),dw3(1),dw3(2),dw3(3))
!      cas ou l endo hydrique est neglige sur la loi de comp
       disolocal=max(dg3(1),dg3(2),dg3(3))
!      print*,'D iso local:',disolocal
!      effet de l'endommagement du au gel sur les resistances
!      (approximation de la chute de resistance due au gel isotrope)
       aux3=(1.d0-disolocal)
!      en traction
       rt0=rt0*aux3
!      stockage de la valeur de ref hydratee pour weibull
       rtw1=rt0
!      cas de l energie de fissuration
       gft0=gft0*aux3
       gftw1=gft0
!      en compression
       aux4=aux3**0.15
       rc0=rc0*aux4
!       *(1.d0-dflu0)
       gfc0=gfc0*aux4
!       *(1.d0-dflu0)
!      print*,'rt local:',rt0,' Gft:',gft0
!      print*,'rc local:',rc0,' Gfc:',gfc0
!      prise en compte de l endommagement diffus sur le comportement en
!      compression (a faire si necessaire)

!***********************************************************************
!     stockage des tailles des elements et calcul ou recuperation
!     (t33, n33) des directions principales des elements (vt33)
!     le calcul des tailles peut elors etre fait  avec b3d_l3
!***********************************************************************
      local=.true.
      call b3d_stock_tail(xmat,nmat,ifour,mfr,nmat0,nmat1,&
      t33,n33,local,vt33,var0,varf,nvari,erreur,gft00,fr10,&
      rt00,ept00,beta1,gama1,nvar1)

!***********************************************************************
!      traitement non local de la resistance à la traction WEIBULL
!      part locale de la methode WL2 (cas de l aleas spatial)
!***********************************************************************
!      print*,'istep',istep
       xb=weib0**(-1)
       xweibull=weib0
!      on teste si le pic de traction est passé ou pas
       if (istep.ne.0) then
!        prise en compte de l effet d echelle de Weibull
!        par methode non  locale
!        recup coeff multiplicateur dt rt Gft du pas precedent
         xwb0=var0(nvar0+57)
         coeffGf0=var0(nvar0+72)
         coefft=var0(nvar0+77)
!        recup du tau de chargement actuel (cf. istep=1 plus bas)
!        si istep=2:2nd passage non local, donc onrecupere le tau
!        de chargement pour le pas actuels
!         tauw0=var0(nvar0+58)
!        test 1er passage
         if (xwb0.le.1.d-4) then
!          zone non chargee au pas precedent
!          on adopte un multiplicateur de Weibull egal a 1
!          ce qui permet de calculer avec rt0 au premier pas de temps
           xwb0=1.d0
           coeffGf0=1.d0
           coefft=1.d0
!          et on suppose le taux de chargement precedent nul
!           tauw0=0.d0
         end if
!        contrainte limite de traction du pas converge precedent
!         su0w=cnlt1*rt0*xwb0
!        test de localisation au pas precedent (s/E >epic)
!         do i=1,6
!           x6(i)=var0(nvar0+15+i)
!         end do
!        diagonalisation de la contrainte seuil du pas precedent
!         call x6x33(x6,x33)
!         call b3d_valp33(x33,x3,vx33)
!        recuperation du maximum
!         ssl0max=max(x3(1),x3(2),x3(3),0.d0)
         if (istep.eq.2) then
!          do i=1,nvari
!            print*,2,var0(nvar0+43),varf(nvar0+43)
!          end do
!          read*
!          2nd passage non local : modif de la resistance a la traction si pre pic
!          Attention calcul ok que si VMAX realiste dans la procedure non locale
!          on recupere alpha=tauref*veq
!           alpha0=var0(nvar0+43)
!          on recupere la variable issue de la procedure non locale dans les var0 (cf unpas.proc)
!          elle vaut alpha/(tau.vmax)
!          calcul du volume equivalent
           veq1=var0(nvar0+43)*vmax0
!          suite a modif de unpas on retrouve pour istep=2 taumax dans var0(nvar0+58)
           veq1=var0(nvar0+43)/var0(nvar0+58)
!           if (veq1.le.0. ) then
!            pb induit par les fonctions d interpolation en zone de fort gradient de chragement
!            normalement impossible avec le second non local dans un pas pas defa
!             print*,'veq<0 ds endo3 :', veq1
             veq1=max(veq1,fminvref*vref0)
!            end if
!          taux de chargement actuel
!           tauw0=var0(nvar0+58)
!          probabilite de defaillance instantanee
!           betak=(var0(nvar0+58))*veq1/vref0
!           xpf1=max((1.d0-exp(-betak)),var0(nvar0+74))
!          on la stocke a la place de tauw pour voir..............
!           varf(nvar0+58)=xpf1
!          test de localisation au pas precedent
!          if (ssl0max.le.su0w) then
           if(var0(nvar0+74).ne.1.)then
!            istep=2 et on a pas encore amorcé la localisation
!            Application de l effet déchelle de Weibull sur la résistance
!             print*,veq1
!            limitation du volume equivalent au volume maxi
             if (veq1.gt.vmax0) then
              print*,'veq',veq1,' vmax',vmax0
              veq=min(veq1,vmax0)
             else
              veq=veq1
             endif
!            limitation du volume charge mini a celui de l element
             call b3d_l3(local,t33,n33,vt33,vt33,l3)
             xvef=l3(1)*l3(2)*l3(3)
             xli=max(l3(1),l3(2),l3(3))
             dpict00=1.d0-cnlt1**(-1)
!            donnees pour endo de Poisson sans effet d echelle
             call b3d_nosnap(0,fr10,rt0,dpict00,Gft0,e0,beta1,gama1&
            ,xli,vref0,weib0,vref0,rtw2,errmesh,coeffGf,1.d0)
             gftw2=coeffgf*gft0
             eptw2=cnlt1*(rtw2/e0)
!            prise en compte des aleas
!            recuperation du coeff d aleas temporel utilise pour calculer taux
!             coefft=var0(nvar0+77)
!             print*,'istep 2 coefft=',coefft
             call b3d_nosnap(istep,fr10,rt0,dpict00,Gft0,e0,beta1,gama1&
            ,xli,vref0,weib0,veq,rteff,errmesh,coeffGf,coefft)
!            coeff amplificateur pour effet d echelle spatial
             xwb1=rteff/(rt0*coefft)
           else
!            istep=2 mais pic deja passe pour ce point
!            print*,'localisation deja amorcee dans endo3d proba'
!            on a passé le pic, on recupere le coeff d effet dechelle spatial du pas precedent
             xwb1=var0(nvar0+57)
!             print*,'on a deja localise on reprend xwb1=',xwb1
!            recuperation du coeff d aleas temporel utilise pour calculer tau spatial
             coefft=var0(nvar0+77)
!            attention nouvelle definition de alpha cf unpas.proc et ci-dessus
             veq=vref0/((xwb1)**weib0)
             coeffgf=var0(nvar0+72)
             errmesh=var0(nvar0+71)
!            print*,'veq',veq,' xwb',xwb1
!            print*,'rt0=rt0*xwb1',rt0,rt0*xwb1
!            comme coefft evolue encore dans la zone fissuree on repasse par nosnap
!             if(coefft.ne.1.) then
!            limitation du volume charge mini a celui de l element
             call b3d_l3(local,t33,n33,vt33,vt33,l3)
             xvef=l3(1)*l3(2)*l3(3)
             xli=max(l3(1),l3(2),l3(3))
             dpict00=1.d0-cnlt1**(-1)
!            donnees pour endo de Poisson sans effet d echelle
             call b3d_nosnap(istep,fr10,rt0,dpict00,Gft0,e0,beta1,gama1&
            ,xli,vref0,weib0,vref0,rtw2,errmesh2,coeffGf2,1.d0)
             gftw2=coeffgf2*gft0
             eptw2=cnlt1*(rtw2/e0)
           end if
!          on stocke veq/vref pour info dans la variable non locale qui n est plus utile
           varf(nvar0+43)=veq/vref0
!          stockage du facteur d effet d echelle dans les variables internes
           varf(nvar0+57)=xwb1
!          stockage facteur d effet d echelle sur l energie de fissuration
           varf(nvar0+72)=coeffGf
!          stockage du coeff  d amplification temporelle a rupture
           varf(nvar0+77)=coefft
!          stockage de l indicateur d erreur de la taille de mailles
!          dans les variables internes
           varf(nvar0+71)=errmesh
!          fin du traitement si 2nd passage non locale
         else
!          istep=1 : pas d effet d echelle hors "non snap back"
!          pour la 1ere etape non locale coefft=1 veq=vref0
           coefft=1.d0
           veq=vref0
!          on verifie la condition de non snap back local
           call b3d_l3(local,t33,n33,vt33,vt33,l3)
           dpict00=1.d0-cnlt1**(-1)
           xli=max(l3(1),l3(2),l3(3))
           call b3d_nosnap(istep,fr10,rt0,dpict00,Gft0,e0,beta1,gama1&
          ,xli,vref0,weib0,veq,rtw2,errmesh,coeffGf,coefft)
           xwb1=rtw2/(rt0*coefft)
           gftw2=Gft0*coeffGf
           eptw2=cnlt1*(rtw2/e0)
         end if
!        istep=1 ou istep=2 : donnees pour calcul probabiliste si istep=2
!        modif des autres parametres de la loi de traction pour rester compatible
!        effet d echelle spatial
         rt0=rt0*xwb1*coefft
!        print*,'Nouvelle resistance a la traction', rt0
!        modif de la deformation au pic pour conserver la non
!        linearite pour avoir ept0>=rt/E
         ept0=cnlt1*(rt0/e0)
!        seuil de rankine en contrainte non endo
         suw1=e0*ept0
!        adaptation des données pour le critere en contrainte effective
!        avec attenuation du coeff de Poisson
         dpic0=1.d0-rt0/e0/ept0
         gft0=gft0*coeffGf
!        modification de l energie de fissuration en fonction de xwb et
!        du volume de lelement
       else
!        pas de calcul non local veq est pris egal au volume de l element fini
!        pas d effet d echelle temporel
!        print*,'juste avant b3d_l3',local,t33,n33,vt33,vt33,l3
         call b3d_l3(local,t33,n33,vt33,vt33,l3)
!        l3 contient alors les tailles principales de l element
!        on calcul la resistance de l element avec les criteres proba et no snap back
         dpict00=1.d0-cnlt1**(-1)
!        la taille est la plus grande des 3 principales de l element
         xli=max(l3(1),l3(2),l3(3))
!        on prend veq=vref0 pour ne pas avoir
!        d effet d echelle probabiliste en calcul local, ainsi, s il y a une minoration
!        de rt ce sera a cause de la taille trop grande de l element uniquement
!        on verifiera alors tout de meme excatement gf
         veq=vref0
         coefft=1.d0
         call b3d_nosnap(istep,fr10,rt0,dpict00,Gft0,e0,beta1,gama1,xli,&
        vref0,weib0,veq,rteff,errmesh,coeffGf,coefft)
!        multiplicateur probabiliste
         xwb1=rteff/rt0
!         print*,'ds endo3d coeffgf:',coeffgf, 'coeff rt',xwb1
!        affectation de la resistance a la traction de calcul
         rt0=rteff
         gft0=gft0*coeffGf
!        volume considere pour minorer la resistance
         varf(nvar0+43)=veq
!        stockage du facteur d effet d echelle dans les variables internes
         varf(nvar0+57)=xwb1
!        stockage facteur d effet d echelle sur l energie de fissuration
         varf(nvar0+72)=coeffGf
!        stockage de l indicateur d erreur de la taille de mailles
!        dans les variables internes
         varf(nvar0+71)=errmesh
!        valeurs considerees pour Poisson identiques
         rtw2=rt0
         eptw2=cnlt1*(rtw2/e0)
         gftw2=Gft0
       end if
!      controle de la valeur de la resistance a la traction
       if (rt0.eq.0.)then
         print*,'Dans endo3d pb de donnees incoherentes pour Weibull'
         print*,'istep',istep
         print*,'rt0',rt0,'xwb1',xwb1,'xwb0',xwb0,'veq',veq,'vref',vref0
         print*,'(vref/veq)**xb',(vref0/veq)**xb,'var0(nvar0+57) ',&
        var0(nvar0+57)
         read*
         call utmess('F','COMPOR1_90')
       end if

!*********************************************************************
!     prise en compte de l'endommagement du coeff de Poisson
!*********************************************************************
!     recuperation du seuil et de l endo de Poisson du pas precedent
      do i=1,6
       call hydr_vari(var0(nvar0+6+i),ssp6(i),hydra0,hydra1,&
      hydras,erreur)
      end do
!     on travaille en considerant la contrainte non endo
!     integrant la pression de disjonction en non sat et la
!     pression de gel s il y a lieu (car rt est reduit par dg)
!     comme indique plus haut
!      print*,'contraintes effectives utilisees dans ENDO3D'
!      print*,sigaf6
      call b3d_partition(sigaf6,sigef3,vsigef33,vsigef33t,&
     sigeft6,sigefc6,sigefc3,sigeft3)
!     mise a jour des seuils
      call b3d_sst(ssp6,0,vsigef33,vsigef33t,sigeft3)
      do i=1,3
       varf(nvar0+6+i)=ssp6(i)
       d03(i)=var0(nvar0+12+i)
      end do
      do i=4,6
       varf(nvar0+6+i)=ssp6(i)
      end do
      local=.true.
      rrr=.false.
      dldd=.false.
      nfid1=1.d0
!     istep impose =0 pour imposer le calcul des tailles ds b3d
!     on utilise ici rtw2 non affecte par maillon faible pour que
!     la contrainte effective reste objective pour le calcul non loca
!     (identique pour les 2 etapes non locales)
!     seul le coeff de non snap back deterministe est pris en compte ici
      call b3d_sd (ssp6,t33,n33,l3,vt33,e0,xnu0,gftw2,fr10,rtw2,eptw2,&
     beta1,gama1,regl0,erreur,d03,df3,st3,vss33,vss33t,local,e23,&
     nfid1,rrr,Rapp6,dpic0,0)
      if (erreur.eq.1) then
       print*,'seuils poisson'
       print*, ssp6
       do i=1,6
        print*,'sigat init(',i,')=',sigat6(i)
       end do
       print*,'d fluage',dflu0
       call utmess('F','COMPOR1_90')
      end if
      do i=1,3
       varf(nvar0+12+i)=df3(i)
      end do
!     matrice d endommagement pour l attenuation du coeff de Poisson
      call b3d_d66(xnu0,st3,d66,e0,.false.,.false.)

!********************************************************************************
!     on en deduit les contraintes totales dans la matrice apres
!     attenuation des coeffs de Poisson
!********************************************************************************
!     prise en compte de lendo localise sur la partie positive des contraintes
      call x6x33(sigeft6,x33)
      call b3d_chrep(sigef33,x33,vss33)
      call x33x6(sigef33,x6)
      call b3d_sigapp(x6,d66,siga6,.true.)
!     calcul des contraintes effectives dans la zone de localisation
      do i=1,3
        sigep6(i)=siga6(i)*st3(i)
      end do
      do i=4,6
        sigep6(i)=x6(i)
      end do
!     retour en base fixe
      call x6x33(sigep6,x33)
      call b3d_chrep(sigef33,x33,vss33t)
      call x33x6(sigef33,sigeft6)
!     contrainte effective (poro) non endo apres attenuation des coeffs de Poisson
      do i=1,6
         siget6(i)=sigeft6(i)+sigefc6(i)
      end do
!     on poursuit les calcul avec ces nouvelles contraintes effectives
      call b3d_partition(siget6,sigef3,vsigef33,vsigef33t,&
     sigeft6,sigefc6,sigefc3,sigeft3)

!********************************************************************************
!     prise en compte de l aleas eventuel sur le champ de contrainte
!*******************************************************************************
!     calcul des contraintes locales (sigi) incluant les autocontraintes eventuelles
      call b3d_covs(aleas0,vsigef33,vsigef33t,sigeft3,&
     sigefc3,sigi6,sigit3,sigic3)

!******************************************************************************
!    WL2 : on utilise cette contrainte effective pour calculer les probabilites
!    instantanees en zone saine pour le 1er passage non local
!******************************************************************************

!     ces effets d echelle ne sont actif qu'en non local
      if((istep.eq.1).or.(istep.eq.2)) then

!     ************ traction maxi ****************************************
!     recherche de la contrainte maxi integrant l aleas du champ de contrainte
      sigmax=max(sigit3(1),sigit3(2),sigit3(3))

!     *********** prise en compte de l aleas sur la resistance *******

!     *********** aspect temporel ************************************

      if(xs1.lt.1.) then
!      traitement effet d echelle temporel possible
!      actualisation du tau de chargement equivalent
       sigseuil=xs1*rtw1
!      temps de reference pour le calage de rtref
       tref=xmat(nmelast+24)
!      tau de chargement temporel maxi pour ce pas
       tau0=var0(nvar0+76)
!      tau de chargement temporel de reference debut de pas  (sans DRGI)
       dsigtmp0=(tau0**(1./xktemp))*(rtw1-sigseuil)
!      tau de chargement fin de pas
       dsigtmp1=max((sigmax-sigseuil),0.d0)
!      tau de chargement de reference fin de pas    (sans DRGI)
       tau1=(dsigtmp1/(rtw1-sigseuil))**xktemp
!      stockage du tau de chargement de reference pour la pas suivant
       varf(nvar0+76)=tau1
!      tau de chargement equivalent (-ln(1-pf))
       if(tau1.eq.tau0)then
          dtaueq=dt*tau0
       else
          if(tau1.ne.0.)then
            dtaueq=(dsigtmp1*tau1-dsigtmp0*tau0)/(dsigtmp1-dsigtmp0)
            dtaueq=dtaueq*dt/(xktemp+1.d0)
          else
            dtaueq=0.d0
          end if
       end if
!      actualisation du tau de chargement equivalent temporel
       taueq1=var0(nvar0+73)+dtaueq
       varf(nvar0+73)=taueq1
!      actualisation du temps chargé equivalent
       if(tau1.ne.0.) then
           tequ1= taueq1/tau1
       else
           tequ1= 0.d0
       end if
       varf(nvar0+75)=tequ1
!      coeff amplicateur de la resistance de reference
       if(tequ1.ne.0.)then
         coefft=((tref/tequ1)**(1./xktemp))*(1.d0-xs1)+xs1
       else
!       en l'abscence de chargement on adopte une valeur forfaitaire tre eleve
!       pour qu il n' y ait pas de risque de rupture
         coefft=coefftmax0
       end if
      else
!      pas d effet d echelle temporel car xs1=1
       coefft=1.d0
      end if
!     stockage du coeff multiplicateur de rt / effet d echelle temporel
!     print*,'istep=',istep,' coefft apres reevaluation=',coefft
!     read*
!     on prend ce coefft que si pas encore localise
      if(var0(nvar0+74).ne.1.)then
!        pas encore localise la resistance peut changer
          varf(nvar0+77)=coefft
      else
!         varf(nvar0+77)=coefft
!         localisation, on reprend la resistance de localisation
          varf(nvar0+77)=var0(nvar0+77)
      end if
!      pas de condition de localisation car affecte aussi la zone localisee
!      (si la fissure se propage lentement il faudra moins d energie)

!      *************traitement de  l aleas spatial local ****************

!      taux de chargement pour le volume de reference et le temps equivalent
!      si on veut ignorer le temps equivalent on met coefft a 1
!       coefft=1.d0
!      rtw1 integre un couplage rgi/wl2 (cf plus haut)
       xtau2=max((sigmax/(rtw1*coefft))*(1.d0-var0(nvar0+74)),xtaumin)
       xtau1=xtau2**xweibull
!       xtau1=min(xtau1,1.d0)
!      on teste avec un tau qui n est pas celui de Weibull !!!
!      pour evacuer les non linearites lors de l evaluation du volume charge
!       xtau1=(1.d0+xweibull*(sigmax/(rtw1*coefft)-1.d0))
!       xtau1=xtau1*(1.d0-var0(nvar0+74))
!       xtau1=max(xtau1,xtaumin)
!        xtau1=xtau2
!       xtau1=min(xtau1,1.d0)
!      prise en compte de la zone non encore localisee uniquement
!       xtau1=(xtau0*(1.d0-var0(nvar0+74))),xtaumin)
!      print*,'taux de chargement pour istep=1',xtau1
!      variable a modifier par la procedure non locale
       if (istep.eq.1) then
!         pour le calcul non local
          varf(nvar0+43)=xtau1*vmax0
          varf(nvar0+58)=xtau1
          goto 999
       else
!         on stocke veq/vref pour voir l ampleur de la zone chargé
          varf(nvar0+43)=veq/vref0
          varf(nvar0+58)=xtau1
       end if
!      fin du traitement des effets d echelle par WL2
      end if

!******************************************************************************
!    endommagement localise de traction apres decouplage des effets de Poisson
!******************************************************************************
      do i=1,6
       call hydr_vari(var0(nvar0+15+i),ssl6(i),hydra0,hydra1,&
      hydras,erreur)
      end do
!     mise a jour des seuils
      call b3d_sst(ssl6,0,vsigef33,vsigef33t,sigit3)
      do i=1,3
       varf(nvar0+15+i)=ssl6(i)
       d03(i)=var0(nvar0+21+i)
      end do
      do i=4,6
       varf(nvar0+15+i)=ssl6(i)
      end do
      local=.true.
      rrr=.false.
      dldd=.false.
      nfid1=1.d0
      call b3d_sd (ssl6,t33,n33,l3,vt33,e0,xnu0,Gft0,fr10,rt0,ept0,&
     beta1,gama1,regl0,erreur,d03,dfl3,st3,vss33,vss33t,local,e2l3,&
     nfid1,rrr,Rapp6,dpic0,0)
      if (erreur.eq.1) then
       print*,'seuils localisation traction'
       print*, ssl6
       do i=1,6
        print*,'sigat init(',i,')=',sigat6(i)
       end do
       print*,'d fluage',dflu0
       call utmess('F','COMPOR1_90')
      end if
      do i=1,3
       varf(nvar0+21+i)=dfl3(i)
      end do
      call  b3d_d66(xnu0,st3,dl66,e0,.false.,.true.)

!***************************************************************************
!     application de l'endommagement localise sur les contraintes effectives
!***************************************************************************
      call x6x33(siget6,x33)
      call b3d_chrep(sigef33,x33,vss33)
      call x33x6(sigef33,x6)
!     prise en compte de l endo localise en base prin endo
      call b3d_sigapp(x6,dl66,sigat6,.true.)
!     retour en base fixe
      call x6x33(sigat6,x33)
      call b3d_chrep(sigal33,x33,vss33t)
      call x33x6(sigal33,sigat6)

!***************************************************************************
!     prise en compte des contraintes de refermeture des fissures localisees
!     les refermetures de fissures ne s appliquent qu' a la partie solide
!***************************************************************************
      do i=1,6
       call hydr_vari(var0(nvar0+24+i),spl6(i),hydra0,hydra1,&
      hydras,erreur)
       call hydr_vari(var0(nvar0+30+i),wfm6(i),hydra0,hydra1,&
      hydras,erreur)
      end do
      if(fl3d) then
!      recuperation des parametres de fluage
       y1sy=xmat(nmelast+nmhydra+1)
       tau1=xmat(nmelast+nmhydra+2)
       tau2=xmat(nmelast+nmhydra+3)
       eafluage=xmat(nmelast+17)
!      recuperation de la consolidation
       cc2=max(varf(nvhydra+48),1.d0)
!      recuperation des deformation visco elastiques du pas precedent
!      et des vitesses de deformation du pas precedent
       do i=1,6
        call hydr_vari(var0(nvar0+44+i),eve6(i),hydra0,hydra1,&
       hydras,erreur)
     call hydr_vari(var0(nvar0+58+i),vve6(i),hydra0,hydra1,&
       hydras,erreur)
     call hydr_vari(var0(nvar0+64+i),vm6(i),hydra0,hydra1,&
       hydras,erreur)
       end do
      else
!      pas de parametres de fluage disponibles
       y1sy=0.d0
       tau1=0.d0
       tau2=0.d0
       eafluage=0.d0
!      recuperation de la consolidation
       cc2=1.d0
!      pas de def visco elastique
       do i=1,6
         eve6(i)=0.d0
         vve6(i)=0.d0
         vm6(i)=0.d0
       end do
      end if
!     recup des contraintes de charg ext du pas precedent
!     et des contraintes de refermeture dans les fissures
      do i=1,6
       sve6(i)=var0(nvar0+50+i)
      end do
!     calcul de la contrainte de refermeture
!     print*,'ds b3d av b3d_srf'
!     print*,y1sy,tau1,tau2,cc2,0.5d0*(teta1+teta2),eafluage
!     temperature moyenne pendant le pas pour le calcul du fluage dans la fissure
      teta12=0.5d0*(teta1+teta2)
!     indicateur de 1er passage pour savoir si on doit initialiser la contrainte
!     initiale dans la fissure avant de demarrer
      if(var0(29).ne.1.)then
        maj0=.true.
      else
        maj0=.false.
      end if

!     recup de l indicateur de localisation
      xloc=var0(nvar0+74)
      call b3d_srf(sigat6,vss33,spl6,l3,e2l3,wfeff3,wfm6,&
     vss33t,sigarf6,xnu0,e0,dpic0,rc0,wref0,tphi0,dl66,&
     erreur,dfl3,ssl6,ept0,siget6,sref1,fl3d,eve6,vve6,sve6,&
     y1sy,tau1,tau2,cc2,teta12,eafluage,dt,vm6,maj0,depst6,&
     sigaf6,xloc)
!     indicateur de localisation et de choix de rt
      varf(nvar0+74)=xloc
!      if((xloc.eq.1.).and.(var0(nvar0+74).ne.1.))then
!       print*,'on vient de localiser avec',xwb1
!      end if

!     ******************************************************************
!     calcul de l increment du volume d ouverture de fissure pour estimer
!     l increment de pression induit
!     ******************************************************************

!     et maj des ouvetures dans les variables internes
      dvfiss0=0.d0
      do i=1,3
        varf(nvar0+36+i)=wfeff3(i)
        dvfiss0=dvfiss0+ varf(nvar0+36+i)-var0(nvar0+36+i)
      end do

!     ******************************************************************
!     correction de la pression pour integrer l ouverture de fissure
      if(pw1.gt.0.)then
       dpw1=-xwsat*(1.d0-bw1)*dvfiss0
!       if(dpw1.ne.0.)then
!         print*,'variation de pression dans endo3d',dpw1
!       end if
      else
        dpw1=0.d0
      end if
!     pw1=pw1+dpw1 debranché pour l instant
      varf(17)=pw1
!     ******************************************************************

!     mise a jour des varibles internes du modele de fissuration
      do i=1,6
!       cf desfinition des variables internes dans idvar4
        varf(nvar0+24+i)=spl6(i)
        varf(nvar0+30+i)=wfm6(i)
        varf(nvar0+44+i)=eve6(i)
        varf(nvar0+50+i)=sve6(i)
        varf(nvar0+58+i)=vve6(i)
        varf(nvar0+64+i)=vm6(i)
      end do
!     contrainte totale resultante des endos diffus et localises de traction
      do i=1,6
        siga6(i)=sigat6(i)+sigarf6(i)
!        print*,'siga6 avant pression(',i,')=',siga6(i)
      end do

!*****************************************************************************
!  contribution des supressions hydriques dans les fissures localisees
!*****************************************************************************

!!!!!!l evolution de Biot n etant pas implementer on laisse faire le solveur!!!!!
! avec son biot constant
      if(mfr.ne.33) then
      do i=1,3
       if (pw1.gt.0.d0) then
        dsw6(i)=-pw1*(dfl3(i)+bw1*(1.d0-dfl3(i)))
       else
        dsw6(i)=0.d0
       end if
      end do
      do i=4,6
        dsw6(i)=0.d0
      end do
!     retour en base fixe si non nul
      if(pw1.gt.0.) then
       call x6x33(dsw6,x33)
       call b3d_chrep(dsw33,x33,vss33t)
       call x33x6(dsw33,dsw6)
      end if
      else
! !!!!!!!!!!! en attendant mieux !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      do i=1,6
       dsw6(i)=0.d0
      end do
      end if
! !!!!!!!!!!!fin premiere partie modif provisoire !!!!!!!!!!!!!!!!!!!!!

!**************************************************************************
!     prise en compte de l'endommagement isotrope de compression
!**************************************************************************
!     calcul sur la partie effective des contraintes dans le solide
      call b3d_partition(siga6,siga3,vsiga33,vsiga33t,&
     sigat6,sigac6,sigac3,sigat3)
!     calcul des longueurs des elements dans les directions principales
!     des contraintes effectives de compression
      vrai=.true.
      call b3d_l3(vrai,t33,n33,vt33,vsiga33,long3)
!     recuperation du seuil et de l endo de compression du pas precedent
      call hydr_vari(var0(nvar0+40),suc0,hydra0,hydra1,&
     hydras,erreur)
      call hydr_vari(var0(nvar0+41),dc0,hydra0,hydra1,&
     hydras,erreur)
!     actualisation de l endommagement de compression cisaillement
!     l aleas n est repercute que si cg0=aleas0 au debut d endo3d
      call b3d_dc(siga3,long3,dcf,dc0,e0,delta0,gfc0,rc0,epc0,&
     beta1,gama1,regl0,fr2,suc0,ifour,cg0,bw1,pw1)
!     stockage des variables internes actualisee
      varf(nvar0+40)=suc0
      varf(nvar0+41)=dcf

!***************************************************************************
!     calcul des contraintes totales
!***************************************************************************
!     composition de l'endommagement isotrope de compression et
!     de l'endommagement
!     localise de traction avec l endo de fluage et l endo thermique
!     depuis juin 2013 dflu n affecte plus que l etage de Maxwell
!     cf calage de S.Multon
      umd0=(1.d0-dcf)*(1.d0-dth0)
!      umd0=(1.d0-dcf)*(1.d0-dflu0)*(1.d0-dth0)
!     calcul des contraintes totales

!     **********************************************************************
!     contribution des pressions hydriques
!     **********************************************************************
      do i=1,6
!        contribution de la surpression dans les fissures localisee de traction
         siga6(i)=siga6(i)+dsw6(i)
!        contribution de la surpression dans les fissures de compression
         siga6(i)=umd0*siga6(i)+dsw6(i)*(1.d0-umd0)
         if (mfr.eq.33) then
!         formualtion poreuse
!         on enleve -bw1pw1  car traite hors du module endo3d par le logiciel
          if(i.le.3) then
           if(pw1.gt.0.) then
!            la surpression est deja prise en compte, on l enleve car dsw est non
!            nul que pour les surpressions
! !!!!! siga6(i)=siga6(i)+bw1*pw1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              continue
! !!!!!! fin 2eme partie modif en attendant mieux !!!!!!!!!!!!!!!!!!!!!!
!             siga6(i)=siga6(i)-dsw6(i)
           end if
          end if
!          print*,'siga6(',i,')=',siga6(i),'dsw6(',i,')=',dsw6(i)
         end if
      end do
!        read*
!     passage des contraintes calculees au code EF
!     print*,'fin d endo3d'
      if(mfr.ne.33) then
!      formulation non poreuse
       do i=1,nstrs
        sigf(i)=siga6(i)
!        print*,'sigf endo3d=',sigf(i)
       end do
      else
!      formulation poreuse
       do i=1,(nstrs-1)
        sigf(i)=siga6(i)
       end do
      end if
!      read*
      if (erreur.ne.0) then
       print*,'erreur dans endo3d'
       errb3d=1
       call utmess('F','COMPOR1_90')
      end if

!********************************************************************************
!     recuperation des variables internes en cas de calcul non local
!********************************************************************************
!     lors d'un calcul non local unpas.pric attribue les varf aux var0
!     on fait l inverse pour les varibles a conserver
999   if (istep.eq.1) then
!       on recopie les var0 ds les varf sauf
!       celle a moyenner
!       le taux de chargement instantane*vmax (43)
!       taux de chragement instantane (58)
!       le coeff d effet d echelle temporel (77)
        do i=1,nvari
          if (i.ne.(nvar0+43)) then
             if (i.ne.(nvar0+58)) then
                if(i.ne.(nvar0+77)) then
                   varf(i)=var0(i)
                end if
             end if
          end if
        end do
      end if
!********************************************************************************
!     remise a zero des variables internes en pre-hydratation
!     excepté l hydratation et la temperature de controle nvar0(1)
!     excepte la taille principale des elements var0(nvar1+1) a var0(nvar1+nvtail)
!     excepte l indicateur d initialisation des conditions initiales var0(29)
!     le materiau est "neuf" a chaque iteration tant q'on a pas passe le seuil
!     d hydratation
!********************************************************************************
      if ((istep.eq.0 .or. istep.eq.2).and.(hydra1.le.hydras)) then
        do i=1,nvari
         if(i.le.nvar1 .or. i.gt.(nvar1+nvtail)) then
!          si pas les vecteurs propres de la taille des elements
           if(i.ne.1) then
!            si pas l hydratation
             if(i.ne.29) then
!             si pas l indicateur d intialisation des CI
              if(fl3d) then
!                si on passe par le fluage on exclue la mise a zero de la temperature
                 if (i.ne. (nvhydra+36)) then
                     varf(i)=0.d0
                 end if
              else
                 varf(i)=0.d0
              end if
             end if
           end if
         end if
        end do
      end if
end subroutine
