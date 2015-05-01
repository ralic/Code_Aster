subroutine fluag3d(xmat, nmat, var0, varf, nvari,&
                   dt, depst, nstrs, sigf, mfr,&
                   erreur, teta1, teta2)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  Gestion du module FLUA_PORO_BETON
!  sous programme de calcul des contraintes effective de fluage propre
!=====================================================================
    implicit none
#include "asterf_types.h"
#include "asterfort/hydr_xmat.h"
#include "asterfort/b3d_actherm.h"
#include "asterfort/hydr_vari.h"
#include "asterfort/b3d_pm.h"
#include "asterfort/flu_inc3d.h"
#include "asterfort/b3d_partition.h"
#include "asterfort/x6x33.h"
#include "asterfort/b3d_chrep.h"
#include "asterfort/x33x6.h"
#include "asterfort/b3d_sigd.h"
#include "asterfort/b3d_dth.h"
#include "asterfort/utmess.h"
!      declaration externe
    integer :: nmat, nvari, nstrs, mfr, iter
    real(kind=8) :: depst(nstrs)
    real(kind=8) :: xmat(nmat), var0(nvari), varf(nvari), sigf(nstrs)
    real(kind=8) :: dt
    integer :: erreur
    real(kind=8) :: teta1, teta2
!
!      declaration locales
    integer :: i, nmat0, nbrhyd3d, nbelas3d, nvhydra
!      nombre de parametres communs(idvisc) et nvhydra(idvar4) nbr vari
    parameter(nbrhyd3d=24,nbelas3d=4,nvhydra=29)
!      nombre de parametre avant le chargement des parametres de fluage
!      CF xmat A MODIFIER SI LE MODELE DE FLUAGE N A PAS SES PARA EN 1ER
    parameter (nmat0=nbelas3d+nbrhyd3d)
    real(kind=8) :: dfmin
    parameter (dfmin=1.d-3)
    real(kind=8) :: depsd6(6), siged6(6), e0df6(6), depst6(6), sigef6(6)
!      variables gerant l evolution de l hydratation
    real(kind=8) :: hydra1, hydra2, hydras
!      variables gerant la consolidation
!      coeffs de ponderation des deformations cumulees pour la
!      construction de la variables interne de consolidation
    real(kind=8) :: xsomme
!      caracteristiques materiau
    real(kind=8) :: e0, xnu0, xk0, x2mu0, xk1, hs1, hs2, xx1, xx2
    real(kind=8) :: Eafluage
!      coeff d activation thermique
    real(kind=8) :: unsurtr, unsurts, unsurt
    real(kind=8) :: easurr, cvth1, cpth1, xxx1, xxx2, cvth2, cpth2
!      calcul de la contrainte equivalente
    real(kind=8) :: x33(3, 3), x3(3)
!      parametres viscoplastiques
    real(kind=8) :: ssg6(6), epsplg6(6), dg3(3), epsplfg6(6)
!      gel dans la matrice
    real(kind=8) :: bg1, pg1
!      contrainte effectives endommagees
    real(kind=8) :: sigef3(3), vsigef33(3, 3), vsigef33t(3, 3)
    real(kind=8) :: sigeft6(6), sigefc6(6), sigefc3(3), sigeft3(3)
    real(kind=8) :: sigaf6(6)
!      contraintes totales apres endommagement
    real(kind=8) :: sigat6(6)
!      direction principale des endo diffus reels
    real(kind=8) :: vplg33(3, 3), vplg33t(3, 3)
!      endo diffus hydrique
    real(kind=8) :: dw3(3), ssw6(6), vssw33(3, 3), vssw33t(3, 3)
!      parametre de confinement
!       real*8 deltaDP,alphaS
!      controle du passage post pic en traction
    aster_logical :: locafluage
!      vitesse de fluage de l etage visqueux
    real(kind=8) :: vflu6(6), vflu33(3, 3)
    real(kind=8) :: vfluf6(6)
    real(kind=8) :: vflui6(6)
!      contraintes non endo init
    real(kind=8) :: sigef06(6), sigef033(3, 3)
!      controle de la boucle de sous iteration
    aster_logical :: err_iter
!      increment de contraintes
    real(kind=8) :: dsigd6(6)
!      pression hydriques
    real(kind=8) :: dpw, pw0
    real(kind=8) :: xk00, xmu00, dt80, epsk0, dmax0, rc1, rt1, epc1, ept1, etaw1
    real(kind=8) :: rc1e, cohet1, epsplg0, xwnsat0, xwnsat, poro0, biot00, umbiot00
    real(kind=8) :: biot0, umbiot0, xwsat, vw1, vw0, xmsrt0, xmsrt1, sfld, vg0, vp0, xmg0
    real(kind=8) :: taug0, beta0, cohec1, x2mu1, hd1, hd2, easurrw, etag1, xk0i
    real(kind=8) :: xk1i, x2mu0i, hd1i, hs2iref, unsek0, unsek01, epsk01
    real(kind=8) :: hs2i, xk0f, xk1f, x2mu1f, hs1f, x2mu1i, hs1i, hd2iref
    real(kind=8) :: hd2i, hd1f, hs2fref, cohet2, cohec2, e1, rt2
    real(kind=8) :: phis0, phid0, phisref1, phidref1, dflu00, dv0, epsvpw
    real(kind=8) :: pw1, bw1, depsv, cci0p, dflu01, cci1p, hs2f, cci0m
    real(kind=8) :: x2mu0f, hd2fref, dflu0, hd2f, vk0, vk1, v2mu0, v2mu1
    real(kind=8) :: vhs1, vhd1, vhs2, vhd2, xx4, phis1, phid1, e0si, e1si
    real(kind=8) :: e2sf, dsigs, e2si, ve1si, ve2si, e0sf, e1sf, ve1sf, ve2sf, dphis1
    real(kind=8) :: e0di, e1di, e2df, ve1df, ve2df, dphid1, siges, dsige, xj2, xi1
    real(kind=8) :: sigeqc1, sigem, phit0, xphit, e2di, ve1di, ve2di, e1df
    real(kind=8) :: dphiv0, phit1, phit2, tauc1, tau3, epsk1, epseq1, depseq
    real(kind=8) :: xw1, dflu1, ddflu0max, ddflu01, cci2p, dth0, tau4, epsk00
    real(kind=8) :: xw3, epseq0, dcci0p, ddflu0
!***********************************************************************
! Chargement DES PROPRIETES MECANIQUES
!      if (mfr.eq.33)then
!      print*,'on est bien dans fluage3d en poreux'
!      do i=1,nmat
!       print*,'xmat(',i,')=',xmat(i)
!      end do
!      read*
!      end if
!     elasticite isotrope pour hydratation achevee
!
!     recuperation du coeff de compressibilite draine
    xk00=xmat(nbelas3d+19)
    if (xk00 .ne. 0.) then
!      print*,'coeff elastique recalcules a partir de k et mu fluag3d'
!      re affection des coeff draines pour le calcul des contraintes
        xk0=xk00
!      coefficient de cisaillement
        xmu00=xmat(nbelas3d+20)
        x2mu0=2.d0*xmu00
!      recalcul des coeffs E et nu compatibles avec k et mu
        e0=9.d0*xk00/(1.d0+3.d0*xk00/xmu00)
        xx1=(2.d0*xmu00)/(3.d0*xk00)
        xnu0=(1.d0-xx1)/(2.d0+xx1)
    else
!      print*,'on adopte les coeff du modele elastique dans fluage3d'
!      young non draine
        e0=xmat(1)
!      poisson non draine
        xnu0=xmat(2)
!      compressibilite non draine
        xk0=e0/3.d0/(1.d0-2.d0*xnu0)
!      cisaillement
        x2mu0=e0/(1.d0+xnu0)
    end if
!
!     endommagemet thermique a 80°C
    dt80=xmat(nbelas3d+21)
!
!     hydratation fin de pas et seuil de percolation meca
    hydra2=xmat(nbelas3d+1)
    varf(1)=hydra2
!     hydratation seuil de percolation meca
    hydras=xmat(nbelas3d+2)
!
!     hydratation debut de pas
    hydra1=var0(1)
    if (hydra1 .eq. 0.) then
!      print*,'initialisation hydatation a ', hydra2
        hydra1=hydra2
    end if
!
!     parametres de fluage pour hydratation achevee et temperature 20°C
!     les temps caracteristiques sont independant de l'hydratation
!     compressibilite etage 1 (Kelvin Voigt)/ Young 0 (elasticite)
    xk1=xmat(nmat0+1)*xk0
!     viscosite  etage 1 (Kelvin Voigt) definie par temps caract / kelvin
    hs1=xmat(nmat0+2)*xk1
!     viscosite initiale etage (Maxwell) definie pat temps caract / elasticite
    hs2=xmat(nmat0+3)*xk0
!     Deformation caracteristique de consolidation spherique
    epsk0=xmat(nmat0+4)
!     Endommagement maxi de fluage
    dmax0=xmat(nmat0+5)
!
!     coeff d'influence de la deformation sphérique sur
!     la contriante equivalente de fluage
!      alphas=xmat(nmat0+5)
!      if (thetad0.lt.0.5001d0) then
!       print*,'Valeur de thetad0 trop faible pour fluag3d'
!       erreur=1
!       return
!      else
!       thetas0=1.d0-thetad0
    erreur=0
!      end if
!
!
!     Pic en compression pour la loi de fluage
    rc1=xmat(nbelas3d+3)
!     Pic en traction pour la loi de fluage
    rt1=xmat(nbelas3d+4)
!     deformation au pic de compression
    epc1=xmat(nbelas3d+5)
!     verif  coherence
    epc1=max(rc1/e0,epc1)
!     deformation au pic de traction
    ept1=xmat(nbelas3d+6)
!     verif coherence
    ept1=max(rt1/e0,ept1)
!     rc effectice
    rc1e=e0*epc1
!     rt effective
    cohet1=e0*ept1
!     deformation caracteristique viscoplastique gel
    epsplg0=xmat(nbelas3d+7)
!     module equivelent pour l eau en non saturé (cf b3d_bwpw)
    xwnsat0=xmat(nbelas3d+18)
    call hydr_xmat(xwnsat0, xwnsat, hydra1, hydras, 4.d0,&
                   erreur)
!     poro totale
    poro0=xmat(nbelas3d+15)
    if (mfr .ne. 33) then
!      on est pas en formulation poreux
!      on adopte les valeurs de fluag3d
!      coeff de Biot
        biot00=xmat(nbelas3d+8)
!      effet de l hydratation sur biot
        umbiot00=1.d0-biot00
        call hydr_xmat(umbiot00, umbiot0, hydra1, hydras, 0.5d0,&
                       erreur)
        biot0=1.d0-umbiot0
!      module de Biot du fluide en sature
        xwsat=xmat(nbelas3d+10)
!      volume d eau final
        vw1=xmat(nbelas3d+9)
!      volume d eau ramnene a la poro actuelle
        vw0=poro0+var0(nvhydra+37)
!      mise a jour de l apport de volume fluide / porosite
        varf(nvhydra+37)=vw1-poro0
    else
!      on est en formulation poreux
!      on adopte la valeur de la poro-meca couplee
!      si biot depend de l hydratation ce nest pas considere ici
        biot0=xmat(nmat-1)
!      module de Biot du fluide en sature
        xwsat=xmat(nmat)
!      xwnsat=xwsat si poreux car coeff unique dans poreux
        xwnsat=xwsat
!      le volume d eau est calcule a partir de l apport de masse
!      fluide normalise
!      l apport de masse fluide est egalement disponible sigf(nstrs)=msr0
        xmsrt0=var0(nvhydra+37)
!       xmsrt1=xmsrt0+sigf(nstrs)
        xmsrt1=sigf(nstrs)
        varf(nvhydra+37)=xmsrt1
        vw0=poro0+var0(nvhydra+37)
        vw1=poro0+var0(nvhydra+37)
    end if
!      print*,'dans fluag3d'
!      print*,'biot',biot0,' mob',xwsat,' vw0', vw0
!      read*
!
!     heterogeneite contrainte hydrique
    sfld=xmat(nbelas3d+11)
!     volume de gel
    vg0=xmat(nbelas3d+12)
!     compressibilite du gel
    xmg0=xmat(nbelas3d+13)
!     poro accessible au gel
    vp0=xmat(nbelas3d+14)
!
!     temps caracteristique ecoulement du gel
    taug0=xmat(nbelas3d+16)
!     energie d activation des viscosites
    Eafluage=xmat(nbelas3d+17)
!
!      xmw1=-1.d0/log(1.d0-dmax0)
!      print*,'tetat3',tetat3
!      print*,'tetac3',tetac3
!
!***********************************************************************
!     calcul des proprietes de fluage a 20°C et hydratation=1
!
!     attention comme on travaille avec epsilon et non gama
!     on redefinit beta=k/2mu (et non k/mu)
    beta0=xk0/x2mu0
!     coeffs multiplicateurs pour la deformation uniaxiale equivalente
    xx2=6.d0*beta0/rc1/(1.d0+2.d0*beta0)
    cohec1=rc1
!     cisaillement kelvin voigt
    x2mu1=xk1/beta0
!     viscosite dev KV
    hd1=hs1/beta0
!     viscosite dev Maxwell
    hd2=hs2/beta0
!
!***********************************************************************
!     coeff de prise en compte de la temperature sur la vitesse de
!     fluage et sur la consolidation
!     ******************************************************************
!     on utilise un vari pour stocker la temperature fin de pas (controle)
    varf(nvhydra+36)=teta2
!     calcul du terme d activation d Arrhenius pour le potentiel
    easurr=Eafluage/8.31D0
!     cas de l'activation des viscostés
!      easurrw=2059.d0/8.31d0
    easurrw=easurr
!     la temperature de reference est 20°C
    unsurtr=0.003412969283276451D0
!     unsurts la temperature de seuil pour la modif du potentiel
!      40°C
!      unsurts=0.0031948D0
!      20°C
    unsurts=unsurtr
!     calcul des coeff d activation thermique
    if (Eafluage .ne. 0.) then
!      debut de pas
        unsurt=(1.D0/(teta1+273.D0))
!      cas de la viscosite
        cvth1=exp(-easurrw*(unsurt-unsurtr))
!      cas du potentiel
        xxx1=unsurts-unsurt
        xxx2=0.5D0*(xxx1+abs(xxx1))
        cpth1=exp(easurr*xxx2)
!      fin de pas
        unsurt=(1.D0/(teta2+273.D0))
!      cas de la viscosite
        cvth2=exp(-easurrw*(unsurt-unsurtr))
!      cas du potentiel
        xxx1=unsurts-unsurt
        xxx2=0.5D0*(xxx1+abs(xxx1))
        cpth2=exp(easurr*xxx2)
    else
        cvth1=1.d0
        cpth1=1.d0
        cvth2=1.d0
        cpth2=1.d0
    end if
!      if((teta2.lt.20.).or.(teta1.lt.20.).or.(dt.eq.0.))then
!       print*, 'teta',teta1,teta2,'dt',dt
!       print*,'teta1',teta1,'cpth1',cpth1,'cvth1',cvth1
!       print*,'teta2',teta2,'cpth2',cpth2,'cvth2',cvth2
!      read*
!      endif
!***********************************************************************
!     initialisation du compteur de sous iteration
    iter=1
    err_iter=.false.
!
!***********************************************************************
!     prise en compte de la temperature au debut du pas
!     modification des viscosites
    hs1=hs1/cvth1
    hd1=hd1/cvth1
    hs2=hs2/cvth1
    hd2=hd2/cvth1
!     prise en compte de la temperature sur la viscosite du gel
    call b3d_actherm(teta1, Eafluage, xmg0, xwsat, taug0,&
                     etag1, etaw1)
!
!***********************************************************************
!     calcul des proprietes meca en fonction du degre d hydratation
!     au debut du pas
!
    call hydr_xmat(xk0, xk0i, hydra1, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(xk1, xk1i, hydra1, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(x2mu0, x2mu0i, hydra1, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(x2mu1, x2mu1i, hydra1, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(hs1, hs1i, hydra1, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(hd1, hd1i, hydra1, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(hs2, hs2iref, hydra1, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(hd2, hd2iref, hydra1, hydras, 0.33d0,&
                   erreur)
!     on suppose que le potentiel de consolidation depend  de l hydratation
!     via son inverse, il est constant sur le pas
    unsek0=epsk0**(-1)
    call hydr_xmat(unsek0, unsek01, hydra1, hydras, 0.33d0,&
                   erreur)
    epsk01=unsek01**(-1)
!     traitement de l incoherence des donnees  d hydratation
    if (erreur .ne. 0) then
        print*,'Pb ds la prise en compte de l hydratation dans fluage3d'
        call utmess('F', 'COMPOR1_90')
    end if
!
!***********************************************************************
!     prise en compte de la consolidation en debut de pas
!     (celle du pas d avant)
    cci0m=max(var0(nvhydra+48),1.d0)
    hs2i=hs2iref*cci0m
    hd2i=hd2iref*cci0m
!
!***********************************************************************
!     prise en compte de la temperature en fin du pas
!     modification des viscosites de reference
    hs1=hs1*cvth1/cvth2
    hd1=hd1*cvth1/cvth2
    hs2=hs2*cvth1/cvth2
    hd2=hd2*cvth1/cvth2
!     modification du potentiel de consolidation
!      epcs2=epcs1/cpth1*cpth2
!      if (teta2.ge.teta1) then
!      le potentiel augmente avec la temperature
!      les variables internes   ne sont pas affectees
!      cvc12=1.d0
!      else
!      lorsque la temperature baisse ! est le coeff de consolidation
!      qui est bloque, donc on modifie les variables internes en consequence
!       cvc12=cpth2/cpth1
!      end if
!
!***********************************************************************
!     calcul des proprietes meca en fonction du degre d hydratation
!     en fin du pas
!
    call hydr_xmat(xk0, xk0f, hydra2, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(xk1, xk1f, hydra2, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(x2mu0, x2mu0f, hydra2, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(x2mu1, x2mu1f, hydra2, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(hs1, hs1f, hydra2, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(hd1, hd1f, hydra2, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(hs2, hs2fref, hydra2, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(hd2, hd2fref, hydra2, hydras, 0.33d0,&
                   erreur)
    call hydr_xmat(cohet1, cohet2, hydra2, hydras, 0.4d0,&
                   erreur)
    call hydr_xmat(cohec1, cohec2, hydra2, hydras, 0.4d0,&
                   erreur)
    call hydr_xmat(e0, e1, hydra1, hydras, 0.33d0,&
                   erreur)
    rt2=rt1*e1/e0
!
!     recuperation des dissipations visqueuses
    phis0=var0(nvhydra+44)
    phid0=var0(nvhydra+45)
!     prise en compte des effets de l hydratation
!     sur les dissipation d energie par fluage
    call hydr_vari(phis0, phisref1, hydra1, hydra2, hydras,&
                   erreur)
    call hydr_vari(phid0, phidref1, hydra1, hydra2, hydras,&
                   erreur)
!
!     endommagement effectif de fluage fin de pas precedent
    dflu00=max(var0(nvhydra+47),0.d0)
!     correction par l hydratation
    call hydr_vari(dflu00, dflu0, hydra1, hydra2, hydras,&
                   erreur)
!
!***********************************************************************
!     decomposition de l increment de deformation
!     chargement de l'increment de deformation totale
!     changement said ok
    if (mfr .ne. 33) then
!      formulation massive classique
        do i = 1, nstrs
            depst6(i)=depst(i)
!        print*,'depst(',i,')=',depst(i)
        end do
        if (nstrs .lt. 6) then
            do i = (nstrs+1), 6
                depst6(i)=0.d0
            end do
        end if
    else
!      formulation poreux la derniere composante de deformation
!      est réservee pour le fluide
!       print*,'on est en poreux dans fluag3d, nstrs=',nstrs
!       read*
        do i = 1, (nstrs-1)
            depst6(i)=depst(i)
!        print*,'depst(',i,')=',depst(i)
        end do
        if ((nstrs-1) .lt. 6) then
            do i = nstrs, 6
                depst6(i)=0.d0
            end do
        end if
    end if
!
!***********************************************************************
!     prise en compte des endos diffus dus au gel et a l eau pour la
!     deduction de la part viscoplastique
!***********************************************************************
!     chargement des variables viscoplastique
    do i = 1, 6
        call hydr_vari(var0(1+i), ssg6(i), hydra1, hydra2, hydras,&
                       erreur)
        call hydr_vari(var0(7+i), epsplg6(i), hydra1, hydra2, hydras,&
                       erreur)
        call hydr_vari(var0(18+i), ssw6(i), hydra1, hydra2, hydras,&
                       erreur)
    end do
!     recuperation des endommagement viscoplastique du pas precedent
    do i = 1, 3
        call hydr_vari(var0(13+i), dg3(i), hydra1, hydra2, hydras,&
                       erreur)
    end do
!     volume visco elastique initial
    dv0=var0(nvhydra+1)+var0(nvhydra+2)+var0(nvhydra+3)
!      print*,'dv0',dv0
!     on reprend les pressions du pas precedent
    pw0=var0(17)
    pg1=var0(18)
!     actualisation de la def viscoplastique, calcul de la pression,
!     et des nouveaux endommagements de pression de gel
    if (mfr .eq. 33) then
!       on est en poreux  l increment de la pression a deja été calculee
!       dp=depst(nstrs)
        dpw=depst(nstrs)
!       recup de la masse de fluide
        if (vw0 .ne. vw1) then
            print*,'fluag3d volume de fluide  init',vw0
            print*,'fluag3d volume de fluide final',vw1
            read*
        end if
!       b3d pm ne calcule que la pression de gel
    end if
!     prise en compte de l ouverture de fissure dans le calcul de pression
    epsvpw=0.d0
    call b3d_pm(vg0, vp0, dv0, depst6, ssg6,&
                dg3, epsplg6, epsplfg6, etag1, bg1,&
                xmg0, pg1, dt, epsplg0, vw0,&
                poro0, biot0, xwsat, pw1, bw1,&
                epsvpw, vplg33, vplg33t, e1, rt2,&
                ept1, erreur, xwnsat, mfr, pw0,&
                dpw, vw1)
!     mise a jour des variables internes de def plastique
    varf(17)=pw1
    varf(18)=pg1
    do i = 1, 6
        varf(7+i)=epsplfg6(i)
        if (i .le. 3) then
            varf(13+i)=dg3(i)
        end if
    end do
!**********************************************************************
!      mise a jour des deformations viscoelastiques pour le fluage
!      on enleve les fissures remplies de gel du systeme paralelle
!      pour les mettre en serie (dgama ds depst6)
    do i = 1, 6
        if (i .le. 3) then
            depst6(i)=depst6(i)-(epsplfg6(i)-epsplg6(i))
        else
            depst6(i)=depst6(i)-2.d0*(epsplfg6(i)-epsplg6(i))
        end if
    end do
!**********************************************************************
!     calcul des increment volumique et deviatoriques pour le modele de
!     fluage a partir de la deformation totale moins la viscoplastique
    depsv=0.d0
    do i = 1, 3
        depsv=depsv+depst6(i)
    end do
    do i = 1, 3
        depsd6(i)=depst6(i)-(depsv/3.d0)
    end do
    do i = 4, 6
        depsd6(i)=0.5d0*depst6(i)
    end do
!***********************************************************************
!     initialisation du compteur d iteration pour les sous iteration internes
    iter=1
!***********************************************************************
!     recup du coeff de consolidation fin du pas precedent ou fin sous iteration prec
 10 continue
    if (iter .eq. 1) then
!         print*,'1er passage fuag3d'
!        si 1ere sous iteration on prend la fin du pas precedent
!        l effet de l hydratation est deja considere a la fin du pas
!        precedent donc on ne s en occupe pas
        cci0p=cci0m
        dflu01=dflu0
    else
!         print*,'sous iteration:' ,iter
!     #  ,' dans fluag2d critere:',cci1p-cci0p
        cci0p=cci1p
        if (iter .gt. 200) then
            print*,'iter max atteint ds fluag3d'
            print*,'reduire la taille des pas'
            print*,depst6,cci1p
            err_iter=.true.
            goto 20
        end if
    end if
!
    if (hydra2 .le. hydras) then
!      pas de consolidation avant le seuil d hydratation
        cci0p=1.d0
    end if
!
!**********************************************************************
!     nouvelle viscosite consolidees pour ce pas pour etage 2
!     modif juin 2013 pour calage Multon endo que sur Maxwell
    hs2f=hs2fref*cci0p*(1.d0-dflu01)
    hd2f=hd2fref*cci0p*(1.d0-dflu01)
    hs2i=hs2iref*cci0m*(1.d0-dflu0)
    hd2i=hd2fref*cci0m*(1.d0-dflu0)
!
!***********************************************************************
!     Calcul des vitesses d evolution des carac meca
!     pour la resolution incrementale du modele
!     (0 si evol negative pour elasticite pour prendre en compte
!     que la solidification est libre de toute contrainte)
    if (dt .gt. 0.d0) then
        vk0=(xk0f-xk0i)/dt
        vk0=0.5d0*(vk0-abs(vk0))
        vk1=(xk1f-xk1i)/dt
        vk1=0.5d0*(vk1-abs(vk1))
        v2mu0=(x2mu0f-x2mu0i)/dt
        v2mu0=0.5d0*(v2mu0-abs(v2mu0))
        v2mu1=(x2mu1f-x2mu1i)/dt
        v2mu1=0.5d0*(v2mu1-abs(v2mu1))
        vhs1=(hs1f-hs1i)/dt
        vhd1=(hd1f-hd1i)/dt
        vhs2=(hs2f-hs2i)/dt
        vhd2=(hd2f-hd2i)/dt
    end if
!
!***********************************************************************
!     decomposition des deformations suivant le modele de fluage
!     ******************************************************************
!     en temperature pour une meme def la dissipation est moindre :
!     on peut majorer  la dissipation si on souhaite stocker
!     une deformation equivalente independant de la temperature
!     coeff correcteur de dissipation par la temperature sur le pas
    xx4=(0.5d0*(cvth1+cvth2))**(-1)
!      xx4=1.d0
!     recuperation des dissipations du pas precedent actualises par l hydratation
    phis1=phisref1
    phid1=phidref1
    if (dt .gt. 0.) then
!     ***fluage spherique***********************************************
!     chargement des variables internes du modele rheologique
        e0si=var0(nvhydra+1)
        e1si=var0(nvhydra+2)
        e2si=var0(nvhydra+3)
        ve1si=var0(nvhydra+4)
        ve2si=var0(nvhydra+5)
!     modif pour assurer la continuite de la contrainte en debut de pas
!      ve2si=ve2si/coeff_s2
        call flu_inc3d(e0si, e1si, e2si, ve1si, ve2si,&
                       xk0i, xk1i, hs1i, hs2i, vk0,&
                       vk1, vhs1, vhs2, depsv, dt,&
                       e0sf, e1sf, e2sf, dsigs, ve1sf,&
                       ve2sf, dphis1)
!      print*,'spherique'
!      print*,e0si,e1si,e2si,ve1si,ve2si,xk0f,xk1f,hs1f,hs2f,
!     #vk0,vk1,vhs1,vhs2,depsv,dt,e0sf,e1sf,e2sf,ve1sf,ve2sf
        varf(nvhydra+1)=e0sf
        varf(nvhydra+2)=e1sf
        varf(nvhydra+3)=e2sf
        varf(nvhydra+4)=ve1sf
        varf(nvhydra+5)=ve2sf
!     actualisation de lenergie dissipee dans le fluage spherique
        if (hydra2 .gt. hydras) then
!      on ne comptabilise la dissipation que si le materiau est solide
            phis1=phis1+dphis1*xx4
        else
            phis1=0.d0
        end if
        varf(nvhydra+44)=phis1
!
!     ***fluages deviatoriques******************************************
        xsomme=0.d0
        do i = 1, 6
            e0di=var0(nvhydra+5*i+1)
            e1di=var0(nvhydra+5*i+2)
            e2di=var0(nvhydra+5*i+3)
            ve1di=var0(nvhydra+5*i+4)
            ve2di=var0(nvhydra+5*i+5)
!      modif pour assurer la continuite de la contrainte en debut de pas
!       ve2di=ve2di/coeff_d2
            call flu_inc3d(e0di, e1di, e2di, ve1di, ve2di,&
                           x2mu0i, x2mu1i, hd1i, hd2i, v2mu0,&
                           v2mu1, vhd1, vhd2, depsd6(i), dt,&
                           e0df6(i), e1df, e2df, dsigd6(i), ve1df,&
                           ve2df, dphid1)
!       print*,'deviatorique ',i
!       print*,e0di,e1di,e2di,ve1di,ve2di,x2mu0f,x2mu1f,hd1f,
!     # hd2f,v2mu0,v2mu1,vhd1,vhd2,depsd6(i),dt,e0df6(i),e1df,e2df,
!     # ve1df,ve2df
            varf(nvhydra+5*i+1)=e0df6(i)
            varf(nvhydra+5*i+2)=e1df
            varf(nvhydra+5*i+3)=e2df
            varf(nvhydra+5*i+4)=ve1df
            varf(nvhydra+5*i+5)=ve2df
!      bilan energie dissipee sur le pas (en prenand en compte le fait
!      qu on travaille avec epsilon et non gama pour les termes hor digonale)
            if (i .le. 3) then
                xsomme=xsomme+dphid1
            else
                xsomme=xsomme+dphid1*2.d0
            end if
        end do
!     stockage energie dissipee cumulee
        if (hydra2 .gt. hydras) then
!      le materiau est solide, on comptabilise
            phid1=phid1+xsomme*xx4
        else
            phid1=0.d0
        end if
        varf(nvhydra+45)=phid1
    end if
!
!***********************************************************************
!     CALCUL DES CONTRAINTES EFFECTIVES
!     chargement des contraintes eff du pas d avant
!     les contraintes effectives sont stockees dans les var0, toutefois
!     si un sig0 a ete initialisee on a...
    siges=0.d0
    do i = 1, 6
        sigef6(i)=var0(nvhydra+37+i)
        if (i .le. 3) then
            siges=siges+sigef6(i)
        end if
    end do
    siges=siges/3.d0
    do i = 1, 3
        siged6(i)=sigef6(i)-siges
    end do
    do i = 4, 6
        siged6(i)=sigef6(i)
    end do
!
    if (dt .gt. 0.) then
!      calcul des nouvelles contraintes effectives
        do i = 1, 3
            sigef6(i)=sigef6(i)+dsigs+dsigd6(i)
        end do
        do i = 4, 6
            sigef6(i)=sigef6(i)+dsigd6(i)
        end do
    else
!       print*,'dt=0 ds fluag3d'
!       read*
!      dt=0, pas de fluage, increment de depst= deps elastique
!      par contre on conserve les vitesses car si dt=0
!      les vitesses n ont pas evoluees,seule le deformation
!      instantanee peut evoluer sur dt=0
        varf(nvhydra+1)=var0(nvhydra+1)+depsv
        varf(nvhydra+2)=var0(nvhydra+2)
        varf(nvhydra+3)=var0(nvhydra+3)
        varf(nvhydra+4)=var0(nvhydra+4)
        varf(nvhydra+5)=var0(nvhydra+5)
        varf(nvhydra+44)=phis1
        do i = 1, 3
            dsige=xk0f*depsv+x2mu0f*depsd6(i)
!       il faudrait ici prendre en compte l evolution possible negative
!       de l hydratation
            sigef6(i)=sigef6(i)+dsige
        end do
        do i = 4, 6
            dsige=x2mu0f*depsd6(i)
            sigef6(i)=sigef6(i)+dsige
        end do
        do i = 1, 6
            varf(nvhydra+5*i+1)=var0(nvhydra+5*i+1)+depsd6(i)
            varf(nvhydra+5*i+2)=var0(nvhydra+5*i+2)
            varf(nvhydra+5*i+3)=var0(nvhydra+5*i+3)
            varf(nvhydra+5*i+4)=var0(nvhydra+5*i+4)
            varf(nvhydra+5*i+5)=var0(nvhydra+5*i+5)
        end do
        varf(nvhydra+45)=phid1
    end if
!
!     rangement des contraintes effectives ds les variables internes
!      print*,'Dans fluage 3D'
    do i = 1, 6
        varf(nvhydra+37+i)=sigef6(i)
!         print*,'sigef(',i,')=',sigef6(i)
    end do
!      if (abs(sigef6(1)).gt.1.d3)then
!       print*,depst
!       print*,epsplg6
!       print*,
!      end if
!
!**********************************************************************
!     calcul des criteres d'endommagement par fluage
!      print*,'sigef6',sigef6
    call b3d_partition(sigef6, sigef3, vsigef33, vsigef33t, sigeft6,&
                       sigefc6, sigefc3, sigeft3)
!     *****************************************************************
!     on borne la contrainte de traction pour considerer que
!     la zone de fluage est limitée en traction par la contrainte pré
!     pic et  non par la contrainte effective, car la zone soumise
!     a la contrainte effective post pic a une taille faible
!     ******************************************************************
!     mettre local fluage a vrai pour activer la reduction
!     de dissipation en + de la reduction du critere
!     ******************************************************************
    locafluage=.false.
    do i = 1, 3
        if (sigef3(i) .gt. cohet2) then
            x3(i)=cohet2
!       il faudra egalement minorer la dissipation visqueuse pour ne pas
!       sur evaluer la consolidation dans l element fini a cause du fluage
!       en traction
            if (dt .ne. 0.) then
!        inutile si pas de temps nul pas de dissip visqueuse
                locafluage=.true.
            end if
        else
            x3(i)=sigef3(i)
        end if
    end do
!     contrainte deviatorique
    xj2=(x3(1)-x3(2))**2+(x3(2)-x3(3))**2+(x3(3)-x3(1))**2
    xj2=sqrt(xj2/6.d0)
!     contrainte moyenne
    xi1=(x3(1)+x3(2)+x3(3))/3.d0
!     contrainte equivalente (equivalence energetique)
    sigeqc1=sqrt(xj2**2+(xi1**2)*0.5d0/beta0)
!     contrainte de compression uniaxiale equivalente
    sigeqc1=sigeqc1/sqrt((1.d0+0.5d0/3.d0/beta0)/3.d0)
!     *****************************************************************
!     correction eventuelle de la dissipation visqueuse
!        print*,'cohesion',cohet2
!        print*,'contraintes prin'
!        print*,sigef3
    if (locafluage) then
!       il faut modifier la dissipation visqueuse
!       on se place pour cela dans la base principale
!       des contraintes actuelles
        do i = 1, 6
            if (i .le. 3) then
                vfluf6(i)=(varf(nvhydra+5))/3.d0+varf(nvhydra+5*i+5)
                vflui6(i)=(var0(nvhydra+5))/3.d0+var0(nvhydra+5*i+5)
            else
                vfluf6(i)=varf(nvhydra+5*i+5)
                vflui6(i)=varf(nvhydra+5*i+5)
            end if
!          vitesse de fluage moyenne sur le pas
            vflu6(i)=0.5d0*(vfluf6(i)+vflui6(i))
        end do
!       passage de la vitesse moyenne de fluage dans la base principale
!       des contraintes effectives
        call x6x33(vflu6, x33)
        call b3d_chrep(vflu33, x33, vsigef33)
        call x33x6(vflu33, vflu6)
!       recuperation des contraintes initiales et passage dans la base prin actuelle
        do i = 1, 6
            sigef06(i)=var0(nvhydra+37+i)
        end do
        call x6x33(sigef06, x33)
        call b3d_chrep(sigef033, x33, vsigef33)
        call x33x6(sigef033, sigef06)
!       modification des dissipations
        dphiv0=0.d0
        do i = 1, 3
!         contrainte moyenne sur la pas dans la direction i
            sigem=0.5d0*(sigef06(i)+sigef3(i))
            if (sigem .gt. cohet2) then
                dphiv0=dphiv0+(cohet2-sigem)*vflu6(i)*dt*xx4
            end if
        end do
!        print*,'d eps v prin'
!        print*,vflu6
!        print*, 'dphi',dphiv0, ' dt',dt
!        print*,dphiv0
!       calcul du coeff minorateur
        phit0=(phisref1+phidref1)
        phit1=(phis1+phid1)
!        print*,'phi av', phit0,' phi ap th', phit1+dphiv0
!       condition de croissance de la dissipation
        phit2=max(phit0,phit1+dphiv0)
!       coeff multiplicateur
        if (hydra2 .gt. hydras) then
            if (phit1 .ne. 0.) then
                xphit=phit2/phit1
            else
                xphit=1.d0
            end if
!            print*,'diss sphe av modif',phis1
            phis1=phis1*xphit
!            print*,'diss sphe ap modif',phis1
!            print*,'diss dev av modif',phid1
            phid1=phid1*xphit
!            print*,'diss dev ap modif',phid1
!            read*
        else
            phid1=0.d0
            phis1=0.d0
        end if
        varf(nvhydra+44)=phis1
        varf(nvhydra+45)=phid1
    end if
!       print*,xj2,xi1,deltadp,sigeqc1,cohec2
!
!**********************************************************************
!     calcul des taux de chargement en compression et traction
    tauc1=max((sigeqc1/cohec2)**2,dfmin)
    tauc1=min(tauc1,1.d0)
!      taut1=max(sigeqt1/cohet2,dfmin)
!      taut1=min(taut1,1.d0)
!      print*,'tauc',tauc1
!
!**********************************************************************
!     calcul de la deformation caracteristique de consolidation à 20°C
!     verification de la condition de croissance de epsk / endo maxi atteint
    if (dmax0 .eq. 0.) then
!       print*,'dmax0=0 ds fluag3d'
        tau3=tauc1
    else
        tau3=dflu0/dmax0
    end if
!     on prend une def de consolidation imposee par l'une des 2 conditions
!     tau de chragement actuel, endomax precedent
    tau4=max(tauc1,tau3)
!     relachement momentane de la consolidation par dilatation thermique
!     differentielle pate granulat
    epsk00=epsk01*0.5d0*(cpth1+cpth2)
    epsk1=epsk00*tau4
!      print*,'epsk0',epsk0,'epsk1',epsk1,'tau4',tau4,'cpth1', cpth1
!
!**********************************************************************
!     deformation equivalente à 20°C sous Rc/3 en fin de pas
!     (20°C car h1s est celui à 20°C : cf calcul de xx2 + haut)
    epseq1=min(xx2*(phis1+phid1),1.d0)
!     stockage ds les vari pour affichage
    varf(nvhydra+46)=epseq1
    epseq0=var0(nvhydra+46)
    depseq=epseq1-epseq0
!      print*,'eps eq',epseq1
!
!**********************************************************************
!     calcul du coeff de consolidation pour le prochain pas
    if (hydra2 .gt. hydras) then
        xw3=epseq1/epsk1
!        on borne le coeff de consolidation pour eviter les pb num
        xw3=min(xw3,6.907d0)
        cci1p=exp(xw3)
    else
        cci1p=1.d0
    end if
    dcci0p=cci1p-cci0m
!     print*,'coeff de consolidation', cci1p
!     read*
!     stockage du coeff de consolidation pour le pas suivant
    varf(nvhydra+48)=cci1p
!
!**********************************************************************
!     endommagement effectif de fluage en fin de pas
!     endommagement asymptotique de fluage à long terme
    xw1=tauc1
!      print*,'Tau de charge endommageant:',xw1
    dflu1=xw1*dmax0
!      print*,'dflu inf',dflu1
!     endommagement effectif de fluage en fin de pas (lendommagement asymptotique
!     n'est atteint que que si la consolidation est achevée
    dflu01=(1.d0-(cci1p**(-1)))*dflu1
!     condition de croissance de l'endommagement
    ddflu0max=max(dflu01-dflu0,0.d0)
    ddflu0=((0.5d0*(cci0m+cci1p))**(-1))*&
     (dmax0/epsk00)*max(depseq,0.d0)
    ddflu01=min(ddflu0,ddflu0max)
    dflu01=dflu0+ddflu01
!     l'endommagement de fluage ne peut pas diminuer
!      print*,'tauc',tauc1
!      print*,'dDflu0',ddflu0,'dDflu0max',ddflu0max
!      print*,'cc',cci1p,'dflu00',Dflu0, 'Dflu01',dflu01
!     stockage de l'endommagement de fluage
    varf(nvhydra+47)=dflu01
!      print*,'dflu0',dflu0
!      print*,'dflu1',dflu01
!
!***********************************************************************
!     test de convergence sur le coeff de consolidation
    if (abs((cci1p-cci0p)/max(cci0p,1.d0)) .gt. 1.d-6) then
!        sous iteration
        iter=iter+1
        goto 10
    end if
!      print*,'Nombre d iteration ds fluag3d:',iter
!     stockage du coeff de consolidation pour le pas suivant
!     apres attenuation due a l evolution de l hydratation
    20    call hydr_vari(cci1p,cci2p,hydra1,hydra2,hydras,erreur)
!     valeur min en cas d hydratation nulle ou inferieure au seuil
    varf(nvhydra+48)=cci2p
!
!
!************************************************************************
!     calcul de l endommagement thermique fin de pas
!     recuperation de l endo thermique et effet de l'hydratation
    call hydr_vari(var0(28), dth0, hydra1, hydra2, hydras,&
                   erreur)
    call b3d_dth(teta2, dth0, dt80)
!     mise a jour de l endo thermique
    varf(28)=dth0
!
!************************************************************************
!     prise en compte des endos diffus, de l amplification de depression
!     en non sature, de la surpression en non poreux sature, de la pression
!     eventuelle de gel
!     print*,'on fait le calcul poro meca 2 ds fluage 3d'
    call b3d_sigd(bg1, pg1, bw1, pw1, sfld,&
                  ssg6, e1, rt2, ept1, mfr,&
                  erreur, dg3, dw3, xnu0, sigaf6,&
                  dflu01, sigef6, xmg0, sigat6, vplg33,&
                  vplg33t, vssw33, vssw33t, ssw6, dth0)
!     stockage des seuils d endo diffus  et de l'endo diffus hydrique
    do i = 1, 6
        varf(18+i)=ssw6(i)
        varf(1+i)=ssg6(i)
        if (i .le. 3) then
            varf(24+i)=dw3(i)
        end if
    end do
!
!************************************************************************
!     stockage dans le vecteur des contraintes du code EF
!      print*,'fin de fluage 3d'
    if (mfr .ne. 33) then
!      cas massif non poreux
        do i = 1, nstrs
            sigf(i)=sigat6(i)
!        sigf(i)=sigef6(i)
!        print*,'sigf(',i,')=',sigf(i)
!        print*,varf(nvhydra+37+i)
        end do
!      read*
    else
!      cas massif poreux
        do i = 1, nstrs-1
            sigf(i)=sigat6(i)
!        print*,'sigf(',i,')=',sigf(i)
!        print*,varf(nvhydra+37+i)
        end do
    end if
    if (err_iter) then
        erreur=1
    end if
end subroutine
