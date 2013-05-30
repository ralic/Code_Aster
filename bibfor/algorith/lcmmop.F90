subroutine lcmmop(fami, kpg, ksp, comp, nbcomm,&
                  cpmono, nmat, nvi, vini, x,&
                  dtime, mod, coeft, epsd, detot,&
                  coel, nbphas, nfs, nsg, toutms,&
                  dvin, nhsr, numhsr, hsr, itmax,&
                  toler, iret)
    implicit none
    include 'asterfort/calsig.h'
    include 'asterfort/lcloca.h'
    include 'asterfort/lcmmec.h'
    include 'asterfort/lcmmfe.h'
    include 'asterfort/lcmmfi.h'
    include 'asterfort/lcnrts.h'
    integer :: kpg, ksp, nmat, nbcomm(nmat, 3), nvi, nbphas, itmax, iret
    integer :: nfs, nsg, nhsr, numhsr(*)
    real(kind=8) :: vini(*), dvin(*), nu, e, x, dtime, coeft(nmat), coel(nmat)
    real(kind=8) :: sigi(6), epsd(6), detot(6)
!     POUR GAGNER EN TEMPS CPU
    real(kind=8) :: toutms(nbphas, nfs, nsg, 7)
    real(kind=8) :: toler, hsr(nsg, nsg, nhsr)
    character(len=*) :: fami
    character(len=16) :: comp(*)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
! TOLE CRP_21 CRS_1404
! ======================================================================
!       IN   FAMI   : FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!            KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!           COMP    :  NOM DU MODELE DE COMPORTEMENT
!           MOD     :  TYPE DE MODELISATION
!           IMAT    :  ADRESSE DU MATERIAU CODE
!         NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!         CPMONO :  NOMS DES LOIS MATERIAU PAR FAMILLE
!           PGL   : MATRICE DE PASSAGE GLOBAL LOCAL
!           NVI     :  NOMBRE DE VARIABLES INTERNES
!           VINI    :  VARIABLES INTERNES A T
!           X       :  INTERVALE DE TEMPS ADAPTATIF
!           DTIME   :  INTERVALE DE TEMPS
!           COEFT   :  COEFFICIENTS MATERIAU INELASTIQUE A T
!           EPSD    :  DEFORMATION TOTALE A T
!           DETOT   :  INCREMENT DE DEFORMATION TOTALE
!     OUT:
!           DVIN    :  DERIVEES DES VARIABLES INTERNES A T
! INTEGRATION DES LOIS POLYCRISTALLINES PAR UNE METHODE DE RUNGE KUTTA
!
!     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
!     INTERNES DU MODELE
!
!       OBJETS DE STOCKAGE DES COMPORTEMENTS:
!           COEFT(*) = Fractions Volumiques et angles de chaque phase
!                      + COEFFICIENT DE CHAQUE COMPORTEMENT MONOCRSITAL
!                        pour chaque famille de systèmes de glissement
!                        à la température actuelle (COEFTF)
!                       et à la température précédente (COEFTD)
!           NBCOMM = indices des coefficents de chaque comportement
!                    dans COEFT(*,2)
!           CPMONO = noms des différentes "briques" de comportement
!
!      STRUCTURE DES OBJETS CREES
!
!           COEFT(*) : Nombre de monocristaux
!                        indice debut premier monocristal
!                        indice debut deuxième monocristal
!..............
!                        indice debut denier monocristal
!                        indice des paramètes localisation
!                        Fv et 3 angles par phase
!           pour chaque monocristal différent
!                 par famille de système de glissement
!                    nb coef écoulement + coef,
!                    nb coef écrou isot + coef,
!                    nb coef ecou cine + coef
!                        puis 2 (ou plus) paramètres localisation
!
!
!           CPMONO(*) : nom de la methode de localisation
!                 puis, pour chaque matériau différent
!                 nom du monocristal, nombre de familles SG, et,
!                    par famille de système de glissement
!                       Nom de la famille
!                       Nom du materiau
!                       Nom de la loi d'écoulement
!                       Nom de la loi d'écrouissage isotrope
!                       Nom de la loi d'écrouissage cinématique
!
!           NBCOMM(*,3) :
!                        Colonne 1      Colonne 2      Colonne3
!                    _____________________________________________
!
!            Ligne 1     Nb phases      Nb var.int.   Nb monocristaux
!                                                     différents
!   pour chaque phase g  Num ligne g    Ind CPMONO    ind frac vol
!   ..................
!   ...................
!   pour chaque phase
!   pour la localisation  indice coef   nb param      0
!   phase g              nb fam g       0            NVIg
!                ... et pour chaque famille de système de glissement :
!             famille 1  ind coef       ind coef      ind coef
!                        ecoulement     ecr iso       ecr cin
!    .....
!         (ind signifie l'indice dans COEFT(*)
!                    _____________________________________________
!     7 variables : tenseur EVP + Norme(EVP)
!    description des variables internes :
!    pour chaque phase
!        6 variables : beta ou epsilonp par phase
!    pour chaque phase
!        pour chaque systeme de glissement
!              3 variables Alpha, Gamma, P
!   1 variable : indic
!     ----------------------------------------------------------------
    character(len=8) :: mod
    character(len=16) :: necoul, necris, necrci
    character(len=24) :: cpmono(5*nmat+1)
    character(len=16) :: loca
    real(kind=8) :: vis(3), dt, evg(6), dl, da, gammas
    real(kind=8) :: evi(6), sigg(6), rp, devg(6), fv
    real(kind=8) :: devi(6), ms(6), taus, dgamma, dalpha, dp
    real(kind=8) :: devgeq, dbeta, beta, dvineq, granb(6)
    real(kind=8) :: crit, sgns, expbp(nsg), dy(nsg)
    integer :: itens, nbfsys, i, nuvi, ifa, nbsys, is, iv
    integer :: indpha, indfv, iphas, indcp, indfa, iexp
    integer :: ifl, nuecou, ihsr
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
! --  VARIABLES INTERNES
!
    do 5 itens = 1, 6
        evi(itens) = vini(itens)
        devi(itens) = 0.d0
 5  end do
    do 55 i = 1, nsg
        dy(i)=0.d0
55  end do
    iret=0
!
    call calsig(fami, kpg, ksp, evi, mod,&
                comp, vini, x, dtime, epsd,&
                detot, nmat, coel, sigi)
!
! LOCALISATION
!   RECUPERATION DU NOMBRE DE PHASES
!      NBPHAS=NBCOMM(1,1)
    loca=cpmono(1)
!     CALCUL DE  B
    do 53 i = 1, 6
        granb(i)=0.d0
53  continue
    do 54 i = 1, 6
        do 54 iphas = 1, nbphas
            indfv=nbcomm(1+iphas,3)
            fv=coeft(indfv)
            granb(i)=granb(i)+fv*vini(7+6*(iphas-1)+i)
54      continue
!
!
!     DEBUT DES VARIABLES INTERNES DES SYSTEMES DE GLISSEMENT
    nuvi=7+6*nbphas
    decal=nuvi
!
!     E et NU  sont utiles pour les règles de localisation
    if (coel(nmat) .eq. 0) then
!        CAS ISOTROPE
        e=coel(1)
        nu=coel(2)
    else if (coel(nmat).eq.1) then
!        CAS ANISOTROPE
!        pour calculer Mu. On prend la moyenne des Gij
        e=1.d0/coel(36+22)+(1.d0/coel(36+29))+(1.d0/coel(72))
        e=2.d0*e/3.d0
        nu=0.d0
    endif
!
    nbsyst=0
!
    do 1 iphas = 1, nbphas
!        INDPHA indice debut phase IPHAS dans NBCOMM
        indpha=nbcomm(1+iphas,1)
        indfv=nbcomm(1+iphas,3)
!
!         recuperer l'orientation de la phase et la proportion
!         INDORI=INDFV+1
        fv=coeft(indfv)
        call lcloca(coeft, e, nu, nmat, nbcomm,&
                    nbphas, sigi, vini, iphas, granb,&
                    loca, sigg)
        nbfsys=nbcomm(indpha,1)
        indcp=nbcomm(1+iphas,2)
!        Nombre de variables internes de la phase (=monocristal)
!         NVIG=NBCOMM(INDPHA,3)
        do 51 itens = 1, 6
            devg(itens) = 0.d0
            evg(itens) = 0.d0
51      continue
        ihsr=numhsr(iphas)
!
        do 6 ifa = 1, nbfsys
!
            necoul=cpmono(indcp+5*(ifa-1)+3)
            necris=cpmono(indcp+5*(ifa-1)+4)
            necrci=cpmono(indcp+5*(ifa-1)+5)
!
            nbsys=nint(toutms(iphas,ifa,1,7))
!
!           indice de la famille IFA
            indfa=indpha+ifa
!
            ifl=nbcomm(indfa,1)
            nuecou=nint(coeft(ifl))
!
            do 7 is = 1, nbsys
!              VARIABLES INTERNES DU SYST GLIS
                do 8 iv = 1, 3
                    nuvi=nuvi+1
                    vis(iv)=vini(nuvi)
 8              continue
                dvin(nuvi-2)=0.d0
                dvin(nuvi-1)=0.d0
                dvin(nuvi )=0.d0
!
!              CALCUL DE LA SCISSION REDUITE
!              PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
!              TAU      : SCISSION REDUITE TAU=SIG:MS
                do 101 i = 1, 6
                    ms(i)=toutms(iphas,ifa,is,i)
101              continue
!
                taus=0.d0
                do 10 i = 1, 6
                    taus=taus+sigg(i)*ms(i)
10              continue
!
!              ECROUISSAGE ISOTROPE
!
!              DECAL est le début des systemes de glissement de la
!              phase en cours
!              NVIG est le nombre de variables internes dela phase G
!
!               IF (NECOUL.NE.'KOCKS_RAUCH') THEN
                if (nuecou .ne. 4) then
!
                    iexp=0
                    if (is .eq. 1) iexp=1
                    call lcmmfi(coeft, indfa, nmat, nbcomm, necris,&
                                is, nbsys, vini, decal, dy(1),&
                                nfs, nsg, hsr(1, 1, ihsr), iexp, expbp,&
                                rp)
!
                endif
!
!              ECOULEMENT VISCOPLASTIQUE:
!              ROUTINE COMMUNE A L'IMPLICITE (PLASTI-LCPLNL)
!              ET L'EXPLICITE (NMVPRK-GERPAS-RK21CO-RDIF01)
!              CAS IMPLCITE : IL FAUT PRENDRE EN COMPTE DTIME
!              CAS EXPLICITE : IL NE LE FAUT PAS (VITESSES)
!              D'OU :
                dt=1.d0
!
                call lcmmfe(taus, coeft, coel, indfa, nmat,&
                            nbcomm, necoul, is, nbsys, vini,&
                            dy(1), rp, vis(1), vis(2), dt,&
                            dalpha, dgamma, dp, crit, sgns,&
                            nfs, nsg, hsr(1, 1, ihsr), iret)
                if (dp .gt. 0.d0) then
!
!                 ECROUISSAGE CINEMATIQUE
!
                    if (nuecou .lt. 4) then
                        call lcmmec(coeft, indfa, nmat, nbcomm, necrci,&
                                    itmax, toler, vis(1), dgamma, dalpha,&
                                    iret)
                        if (iret .ne. 0) goto 9999
                    endif
!                 DEVG designe ici DEPSVPG
                    do 9 itens = 1, 6
                        devg(itens)=devg(itens)+ms(itens)*dgamma
 9                  continue
!
!                 EVG designe ici EPSVPG
                    if (loca .eq. 'BETA') then
                        gammas=vis(2)+dgamma
                        do 19 itens = 1, 6
                            evg(itens)=evg(itens)+ms(itens)*gammas
19                      continue
                    endif
!
                    dvin(nuvi-2)=dalpha
                    dvin(nuvi-1)=dgamma
                    dvin(nuvi )=dp
                else
                    dvin(nuvi-2)=0.d0
                    dvin(nuvi-1)=0.d0
                    dvin(nuvi )=0.d0
                endif
                nbsyst=nbsyst+1
 7          continue
!
 6      continue
!
        decal = nuvi
!
!         "homogenesisation" des déformations viscoplastiques
        do 20 i = 1, 6
            devi(i)=devi(i)+fv*devg(i)
20      continue
        devgeq=lcnrts(devg)/1.5d0
!         localisation BETA
        if (loca .eq. 'BETA') then
            dl=coeft(nbcomm((nbphas+2),1))
            da=coeft(nbcomm((nbphas+2),1)+1)
            do 21 i = 1, 6
                beta=vini(7+6*(iphas-1)+i)
                dbeta=devg(i)-dl*(beta-da*evg(i))*devgeq
                dvin(7+6*(iphas-1)+i)=dbeta
21          continue
        else
            do 22 i = 1, 6
                dvin(7+6*(iphas-1)+i)=devg(i)
22          continue
!
        endif
!
! fin boucle sur nombre de phases
 1  continue
!
! --    DERIVEES DES VARIABLES INTERNES
!
    do 30 itens = 1, 6
        dvin(itens)= devi(itens)
30  end do
!     Norme de DEVP cumulée
    dvineq = lcnrts( devi ) / 1.5d0
!
    dvin(7)= dvineq
    do 66 itens = 1, 6*nbphas
        dvin(nuvi+i)=0.d0
66  end do
    dvin(nvi) = 0
9999  continue
end subroutine
