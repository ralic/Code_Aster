subroutine connor(melflu, typflu, freq, base, nuor,&
                  amoc, carac, masg, lnoe, nbm,&
                  vite, rho, abscur)
!
! aslint: disable=W1306
    implicit none
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
!
!  CALCUL DES VITESSES EFFICACES ET CRITIQUES PAR LA METHODE DE CONNORS
!  IN : MELFLU : NOM DU CONCEPT DE TYPE MELASFLU PRODUIT
!  IN : TYPFLU : NOM DU CONCEPT DE TYPE TYPE_FLUI_STRU DEFINISSANT LA
!                CONFIGURATION ETUDIEE
!  IN : FREQ   : LISTE DES FREQUENCES ETUDIES
!  IN : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES SELECTIONNES POUR
!                LE COUPLAGE (PRIS DANS LE CONCEPT MODE_MECA)
!  IN : AMOR   : LISTE DES AMORTISSEMENTS REDUITS MODAUX
!  IN : AMOC   : LISTE DES AMORTISSEMENTS REDUITS MODAUX DE CONNORS
!  IN : CARAC  : CARACTERISTIQUES GEOMETRIQUES DU TUBE
!  IN : MASG   : MASSES GENERALISEES DES MODES PERTURBES, SUIVANT LA
!                DIRECTION CHOISIE PAR L'UTILISATEUR
!  IN : LNOE   : NOMBRE DE NOEUDS
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : VITE   : LISTE DES VITESSES D'ECOULEMENT ETUDIEES
!  IN : RHO    : MASSE VOLUMIQUE DU TUBE
!  IN : ABSCUR : ABSCISSE CURVILIGNE DES NOEUDS
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! -------------------------
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/dismoi.h"
#include "asterfort/extmod.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdscr.h"
#include "asterfort/rsadpa.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: melflu
    character(len=8) :: typflu, base
    integer :: nbm
    integer :: nuor(nbm), lnoe
    real(kind=8) :: amoc(nbm), masg(nbm), carac(2), freq(nbm)
    real(kind=8) :: vite(lnoe), abscur(lnoe)
    real(kind=8) :: rho(2*lnoe)
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: coef(nbm), delta(nbm), rhotub, rhos
    integer :: imode, im, ifsvr, ifsvi, nbma, nbzex, nmamin, nmamax
    integer :: iener, ima, izone, ivcn, iven, icste, modul, nbval, i, j
    integer :: jconn, modul2, k, jzone, ifsvk, ide, ire, neq, idep, ibid
    integer :: ldepl(6), lmasg, increm, id, irap
    real(kind=8) :: di, de, mastub, ltube, numera(nbm), denomi
    real(kind=8) :: pas, correl, a, b, c, d, e, f, mphi2(nbm)
    real(kind=8) :: modetr(3*lnoe*nbm), mode(lnoe*nbm)
    real(kind=8) :: coeff1, coeff2, coeff3, coeff4, coeff5, coeff6
    real(kind=8) :: rhoeq
    real(kind=8) :: mass(nbm)
    character(len=8) :: depla(3), depl, k8b
    character(len=14) :: numddl
    character(len=19) :: masse
    character(len=24) :: fsvr, fsvi, fsvk
    data depla  /'DX      ','DY      ','DZ      '/
    data ldepl  /1,2,3,4,5,6/
!-----------------------------------------------------------------------
    call jemarq()
!
!     CALCUL DES CONSTANTES INDEPENDANTES DE L ABSCISSE CURVILIGNE
!     ------------------------------------------------------------
!
    fsvr = typflu//'           .FSVR'
    call jeveuo(fsvr, 'L', ifsvr)
!
    fsvi = typflu//'           .FSVI'
    call jeveuo(fsvi, 'L', ifsvi)
!
    fsvk = typflu//'           .FSVK'
    call jeveuo(fsvk, 'L', ifsvk)
!
    call jeveuo('&&MDCONF.TEMPO', 'L', jzone)
!
    de = carac(1)
    di = carac(1)-2*carac(2)
    correl = zr(ifsvr)
    pas = zr(ifsvr+1)
    rhotub = zr(ifsvr+2)
    nbma = lnoe-1
    nbzex = zi(ifsvi+1)
!
    nbval=1
    do 5 i = 1, nbzex
        nbval=nbval*zi(ifsvi+1+nbzex+i)
 5  continue
!
! =======================
!     VECTEUR DE TRAVAIL CONTENANT POUR CHAQUE MODE ET CHAQUE ZONE
!     LA VALEUR DE L ENERGIE DU AU FLUIDE
!     PAR ORDRE ON DONNE POUR LE MODE 1 LES VALEURS SUR CHAQUE ZONE
!     PUIS LE MODE 2 ET ETC
!
    call wkvect('&&CONNOR.ENERGI', 'V V R', 2*nbzex*nbm, iener)
    call wkvect('&&CONNOR.CSTE', 'V V R', nbzex, icste)
!
    call wkvect(melflu(1:8)//'.VCN', 'G V R', 2*nbval*nbm, ivcn)
    call wkvect(melflu(1:8)//'.VEN', 'G V R', 2*nbm, iven)
    call wkvect(melflu(1:8)//'.MASS', 'V V R', 2, jconn)
    call wkvect(melflu(1:8)//'.RAP', 'G V R', 2*nbval*nbm, irap)
!
    mastub = 0.d0
    rhos = 0.d0
    ltube = abscur(lnoe)-abscur(1)
!
!
! ---      DIRECTION DANS LAQUELLE AGISSENT LES FORCES FLUIDELASTIQUES
    idep = 0
    depl = zk8(ifsvk+1)
    do 7 ide = 1, 3
        if (depla(ide) .eq. depl) idep = ide
 7  continue
! ---  DEFORMEES MODALES
!
!
    call dismoi('F', 'REF_MASS_PREM', base, 'RESU_DYNA', ibid,&
                masse, ire)
    call mtdscr(masse)
    call dismoi('F', 'NOM_NUME_DDL', masse, 'MATR_ASSE', ibid,&
                numddl, ire)
    call dismoi('F', 'NB_EQUA', masse, 'MATR_ASSE', neq,&
                k8b, ire)
!
!
!     EXTRACTION DE LA COMPOSANTE SELON LA DIRECTION DE L ECOULEMENT DES
!     DIFFERENTS MODES
    call extmod(base, numddl, nuor, nbm, mode,&
                neq, lnoe, idep, 1)
!
!     EXTRACTION DES COMPOSANTES DE TRANSLATION DES  DIFFERENTS MODES
    call extmod(base, numddl, nuor, nbm, modetr,&
                neq, lnoe, ldepl, 3)
!
    rhoeq = 0.d0
!
    do 10 ima = 1, nbma
!
        rhoeq = rhoeq + (&
                abscur(ima+1)-abscur(ima))* (rhotub+(di**2/( de**2-di**2))*((rho(ima+lnoe)+ rho(i&
                &ma+lnoe+1))/2)+ (2*correl/ r8pi())*(de**2/(de**2-di**2))*((rho(ima)+ rho(ima+1))&
                &/2)&
                )
        rhos = rhos + (abscur(ima+1)-abscur(ima))* ((rho(ima)+rho(ima+ 1))/2)
10  continue
!
    mastub = ((r8pi()/4)*(de**2-di**2)*rhoeq)/ltube
    rhos = rhos/ltube
    zr(jconn ) = mastub
    zr(jconn+1) = rhos
!
!
!
!     1)  METHODE GEVIBUS
!         ===============
!
!     CALCUL DE LA VITESSE CRITIQUE INTER TUBES POUR CHAQUE COMBINAISON
!     DES CONSTANTE DE CONNORS ET POUR CHAQUE MODE
!
    do 20 im = 1, nbm
!
        imode=nuor(im)
!        LES LIGNES COMMENTARISEES CORRESPONDENT A
!        UN AMORTISSEMENT MODAL CALCULE PAR LA FORMULE :
!        AMORTISSEMENT/(4*PI*(MASSE GENERALISE*FREQUENCE)
!
!        AMORED(IMODE)=AMOC(IMODE)/(4*R8PI()*MASG(IMODE)*FREQ(IMODE))
!        DELTA(IMODE)=(2*R8PI()*AMORED(IMODE))/SQRT(1-AMORED(IMODE)**2)
!
        delta(im)=(2*r8pi()*amoc(im))/sqrt(1-amoc(im)**2)
        coef(im)=freq(imode)*sqrt(mastub*delta(im)/rhos)
!
        do 30 izone = 1, nbzex
!
            nmamin=zi(jzone+2*(izone-1)+1)
            nmamax=zi(jzone+2*(izone-1)+2)-1
!     RHO EST DE LA FORME A*S+B
!     V   EST DE LA FORME C*S+D
!     PHI EST DE LA FORME E*S+F
            do 40 ima = nmamin, nmamax
                a=(rho(ima+1)-rho(ima))/(abscur(ima+1)-abscur(ima))
                b=rho(ima)-a*abscur(ima)
!
                c=(vite(ima+1)-vite(ima))/(abscur(ima+1)-abscur(ima))
                d=vite(ima)-c*abscur(ima)
!
                e=(mode(lnoe*(im-1)+ima+1)-mode(lnoe*(im-1)+ima))&
                /(abscur(ima+1)-abscur(ima))
                f=mode(lnoe*(im-1)+ima)-e*abscur(ima)
!
!    COEFFICIENT DU POLYNOME DU 5 DEGRES RESULTAT DE RHO*V**2*PHI**2
!
                coeff1=a*c**2*e**2
                coeff2=2*a*e*f*(c**2)+2*a*(e**2)*c*d+(c**2)*(e**2)*b
                coeff3=a*(f**2)*(c**2)+4*a*e*f*c*d+a*(e**2)*(d**2)+&
                2*e*f*(c**2)*b+2*(e**2)*c*d*b
                coeff4=2*a*c*d*(f**2)+2*a*e*f*(d**2)+(f**2)*(c**2)*b+&
                4*e*f*c*d*b+(e**2)*(d**2)*b
                coeff5=2*c*d*(f**2)*b+2*e*f*(d**2)*b+(d**2)*(f**2)*a
                coeff6=(d**2)*(f**2)*b
!
                zr(iener-1+nbzex*(im-1)+izone)= zr(iener-1+nbzex*(im-&
                1)+izone)+ (coeff1*(abscur(ima+1)**6-abscur(ima)**6))/&
                6+ (coeff2*(abscur(ima+1)**5-abscur(ima)**5))/5+&
                (coeff3*(abscur(ima+1)**4-abscur(ima)**4))/4+ (coeff4*&
                (abscur(ima+1)**3-abscur(ima)**3))/3+ (coeff5*(abscur(&
                ima+1)**2-abscur(ima)**2))/2+ coeff6*(abscur(ima+1)-&
                abscur(ima))
40          continue
30      continue
20  continue
!
    do 50 i = 1, nbval
        modul=1
!
        do 60 j = 1, nbzex
            modul=1
            do 65 k = (j+1), nbzex
                modul=modul*zi(ifsvi+1+nbzex+k)
65          continue
            if (j .eq. 1) then
                pas=dble((i-1)/modul)
            else
                modul2=modul*zi(ifsvi+1+nbzex+j)
                pas=dble(mod(i-1,modul2)/modul)
            endif
            zr(icste-1+j)=zr(ifsvr+3+2*(j-1))+pas* (zr(ifsvr+3+2*(j-1)&
            +1)-zr(ifsvr+3+2*(j-1))) /(zi(ifsvi+1+nbzex+j)-1)
60      continue
!
        do 70 im = 1, nbm
            numera(im)=0.d0
            denomi=0.d0
            do 80 izone = 1, nbzex
                numera(im) = numera(im)+ zr(iener-1+nbzex*(im-1)+ izone)
                denomi = denomi+zr(icste-1+izone)**(-2)* zr(iener-1+ nbzex*(im-1)+izone)
80          continue
            zr(ivcn-1+(im-1)*nbval+i)= sqrt((numera(im)/denomi))*coef(&
            im)
70      continue
50  continue
!
!    CALCUL DE LA VITESSE EFFICACE POUR CHAQUE MODE PROPRE
!
    do 90 im = 1, nbm
!
!    LA MASSE LINEIQUE DU TUBE EST DE LA FORME A*S+B
!    LE MODE PROPRE DU TUBE EST DE LA FORME C*S+D
!
        mphi2(im)=0.d0
        do 100 ima = 1, nbma
!
            a=(0.5d0*r8pi()*(di**2)*(rho(lnoe+ima+1)-rho(lnoe+ima))+&
            correl*(de**2)*(rho(ima+1)-rho(ima)))/ (2*(abscur(ima+1)-&
            abscur(ima)))
            b=r8pi()*rhotub*(de**2-di**2)/4+r8pi()*(di**2)/4* (rho(&
            lnoe+ima)-(rho(lnoe+ima+1)-rho(lnoe+ima))/ (abscur(ima+1)-&
            abscur(ima))*abscur(ima))+ correl*(de**2)/2*(rho(ima)-(&
            rho(ima+1)-rho(ima))/ (abscur(ima+1)-abscur(ima))*abscur(&
            ima))
!
            c=(mode(lnoe*(im-1)+ima+1)-mode(lnoe*(im-1)+ima))/&
            (abscur(ima+1)-abscur(ima))
            d=mode(lnoe*(im-1)+ima)-c*abscur(ima)
!
            coeff1 = a*(c**2)
            coeff2 = 2*c*d*a+(c**2)*b
            coeff3 = (d**2)*a+2*c*d*b
            coeff4 = (d**2)*b
            mphi2(im) = mphi2(im)+ (coeff1*(abscur(ima+1)**4-abscur( ima)**4))/4+ (coeff2*(abscur&
                        &(ima+1)**3-abscur(ima)**3))/3+ (coeff3*(abscur(ima+1)**2-abscur(ima)**2)&
                        &)/2+ coeff4*( abscur(ima+1)-abscur(ima))
!
100      continue
!
        zr(iven-1+im) = sqrt((numera(im)*mastub) / (mphi2(im)*rhos))
        do 95 i = 1, nbval
            zr(irap-1+(im-1)*nbval+i) = zr(iven-1+im)/ zr(ivcn-1+(im- 1)*nbval+i)
95      continue
90  continue
!
!
!
!
!     2)  FORMULATION ALTERNATIVE
!         ===============
!
!     CALCUL DE LA VITESSE CRITIQUE INTER TUBES POUR CHAQUE COMBINAISON
!     DES CONSTANTE DE CONNORS ET POUR CHAQUE MODE
!
    do 120 im = 1, nbm
!
        imode=nuor(im)
!        LES LIGNES COMMENTARISEES CORRESPONDENT A
!        UN AMORTISSEMENT MODAL CALCULE PAR LA FORMULE :
!        AMORTISSEMENT/(4*PI*(MASSE GENERALISE*FREQUENCE)
!
!        AMORED(IMODE)=AMOC(IMODE)/(4*R8PI()*MASG(IMODE)*FREQ(IMODE))
!        DELTA(IMODE)=(2*R8PI()*AMORED(IMODE))/SQRT(1-AMORED(IMODE)**2)
!
        do 130 izone = 1, nbzex
!
            nmamin=zi(jzone+2*(izone-1)+1)
            nmamax=zi(jzone+2*(izone-1)+2)-1
!     RHO EST DE LA FORME A*S+B
!     V   EST DE LA FORME C*S+D
!     PHI EST DE LA FORME E*S+F
            do 140 ima = nmamin, nmamax
                a=(rho(ima+1)-rho(ima))/(abscur(ima+1)-abscur(ima))
                b=rho(ima)-a*abscur(ima)
!
                c=(vite(ima+1)-vite(ima))/(abscur(ima+1)-abscur(ima))
                d=vite(ima)-c*abscur(ima)
!
                coeff1=0.d0
                coeff2=0.d0
                coeff3=0.d0
                coeff4=0.d0
                coeff5=0.d0
                coeff6=0.d0
!
! PRISE EN COMPTE DES DDL DE TRANSLATION
!
                do 145 id = 1, 3
                    increm = (lnoe*(im-1)+ima-1)*3+id
                    e=(modetr(increm+3)-modetr(increm)) /(abscur(ima+&
                    1)-abscur(ima))
                    f=modetr(increm)-e*abscur(ima)
!
!    COEFFICIENT DU POLYNOME DU 5 DEGRES RESULTAT DE RHO*V**2*PHI**2
!
                    coeff1=a*c**2*e**2+coeff1
                    coeff2=2*a*e*f*(c**2)+2*a*(e**2)*c*d+(c**2)*(e**2)&
                    *b+ coeff2
                    coeff3=a*(f**2)*(c**2)+4*a*e*f*c*d+a*(e**2)*(d**2)&
                    + 2*e*f*(c**2)*b+2*(e**2)*c*d*b+coeff3
                    coeff4=2*a*c*d*(f**2)+2*a*e*f*(d**2)+(f**2)*(c**2)&
                    *b+ 4*e*f*c*d*b+(e**2)*(d**2)*b+coeff4
                    coeff5=2*c*d*(f**2)*b+2*e*f*(d**2)*b+(d**2)*(f**2)&
                    *a+ coeff5
                    coeff6=(d**2)*(f**2)*b+coeff6
!
145              continue
!
                zr(iener-1+nbzex*(im-1)+izone+nbzex*nbm)= zr(iener-1+&
                nbzex*(im-1)+izone+nbzex*nbm)+ (coeff1*(abscur(ima+1)&
                **6-abscur(ima)**6))/6+ (coeff2*(abscur(ima+1)**5-&
                abscur(ima)**5))/5+ (coeff3*(abscur(ima+1)**4-abscur(&
                ima)**4))/4+ (coeff4*(abscur(ima+1)**3-abscur(ima)**3)&
                )/3+ (coeff5*(abscur(ima+1)**2-abscur(ima)**2))/2+&
                coeff6*(abscur(ima+1)-abscur(ima))
140          continue
130      continue
120  continue
!
    do 150 i = 1, nbval
        modul=1
!
        do 160 j = 1, nbzex
            modul=1
            do 165 k = (j+1), nbzex
                modul=modul*zi(ifsvi+1+nbzex+k)
165          continue
            if (j .eq. 1) then
                pas=dble((i-1)/modul)
            else
                modul2=modul*zi(ifsvi+1+nbzex+j)
                pas=dble(mod(i-1,modul2)/modul)
            endif
            zr(icste-1+j)=zr(ifsvr+3+2*(j-1))+pas* (zr(ifsvr+3+2*(j-1)&
            +1)-zr(ifsvr+3+2*(j-1))) /(zi(ifsvi+1+nbzex+j)-1)
160      continue
!
        do 170 im = 1, nbm
            numera(im)=0.d0
            denomi=0.d0
            do 180 izone = 1, nbzex
                numera(im) = numera(im)+ zr(iener-1+nbzex*(im-1)+ izone+nbzex*nbm)
                denomi = denomi+zr(icste-1+izone)**(-2)* zr(iener-1+ nbzex*(im-1)+izone+nbzex*nbm&
                         &)
180          continue
            zr(ivcn-1+(im-1)*nbval+i+nbm*nbval)= sqrt((numera(im)/&
            denomi))*coef(im)
170      continue
150  continue
!
!    CALCUL DE LA VITESSE EFFICACE POUR CHAQUE MODE PROPRE
!
    do 190 im = 1, nbm
!
!    LA MASSE GENERALISEE DU MODE IM
!
        call rsadpa(base, 'L', 1, 'MASS_GENE', nuor(im),&
                    0, sjv=lmasg, styp=k8b)
        mass(im) = zr(lmasg)
        zr(iven-1+nbm+im) = sqrt((numera(im)*mastub) / (mass(im)*rhos) )
        do 195 i = 1, nbval
            zr(irap-1+(im-1)*nbval+i+nbm*nbval) = zr(iven-1+nbm+im)/ zr(ivcn-1+(im-1)*nbval+i+nbm&
                                                  &*nbval)
195      continue
190  continue
!
!
!
    call jedema()
end subroutine
