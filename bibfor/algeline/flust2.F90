subroutine flust2(melflu, typflu, base, noma, nuor,&
                  amor, freq, masg, fact, vite,&
                  nbm, npv, nivpar, nivdef)
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION :  CALCUL DES PARAMETRES DE COUPLAGE FLUIDE-STRUCTURE
! -----------    POUR UNE CONFIGURATION DE TYPE "GRAPPE DE COMMANDE"
!
!                OPERATEUR APPELANT : CALC_FLUI_STRU , OP0144
!
!  IN : MELFLU : NOM DU CONCEPT DE TYPE MELASFLU PRODUIT
!  IN : TYPFLU : NOM DU CONCEPT DE TYPE TYPE_FLUI_STRU DEFINISSANT LA
!                CONFIGURATION ETUDIEE
!  IN : BASE   : NOM DU CONCEPT DE TYPE MODE_MECA DEFINISSANT LA BASE
!                MODALE DU SYSTEME AVANT PRISE EN COMPTE DU COUPLAGE
!  IN : NOMA   : NOM DU CONCEPT DE TYPE MAILLAGE
!  IN : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES SELECTIONNES POUR
!                LE COUPLAGE (PRIS DANS LE CONCEPT MODE_MECA)
!  IN : AMOR   : LISTE DES AMORTISSEMENTS REDUITS MODAUX INITIAUX
!  IN : VITE   : LISTE DES VITESSES D'ECOULEMENT ETUDIEES
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : NPV    : NOMBRE DE VITESSES D'ECOULEMENT
!  IN : NIVPAR : NIVEAU D'IMPRESSION DANS LE FICHIER RESULTAT POUR LES
!                PARAMETRES DU COUPLAGE (FREQ,AMOR)
!  IN : NIVDEF : NIVEAU D'IMPRESSION DANS LE FICHIER RESULTAT POUR LES
!                DEFORMEES MODALES
!  OUT: FREQ   : LISTE DES FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX
!                PERTURBES PAR L'ECOULEMENT
!  OUT: MASG   : MASSES GENERALISEES DES MODES PERTURBES, SUIVANT LA
!                DIRECTION CHOISIE PAR L'UTILISATEUR
!  OUT: FACT   : PSEUDO FACTEUR DE PARTICIPATION
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! -------------------------
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
    include 'asterc/r8pi.h'
    include 'asterfort/cpdepl.h'
    include 'asterfort/fluimp.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mdconf.h'
    include 'asterfort/pacouc.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rslipa.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: melflu
    character(len=8) :: typflu, base, noma
    integer :: nuor(*)
    real(kind=8) :: amor(*), freq(*), masg(*), fact(*), vite(*)
    integer :: nbm, npv, nivpar, nivdef
!
! VARIABLES LOCALES
! -----------------
    integer :: ibid
    real(kind=8) :: carac(2)
    logical :: lnul, lneg, calcul(2)
    character(len=8) :: k8b
    character(len=24) :: fsic
!
! FONCTIONS EXTERNES
! ------------------
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!-----------------------------------------------------------------------
    integer :: iamfr, icodim, icoupl, ier, ifreqi, igrap, imist
    integer :: imod, ind, ipoids, iv, iwork, lfact, lfsic
    integer :: lmasg, lwork(1), n1, nt, numod
    real(kind=8) :: fi, phid, phie(1), pi, rbid, vlim
!-----------------------------------------------------------------------
    call jemarq()
!
!
!-----1.DETERMINATION DU CAS DE CALCUL
!       PRISE EN COMPTE OU NON DU COUPLAGE FLUIDE-STRUCTURE
!
    fsic = typflu//'           .FSIC'
    call jeveuo(fsic, 'L', lfsic)
    icoupl = zi(lfsic+1)
!
!
!-----2.SI PRISE EN COMPTE DU COUPLAGE FLUIDE-STRUCTURE
!       VERIFICATION DU SIGNE DES VITESSES (STRICTEMENT POSITIF)
!
    vlim = 1.d-5
    if (icoupl .eq. 1) then
        lnul = .false.
        lneg = .false.
        do 10 iv = 1, npv
            if (dble(abs(vite(iv))) .lt. vlim) then
                lnul = .true.
                goto 11
            else if (vite(iv).lt.0.d0) then
                lneg = .true.
                goto 11
            endif
10      continue
11      continue
        if (lnul .or. lneg) then
            call u2mess('F', 'ALGELINE_43')
        endif
    endif
!
!
!-----3.ACCES AUX OBJETS UTILES
    call rslipa(base, 'FREQ', '&&FLUST2.LIFREQ', ifreqi, n1)
!
!
!
!
!-----4.REMPLISSAGE DES OBJETS .VALE DES CHAMPS DE DEPLACEMENTS
!
    call cpdepl(melflu, base, nuor, nbm)
!
!
!-----5.SI PRISE EN COMPTE DU COUPLAGE FLUIDE-STRUCTURE
!       EXECUTION DU CALCUL
!
    if (icoupl .eq. 1) then
!
!-------5.1.CREATION D'OBJETS DE TRAVAIL
!
        call wkvect('&&FLUST2.TEMP.CODIM', 'V V R', 4, icodim)
        call wkvect('&&FLUST2.TEMP.MIST', 'V V R', nbm, imist)
        call wkvect('&&FLUST2.TEMP.AMFR', 'V V R', 2*nbm, iamfr)
        call wkvect('&&FLUST2.TEMP.POIDS', 'V V R', 2*nbm, ipoids)
!
        nt = 2
        lwork(1) = 2*nt*nt + 10*nt + 2
        call wkvect('&&FLUST2.TEMP.WORK', 'V V R', lwork(1), iwork)
!
!-------5.2.OPERATIONS SIMULTANEES :
!           - RECUPERATION DES MASSES MODALES INITIALES
!           - RECUPERATION DES FREQUENCES INITIALES
!           - CALCUL DES AMORTISSEMENTS MODAUX INITIAUX
!           - CALCUL DES PSEUDO FACTEURS DE PARTICIPATION EN EAU
!
        pi = r8pi()
!
        do 30 imod = 1, nbm
!
            numod = nuor(imod)
!
            call rsadpa(base, 'L', 1, 'MASS_GENE', numod,&
                        0, lmasg, k8b)
            zr(imist+imod-1) = zr(lmasg)
!
            fi = zr(ifreqi+numod-1)
            zr(iamfr+imod-1) = 4.d0*pi*fi*amor(imod)*zr(lmasg)
            zr(iamfr+nbm+imod-1) = fi
!
!
            call rsadpa(base, 'L', 1, 'FACT_PARTICI_DX', numod,&
                        0, lfact, k8b)
            fact(3*(imod-1)+1) = zr(lfact ) * masg(imod)
            fact(3*(imod-1)+2) = zr(lfact+1) * masg(imod)
            fact(3*(imod-1)+3) = zr(lfact+2) * masg(imod)
!
30      continue
!
!-------5.3.TYPE DE CONFIGURATION GRAPPE --> VARIABLE INDIC ---
!           RECUPERATION DU DIAMETRE EXTERIEUR DU TUBE
!           RECUPERATION DES GRANDEURS GEOMETRIQUES CARACTERISTIQUES
!           DEDUCTION DE COEFFICIENTS DE DIMENSIONNEMENT
!           CALCUL DES PONDERATIONS DUES AUX DEFORMEES MODALES
!           CALCUL DES MASSES MODALES EN EAU
!
        call mdconf(typflu, base, noma, nbm, ibid,&
                    nuor, 0, igrap, lwork, masg,&
                    zr(icodim), zr(ipoids), phie, rbid)
!
!
!-------5.4.CALCUL DES PARAMETRES MODAUX SOUS ECOULEMENT
!
        call pacouc(typflu, masg, zr(icodim), vite, zr(ipoids),&
                    zr(imist), freq, zr(iamfr), nbm, igrap,&
                    npv, zr(iwork), lwork, phie, rbid,&
                    ier)
!
!-------5.5.IMPRESSIONS DANS LE FICHIER RESULTAT SI DEMANDEES
!
        if (nivpar .eq. 1 .or. nivdef .eq. 1) then
            phid = phie(1) * (1172.d0/890.d0 - 1.d0)
            carac(1)=phid
            carac(2)=0.d0
            calcul(1)=.true.
            calcul(2)=.false.
            call fluimp(2, nivpar, nivdef, melflu, typflu,&
                        nuor, freq, zr(ifreqi), nbm, vite,&
                        npv, carac, calcul, rbid)
        endif
!
!
!-----6.SINON (COUPLAGE FLUIDE-STRUCTURE NON PRIS EN COMPTE)
!
    else
!
!-------6.1.REMPLISSAGE DES OBJETS .MASG .FACT
!
        do 100 imod = 1, nbm
            numod = nuor(imod)
            call rsadpa(base, 'L', 1, 'MASS_GENE', numod,&
                        0, lmasg, k8b)
            call rsadpa(base, 'L', 1, 'FACT_PARTICI_DX', numod,&
                        0, lfact, k8b)
            masg(imod) = zr(lmasg)
            fact(3*(imod-1)+1) = zr(lfact ) * masg(imod)
            fact(3*(imod-1)+2) = zr(lfact+1) * masg(imod)
            fact(3*(imod-1)+3) = zr(lfact+2) * masg(imod)
100      continue
!
!-------6.2.REMPLISSAGE DE L'OBJET .FREQ
!
        do 110 iv = 1, npv
            do 120 imod = 1, nbm
                numod = nuor(imod)
                ind = 2*nbm*(iv-1)+2*(imod-1)+1
                freq(ind) = zr(ifreqi+numod-1)
                freq(ind+1) = amor(imod)
120          continue
110      continue
!
    endif
!
! --- MENAGE
!
    call jedetr('&&FLUST2.LIFREQ')
    call jedetr('&&FLUST2.TEMP.CODIM')
    call jedetr('&&FLUST2.TEMP.MIST')
    call jedetr('&&FLUST2.TEMP.AMFR')
    call jedetr('&&FLUST2.TEMP.POIDS')
    call jedetr('&&FLUST2.TEMP.WORK')
!
    call jedema()
!
! --- FIN DE FLUST2.
end subroutine
