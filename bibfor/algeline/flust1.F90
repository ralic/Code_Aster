subroutine flust1(melflu, typflu, base, nuor, amor,&
                  amoc, freq, masg, fact, vite,&
                  nbm, calcul, npv, nivpar, nivdef)
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
! -----------    POUR UNE CONFIGURATION DE TYPE "FAISCEAU DE TUBES
!                SOUS ECOULEMENT TRANSVERSE"
!
!                OPERATEUR APPELANT : CALC_FLUI_STRU , OP0144
!
!  IN : MELFLU : NOM DU CONCEPT DE TYPE MELASFLU PRODUIT
!  IN : TYPFLU : NOM DU CONCEPT DE TYPE TYPE_FLUI_STRU DEFINISSANT LA
!                CONFIGURATION ETUDIEE
!  IN : BASE   : NOM DU CONCEPT DE TYPE MODE_MECA DEFINISSANT LA BASE
!                MODALE DU SYSTEME AVANT PRISE EN COMPTE DU COUPLAGE
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
!  OUT: FREQ   : FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX PERTUBES
!                PAR L'ECOULEMENT
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
#include "jeveux.h"
!
#include "asterc/r8pi.h"
#include "asterfort/connor.h"
#include "asterfort/cpdepl.h"
#include "asterfort/dismoi.h"
#include "asterfort/fluimp.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdconf.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/pacouc.h"
#include "asterfort/pteddl.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslipa.h"
#include "asterfort/wkvect.h"
    character(len=19) :: melflu
    character(len=8) :: typflu, base
    integer :: nuor(*)
    real(kind=8) :: amor(*), amoc(*), freq(*), masg(*), fact(*)
    real(kind=8) :: vite(*)
    real(kind=8) :: carac(2)
    integer :: nbm, npv, nivpar, nivdef, n1
    logical :: calcul(2)
!
! VARIABLES LOCALES
! -----------------
    character(len=8) :: depl, mailla, k8b
    character(len=14) :: numddl
    character(len=19) :: masse
    character(len=24) :: fsic, fsvk, nomnoe
    character(len=24) :: vale
    integer :: lzone
!
! FONCTIONS EXTERNES
! ------------------
!     EXTERNAL      R8PI
!
! DATA
! ----
!-----------------------------------------------------------------------
    integer :: iamfr, ibid, icoupl, ieq, ier, ifreqi, ifsic
    integer :: ifsvk, im, imod, ind, indic, ior, ire
    integer :: iret, iv, ivale, kmasg, kref, labsc, laux1
    integer :: laux2, lddl, ldefm, lfact, lires, lmasg, lmasse
    integer :: lnoe, lprofv, lrho, lvale, neq, nt
    real(kind=8) :: pi, rval1
!-----------------------------------------------------------------------
    data  vale  /'                   .VALE'/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    pi = r8pi()
!
!
! --- 1.RECUPERATION DES INFORMATIONS APPORTEES PAR LE CONCEPT ---
! ---   TYPE_FLUI_STRU                                         ---
!
! --- 1.1.PRISE EN COMPTE OU NON DU COUPLAGE
!
    fsic = typflu//'           .FSIC'
    call jeveuo(fsic, 'L', ifsic)
    icoupl = zi(ifsic+1)
!
! --- 1.2.DIAMETRE EXTERIEUR DU TUBE (TEST SI SECTION CONSTANTE)
!
    fsvk = typflu//'           .FSVK'
    call jeveuo(fsvk, 'L', ifsvk)
!
    depl = zk8(ifsvk+1)
!
! --- 1.4.NOMBRE DE POINTS DE DISCRETISATION DU TUBE
!
    call jeveuo(base//'           .REFD', 'L', kref)
    masse = zk24(kref+1)
    call mtdscr(masse)
    call jeveuo(masse//'.&INT', 'L', lmasse)
    call dismoi('F', 'NOM_NUME_DDL', masse, 'MATR_ASSE', ibid,&
                numddl, ire)
    call dismoi('F', 'NOM_MAILLA', masse, 'MATR_ASSE', ibid,&
                mailla, ire)
    call dismoi('F', 'NB_EQUA', masse, 'MATR_ASSE', neq,&
                k8b, ire)
!
!-----RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
!
    nomnoe = mailla//'.NOMNOE'
    call jelira(nomnoe, 'NOMUTI', lnoe, k8b)
!
!-----CREATION ET REMPLISSAGE DES OBJETS DE TRAVAIL DEPENDANT DU
!-----TYPE DU CONFIGURATION
!
! --- 1.5.CONCEPT DE TYPE FONCTION DEFINISSANT LE PROFIL DE VITESSE
! --- A PARTIR DES PROFIL DE VITESSE DE CHAQUE ZONE ON CONSTRUIT UN
! --- PROFIL UNIQUE, AINSI QUE LES VECTEUR IRES (TYPE DE RESEAU) ET
! --- VMOY (VITESSE MOYENNE) DE CHAQUE POINT DU TUBE.
! --- VMOYTO EST LA VITESSE MOYENNE SUR L ENSEMBLE DES ZONES.
! --- LES PROFILS DE VITESSE NE SONT PAS NORMES.
!
    call wkvect('&&FLUST1.TEMP.PROFV', 'V V R', (2*lnoe+1), lprofv)
    call wkvect('&&FLUST1.TEMP.PROFR', 'V V R', 2*lnoe, lrho)
    call wkvect('&&FLUST1.TEMP.ABSC', 'V V R', lnoe, labsc)
    call wkvect('&&FLUST1.TEMP.IRES', 'V V I', lnoe, lires)
    call wkvect('&&FLUST1.TEMP.ZONE', 'V V I', 2*lnoe, lzone)
    if (icoupl .eq. 1) then
        call wkvect('&&FLUST1.TEMP.DEFM', 'V V R', nbm*lnoe, ldefm)
    endif
!
! ---
    call mdconf(typflu, base, k8b, nbm, lnoe,&
                nuor, 0, indic, zi(lires), zr(lprofv),&
                zr(lrho), zr(ldefm), carac, zr(labsc))
!
!
! --- 2. CALCUL DES MASS_GENE_DX, _DY, _DZ ---
!        REMPLISSAGE DES OBJETS .MASG .FACT ---
!
    call wkvect('&&FLUST1.TEMP.LAUX1', 'V V R', neq, laux1)
    call wkvect('&&FLUST1.TEMP.LAUX2', 'V V R', neq, laux2)
    call wkvect('&&FLUST1.TEMP.MASG', 'V V R', nbm, kmasg)
    call wkvect('&&FLUST1.POSITION.DDL', 'V V I', neq, lddl)
    call pteddl('NUME_DDL', numddl, 1, depl, neq,&
                zi(lddl))
    do 80 ieq = 0, neq-1
        zr(laux1+ieq) = zi(lddl+ieq)
80  end do
    call mrmult('ZERO', lmasse, zr(laux1), zr(laux2), 1,&
                .true.)
    do 100 im = 1, nbm
        ior = nuor(im)
        call rsexch('F', base, 'DEPL', ior, vale(1:19),&
                    iret)
        call jeveuo(vale, 'L', lvale)
        rval1 = 0.0d0
        do 90 ieq = 0, neq-1
            rval1 = rval1 + zr(lvale+ieq)*zr(laux2+ieq)*zr(lvale+ieq)
90      continue
        zr(kmasg+im-1) = rval1
!
        call rsadpa(base, 'L', 1, 'FACT_PARTICI_DX', ior,&
                    0, lfact, k8b)
        call rsadpa(base, 'L', 1, 'MASS_GENE', ior,&
                    0, lmasg, k8b)
        masg(im) = zr(lmasg)
        fact(3*(im-1)+1) = zr(lfact ) * masg(im)
        fact(3*(im-1)+2) = zr(lfact+1) * masg(im)
        fact(3*(im-1)+3) = zr(lfact+2) * masg(im)
100  end do
!
!
! --- 3.REMPLISSAGE DES OBJETS .VALE DES CHAMPS DE DEPLACEMENTS ---
!
    call cpdepl(melflu, base, nuor, nbm)
!
!
! --- 4.CALCUL DES PARAMETRES DE COUPLAGE SI DEMANDE ---
    call rslipa(base, 'FREQ', '&&FLUST1.LIFREQ', ifreqi, n1)
!
!
    if (calcul(1)) then
        if (icoupl .eq. 1) then
!
            call wkvect('&&FLUST1.TEMP.AMFR', 'V V R', 2*nbm, iamfr)
            do 110 im = 1, nbm
                imod = nuor(im)
                zr(iamfr+im-1) = 4.d0*pi*zr(ifreqi+imod-1)*amor(im)* zr(kmasg+im-1)
                zr(iamfr+nbm+im-1) = zr(ifreqi+imod-1)
110          continue
!
            nt = 2
            lvale = 2*nt*nt + 10*nt + 2
            call wkvect('&&FLUST1.TEMP.VALE', 'V V R', lvale, ivale)
!
!-------LANCEMENT DU CALCUL
!
            call pacouc(typflu, zr(lprofv), zr(lrho), vite, zr(ldefm),&
                        zr(kmasg), freq, zr(iamfr), nbm, lnoe,&
                        npv, zr(ivale), zi(lires), carac, zr(labsc),&
                        ier)
!
        else
!
!-------REMPLISSAGE DE L'OBJET .FREQ
!
            do 140 iv = 1, npv
                do 130 im = 1, nbm
                    imod = nuor(im)
                    ind = 2*nbm*(iv-1)+2*(im-1)+1
                    freq(ind) = zr(ifreqi+imod-1)
                    freq(ind+1) = amor(im)
130              continue
140          continue
!
        endif
    endif
    if (calcul(2)) then
        call connor(melflu, typflu, zr(ifreqi), base, nuor,&
                    amoc, carac, masg, lnoe, nbm,&
                    zr(lprofv), zr(lrho), zr(labsc))
    endif
!
!
!
!
! --- 5.IMPRESSIONS DANS LE FICHIER RESULTAT SI DEMANDEES ---
!
    if (nivpar .eq. 1 .or. nivdef .eq. 1) then
        call fluimp(1, nivpar, nivdef, melflu, typflu,&
                    nuor, freq, zr( ifreqi), nbm, vite,&
                    npv, carac, calcul, amoc)
    endif
!
!     NETTOYAGE SUR LA VOLATILE
    call jedetr('&&FLUST1.TEMP.PROFV')
    call jedetr('&&FLUST1.TEMP.PROFR')
    call jedetr('&&FLUST1.TEMP.ABSC')
    call jedetr('&&FLUST1.TEMP.IRES')
    call jedetr('&&FLUST1.TEMP.ZONE')
    call jedetr('&&FLUST1.TEMP.DEFM')
    call jedetr('&&FLUST1.TEMP.LAUX1')
    call jedetr('&&FLUST1.TEMP.LAUX2')
    call jedetr('&&FLUST1.TEMP.MASG')
    call jedetr('&&FLUST1.POSITION.DDL')
    call jedetr('&&FLUST1.LIFREQ')
    call jedetr('&&FLUST1.TEMP.AMFR')
    call jedetr('&&FLUST1.TEMP.VALE')
!
    call jedetr('&&COEFMO.COMPT')
    call jedetr('&&COEFMO.EXTR')
    call jedetr('&&COEFMO.VRZO')
    call jedetr('&&COEFMO.ALARM')
    call jedetr('&&COEFAM.CDI')
    call jedetr('&&COEFAM.CDR1')
    call jedetr('&&COEFAM.CDR2')
    call jedetr('&&COEFRA.CKI')
    call jedetr('&&COEFRA.CKR1')
    call jedetr('&&COEFRA.CKR2')
    call jedetr('&&PACOUC.TRAV1')
    call jedetr('&&PACOUC.TRAV2')
    call jedetr('&&MDCONF.TEMPO')
!
    call jedema()
!
! --- FIN DE FLUST1.
end subroutine
