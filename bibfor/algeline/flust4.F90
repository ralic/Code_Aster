subroutine flust4(melflu, typflu, base, noma, nuor,&
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
!  CALCUL DES PARAMETRES DE COUPLAGE FLUIDE-STRUCTURE POUR UNE
!  CONFIGURATION DE TYPE "COQUES CYLINDRIQUES COAXIALES"
!  OPERATEUR APPELANT : CALC_FLUI_STRU , OP0144
!-----------------------------------------------------------------------
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
!  OUT: MASG   : MASSES GENERALISEES DES MODES PERTURBES
!  OUT: FACT   : PSEUDO FACTEUR DE PARTICIPATION
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/bijmoc.h"
#include "asterfort/cpdepl.h"
#include "asterfort/fluimp.h"
#include "asterfort/geocoq.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mefgmn.h"
#include "asterfort/modcoq.h"
#include "asterfort/modeau.h"
#include "asterfort/pacouc.h"
#include "asterfort/poibij.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rslipa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbm, npv, nivpar, nivdef, nuor(*)
    real(kind=8) :: amor(*), freq(*), masg(*), vite(*), fact(*)
    character(len=8) :: typflu, base, noma
    character(len=19) :: melflu
!
    logical :: vneg, vpos, calcul(2)
    real(kind=8) :: mcf0, ksi, carac(2)
    character(len=8) :: caelem, mater1, mater2, k8b
    character(len=24) :: fsvi, fsvr, fsvk, fsgm
    complex(kind=8) :: bii
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iamfr, iaxe, icoef, icomp, ier, ifr, ifreqi
    integer :: igeom, iicoq, im, imaj, imasse, imod, ior
    integer :: iorco, iv, ivabs, ivcpr, iwork, jmod
    integer :: kec, lfact, lfsgm, lfsvi, lfsvk, lfsvr, lmasg
    integer :: lwork, n1, nt, numod
    real(kind=8) :: cf0, fi, hmoy, pi, rbid, s0
    real(kind=8) :: u0
!-----------------------------------------------------------------------
    call jemarq()
    ifr = iunifi('RESULTAT')
!
    pi = r8pi()
!
!
! --- 1.VERIFICATION DU SIGNE DES VITESSES
! ---   LES VITESSES ETUDIEES DOIVENT TOUTES ETRE DU MEME SIGNE
!
    vneg = .false.
    vpos = .false.
    do 10 iv = 1, npv
        if (vite(iv) .lt. 0.d0) then
            vneg = .true.
        else if (vite(iv).gt.0.d0) then
            vpos = .true.
        endif
10  end do
    if (vneg .and. vpos) then
        call utmess('F', 'ALGELINE_48')
    else if (vneg) then
        kec = -1
    else
        kec = 1
    endif
!
    call wkvect('&&FLUST4.TEMP.VABS', 'V V R', npv, ivabs)
    if (vneg) then
        do 11 iv = 1, npv
            zr(ivabs+iv-1) = dble(abs(vite(iv)))
11      continue
    else
        do 12 iv = 1, npv
            zr(ivabs+iv-1) = vite(iv)
12      continue
    endif
!
!
! --- 2.RECUPERATION DES INFORMATIONS APPORTEES PAR LE CONCEPT  ---
! ---   TYPE_FLUI_STRU                                          ---
!
    fsvi = typflu//'           .FSVI'
    call jeveuo(fsvi, 'L', lfsvi)
    imasse = zi(lfsvi)
    iaxe = zi(lfsvi+1)
!
    fsvr = typflu//'           .FSVR'
    call jeveuo(fsvr, 'L', lfsvr)
!
    fsvk = typflu//'           .FSVK'
    call jeveuo(fsvk, 'L', lfsvk)
    caelem = zk8(lfsvk)
    mater1 = zk8(lfsvk+1)
    mater2 = zk8(lfsvk+2)
!
    fsgm = typflu//'           .FSGM'
    call jeveuo(fsgm, 'L', lfsgm)
!
!
! --- 3.CREATION DES GROUPES DE NOEUDS CORRESPONDANT AUX COQUES ---
! ---   INTERNE ET EXTERNE, A PARTIR DES GROUPES DE MAILLES     ---
!
    call mefgmn(noma, 2, zk24(lfsgm))
!
!
! --- 4.DETERMINATION DES GRANDEURS GEOMETRIQUES CARACTERISTIQUES ---
! ---   DE LA CONFIGURATION                                       ---
!
    call wkvect('&&FLUST4.TEMP.GEOM', 'V V R', 9, igeom)
    call geocoq(noma, zk24(lfsgm), caelem, iaxe, zr(igeom))
!
    hmoy = zr(igeom)
!
!
! --- 5.CARACTERISATION DES DEFORMEES MODALES AVANT PRISE EN COMPTE ---
! ---   DU COUPLAGE                                                 ---
!
    call wkvect('&&FLUST4.TEMP.ICOQ', 'V V I', nbm, iicoq)
    call wkvect('&&FLUST4.TEMP.ORCO', 'V V R', 4*nbm, iorco)
    call wkvect('&&FLUST4.TEMP.COEF', 'V V R', 10*nbm, icoef)
!
    call rslipa(base, 'FREQ', '&&FLUST4.LIFREQ', ifreqi, n1)
    call modcoq(base, nuor, nbm, mater1, mater2,&
                noma, zk24(lfsgm), iaxe, kec, zr(igeom),&
                zi(iicoq), zr(iorco), zr(icoef), ifreqi)
!
!
! --- 6.PRISE EN COMPTE DU COUPLAGE FLUIDELASTIQUE
!
    write(ifr,*) '<FLUST4> COUPLAGE FLUIDE-STRUCTURE POUR COQUE_COAX'
    write(ifr,*)
    call wkvect('&&FLUST4.TEMP.MAJ', 'V V R', nbm, imaj)
    call wkvect('&&FLUST4.TEMP.AMFR', 'V V R', 2*nbm, iamfr)
    nt = 2
    lwork = 2*nt*nt + 10*nt + 2
    call wkvect('&&FLUST4.TEMP.WORK', 'V V R', lwork, iwork)
!
!
    u0 = 0.d0
    cf0 = 0.d0
    mcf0 = 1.d0
    s0 = 0.d0
!     =================================================================
!
! --- 6.1.CAS OU LES EFFETS DE MASSE AJOUTEE ONT DEJA ETE PRIS EN COMPTE
!
!     =================================================================
    if (imasse .eq. 0) then
!
!-------6.1.1.ON RECOPIE LES MASSES GENERALISEES ET LES DEFORMEES
!
        do 50 im = 1, nbm
            ior = nuor(im)
            call rsadpa(base, 'L', 1, 'MASS_GENE', ior,&
                        0, lmasg, k8b)
            call rsadpa(base, 'L', 1, 'FACT_PARTICI_DX', ior,&
                        0, lfact, k8b)
            masg(im) = zr(lmasg)
            fact(3*(im-1)+1) = zr(lfact ) * masg(im)
            fact(3*(im-1)+2) = zr(lfact+1) * masg(im)
            fact(3*(im-1)+3) = zr(lfact+2) * masg(im)
50      continue
        call cpdepl(melflu, base, nuor, nbm)
!
!-------6.1.2.CALCUL DE LA MATRICE DE MASSE AJOUTEE A RETRANCHER AUX
!             EXCITATIONS MODALES DUES AUX FORCES FLUIDELASTIQUES
!
        write(ifr,*) 'CALCUL DES MASSES MODALES AJOUTEES PAR LE FLUIDE'
        write(ifr,*)
        do 20 imod = 1, nbm
!
            numod = nuor(imod)
            write(ifr,'(A9,I3)') 'NUMOD = ',numod
            fi = zr(ifreqi+numod-1)
            ksi = amor(imod)
!
            call bijmoc(u0, zr(igeom), cf0, mcf0, zr(lfsvr),&
                        imod, imod, nbm, zi(iicoq), zr(iorco),&
                        zr(icoef), s0, s0, bii)
!
            zr(imaj+imod-1) = -1.d0*dble(bii)
            write(ifr,'(A5,G23.16)') 'MI = ',zr(imaj+imod-1)
            write(ifr,*)
!
            zr(iamfr+imod-1) = 4.d0*pi*fi*ksi*masg(imod)
            zr(iamfr+nbm+imod-1) = fi
!
20      continue
!
!-------6.1.3.CALCUL DES NOUVEAUX PARAMETRES MODAUX SOUS ECOULEMENT
!
        call pacouc(typflu, zr(imaj), zr(iorco), zr(ivabs), zr(icoef),&
                    masg, freq, zr(iamfr), nbm, imasse,&
                    npv, zr(iwork), zi(iicoq), zr( igeom), rbid,&
                    ier)
!
!-------6.1.4.CALCUL D'UN CRITERE DE POIDS DES TERMES EXTRADIAGONAUX
!             DE LA MATRICE B(S) PAR RAPPORT AUX TERMES DIAGONAUX
!
        if (nbm .gt. 1) call poibij(npv, zr(ivabs), zr(igeom), zr(lfsvr), nbm,&
                                    zi(iicoq), zr(iorco), zr(icoef), freq, imasse,&
                                    zr(imaj), rbid)
!     =================================================================
!
! --- 6.2.CAS GENERAL
!
!     =================================================================
    else
!
!-------6.2.1.CALCUL DES MODES EN EAU AU REPOS
!
        call wkvect('&&FLUST4.TEMP.VCPR', 'V V R', nbm*nbm, ivcpr)
!
        call modeau(melflu, noma, zr(igeom), zr(lfsvr), base,&
                    zr(ifreqi), nbm, nuor, zi(iicoq), zr(iorco),&
                    zr(icoef), amor, masg, fact, zr( iamfr),&
                    zr(ivcpr), zr(imaj))
!
        write(ifr,*) 'RESULTATS DU CALCUL DES MODES EN EAU AU REPOS'
        write(ifr,*)
        write(ifr,*) 'FREQUENCES PROPRES'
        do 30 imod = 1, nbm
            write(ifr,'(I3,1X,G23.16)') imod,zr(iamfr+nbm+imod-1)
30      continue
        write(ifr,*)
        write(ifr,*) 'DECOMPOSITION MODES EN EAU AU REPOS/MODES EN AIR'
        do 31 jmod = 1, nbm
            write(ifr,'(A24,I3)') 'MODE EN EAU AU REPOS NO ',jmod
            icomp = ivcpr + nbm*(jmod-1)
            do 32 imod = 1, nbm
                write(ifr,'(G23.16,1X,A23,I3)') zr(icomp+imod-1),&
                'SUIVANT MODE EN AIR NO ',imod
32          continue
            write(ifr,*)
31      continue
        write(ifr,*)
        write(ifr,*) 'MASSES MODALES'
        do 33 imod = 1, nbm
            write(ifr,'(I3,1X,G23.16)') imod,masg(imod)
33      continue
        write(ifr,*)
!
!-------6.2.2.CALCUL DES NOUVEAUX PARAMETRES MODAUX SOUS ECOULEMENT
!
        call pacouc(typflu, zr(imaj), zr(iorco), zr(ivabs), zr(icoef),&
                    masg, freq, zr(iamfr), nbm, imasse,&
                    npv, zr(iwork), zi(iicoq), zr( igeom), zr(ivcpr),&
                    ier)
!
!-------6.2.3.CALCUL D'UN CRITERE DE POIDS DES TERMES EXTRADIAGONAUX
!             DE LA MATRICE B(S) PAR RAPPORT AUX TERMES DIAGONAUX
!
        if (nbm .gt. 1) call poibij(npv, zr(ivabs), zr(igeom), zr(lfsvr), nbm,&
                                    zi(iicoq), zr(iorco), zr(icoef), freq, imasse,&
                                    zr(imaj), zr( ivcpr))
!
    endif
!
!
! --- 7.IMPRESSIONS DANS LE FICHIER RESULTAT SI DEMANDEES ---
!
    if (nivpar .eq. 1 .or. nivdef .eq. 1) then
        carac(1)=2.d0*hmoy
        carac(2)=0.d0
        calcul(1)=.true.
        calcul(2)=.false.
        call fluimp(4, nivpar, nivdef, melflu, typflu,&
                    nuor, freq, zr( ifreqi), nbm, vite,&
                    npv, carac, calcul, rbid)
    endif
!
! --- MENAGE
!
    call jedetr('&&FLUST4.TEMP.VABS')
    call jedetr('&&FLUST4.TEMP.GEOM')
    call jedetr('&&FLUST4.TEMP.ICOQ')
    call jedetr('&&FLUST4.TEMP.ORCO')
    call jedetr('&&FLUST4.TEMP.COEF')
    call jedetr('&&FLUST4.LIFREQ')
    call jedetr('&&FLUST4.TEMP.MAJ')
    call jedetr('&&FLUST4.TEMP.AMFR')
    call jedetr('&&FLUST4.TEMP.WORK')
    call jedetr('&&FLUST4.TEMP.VCPR')
    call jedetc('V', '&&MEFGMN', 1)
!
    call jedema()
!
end subroutine
