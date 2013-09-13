subroutine mefist(melflu, ndim, som, alpha, ru,&
                  promas, provis, matma, numgrp, nuor,&
                  freq, masg, fact, facpar, vite,&
                  xint, yint, rint, z, phix,&
                  phiy, defm, itypg, zg, hg,&
                  dg, tg, cdg, cpg, rugg,&
                  base)
! aslint: disable=W1501,W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/crnslv.h"
#include "asterfort/detrsd.h"
#include "asterfort/gnomsd.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jevtbl.h"
#include "asterfort/ltcrsd.h"
#include "asterfort/ltnotb.h"
#include "asterfort/mefcir.h"
#include "asterfort/mefeig.h"
#include "asterfort/mefgec.h"
#include "asterfort/mefger.h"
#include "asterfort/mefmat.h"
#include "asterfort/mefpre.h"
#include "asterfort/mefrec.h"
#include "asterfort/mefrep.h"
#include "asterfort/mefrot.h"
#include "asterfort/mefsma.h"
#include "asterfort/mefver.h"
#include "asterfort/nummo1.h"
#include "asterfort/pmavec.h"
#include "asterfort/smosli.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    integer :: ndim(14), numgrp(*), nuor(*)
    real(kind=8) :: fact(*), facpar(*), rtbloc
    real(kind=8) :: som(9), alpha, ru, matma(*), freq(*), masg(*), vite(*)
    real(kind=8) :: xint(*), yint(*), rint(*), z(*), phix(*), phiy(*), defm(*)
    character(len=8) :: promas, provis, base
    character(len=19) :: melflu
    integer :: itypg(*)
    real(kind=8) :: zg(*), hg(*), dg(*), tg(*), cdg(*), cpg(*), rugg(*)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     AFFECTATION
!     OPERATEUR APPELANT : OP0144 , FLUST3
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : MELFLU : NOM DU CONCEPT DE TYPE MELASFLU PRODUIT
! IN  : NDIM   : TABLEAU DES DIMENSIONS
! IN  : SOM    : COORDONNEES DES SOMMETS DE L'ENCEINTE RECTANGULAIRE
!                OU XEXT,YEXT,REXT
! IN  : ALPHA  : COEFFICIENT DE PROPORTIONALITE DE LA PESENTEUR PAR
!                RAPPORT A LA VALEUR STANDARD (9.81). LA PROJECTION DU
!                VECTEUR V SUIVANT Z VAUT 9.81*ALPHA.
! IN  : RU     : RUGOSITE DES CYLINDRES
! IN  : PROMAS : PROFIL DE MASSE VOLUMIQUE DU FLUIDE, DE TYPE FONCTION
! IN  : PROVIS : PROFIL DE VISCOSITE DU FLUIDE, DE TYPE FONCTION
! IN  : MATMA  : VECTEUR CONTENANT LES MATRICES MODALES, MASSE,RIGIDITE,
!                AMORTISSEMENT
! IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE
! IN  : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES SELECTIONNES POUR
!                LE COUPLAGE (PRIS DANS LE CONCEPT MODE_MECA)
! IN  : FREQ   : FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX PERTURBES
!                PAR L'ECOULEMENT
! IN  : MASG   : MASSES GENERALISEES DES MODES PERTURBES, SUIVANT LA
!                DIRECTION CHOISIE PAR L'UTILISATEUR
! IN  : VITE   : LISTE DES VITESSES D'ECOULEMENT ETUDIEES
! IN  : XINT   : COORDONNEES 'X' DES CENTRES DES CYLINDRES DANS
!                LE REPERE AXIAL
! IN  : YINT   : COORDONNEES 'Y' DES CENTRES DES CYLINDRES DANS
!                LE REPERE AXIAL
! IN  : RINT   : RAYONS DES CYLINDRES
! IN  : Z      : COORDONNEES 'Z'  DES DES POINTS DE DISCRETISATION DANS
!                LE REPERE AXIAL
! IN  : PHIX   : DEFORMEES MODALES INTERPOLEES DANS LE REPERE AXIAL
! IN  : PHIY   : DEFORMEES MODALES INTERPOLEES DANS LE REPERE AXIAL
! IN  : DEFM   : DEFORMEES MODALES DANS LE REPERE PHYSIQUE
!
! IN  : ITYPG  : VECTEUR DES TYPES DE GRILLE
! IN  : ZG     : COORDONNEES 'Z' DES POINTS DE DISCRETISATION
!                DES GRILLES
! IN  : HG     : LONGUEURS, DANS LA DIRECTION AXIALE, DE CHAQUE TYPE
!                DE GRILLE
! IN  : DG     : LARGEURS (OU COTES), DANS LE PLAN PERPENDICULAIRE
!                A L'AXE DU FAISCEAU, DE CHAQUE TYPE DE GRILLE
! IN  : TG     : EPAISSEURS, DANS LE PLAN PERPENDICULAIRE A L'AXE
!                DU FAISCEAU, DE CHAQUE TYPE DE GRILLE
! IN  : CDG    : COEFFICIENTS DE TRAINEE DE CHAQUE TYPE DE GRILLE
! IN  : CPG    : PENTES DU COEFFICIENT DE PORTANCE DE CHAQUE TYPE
!                DE GRILLE
! IN  : RUGG   : RUGOSITES DE CHAQUE TYPE DE GRILLE
!  IN : BASE   : NOM DU CONCEPT DE TYPE MODE_MECA DEFINISSANT LA BASE
!                MODALE DU SYSTEME AVANT PRISE EN COMPTE DU COUPLAGE
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: i, j, nbpara, iret
!-----------------------------------------------------------------------
    integer :: iaflu, ialfi, ialfr, iaxg, ibeta, icf, icfg
    integer :: icham, icp, id, idcent, idpst, idvit, iencei
    integer :: ifi, ificen, ifm, ifre, ii, iimag, iind
    integer :: iksi, imat1, imat2, imata, imatc, imatm, imatr
    integer :: imatv, ind, iorig, ipm, ippxx, ippxy, ippyx
    integer :: ippyy, ipst, ire, ireel, irho, irhog, isgn
    integer :: itmp, ivec, ivisc, iviscg, ivit, ivitg
    integer :: ivnxx, ivnxy, ivnyx, ivnyy, iwct, ixig, jj
    integer :: k, kk, m, n, nbcyl, nbfin, nbgrp
    integer :: nbgtot, nbmod, nbnoe, nbtot, nbv, nbz, nima
    integer :: nima2, nn, ntypg, nv, nv0
    real(kind=8) :: dh, vit0
!-----------------------------------------------------------------------
    parameter    ( nbpara = 5 )
    complex(kind=8) :: c16b
    character(len=8) :: typara(nbpara)
    character(len=14) :: nugene
    character(len=24) :: noobj
    character(len=19) :: riggen, masgen, amogen, valek(3)
    character(len=16) :: nopara(nbpara)
    character(len=19) :: nomt19
    character(len=24) :: nomcha
!
    data nopara / 'NUME_VITE', 'VITE_FLUI',&
     &              'MATR_MASS', 'MATR_AMOR', 'MATR_RIGI' /
    data typara / 'I', 'R', 'K24', 'K24', 'K24' /
! ----------------------------------------------------------------------
    call jemarq()
!
! --- LECTURE DES DIMENSIONS
    nbz = ndim(1)
    nbmod = ndim(2)
    nbcyl = ndim(3)
    nbgrp = ndim(4)
    iencei = ndim(6)
    nima = ndim(7)
    nima2 = ndim(8)
    nbv = ndim(9)
    nbnoe = ndim(11)
    nbtot = nbcyl*(2*nima+1)*(2*nima+1)
    nbfin = nbtot + 4*(nima2)*(nima2+2*nima+1)
    ntypg = ndim(13)
    nbgtot = ndim(14)
!
! --- ON CREE UN NUMME_DDL_GENE POUR STOCKER LES MATRICES
!
!
!     DETERMINATION DU NOM DE LA SD CACHEE NUME_DDL_GENE
    noobj ='12345678.NU000.NUME.PRNO'
    call gnomsd(' ', noobj, 12, 14)
    nugene=noobj(1:14)
!
!     -- FABRICATION DU STOCKAGE MORSE :
    call nummo1(nugene, base, nbmod, 'PLEIN')
!
!     -- FABRICATION DU STOCKAGE LIGN_CIEL :
    rtbloc=jevtbl('TAILLE_BLOC')
    call smosli(nugene//'.SMOS', nugene//'.SLCS', 'G', rtbloc)
!
!     -- SOLVEUR PAR DEFAUT :
    call crnslv(nugene, 'LDLT', 'SANS', 'G')
!
! --- ON GREFFE UNE STRUCTURE TABLE AU CONCEPT "MELFLU" POUR
!     STOCKER LES MATRICES CREEES
!
    nomt19 = ' '
    call jeexin(melflu//'.LTNT', iret)
    if (iret .ne. 0) then
        call ltnotb(melflu, 'MATR_GENE', nomt19)
        call detrsd('TABLE', nomt19)
    else
        call ltcrsd(melflu, 'G')
    endif
    call ltnotb(melflu, 'MATR_GENE', nomt19)
!
    call jeexin(nomt19//'.TBBA', iret)
    if (iret .ne. 0) call detrsd('TABLE', nomt19)
!
    call tbcrsd(nomt19, 'G')
    call tbajpa(nomt19, nbpara, nopara, typara)
!
! --- TABLEAUX DE TRAVAIL - ALLOCATION MEMOIRE
    call wkvect('&&MEFIST.TMP.PST', 'V V R', nbz*5, ipst)
    idpst = ipst + nbz
    icp = idpst + nbz
    icf = icp + nbz
    ire = icf + nbz
!
    nn = nbcyl*(2+2*nbcyl+8*nbgrp) + 4*nbgrp
    call wkvect('&&MEFIST.TMP.DIV', 'V V R', nn, idcent)
    ificen = idcent + nbcyl
    id = ificen + nbcyl
    ifi = id + nbcyl*nbcyl
    ippxx = ifi + nbcyl*nbcyl
    ippxy = ippxx + nbcyl*nbgrp
    ippyx = ippxy + nbcyl*nbgrp
    ippyy = ippyx + nbcyl*nbgrp
    ivnxx = ippyy + nbcyl*nbgrp
    ivnxy = ivnxx + nbcyl*nbgrp
    ivnyx = ivnxy + nbcyl*nbgrp
    ivnyy = ivnyx + nbcyl*nbgrp
    itmp = ivnyy + nbcyl*nbgrp
!
    call wkvect('&&MEFIST.TMP.BET', 'V V R', nbfin, ibeta)
    call wkvect('&&MEFIST.TMP.SGN', 'V V I', nbfin*2, isgn)
    iorig = isgn + nbfin
!
    call wkvect('&&MEFIST.TMP.VIT', 'V V R', (nbz+1)*4, ivit)
    irho = ivit + nbz + 1
    ivisc = irho + nbz + 1
    idvit = ivisc + nbz + 1
!
    call wkvect('&&MEFIST.TMP.VVV', 'V V R', nbmod, ivec)
    call wkvect('&&MEFIST.TMP.MAT', 'V V R', 3*nbmod*nbmod, imatm)
    imatr = imatm + nbmod*nbmod
    imata = imatr + nbmod*nbmod
!
    nn = 2*nbmod*(7+5*2*nbmod)
    call wkvect('&&MEFIST.TMP.VEC', 'V V R', nn, ireel)
    iimag = ireel + 2*nbmod
    ialfr = iimag + 2*nbmod
    ialfi = ialfr + 2*nbmod
    iwct = ialfi + 2*nbmod
    imat1 = iwct + 4*nbmod
    imat2 = imat1 + 2*nbmod*2*nbmod
    imatv = imat2 + 2*nbmod*2*nbmod
    imatc = imatv + 2*nbmod*2*nbmod
!
    call wkvect('&&MEFIST.TMP.IND', 'V V I', nbmod*2, iind)
    call wkvect('&&MEFIST.TMP.FRE', 'V V R', nbmod*2, ifre)
    iksi = ifre + nbmod
!
    if (ntypg .ne. 0) then
        call wkvect('&&MEFIST.TMP.COEFG', 'V V R', 4*nbgtot, icfg)
        irhog = icfg + nbgtot
        iviscg = irhog + nbgtot
        ivitg = iviscg + nbgtot
        call wkvect('&&MEFIST.TMP.SECTG', 'V V R', 2*ntypg+2, iaxg)
        ixig = iaxg + ntypg
        iaflu = ixig + ntypg
        ipm = iaflu + 1
    else
        icfg = 1
        irhog = 1
        iviscg = 1
        ivitg = 1
        iaxg = 1
        ixig = 1
        iaflu = 1
        ipm = 1
    endif
!
!
! --- ENCEINTE CIRCULAIRE
! --- CALCUL DES COORDONNEES POLAIRES ABSOLUES ET RELATIVES DES CENTRES
! --- DES CYLINDRES ET VERIFICATION DE L INCLUSION DES FAISCEAUX DANS
! --- L ENCEINTE
    if (iencei .eq. 1) then
        call mefver(ndim, som, xint, yint, rint)
        call mefgec(ndim, nbcyl, som, xint, yint,&
                    rint, zr(idcent), zr( ificen), zr(id), zr(ifi))
!
! --- ENCEINTE RECTANGULAIRE
! --- VERIFICATION DE L'ORDRE ET DE LA BONNE DISPOSITION DES SOMMETS DE
! --- L ENCEINTE, ET MISE EN FORME DES DONNEES POUR LA PRISE EN COMPTE
! --- DES CONDITIONS AUX LIMITES PAR UNE METHODE DERIVEE DE LA METHODE
! --- DES IMAGES
    else if (iencei.eq.2) then
        call mefver(ndim, som, xint, yint, rint)
        call mefger(ndim, som, xint, yint, rint,&
                    zi(isgn), zi(iorig), zr(ibeta))
! ---
    else
        call utmess('F', 'ALGELINE_85')
    endif
!
! --- CALCUL DES COEFFICIENTS INTERVENANT DANS L EXPRESSION DES
! --- FORCES DE PRESSION PERTURBEE, ET DES FORCES NORMALES DE
! --- FROTTEMENTS SUR CHAQUE CYLINDRES
!
    if (iencei .eq. 1) then
        call mefcir(ndim, nbcyl, nbgrp, numgrp, som,&
                    rint, zr(idcent), zr(ificen), zr(id), zr(ifi),&
                    zr(ippxx), zr(ippxy), zr(ippyx), zr( ippyy), zr(ivnxx),&
                    zr(ivnxy), zr(ivnyx), zr(ivnyy), zr(itmp))
    else if (iencei.eq.2.or.iencei.eq.0) then
        call mefrec(ndim, nbcyl, nbgrp, numgrp, xint,&
                    yint, rint, zi(isgn), zi(iorig), zr(ibeta),&
                    zr(ippxx), zr(ippxy), zr(ippyx), zr(ippyy), zr(ivnxx),&
                    zr(ivnxy), zr(ivnyx), zr(ivnyy), zr(itmp))
    endif
!
!
    ifm = iunifi('MESSAGE')
!
!
! --- CALCUL DES CARACTERISTIQUES MODALES EN FLUIDE AU REPOS
!
    nv0 = 0
    do 10 nv = 1, nbv
        if (vite(nv) .eq. 0.0d0) then
            nv0 = nv
            goto 11
        endif
10  end do
11  continue
!
    vit0 = 0.0d0
    write (ifm,6001) '<MEFIST> TRAITEMENT DE LA VITESSE D '&
     &   ,'ECOULEMENT: VIT0 = ',vit0,' M/S'
!
!.....CALCUL DU DIAMETRE HYDRAULIQUE, ET DES NOMBRES DE REYNOLDS
    call mefrot(ndim, som, vit0, promas, provis,&
                z, ru, rint, zr(ire), zr(icp),&
                zr(icf), dh, zr(ivit), zr(irho), zr(ivisc),&
                itypg, zg, tg, dg, rugg,&
                zr(iaxg), zr(ixig), zr(iaflu), zr(ipm), zr(icfg),&
                zr(ivitg), zr(irhog), zr(iviscg))
    som(9) = dh
!
!.....CALCUL DE LA PRESSION ET DU GRADIENT DE PRESSION STATIONNAIRE
    call mefpre(ndim, alpha, z, zr(icf), dh,&
                zr(ivit+1), zr(irho+1), zr(ipst), zr(idpst), zr(idvit),&
                itypg, zg, hg, zr(iaxg), zr(ipm),&
                zr(ixig), zr(iaflu), cdg, zr(icfg), zr(ivitg),&
                zr(irhog))
!
!.....CALCUL DES MATRICES DE MASSE, DE RAIDEUR, D AMORTISSEMENT SOUS
!.....ECOULEMENT (PROJECTION DES EFFORTS FLUIDES SUR BASE MODALE EN
!.....AIR)
    call mefmat(ndim, numgrp, nbz, nbgrp, nbmod,&
                matma, zr(idcent), zr(icp), zr(icf), zr(ivit),&
                zr(irho), zr(ipst), zr(idpst), rint, phix,&
                phiy, z, zr(imatm), zr(imatr), zr(imata),&
                itypg, zr(iaxg), zg, zr(irhog), zr(ivitg),&
                cdg, cpg)
!
!.....RESOLUTION DU PROBLEME GENERALISE SOUS ECOULEMENT - CALCUL DES
!.....VALEURS ET VECTEURS PROPRES
    call mefeig(ndim, nbmod, zr(imatm), zr(imatr), zr(imata),&
                zr(ifre), zr(iksi), zr(imatv), zr(ialfr), zr(ialfi),&
                zr(imat1), zr(imat2), zr(iwct), zr(imatc), zi(iind))
!
!.....PRISE EN COMPTE DE L'AMORTISSEMENT FLUIDE AU REPOS
    call mefrep(nbz, nbmod, nbcyl, nbgrp, numgrp,&
                z, zr(ifre), zr(irho+1), zr(ivisc), rint,&
                phix, phiy, zr(idcent), matma)
    call mefmat(ndim, numgrp, nbz, nbgrp, nbmod,&
                matma, zr(idcent), zr(icp), zr(icf), zr(ivit),&
                zr(irho), zr(ipst), zr(idpst), rint, phix,&
                phiy, z, zr(imatm), zr(imatr), zr(imata),&
                itypg, zr(iaxg), zg, zr(irhog), zr(ivitg),&
                cdg, cpg)
    call mefeig(ndim, nbmod, zr(imatm), zr(imatr), zr(imata),&
                zr(ifre), zr(iksi), zr(imatv), zr(ialfr), zr(ialfi),&
                zr(imat1), zr(imat2), zr(iwct), zr(imatc), zi(iind))
!
!.....STOCKAGE DES RESULTATS EN FLUIDE AU REPOS LE CAS ECHEANT
!.....MATRICES DE MASSES GENERALISEES, FREQUENCE, AMORTISSEMENT
    if (nv0 .ne. 0) then
!
        do 20 n = 1, nbmod
            nn = zi(iind+n-1)
            ii = (nn-1)*2*nbmod
            jj = n + nbmod*(nv0-1)
            kk = 3*(n-1) + nbmod*(nv0-1)
            call pmavec('ZERO', nbmod, zr(imatm), zr(imatv+ii), zr(ivec))
            masg(jj) = ddot(nbmod,zr(imatv+ii),1,zr(ivec),1)
            fact(kk+1) = ddot(nbmod,zr(ivec),1,facpar,1)
            fact(kk+2) = ddot(nbmod,zr(ivec),1,facpar(nbmod+1),1)
            fact(kk+3) = ddot(nbmod,zr(ivec),1,facpar(2*nbmod+1),1)
20      continue
!
        do 30 j = 1, nbmod
            ind = 2*nbmod*(nv0-1) + 2*(j-1) +1
            freq(ind) = zr(ifre+j-1)
            freq(ind+1) = zr(iksi+j-1)
30      continue
!
!........STOCKAGE DES DEFORMEES MODALES APRES REPROJECTION SUR BASE
!........PHYSIQUE
        nomcha(1:13) = melflu(1:8)//'.C01.'
        nomcha(20:24) = '.VALE'
        do 40 k = 1, nbmod
            kk = zi(iind+k-1)
            write(nomcha(14:16),'(I3.3)') nuor(k)
            write(nomcha(17:19),'(I3.3)') nv0
            call jeveuo(nomcha, 'E', icham)
            do 41 j = 1, nbnoe*6
                ind = icham+j-1
                zr(ind) = 0.d0
                do 42 m = 1, nbmod
                    zr(ind) = zr(ind) + defm(j+(m-1)*nbnoe*6) * zr( imatv+m-1+(kk-1)*2*nbmod)
42              continue
41          continue
            call jelibe(nomcha)
40      continue
!
!........IMPRESSIONS
!
        write (ifm,*)
        write (ifm,*) '         RESULTAT MODULE COUPLAGE FLUIDE-STRUCTURE'
        write (ifm,7001) '         VITESSE GAP(M/S) : ',vit0
        write (ifm,*) ' ************************************************'&
     &        ,'*******************'
        write (ifm,*) ' *                   *      FREQUENCES      *    '&
     &     ,'                  *'
        write (ifm,*) ' *        MODE       *         SOUS         *    '&
     &     ,'AMORTISSEMENT     *'
        write (ifm,*) ' *                   *    ECOULEMENT(HZ)    *    '&
     &     ,'     ( % )        *'
        write (ifm,*) ' ************************************************'&
     &     ,'*******************'
        do 50 i = 1, nbmod
            write (ifm,7002) ' *      ',nuor(i),'         *    ',&
     &           zr(ifre+i-1),'     *    ',100.d0*zr(iksi+i-1),'     *'
50      end do
        write (ifm,*) ' ************************************************'&
     &     ,'*******************'
        write (ifm,*)
!
    endif
!
! --- FIN DU CALCUL DES CARACTERISTIQUES EN FLUIDE AU REPOS
!
! --- BOUCLE SUR LES VITESSES D'ECOULEMENT
!
    do 100 nv = 1, nbv
!
        if (nv .eq. nv0) goto 100
!
        vit0 = vite(nv)
        write (ifm,6001) '<MEFIST> TRAITEMENT DE LA VITESSE D '&
        ,'ECOULEMENT: VIT0 = ',vit0,' M/S'
!
!........CALCUL DU DIAMETRE HYDRAULIQUE, ET DES NOMBRES DE REYNOLDS
        call mefrot(ndim, som, vit0, promas, provis,&
                    z, ru, rint, zr(ire), zr(icp),&
                    zr(icf), dh, zr(ivit), zr(irho), zr(ivisc),&
                    itypg, zg, tg, dg, rugg,&
                    zr(iaxg), zr(ixig), zr(iaflu), zr(ipm), zr(icfg),&
                    zr( ivitg), zr(irhog), zr(iviscg))
        som(9) = dh
!
!........CALCUL DE LA PRESSION ET DU GRADIENT DE PRESSION STATIONNAIRE
        call mefpre(ndim, alpha, z, zr(icf), dh,&
                    zr(ivit+1), zr(irho+1), zr(ipst), zr(idpst), zr(idvit),&
                    itypg, zg, hg, zr(iaxg), zr(ipm),&
                    zr( ixig), zr(iaflu), cdg, zr(icfg), zr(ivitg),&
                    zr(irhog))
!
!........CALCUL DES MATRICES DE MASSE, DE RAIDEUR, D AMORTISSEMENT SOUS
!........ECOULEMENT (PROJECTION DES EFFORTS FLUIDES SUR BASE MODALE EN
!........AIR)
        call mefmat(ndim, numgrp, nbz, nbgrp, nbmod,&
                    matma, zr(idcent), zr(icp), zr(icf), zr(ivit),&
                    zr(irho), zr(ipst), zr(idpst), rint, phix,&
                    phiy, z, zr(imatm), zr(imatr), zr(imata),&
                    itypg, zr(iaxg), zg, zr(irhog), zr(ivitg),&
                    cdg, cpg)
!
!........RESOLUTION DU PROBLEME GENERALISE SOUS ECOULEMENT - CALCUL DES
!........VALEURS ET VECTEURS PROPRES
        call mefeig(ndim, nbmod, zr(imatm), zr(imatr), zr(imata),&
                    zr(ifre), zr(iksi), zr(imatv), zr(ialfr), zr(ialfi),&
                    zr(imat1), zr(imat2), zr(iwct), zr(imatc), zi(iind))
!
!........STOCKAGE DES RESULTATS POUR LA VITESSE D'ECOULEMENT COURANTE
!........MATRICES DE MASSES GENERALISEES, FREQUENCE, AMORTISSEMENT
        do 60 n = 1, nbmod
            nn = zi(iind+n-1)
            ii = (nn-1)*2*nbmod
            jj = n + nbmod*(nv-1)
            kk = 3*(n-1) + nbmod*(nv-1)
            call pmavec('ZERO', nbmod, zr(imatm), zr(imatv+ii), zr(ivec))
            masg(jj) = ddot(nbmod,zr(imatv+ii),1,zr(ivec),1)
            fact(kk+1) = ddot(nbmod,zr(ivec),1,facpar,1)
            fact(kk+2) = ddot(nbmod,zr(ivec),1,facpar(nbmod+1),1)
            fact(kk+3) = ddot(nbmod,zr(ivec),1,facpar(2*nbmod+1),1)
60      continue
!
        do 70 j = 1, nbmod
            ind = 2*nbmod*(nv-1) + 2*(j-1) +1
            freq(ind) = zr(ifre+j-1)
            freq(ind+1) = zr(iksi+j-1)
70      continue
!
!........STOCKAGE DES DEFORMEES MODALES APRES REPROJECTION SUR BASE
!........PHYSIQUE
        nomcha(1:13) = melflu(1:8)//'.C01.'
        nomcha(20:24) = '.VALE'
        do 80 k = 1, nbmod
            kk = zi(iind+k-1)
            write(nomcha(14:16),'(I3.3)') nuor(k)
            write(nomcha(17:19),'(I3.3)') nv
            call jeveuo(nomcha, 'E', icham)
            do 81 j = 1, nbnoe*6
                ind = icham+j-1
                zr(ind) = 0.d0
                do 82 m = 1, nbmod
                    zr(ind) = zr(ind) + defm(j+(m-1)*nbnoe*6) * zr( imatv+m-1+(kk-1)*2*nbmod)
82              continue
81          continue
            call jelibe(nomcha)
80      continue
!
!........IMPRESSIONS
!
        write (ifm,*)
        write (ifm,*) '         RESULTAT MODULE COUPLAGE FLUIDE-STRUCTURE'
        write (ifm,7001) '         VITESSE GAP(M/S) : ',vit0
        write (ifm,*) ' ************************************************'&
     &        ,'*******************'
        write (ifm,*) ' *                   *      FREQUENCES      *    '&
     &     ,'                  *'
        write (ifm,*) ' *        MODE       *         SOUS         *    '&
     &     ,'AMORTISSEMENT     *'
        write (ifm,*) ' *                   *    ECOULEMENT(HZ)    *    '&
     &     ,'     ( % )        *'
        write (ifm,*) ' ************************************************'&
     &     ,'*******************'
        do 90 i = 1, nbmod
            write (ifm,7002) ' *      ',nuor(i),'         *    ',&
     &           zr(ifre+i-1),'     *    ',100.d0*zr(iksi+i-1),'     *'
90      end do
        write (ifm,*) ' ************************************************'&
     &     ,'*******************'
        write (ifm,*)
!
! ------ ON STOCKE LES MATRICES  ZR(IMATM), ZR(IMATA), ZR(IMATR)
!
!        DETERMINATION DU NOM DES SD CACHEES MATR_ASSE_GENE
        noobj ='12345678.RIGGEN0000.REFA'
        call gnomsd(' ', noobj, 16, 19)
        riggen=noobj(1:19)
!
        noobj ='12345678.MASGEN0000.REFA'
        call gnomsd(' ', noobj, 16, 19)
        masgen=noobj(1:19)
!
        noobj ='12345678.AMOGEN0000.REFA'
        call gnomsd(' ', noobj, 16, 19)
        amogen=noobj(1:19)
!
        valek(1) = masgen
        valek(2) = amogen
        valek(3) = riggen
!
        call mefsma(zr(imatm), zr(imata), zr(imatr), nugene, masgen,&
                    amogen, riggen)
!
        call tbajli(nomt19, nbpara, nopara, nv, vit0,&
                    c16b, valek, 0)
!
! --- FIN DE BOUCLE SUR LES VITESSES D ECOULEMENT
100  end do
!
! --- FORMATS D'IMPRESSION
!
    6001 format (1p,1x,a,a,d13.6,a)
    7001 format (1p,1x,a,d13.6)
    7002 format (1p,1x,a,i4,a,d13.6,a,d13.6,a)
!
! --- MENAGE
!
    call jedetr('&&MEFIST.TMP.PST')
    call jedetr('&&MEFIST.TMP.DIV')
    call jedetr('&&MEFIST.TMP.BET')
    call jedetr('&&MEFIST.TMP.SGN')
    call jedetr('&&MEFIST.TMP.VIT')
    call jedetr('&&MEFIST.TMP.VVV')
    call jedetr('&&MEFIST.TMP.MAT')
    call jedetr('&&MEFIST.TMP.VEC')
    call jedetr('&&MEFIST.TMP.IND')
    call jedetr('&&MEFIST.TMP.FRE')
    call jedetr('&&MEFIST.TMP.COEFG')
    call jedetr('&&MEFIST.TMP.SECTG')
!
    call jedema()
end subroutine
