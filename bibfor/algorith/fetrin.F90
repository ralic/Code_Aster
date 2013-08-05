subroutine fetrin(nbsd, nbi, vdo, vd1, matas,&
                  vsdf, vddl, colaux, chsecm, sdfeti,&
                  vlagi, option, chsol, testco, lrigid,&
                  dimgi, irr, nomggt, ipiv, nomgi,&
                  lstogi, infofe, irex, iprj, ifm,&
                  ifiv, nbproc, rang, k24irr)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL, POUR FETI, DU RESIDU INITIAL OU
!    RECONSTRUCTION VECTEUR DEPLACEMENT GLOBAL SOLUTION SI CONVERGENCE
!
!      IN   NBSD: IN   : NOMBRE DE SOUS-DOMAINES
!      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
!      IN    VD1: VR8  : VECTEUR AUXILIAIRE DE TAILLE NBI
!      IN  MATAS: CH19 : NOM DE LA MATR_ASSE GLOBALE
!      IN   VSDF: VIN  : VECTEUR MATR_ASSE.FETF INDIQUANT SI
!                         SD FLOTTANT
!      IN   VDDL: VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
!      IN COLAUX: COL  : COLLECTION TEMPORAIRE DE REEL
!      IN CHSECM: K19  : CHAM_NO SECOND MEMBRE GLOBAL
!      IN SDFETI: CH19 : SD DECRIVANT LE PARTIONNEMENT FETI
!      IN VLAGI : VR8  : VECTEUR LAGRANGE INITIAL OU SOLUTION
!      IN OPTION:  IN  : 1 -> RESIDU INIT., 2-> RECONSTRUCTION U SOL
!
!      SI OPTION=1
!      OUT   VDO: VR8  : VECTEUR OUTPUT DE TAILLE NBI
!
!      SI OPTION=2
!      IN  TESTCO: R8 : PARAMETRE DE TEST DE LA CONT. A L'INTERFACE
!      IN    IRR : IN : ADRESSE OBJET JEVEUX VECTEUR RESIDU PROJETE
!      IN LRIGID: LO  : LOGICAL INDIQUANT LA PRESENCE D'AU MOINS UN
!         SOUS-DOMAINES FLOTTANT
!      IN  DIMGI:  IN : TAILLE DE GIT*GI
!      IN CHSOL: CH19 : CHAM_NO SOLUTION GLOBAL
!     IN/OUT IPIV: VIN : ADRESSE VECTEUR DECRIVANT LE PIVOTAGE LAPACK
!                     POUR INVERSER (GIT)*GI
!     IN  LSTOGI: LO : TRUE, GI STOCKE, FALSE, RECALCULE
!     IN IREX/IPRJ/IFIV: IN : ADRESSES VECTEURS AUXILAIRES EVITANT
!                         DES APPELS JEVEUX.
!    IN NOMGI/NOMGGT: K24 : NOM DES OBJETS JEVEUX GI ET GIT*GI
!
!     POUR LES DEUX OPTIONS
!     IN RANG  : IN  : RANG DU PROCESSEUR
!     IN NBPROC: IN  : NOMBRE DE PROCESSEURS
!     IN K24IRR : K24 : NOM DE L'OBJET JEVEUX VDO POUR LE PARALLELISME
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
! aslint: disable=W1304,W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/fetmpi.h"
#include "asterfort/fetprj.h"
#include "asterfort/fetrex.h"
#include "asterfort/fettsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rltfr8.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
#include "blas/daxpy.h"
    integer :: nbsd, nbi, vddl(nbsd), vsdf(nbsd), option, dimgi, irr, ipiv, irex
    integer :: iprj, ifm, ifiv, nbproc, rang
    real(kind=8) :: vdo(nbi), vd1(nbi), vlagi(nbi), testco
    character(len=19) :: matas, chsecm, sdfeti, chsol
    character(len=24) :: colaux, infofe, k24irr, nomgi, nomggt
    logical :: lrigid, lstogi
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idd, ifetm, nbddl, idd1, jxsol, ifets, ivale, ivalg, nbsol, lmat
    integer :: ifetc, typsym, j, j1, iprnog, ibid, irefe, ideeq, ino, icmp
    integer :: nbddl1, iprno, lprno, nec2, idime, nbno, ivals, dvalg, ivalcs
    integer :: nbmc, ifetp, nbmc1, jxsol1, ialpha, ifetr, idecai, idecaa, lconl
    integer :: nblili, ilil, lprnol, iprnol, nbcmp, iret, ikflin, ilig, nbcha
    integer :: icha, ifetl1, k, ifetl3, lfetl3, nivmpi, ilimpi, ifeti, nbpb
    integer :: lcon1, iaux1, iaux0, iaux2, ifetl5, jfel4, iaux3, ksol, nddl
    integer :: inoold, iretn, iretl, jmult, icol3
    character(len=8) :: k8bid, nomsd
    character(len=19) :: matdd, chsmdd, prfchn, chamls, prfchg, k19b
    character(len=24) :: nomsdp, nomsdr, k24b, lilil, ligrl, prnol, kflin, ligr2
    character(len=24) :: nomsda, k24alp, k24val, kfcfl, k24mul, colau3
    real(kind=8) :: rauxl, alpha, rbid
!     REAL*8       RMIN
    logical :: ldup, lpara, lbid, lrec
    integer(kind=4) :: nbddl4, nbi4
!
! CORPS DU PROGRAMME
    call jemarq()
!
! PLUS PETITE VALEUR REELLE
!      RMIN=R8MIEM()
!
! INIT. NBRE DE SECOND MEMBRES SOLUTION POUR RLTFR8
    nbsol=1
!
! INIT. NOM OBJET JEVEUX POUR PRODUIT PAR PSEUDO-INVERSE LOCALE
    nomsdp=matas//'.FETP'
    nomsdr=matas//'.FETR'
!
! INITS DIVERSES
    nbi4=nbi
    if (nbproc .eq. 1) then
        lpara=.false.
    else
        lpara=.true.
    endif
    if (infofe(10:10) .eq. 'T') then
        nivmpi=2
    else
        nivmpi=1
    endif
    ifm=zi(ifiv)
! ADRESSE JEVEUX OBJET FETI & MPI
    call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
! POUR RECUPERER LE NOM DES SOUS-DOMAINES PAR JENUNO
    nomsda=sdfeti(1:19)//'.FETA'
!
    if (option .eq. 1) then
!
! INIT. VECTEUR SOLUTION LOCAL ET AUX.
        do 10 j = 1, nbi
            vd1(j)=0.d0
            vdo(j)=0.d0
10      continue
!
    else if (option.eq.2) then
!
! OBJETS JEVEUX POINTANT SUR LA LISTE DES CHAM_NOS LOCAUX
        call jeveuo(chsol//'.FETC', 'L', ifets)
! PROF_CHNO DU DOMAINE GLOBAL
        call jeveuo(chsol//'.REFE', 'L', irefe)
        prfchg=zk24(irefe+1)(1:19)
        call jeveuo(prfchg//'.PRNO', 'L', iprno)
        call jelira(jexnum(prfchg//'.PRNO', 1), 'LONMAX', lprno, k8bid)
        call jeveuo(sdfeti//'.FDIM', 'L', idime)
! NBRE DE NOEUDS DU MAILLAGE
        nbno=zi(idime+4)
! LONGUEUR DU VECTEUR D'ENTIERS CODES + 2
        nec2=lprno/nbno
!
        k24val=chsol//'.VALE'
! .VALE DU CHAM_NO SOLUTION GLOBAL
        call jeveuo(k24val, 'E', ivals)
!
        call jeveuo(sdfeti//'.FETI', 'L', ifeti)
! TAILLE DU PROBLEME GLOBAL
        call jelira(k24val, 'LONMAX', nbpb, k8bid)
!
! CALCUL ALPHA SI MODES DE CORPS RIGIDES
        if (lrigid) then
            k24alp='&&FETI.ALPHA.MCR'
            call wkvect(k24alp, 'V V R', nbi, ialpha)
!
            call fetprj(nbi, zr(irr), zr(ialpha), nomggt, lrigid,&
                        dimgi, 2, sdfeti, ipiv, nbsd,&
                        vsdf, vddl, matas, nomgi, lstogi,&
                        infofe, irex, iprj, nbproc, rang,&
                        k24alp)
!
            idecaa=ialpha
        endif
! OBJET POUR RECONSTRUCTION DES DIRICHLETS
        kflin=sdfeti(1:19)//'.FLIN'
        kfcfl=sdfeti(1:19)//'.FCFL'
! OBJET POUR ACCELERER LA RECONSTRUCTION DU CHAMP SOLUTION
        colau3='&&FETI.DVALG'
    else
        ASSERT(.false.)
    endif
!
! OBJET JEVEUX POINTANT SUR LA LISTE DES MATR_ASSE
    ifetm=zi(ifiv+1)
! OBJET JEVEUX POINTANT SUR LA LISTE DES CHAM_NO SECOND MEMBRE
    call jeveuo(chsecm//'.FETC', 'L', ifetc)
!
! MONITORING
    if (infofe(1:1) .eq. 'T') then
        if (option .eq. 1) then
            write(ifm,*)'<FETI/FETRIN', rang,&
     &                '> CALCUL (KI)-*(FI-RIT*LANDA0)'
        else
            write(ifm,*)'<FETI/FETRIN', rang,&
     &                '> CALCUL (KI)-*(FI-RIT*LANDAS)'
        endif
    endif
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
    do 100 idd = 1, nbsd
! LE SOUS-DOMAINE IDD EST IL CONCERNE PAR LE PROCESSUS ACTUEL ?
        if (zi(ilimpi+idd) .eq. 1) then
            call jemarq()
            idd1=idd-1
            call jenuno(jexnum(nomsda, idd), nomsd)
!
! MATR_ASSE ASSOCIEE AU SOUS-DOMAINE IDD
            matdd=zk24(ifetm+idd1)(1:19)
! DESCRIPTEUR DE LA MATRICE DU SOUS-DOMAINE
            k=ifiv+2+idd1*5
            lmat=zi(k)
! NOMBRE DE BLOC DE STOCKAGE DE LA MATRICE KI/ TYPE DE SYMETRIE
            typsym=zi(lmat+4)
! NBRE DE DDLS DU SOUS-DOMAINE
            nbddl=vddl(idd)
            nbddl4=nbddl
            nbddl1=nbddl-1
! VECTEUR AUXILIAIRE DE TAILLE NDDL(SOUS_DOMAINE_IDD)
            jxsol=zi(k+3)
! SECOND MEMBRE LOCAL AU SOUS-DOMAINE
            chsmdd=zk24(ifetc+idd1)(1:19)
            call jeveuo(chsmdd//'.VALE', 'L', ivale)
!
! EXTRACTION AU SOUS-DOMAINE IDD: (RIDD)T * LANDA (0 OU SOL)
            call fetrex(2, idd, nbi, vlagi, nbddl,&
                        zr(jxsol), irex)
!
! RECOPIE DE FIDD - (RIDD)T*LANDA (0 OU SOL) DANS VECTEUR AUX
! POUR R8AXPY ON STOCKE L'OPPOSE, ON COMPENSERA PAR LA SUITE
! SUIVANT LES OPTIONS
            call daxpy(nbddl4, -1.d0, zr(ivale), 1, zr(jxsol),&
                       1)
!
! SCALING VIA ALPHA DES COMPOSANTES DU SECOND MEMBRE DUES AUX LAGRANGES
! SYSTEME: K * U= ALPHA * F ---> K * U/ALPHA = F
! ADRESSE MATDD.CONL SI IL EXISTE
            lconl=zi(k+1)
            if (lconl .ne. 0) then
                lcon1=zi(k+2)
                do 15 j = 1, nbddl
                    j1=j-1
                    zr(jxsol+j1)=zr(lcon1+j1)*zr(jxsol+j1)
15              continue
            endif
! -------------------------------------------------
! ----  SOUS-DOMAINE NON FLOTTANT
! -------------------------------------------------
! NOMBRES DE MODES DE CORPS RIGIDES DU SOUS-DOMAINE IDD
            nbmc=vsdf(idd)
            if (nbmc .eq. 0) then
!
! CALCUL DE (KI)-FI PAR MULT_FRONT
                call rltfr8(matdd, nbddl, zr(jxsol), nbsol, typsym)
            else
! -------------------------------------------------
! ----  SOUS-DOMAINE FLOTTANT
! -------------------------------------------------
! CALCUL DE (KI)+FI PAR MULT_FRONT
                call rltfr8(matdd, nbddl, zr(jxsol), nbsol, typsym)
                call jeveuo(jexnom(nomsdp, nomsd), 'L', ifetp)
!
                nbmc1=nbmc-1
                jxsol1=jxsol-1
                do 25 j = 0, nbmc1
                    zr(jxsol1+zi(ifetp+j))=0.d0
25              continue
            endif
! SCALING DES COMPOSANTES DE ZR(LXSOL) POUR CONTENIR LA SOL. REELLE U
            if (lconl .ne. 0) then
                do 27 j = 1, nbddl
                    j1=j-1
                    zr(jxsol+j1)=zr(lcon1+j1)*zr(jxsol+j1)
27              continue
            endif
! -------------------------------------------------
! ----  CALCUL RESIDU INITIAL
! -------------------------------------------------
            if (option .eq. 1) then
! RESTRICTION DU SOUS-DOMAINE IDD SUR L'INTERFACE: (RIDD) * ...
                call fetrex(1, idd, nbddl, zr(jxsol), nbi,&
                            vd1, irex)
!
! CUMUL DANS LE VECTEUR VDO=SOMME(I=1,NBSD)(RI*((KI)+ *(FI-RIT*LANDA0))
                call daxpy(nbi4, -1.d0, vd1, 1, vdo,&
                           1)
!
            else
!
! -------------------------------------------------
! ----  RECONSTRUCTION SOLUTION U GLOBALE
! -------------------------------------------------
! RAJOUT EVENTUEL DES PARTIES DUES AUX MODES DE CORPS RIGIDES
                if (nbmc .ne. 0) then
!
! COMPOSANTES DES MODES DE CORPS RIGIDES
                    call jeveuo(jexnom(nomsdr, nomsd), 'L', ifetr)
                    idecai=ifetr
                    do 32 j = 1, nbmc
!
! COEFF. ALPHAI MULTIPLICATEUR
                        alpha=zr(idecaa)
! USOLI = USOLI + BI * ALPHAI
                        call daxpy(nbddl4, alpha, zr(idecai), 1, zr(jxsol),&
                                   1)
                        idecai=idecai+nbddl
                        idecaa=idecaa+1
32                  continue
                endif
!
! AS-T-ON DEJA LES INFOS POUR FAIRE LA JOINTURE ULOCAL/UGLOBAL ?
                call jeveuo(jexnom(colau3, nomsd), 'E', icol3)
                if (zi(icol3) .eq. -999) then
! ON A DEJA L'INFO, PAS BESOIN DE LA RECONSTRUIRE
                    lrec=.true.
                else
! L'INVERSE
                    lrec=.false.
                endif
! PROF_CHNO DU SOUS-DOMAINE IDD
                call jeveuo(chsmdd//'.REFE', 'L', irefe)
                prfchn=zk24(irefe+1)(1:19)
                call jeveuo(prfchn//'.DEEQ', 'L', ideeq)
!
! .VALE DU CHAM_NO LOCAL IDD
                chamls=zk24(ifets+idd1)(1:19)
                call jeveuo(chamls//'.VALE', 'E', ivalcs)
!
!----------------------------------------------------------------------
! ON CONSTRUIT LA JOINTURE UI LOCAL / UI GLOBAL POUR LA PREMIERE FOIS
!----------------------------------------------------------------------
                if (.not.lrec) then
! --------------------------------
! ----  BOUCLE SUR LES DDLS PHYSIQUE DU CHAM_NO LOCAL
! --------------------------------
                    inoold=0
                    nddl=0
                    do 35 j = 0, nbddl1
!
! NUMERO DU NOEUD DU MAILLAGE (INO) ET DE SA COMPOSANTE (ICMP) CORRES
! PONDANT A L'EQUATION J DU CHAM_NO LOCAL
                        ino=zi(ideeq+2*j)
                        icmp=zi(ideeq+2*j+1)
!
                        if (icmp*ino .gt. 0) then
!
                            if (ino .ne. inoold) then
                                inoold=ino
                                nddl=0
                            else
                                nddl=nddl+1
                            endif
! NOEUD PHYSIQUE
! LA MISE AU CARRE DE LA VALEUR DES NOEUDS D'INTERFACE EST FAITE MAINTE
! NANT A LA FIN DE FETRIN VIA L'OBJET '&&FETI.MULT' CONSTRUIT PAR
! NUMERO.
! DECALAGE DANS LE .VALE DU CHAM_NO GLOBAL CORRESPONDANT A (INO,ICMP)
                            dvalg=zi(iprno+(ino-1)*nec2) + nddl
                            zi(icol3+j+1)=dvalg
!
! VALEUR UI A TRANSFERRER SUR LE CHAM_NO GLOBAL ET SUR LE LOCAL
! EN TENANT COMPTE D'UNE EVENTUELLE MULTIPLICITE SI NOEUD PHYSIQUE
! D'INTERFACE
                            rauxl=-zr(jxsol+j)
                            zr(ivalcs+j)=rauxl
                            ivalg=ivals-1+dvalg
!
! TEST POUR VERIFIER LA CONTINUITE AUX INTERFACES DEBRANCHE POUR
! COHERENCE PARALLELISME/SEQUENTIEL.
!                RAUX=ZR(IVALG)
!                UMOY=RAUX+RAUXL
!                IF (ABS(RAUX).GT.RMIN) THEN
!                  RAUX1=ABS((RAUX-RAUXL)/UMOY)
!                  IF (RAUX1.GT.TESTCO) THEN
!                    IMSG(1)=INO
!                    IMSG(2)=ICMP
!                    RAUX1=100.D0*RAUX1
!                     'PB POTENTIEL DE CONTINUITE ?')
!                     'INTERFACE (INO,ICMP)= ',2,IMSG)
!                     ERREUR INTERFACE (EN %)= ',1,RAUX1)
!                  ENDIF
!                ENDIF
!
! AFFECTATION EFFECTIVE DE UI VERS U
                            zr(ivalg)=zr(ivalg)+rauxl
!
! MONITORING
!              IF (INFOFE(4:4).EQ.'T') THEN
!                WRITE(IFM,*)'NOEUD PHYSIQUE '
!                WRITE(IFM,*)IDD,J,INO,ICMP,NDDL
!               WRITE(IFM,*)IVALG,RAUXL,ZR(IVALG)
!              ENDIF
!
                        else
! MONITORING
!            IF (INFOFE(4:4).EQ.'T') THEN
!              WRITE(IFM,*)'NOEUD TARDIF NON PRIS DANS CETTE PASSE '
!              WRITE(IFM,*)IDD,J,INO,ICMP,-ZR(JXSOL+J)
!            ENDIF
!
                        endif
35                  continue
!
! --------------------------------
! ----  FIN BOUCLE SUR LES DDLS PHYSIQUE DU CHAM_NO LOCAL
! --------------------------------
!
! --------------------------------
! ----  BOUCLE SUR LES LIGRELS TARDIFS DU CHAM_NO LOCAL POUR CONSTRUIRE
! ----  LES DDLS DES NOEUDS TARDIFS ASSOCIES A DES MAILLES TARDIVES
! ---- EX. LES LAGRANGES. CONTRE-EX: LES DDLS DE CONTACT CONTINUE DEJA
! ----  PRIS EN COMPTE PRECEDEMMENT
! --------------------------------
!
                    lilil=prfchn//'.LILI'
                    prnol=prfchn//'.PRNO'
                    call jelira(lilil, 'NOMMAX', nblili, k8bid)
! LILI(1)=MAILLAGE, LILI(2)=MODELE, LILI(3...)=LIGREL TARDIF
! SI NECESSAIRE
                    call jeexin(jexnom(kflin, nomsd), iretn)
                    call jeexin(jexnom(kfcfl, nomsd), iretl)
                    if (nblili .le. 2) then
                        ASSERT((iretn.eq.0).and.(iretl.eq.0))
                    else
                        ASSERT((iretn.ne.0).or.(iretl.ne.0))
                        if (iretn .ne. 0) then
                            call jeveuo(jexnom(kflin, nomsd), 'L', ikflin)
                            ikflin=ikflin-1
                            call jelira(jexnom(kflin, nomsd), 'LONMAX', nbcha, k8bid)
                        endif
                    endif
                    do 90 ilil = 3, nblili
! NOM DU LIGREL ILI, LIGRL
                        call jenuno(jexnum(lilil, ilil), ligrl)
                        call jeexin(jexnum(prnol, ilil), iret)
! IL EST POSSIBLE D'AVOIR UN LIGREL DE MAILLE TARDIVE SANS NOEUD TARDIF
! AUQUEL CAS ON NE FAIT RIEN. LES VALEURS DES DDLS PORTEES PAR LES
! NOEUDS PHYSIQUES ONT DEJA ETE REPORTEES DANS LA BOUCLE PRECEDENTE
! SUR LE CHAM_NO LOCAL.
! ICI, IL NE S'AGIT DE REPORTER QUE LES VALEURS DES DDLS DES NOEUDS
! TARDIFS
                        if (iret .ne. 0) then
                            call jelira(jexnum(prnol, ilil), 'LONMAX', lprnol, k8bid)
                            call jeveuo(jexnum(prnol, ilil), 'L', iprnol)
                            lprnol=lprnol/nec2
!
! --------------------------------
! BOUCLE SUR LES NOEUDS TARDIFS DU LIGREL LIGRL
! --------------------------------
                            do 80 ino = 1, lprnol
! ADRESSE DANS LE CHAM_NO LOCAL
                                j=zi(iprnol+(ino-1)*nec2)
                                ASSERT(j.gt.0)
! NOMBRE DE COMPOSANTE
                                nbcmp=zi(iprnol+(ino-1)*nec2+1)
                                ASSERT(nbcmp.eq.1)
!
! --------------------------------
! ON PARCOURT LA LISTE DES LIGRELS TARDIFS DU SOUS-DOMAINE IDD POUR
! CONFIRMER LE NOM DU LIGREL A RECHERCHER DANS LE PRNO GLOBAL
! --------------------------------
                                do 50 icha = 1, nbcha
                                    ligr2=zk24(ikflin+icha)
                                    if (ligr2 .eq. ligrl) then
! LIGREL TARDIF NON DUPLIQUE DE NOM LIGR2=LIGRL
                                        ldup=.false.
                                        goto 55
                                    else
! ON PARCOURT LES FILS DU LIGREL POUR LE SOUS-DOMAINE CONCERNE
                                        call jeveuo(ligr2(1:19)// '.FEL1', 'L', ifetl1)
                                        k24b=zk24(ifetl1+idd-1)
! ON A TROUVE LE LIGREL DE CHARGE
                                        if (k24b .eq. ligrl) then
! LIGRL LIGREL TARDIF DUPLIQUE DE PERE LIGR2
                                            ldup=.true.
                                            goto 55
                                        endif
                                    endif
50                              continue
                                ASSERT(.false.)
55                              continue
!
! SI LIGREL DUPLIQUE, IL FAUT RETROUVER SON INDICE DANS LE  PRNO GLOBAL
                                if (ldup) then
                                    call jeveuo(ligr2(1:19)//'.FEL3', 'L', ifetl3)
                                    call jelira(ligr2(1:19)//'.FEL3', 'LONMAX', lfetl3, k8bid)
                                    call jeexin(ligr2(1:19)//'.FEL5', iret)
                                    if (iret .ne. 0) call jeveuo(ligr2(1:19 )//'.FEL5', 'L',&
                                                                 ifetl5)
                                    lfetl3=lfetl3/2
                                    do 60 k = 1, lfetl3
                                        iaux0=ifetl3+2*(k-1)+1
                                        iaux1=zi(iaux0)
                                        if (iaux1 .gt. 0) then
! NOEUD TARDIF LIE A UN DDL NON SITUE SUR L'INTERFACE
                                            if ((iaux1.eq.idd) .and. (zi( iaux0-1).eq.-ino)) then
                                                ksol=k
                                                goto 65
                                            endif
                                        else if (iaux1.lt.0) then
! C'EST UN NOEUD TARDIF LIE A UN DDL PHYSIQUE DE L'INTERFACE
                                            if (iret .ne. 0) iaux2=( zi( ifetl5)/3)-1
                                            do 56 jfel4 = 0, iaux2
                                                iaux3=ifetl5+3*jfel4+3
                                                if ((zi(iaux3-1).eq.idd) .and.&
                                                    (zi(iaux3-2).eq.-ino)) then
! VOICI SON NUMERO LOCAL CONCERNANT LE SD
                                                    ksol=zi(iaux3)
                                                    goto 65
                                                endif
56                                          continue
                                        endif
60                                  continue
                                    ASSERT(.false.)
65                                  continue
                                else
                                    ksol=ino
                                endif
! INFO DU PRNO GLOBAL
                                call jenonu(jexnom(prfchg//'.LILI', ligr2), ilig)
                                call jeveuo(jexnum(prfchg//'.PRNO', ilig), 'L', iprnog)
! DECALAGE DANS LE .VALE DU CHAM_NO GLOBAL
                                dvalg=zi(iprnog+(ksol-1)*nec2)
                                zi(icol3+j)=dvalg
!
! VALEUR UI A TRANSFERRER SUR LE CHAM_NO GLOBAL ET SUR LE LOCAL
! ON DECALE DE J-1 AU LIEU DE J POUR NOEUD PHYSIQUE CAR J COMMENCE A 1
                                rauxl=-zr(jxsol+j-1)
                                zr(ivalcs+j-1)=rauxl
!
! TEST POUR VERIFIER LA CONTINUITE AUX INTERFACES, NORMALEMENT INACTIVE
! POUR L'INSTANT CAR PAS DE LAGRANGE A L'INTERFACE: LA SDFETI NE CONNAIT
! PAS LEUR MULTIPLICITE
! TEST POUR VERIFIER LA CONTINUITE AUX INTERFACES DEBRANCHE POUR
! COHERENCE PARALLELISME/SEQUENTIEL.
                                ivalg=ivals-1+dvalg
!
!                RAUX=ZR(IVALG)
!                UMOY=(RAUX+RAUXL)*0.5D0
!                IF (ABS(RAUX).GT.RMIN) THEN
!                  RAUX1=ABS((RAUX-RAUXL)/UMOY)
!                  IF (RAUX1.GT.TESTCO) THEN
!                    RAUX1=100.D0*RAUX1
!     &              'PB POTENTIEL DE CONTINUITE ?')
!                    'LAGRANGE INO= ',1,-INO)
!                    'DU LIGREL TARDIF ',1,LIGRL)
!                  'ERREUR INTERFACE (EN %)= ',1,RAUX1)
!                  ENDIF
!                ENDIF
!
! AFFECTATION EFFECTIVE DE UI VERS U
                                zr(ivalg)=zr(ivalg)+rauxl
!
! MONITORING
!              IF (INFOFE(4:4).EQ.'T') THEN
!                WRITE(IFM,*)'NOEUD TARDIF',LDUP
!                WRITE(IFM,*)IDD,LIGRL,INO,J,K,DVALG,RAUXL
!              ENDIF
80                          continue
                        endif
90                  continue
!
! POUR LE PROCHAIN PASSAGE ON TAG LA SD
                    zi(icol3)=-999
                else
!----------------------------------------------------------------------
! ON REUTILISE LA JOINTURE UI LOCAL / UI GLOBAL
!----------------------------------------------------------------------
                    do 95 j = 1, nbddl
                        rauxl=-zr(jxsol-1+j)
                        zr(ivalcs-1+j)=rauxl
                        ivalg=ivals-1+zi(icol3+j)
                        zr(ivalg)=zr(ivalg)+rauxl
95                  continue
                endif
!
! POUR ECRITURE FICHIER (SI INFOFE(14:14)='T')
                if (option .eq. 2) then
                    call fettsd(infofe, idd, nbddl, ibid, sdfeti,&
                                k24b, ideeq, ivalcs, ibid, ifm,&
                                lbid, ibid, ibid, ibid, k19b,&
                                8, lbid)
                    call fettsd(infofe, idd, nbddl, ibid, sdfeti,&
                                k24b, ideeq, ivalcs, ibid, ifm,&
                                lbid, ibid, ibid, ibid, k19b,&
                                11, lbid)
                endif
! MONITORING
                if (infofe(2:2) .eq. 'T') call utimsd(ifm, 2, .false., .true., chamls(1:19),&
                                                      1, ' ')
! FIN DU IF OPTION
            endif
            call jedema()
        else
! DECALAGE DU NOMBRES DE MODES DE CORPS RIGIDES DU SOUS-DOMAINE IDD
! NON PRIS EN COMPTE PAR CE PROCESSEUR POUR OPTION=2
            if ((option.eq.2) .and. (lrigid)) idecaa=idecaa+vsdf(idd)
!
! FIN DU IF ILIMPI
        endif
100  end do
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
    if (lpara) then
        if (option .eq. 1) then
! REDUCTION DU RESIDU INITIAL POUR LE PROCESSUS MAITRE
            call fetmpi(7, nbi, ifm, nivmpi, ibid,&
                        ibid, k24irr, k24b, k24b, rbid)
!
        else
! REDUCTION DU CHAM_NO GLOBAL POUR TOUS LES PROCESSEURS
            call fetmpi(71, nbpb, ifm, nivmpi, ibid,&
                        ibid, k24val, k24b, k24b, rbid)
        endif
    endif
! CHAQUE PROC DISPOSE DE LA SOLUTION ET DOIT SCALER LES COMPOSANTES A
! L'INTERFACE
    if (option .eq. 2) then
        k24mul='&&FETI.MULT'
        call jeveuo(k24mul, 'L', jmult)
        do 120 j = 1, nbpb
            j1=j-1
            zr(ivals+j1)=zr(ivals+j1)/zi(jmult+j1)
120      continue
        if ((rang.eq.0) .and. (infofe(2:2).eq.'T')) call utimsd(ifm, 2, .false., .true.,&
                                                                k24val(1:19), 1, ' ')
    endif
!
! POUR TEST SUR LA SD_FETI (SI INFOFE(12:12)='T')
    if ((option.eq.2) .and. (rang.eq.0)) then
        k24b(1:19)=prfchg
        call fettsd(infofe, ibid, nbpb, ibid, sdfeti,&
                    k24b, ibid, ivals, ibid, ifm,&
                    lbid, ibid, ibid, ibid, k19b,&
                    5, lbid)
    endif
! POUR ECRITURE FICHIER (SI INFOFE(14:14)='T')
    if (option .eq. 2) then
        k24b(1:19)=prfchg
        call fettsd(infofe, ibid, nbpb, ibid, sdfeti,&
                    k24b, ibid, ivals, ibid, ifm,&
                    lbid, ibid, ibid, ibid, k19b,&
                    9, lbid)
    endif
    if ((option.eq.2) .and. (lrigid)) call jedetr(k24alp)
!
    call jedema()
end subroutine
