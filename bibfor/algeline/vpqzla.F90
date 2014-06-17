subroutine vpqzla(typeqz, qrn, iqrn, lqrn, qrar,&
                  qrai, qrba, qrvl, lvec, kqrn,&
                  lvalpr, nconv, omecor, ktyp, kqrnr,&
                  neqact, ilscal, irscal, optiof, omemin,&
                  omemax, omeshi, ddlexc, nfreq, lmasse,&
                  lraide, lamor, numedd, sigma, icscal,&
                  ivscal, iiscal, bwork, flage)
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
!     SUBROUTINE ASTER ORCHESTRANT LA METHODE QZ (VERSION LAPACK).
!     EN GENERALISEE OU EN QUADRATIQUE AVEC MATRICES K, M  ET C
!     SYMETRIQUES OU NON. K E R/C, M E R, C E R.
!-----------------------------------------------------------------------
! CE PROGRAMME EFFECUE LES TACHES SUIVANTES:
!
!    - PASSAGE DES MATRICES EN STOCKAGE MORSE A DES MATRICES PLEINES
!    (NECESSAIRE POUR UTILISER LAPACK)
!    - APPEL AUX ROUTINES LAPACK DE RESOLUTION DE PB GENERALISES
!          BETA * A * X - ALPHA * B * X = 0
!       ON PEUT ALORS CALCULER LES VALEURS PROPRES DU PROBLEME
!       GENERALISE ENFAISANT LE RAPPORT ALPHA/BETA SI BETA EST NON NUL.
!    - TRI DES VALEURS PROPRES: SEUL LES MODES PROPRES CORRESPONDANT
!    AUX DDL ACTIFS SONT CONSERVES. LES MODES PROVENANT DES LAGRANGES
!    OU DES DDL BLOQUES DONNENT DES VALEURS NULLES POUR BETA.
!
!   --------------------------------------------------------------------
!     PARAMETRES D'APPELS:
! (A/B SIGNIFIE SD DE TYPE A SI K EST REELLE, TYPE B SI K EST COMPLEXE
!  AVEC A ET B SOIT R, SOIT C)
!
! IN  TYPEQZ : K  : TYPE DE METHODE : QR, QZ_SIMPLE OU QZ_EQUI.
! IN  QRN    : IS : DIMENSION DU SYSTEME
! IN  IQRN   : IS : ADRESSE JEVEUX DE LA MATRICE A PLEINE (R/C)
! IN  LQRN   : IS : ADRESSE JEVEUX DE LA MATRICE B PLEINE (R/C)
! IN  QRAR   : IS : ADRESSE JEVEUX DE RE(ALPHA) (SI K E R)
!                   OU ALPHA E C (SI K E C)
! IN  QRAI   : IS : ADRESSE JEVEUX DE IM(ALPHA)
!                   SI K E R, SINON VIDE
! IN  QRBA   : IS : ADRESSE JEVEUX DE BETA (R/C)
! IN  QRVL   : IS : ADRESSE JEVEUX D'UN VECTEUR AUX POUR LAPACK (R/C)
! OUT LVEC   : IS : ADRESSE JEVEUX  MATRICE DES VECTEURS PROPRES (R/C)
! IN  KQRN   : IS : ADRESSE JEVEUX D'UN VECTEUR AUX POUR LAPACK (R/C)
! OUT LVALPR : IS : ADRESSE JEVEUX DU VECTEUR DES VALEURS PROPRES (R/C)
!                   EN QUADRATIQUE, ELEMENT DE C
! OUT NCONV  : IS : NOMBRE DE MODES CONVERGES RETENUS APRES TRI
! IN  OMECOR : R8 : SEUIL DE MODE RIGIDE
! IN  KTYP   : K1 : TYPE DE LA MATRICE DE RAIDEUR
! IN  KQRNR  : IS : ADRESSE JEVEUX VECTEUR AUX LAPACK SI K E C (R)
! IN  NEQACT : IS : NOMBRE DE DDL ACTIFS
! IN  ILSCAL/IRSCAL : IS : ADRESSE JEVEUX VECTEURS AUX POUR QZ_EQUI
! IN  OPTIOF : K16: OPTION DEMANDEE (BANDE, PLUS_PETITE,CENTRE,TOUT)
! IN  OMEMIN/OMEMAX: R8 : FREQS MIN ET MAX DE LA BANDE RECHERCHEE
! IN  OMESHI : R8 : VALEUR  RETENUE DU SHIFT PAR VPFOPR EN GENE REEL
! IN  DDLEXC : IS : DDLEXC(1..QRN) VECTEUR POSITION DES DDLS BLOQUES.
! IN  NFREQ  : IS : NBRE DE MODES DEMANDES SI OPTIOF=CENTRE OU
!                   PLUS_PETITE
! IN  LMASSE : IS : DESCRIPTEUR DE LA MATRICE DE MASSE M (R)
! IN  LRAIDE : IS : DESCRIPTEUR DE LA MATRICE DE RAIDEUR K (R/C)
! IN  LAMOR  : IS : DESCRIPTEUR DE LA MATRICE DE D'AMORTISSEMENT ET/OU
!                   D'EFFET GYROSCOPIQUE (R)
! IN  NUMEDD : K19: NOM DU NUME_DDL
! IN  SIGMA  : C16: VALEUR DU SHIFT EN GENE COMPLEXE ET QUADRATIQUE
! IN ICSCAL/IVSCAL: IS :ADRESSE JEVEUX VECTEURS AUX POUR QZ_EQUI
! IN IISCAL: IS : ADR. JEVEUX VECTEURS AUX POUR QZ_EQUI
! INOUT bwork: L4 : vecteur de travail de type logical*4
!
! OUT FLAGE  : LO : FLAG PERMETTANT DE GERER LES IMPRESSIONS
!-----------------------------------------------------------------------
! aslint: disable=W1304,W1504,C9991
! C9991 : a cause du compilo gfortran 4.4.5 qui se plante

    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterc/matfpe.h"
#include "asterc/r8depi.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/freqom.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mcmult.h"
#include "asterfort/mrmult.h"
#include "asterfort/utmess.h"
#include "asterfort/vpgsmm.h"
#include "asterfort/vpordc.h"
#include "asterfort/vpordo.h"
#include "asterfort/wkvect.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dggev.h"
#include "blas/dggevx.h"
#include "blas/dnrm2.h"
#include "blas/dsygv.h"
#include "blas/dznrm2.h"
#include "blas/zaxpy.h"
#include "blas/zcopy.h"
#include "blas/zggev.h"
#include "blas/zggevx.h"
!
    integer :: qrn, iqrn, lqrn, qrar, qrai, icscal, ivscal, iiscal, qrba
    integer :: qrvl, lvec, kqrn, lvalpr, nconv, kqrnr, neqact, ilscal, irscal
    integer :: ddlexc(*), nfreq, lmasse, lraide, lamor
    character(len=1) :: ktyp, kmsg
    character(len=16) :: typeqz, optiof
    character(len=19) :: numedd
    real(kind=8) :: omecor, omemin, omemax, omeshi
    complex(kind=8) :: sigma
    logical :: flage
    logical(kind=4) :: bwork(*)
!
!-----------------------------------------------------------------------
! DECLARATION VARIABLES LOCALES
!
    integer :: i, j, decal, ideb, ifin, qrlwo, qrlwor, kqrn2, iauxh, vali(5), ifm, niv, iret
    integer :: ivalr
    integer :: ivalm,  ihcol, ivp1, ivp2, ivala, j2, iauxh2, qrns2, lvec2, lvec3, lvec4
    integer :: imult, typlin, iaux1, iaux2, iaux3, ivala1, ivalr1, ivalm1, lvecn, jm1, iauxh1, im1
    integer :: j2m1, iaux21, ics1
    integer :: ldvl
    integer(kind=4) :: qrinfo, ilo, ihi
    real(kind=8) :: abnrm, bbnrm, baux, rauxi, aaux, valr(4), raux, anorm, bnorm, prec2, vpinf
    real(kind=8) :: prec, vpmax, vpcour, alpha, prec3, run, rzero, rauxr, rauxm, cnorm, caux
    real(kind=8) :: coefn, anorm1, bnorm1, f1, f2, fr, anorm2, anorm3, depi, aaux1, baux1, caux1
    real(kind=8) :: abnorm, prec1
    complex(kind=8) :: cun, czero, cauxm, cauxr, cauxa, cauxm2, freq, freq2, cauxa1, cauxm1
    complex(kind=8) :: cauxr1
    character(len=1) :: kbal, ksens, valk
    character(len=24) :: nomrai, nommas, nomamo
    logical :: lkr, ltest, lc, ldebug, lnsa, lnsr, lnsm, lqze
    integer, pointer :: smdi(:) => null()
!
!-----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
    call matfpe(-1)
!
!------
!------
! INITS
!------
!------
!     PRECISIONS MACHINE
    prec=r8prem()*2.d0
    prec1=r8miem()*10.d0
!     PARAMETRES (EMPIRIQUES !) POUR SELECTIONNER LES VPS AVEC QZ_QR
    prec2=r8maem()*0.5d0
    prec3=-2.d0*omecor
!     PARAMETRE DE REORTHO DE ISGM
    alpha = 0.717d0
    depi=r8depi()
! ---- PARAMETRES POUR LAPACK
    qrlwo = -1
! ---- ON CHERCHE LES VECTEURS PROPRES A DROITE
    ldvl = 1
! ---- METTRE LTEST=.TRUE. SI ON VEUX FAIRE DES TESTS UNITAIRES SUR LES
! ---- SOLVEURS LAPACK.
! ---- IDEM AVEC LDEBUG POUR DIAGNOSTIQUER UN BUG
! ---- IDEM POUR VERIFIER LES MODES (EN QUADRATIQUE)
    ltest=.false.
!      LTEST=.TRUE.
    ldebug=.false.
!      LDEBUG=.TRUE.
    if (typeqz(1:7) .eq. 'QZ_EQUI') then
        lqze=.true.
    else
        lqze=.false.
    endif
    if (lamor .ne. 0) then
! ---- QRN DOIT ETRE PAIRE EN QUADRATIQUE
        qrns2=qrn/2
        lc=.true.
        if ((qrns2*2) .ne. qrn) ASSERT(.false.)
        imult=2
    else
        lc=.false.
        imult=1
    endif
    valk=' '
    cun=dcmplx(1.d0,0.d0)
    czero=dcmplx(0.d0,0.d0)
    run=1.d0
    rzero=0.d0
    if (ktyp .eq. 'R') then
        lkr=.true.
    else if (ktyp.eq.'C') then
        lkr=.false.
    else
! ---- OPTION ILLICITE
        ASSERT(.false.)
    endif
! ---- MATRICES NON SYMETRIQUES ?
    lnsa=.false.
    lnsr=.false.
    lnsm=.false.
    if (lc) then
        if (zi(lamor+4) .eq. 0) lnsa=.true.
    endif
    if (zi(lraide+4) .eq. 0) lnsr=.true.
    if (zi(lmasse+4) .eq. 0) lnsm=.true.
!
!--------------------------------------------------
!--------------------------------------------------
! CONVERSION DES MATRICES MORSE EN MATRICES PLEINES
!--------------------------------------------------
!--------------------------------------------------
!
! ---- MATRICES K ET M REELLES SYMETRIQUES
    nomrai=zk24(zi(lraide+1))
    nommas=zk24(zi(lmasse+1))
    call jeveuo(jexnum(nomrai(1:19)//'.VALM', 1), 'L', ivalr)
    if (lnsr) call jeveuo(jexnum(nomrai(1:19)//'.VALM', 2), 'L', ivalr1)
    call jeveuo(jexnum(nommas(1:19)//'.VALM', 1), 'L', ivalm)
    if (lnsm) call jeveuo(jexnum(nommas(1:19)//'.VALM', 2), 'L', ivalm1)
    if (lc) then
        nomamo=zk24(zi(lamor+1))
        call jeveuo(jexnum(nomamo(1:19)//'.VALM', 1), 'L', ivala)
        if (lnsa) call jeveuo(jexnum(nomamo(1:19)//'.VALM', 2), 'L', ivala1)
    endif
    call jeveuo(numedd(1:14)//'.SMOS.SMHC', 'L', ihcol)
    call jeveuo(numedd(1:14)//'.SMOS.SMDI', 'L', vi=smdi)
!
! ---- PB MODAL GENERALISE
    if (.not.lc) then
! ---- MATRICES K ET M REELLES SYMETRIQUES
        if ((lkr) .and. (.not.lnsr) .and. (.not.lnsm)) then
            ideb = 1
            do 31 j = 1, qrn
                jm1=j-1
                ifin = smdi(jm1+1)
                do 30 i = ideb, ifin
                    im1=i-1
                    iauxh=zi4(ihcol+im1)
                    iauxh1=iauxh-1
                    rauxr=zr(ivalr+im1)
                    rauxm=zr(ivalm+im1)
!
! ------ MATRICE A ET B TRIANGULAIRE SUP
                    zr(iqrn+jm1*qrn+iauxh1) = rauxr
                    zr(lqrn+jm1*qrn+iauxh1) = rauxm
! ------ MATRICE A ET B TRIANGULAIRE INF
                    zr(iqrn+qrn*iauxh1+jm1) = rauxr
                    zr(lqrn+qrn*iauxh1+jm1) = rauxm
30              continue
                ideb = ifin+1
31          continue
        else
! ---- MATRICES K COMPLEXE ET M REELLE OU K/M NON SYMETRIQUES
            ideb = 1
            do 33 j = 1, qrn
                jm1=j-1
                ifin = smdi(j)
                do 32 i = ideb, ifin
                    im1=i-1
                    iauxh=zi4(ihcol+im1)
                    iauxh1=iauxh-1
! ------ MATRICE A ET B TRIANGULAIRE SUP
                    if (lkr) then
                        cauxr=zr(ivalr+im1)*cun
                    else
                        cauxr=zc(ivalr+im1)
                    endif
                    cauxm=zr(ivalm+im1)*cun
! ------ MATRICE A ET B TRIANGULAIRE INF
                    if (lnsr .and. (iauxh.ne.j)) then
                        if (lkr) then
                            cauxr1=zr(ivalr1+im1)*cun
                        else
                            cauxr1=zc(ivalr1+im1)
                        endif
                    else
                        cauxr1=cauxr
                    endif
                    if (lnsm .and. (iauxh.ne.j)) then
                        cauxm1=zr(ivalm1+im1)*cun
                    else
                        cauxm1=cauxm
                    endif
                    zc(iqrn+jm1*qrn+iauxh1) = cauxr
                    zc(lqrn+jm1*qrn+iauxh1) = cauxm
                    zc(iqrn+qrn*iauxh1+jm1) = cauxr1
                    zc(lqrn+qrn*iauxh1+jm1) = cauxm1
32              continue
                ideb = ifin+1
33          continue
        endif
    else
! ---- PB MODAL QUADRATIQUE
!
! ---- ESTIMATION PREALABLE DE NORME L1 (ET LINFINI SI SYM) DE K, M ET C
        call wkvect('&&VPQZLA.NORME', 'V V R', 3*qrns2, lvecn)
        call jerazo('&&VPQZLA.NORME', 3*qrns2, 1)
        ideb = 1
! ---   J: NUMERO DE COLONNE, IAUXH: DE LIGNE
        do 39 j = 1, qrns2
            ifin = smdi(j)
            jm1=j-1
            do 38 i = ideb, ifin
                im1=i-1
                iauxh=zi4(ihcol+im1)
                iauxh1=iauxh-1
! ---       PARTIE TRIANGULAIRE SUP
                if (lkr) then
                    aaux=abs(zr(ivalr+im1))
                else
                    aaux=abs(zc(ivalr+im1))
                endif
                baux=abs(zr(ivalm+im1))
                caux=abs(zr(ivala+im1))
! ---       PARTIE TRIANGULAIRE INF
                if (iauxh .ne. j) then
                    if (lnsr) then
                        if (lkr) then
                            aaux1=abs(zr(ivalr1+im1))
                        else
                            aaux1=abs(zc(ivalr1+im1))
                        endif
                    else
                        aaux1=aaux
                    endif
                    if (lnsm) then
                        baux1=abs(zr(ivalm1+im1))
                    else
                        baux1=baux
                    endif
                    if (lnsa) then
                        caux1=abs(zr(ivala1+im1))
                    else
                        caux1=caux
                    endif
                endif
                zr(lvecn+jm1) =zr(lvecn+jm1) +aaux
                zr(lvecn+jm1+qrns2) =zr(lvecn+jm1+qrns2)+baux
                zr(lvecn+jm1+qrn) =zr(lvecn+jm1+qrn) +caux
                if (iauxh .ne. j) then
                    zr(lvecn+iauxh1) =zr(lvecn+iauxh1) +aaux1
                    zr(lvecn+iauxh1+qrns2)=zr(lvecn+iauxh1+qrns2)+&
                    baux1
                    zr(lvecn+iauxh1+qrn) =zr(lvecn+iauxh1+qrn)&
                    +caux1
                endif
38          continue
            ideb = ifin+1
39      continue
        anorm=0.d0
        bnorm=0.d0
        cnorm=0.d0
        do 40 j = 1, qrns2
            jm1=j-1
            anorm=max(anorm,zr(lvecn+jm1))
            bnorm=max(bnorm,zr(lvecn+jm1+qrns2))
            cnorm=max(cnorm,zr(lvecn+jm1+qrn))
40      continue
        call jedetr('&&VPQZLA.NORME')
! ---- ERREUR DONNEES OU CALCUL
!        IF (ANORM*BNORM*CNORM.EQ.0.D0) ASSERT(.FALSE.)
! ---- COEF MULTIPLICATEUR (EQUILIBRAGE) POUR LA LINEARISATION PB QUAD
        coefn=(anorm+bnorm+cnorm)/(3*qrns2)
!
        if (niv .ge. 2) then
            valr(1)=anorm
            valr(2)=bnorm
            valr(3)=cnorm
            valr(4)=coefn
            call utmess('I', 'ALGELINE6_25', nr=4, valr=valr)
        endif
! ---- ON PASSE EN COMPLEXE MEME SI K EST REELLE, POUR PLUS DE
!      ROBUSTESSE. ON PROPOSE DEUX TYPES DE LINEARISATION. ON PREND LA
!      DEUXIEME, PLUS PROCHE DE CELLE DE TRI_DIAG/SORENSEN
        typlin=2
        ideb = 1
!       J NUMERO DE COLONNE
        do 37 j = 1, qrns2
            jm1=j-1
            j2=j+qrns2
            j2m1=j2-1
            ifin = smdi(jm1+1)
            do 36 i = ideb, ifin
                im1=i-1
!           IAUXH NUMERO DE LIGNE
                iauxh=zi4(ihcol+im1)
                iauxh1=iauxh-1
                iauxh2=iauxh+qrns2
                iaux21=iauxh2-1
!
! --- MATRICE A ET B TRIANGULAIRE SUP
                if (lkr) then
                    cauxr=zr(ivalr+im1)*cun
                else
                    cauxr=zc(ivalr+im1)
                endif
                cauxm=zr(ivalm+im1)*cun
                cauxa=zr(ivala+im1)*cun
                cauxm2=coefn*cun
!
! ------ MATRICE A ET B TRIANGULAIRE INF
                if (lnsr .and. (iauxh.ne.j)) then
                    if (lkr) then
                        cauxr1=zr(ivalr1+im1)*cun
                    else
                        cauxr1=zc(ivalr1+im1)
                    endif
                else
                    cauxr1=cauxr
                endif
                if (lnsm .and. (iauxh.ne.j)) then
                    cauxm1=zr(ivalm1+im1)*cun
                else
                    cauxm1=cauxm
                endif
                if (lnsa .and. (iauxh.ne.j)) then
                    cauxa1=zr(ivala1+im1)*cun
                else
                    cauxa1=cauxa
                endif
!
                if (typlin .eq. 2) then
! MATRICE COMPAGNON A
                    zc(iqrn+jm1*qrn+iauxh1) = -cauxr
                    zc(iqrn+qrn*iauxh1+jm1) = -cauxr1
                    if (j .eq. iauxh) zc(iqrn+j2m1*qrn+iaux21) = cauxm2
!
! MATRICE COMPAGNON B
                    zc(lqrn+jm1*qrn+iauxh1) = cauxa
                    zc(lqrn+qrn*iauxh1+jm1) = cauxa1
                    zc(lqrn+j2m1*qrn+iauxh1) = cauxm
                    zc(lqrn+qrn*iaux21+jm1) = cauxm1
                    if (j .eq. iauxh) zc(lqrn+jm1*qrn+iaux21) = cauxm2
!
!
                else if (typlin.eq.1) then
! MATRICE COMPAGNON A
                    zc(iqrn+jm1*qrn+iaux21) = -cauxr
                    zc(iqrn+qrn*iauxh1+j2m1) = -cauxr1
                    zc(iqrn+j2m1*qrn+iauxh1) = cauxm2
                    zc(iqrn+qrn*iaux21+jm1) = cauxm2
                    zc(iqrn+j2m1*qrn+iaux21) = -cauxa
                    zc(iqrn+qrn*iaux21+j2m1) = -cauxa1
! MATRICE COMPAGNON B
                    zc(lqrn+jm1*qrn+iauxh1) = cauxm2
                    zc(lqrn+qrn*iauxh1+jm1) = cauxm2
                    zc(lqrn+j2m1*qrn+iaux21) = cauxm
                    zc(lqrn+qrn*iaux21+j2m1) = cauxm1
                else
                    ASSERT(.false.)
                endif
36          continue
            ideb = ifin+1
37      continue
    endif
!
! ---- TESTS UNITAIRES SI LTEST=.TRUE.
    if (ltest) then
        if (lkr .and. (.not.lc) .and. (.not.lnsm) .and. (.not.lnsr)) then
            do 61 i = 1, qrn
                do 60 j = 1, qrn
                    zr(iqrn-1+(j-1)*qrn+i)=rzero
                    zr(lqrn-1+(j-1)*qrn+i)=rzero
60              continue
61          continue
            do 62 i = 1, qrn
                zr(iqrn-1+(i-1)*qrn+i)=i*run
                zr(lqrn-1+(i-1)*qrn+i)=run
62          continue
        else
            do 64 i = 1, qrn
                do 63 j = 1, qrn
                    zc(iqrn-1+(j-1)*qrn+i)=czero
                    zc(lqrn-1+(j-1)*qrn+i)=czero
63              continue
64          continue
            do 65 i = 1, qrn
                zc(iqrn-1+(i-1)*qrn+i)=i*cun
                zc(lqrn-1+(i-1)*qrn+i)=cun
65          continue
        endif
    endif
!
! ---- FILTRAGE DES TERMES TROP PETITS POUR EVITER L'APPARITION
! ---- DE MODES FANTOMES DS LAPACK DUS AUX ERREURS D'ARRONDIS
!      IF ((LKR).AND.(.NOT.LC).AND.(.NOT.LNSR).AND.(.NOT.LNSM)) THEN
!        DO 77 J=1,QRN
!         JM1=J-1
!          DO 76 I=1,QRN
!           IM1=I-1
!           AAUX=ZR(IQRN+JM1*QRN+IM1)
!           BAUX=ZR(LQRN+JM1*QRN+IM1)
!           IF (ABS(ABS(AAUX).LT.PREC)) ZR(IQRN+JM1*QRN+IM1)=RZERO
!           IF (ABS(ABS(BAUX).LT.PREC)) ZR(LQRN+JM1*QRN+IM1)=RZERO
!   76     CONTINUE
! C  77   CONTINUE
!      ELSE
!        DO 79 J=1,QRN
!         JM1=J-1
!          DO 78 I=1,QRN
!           IM1=I-1
!           CAUXA=ZC(IQRN+JM1*QRN+IM1)
!           CAUXA1=ZC(LQRN+JM1*QRN+IM1)
!           F1=DBLE(CAUXA)
!           F2=DIMAG(CAUXA)
!           IF (ABS(F1).LT.PREC) F1=RZERO
!           IF (ABS(F2).LT.PREC) F2=RZERO
!           ZC(IQRN+JM1*QRN+IM1)=DCMPLX(F1,F2)
!           F1=DBLE(CAUXA1)
!           F2=DIMAG(CAUXA1)
!           IF (ABS(F1).LT.PREC) F1=RZERO
!           IF (ABS(F2).LT.PREC) F2=RZERO
!           ZC(LQRN+JM1*QRN+IM1)=DCMPLX(F1,F2)
!   78     CONTINUE
!   79   CONTINUE
!      ENDIF
!
! ---- CALCUL DE LA NORME INFINIE DE A ET B
    anorm=0.d0
    bnorm=0.d0
    do 44 i = 1, qrn
        im1=i-1
        aaux=0.d0
        baux=0.d0
        if ((lkr) .and. (.not.lc) .and. (.not.lnsr) .and. (.not.lnsm)) then
            do 41 j = 1, qrn
                jm1=j-1
                aaux=aaux+abs(zr(iqrn+jm1*qrn+im1))
                baux=baux+abs(zr(lqrn+jm1*qrn+im1))
41          continue
        else
            do 42 j = 1, qrn
                jm1=j-1
                aaux=aaux+abs(zc(iqrn+jm1*qrn+im1))
                baux=baux+abs(zc(lqrn+jm1*qrn+im1))
42          continue
        endif
        anorm=max(anorm,aaux)
        bnorm=max(bnorm,baux)
44  end do
! ---- CALCUL DE LA NORME L1 DE A ET B
    anorm1=0.d0
    bnorm1=0.d0
    do 440 j = 1, qrn
        jm1=j-1
        aaux=0.d0
        baux=0.d0
        if ((lkr) .and. (.not.lc) .and. (.not.lnsr) .and. (.not.lnsm)) then
            do 410 i = 1, qrn
                im1=i-1
                aaux=aaux+abs(zr(iqrn+jm1*qrn+im1))
                baux=baux+abs(zr(lqrn+jm1*qrn+im1))
410          continue
        else
            do 420 i = 1, qrn
                im1=i-1
                aaux=aaux+abs(zc(iqrn+jm1*qrn+im1))
                baux=baux+abs(zc(lqrn+jm1*qrn+im1))
420          continue
        endif
        anorm1=max(anorm1,aaux)
        bnorm1=max(bnorm1,baux)
440  end do
! ---- ERREUR DONNEES OU CALCUL
    if (anorm*bnorm*anorm1*bnorm1 .eq. 0.d0) ASSERT(.false.)
    if (niv .ge. 2) then
        valr(1)=anorm
        valr(2)=bnorm
        valr(3)=anorm1
        valr(4)=bnorm1
        call utmess('I', 'ALGELINE6_26', nr=4, valr=valr)
    endif
!
! --- POUR SORTIE FICHIER FORT.17
!      WRITE(17,*)QRN
!      WRITE(17,*)ANORM,BNORM
!      DO J=1,QRN
!        DO I=1,QRN
!         CAUXR=ZC(IQRN+(J-1)*QRN+I-1)
!         CAUXM=ZC(LQRN+(J-1)*QRN+I-1)
!         WRITE(17,'(I4,1X,I4,1X,E17.8,1X,E17.8)')
!     &     I,J,DBLE(CAUXR),DBLE(CAUXM)
!       ENDDO
!      ENDDO
!-------------------------------------------------------------------
!-------------------------------------------------------------------
! RESOLUTION LAPACK PROPREMENT DITE, EN 2 PASSES
! 1ERE PASSE: POUR ESTIMER L'ESPACE DE TRAVAIL OPTIMAL EN TEMPS
! 2ND PASSE : RESOLUTION VALEURS PROPRES ET VECTEURS PROPRES A DROITE
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!
! ---- ADRESSE VECTEURS PROPRES
    if (lc) then
        call wkvect('&&VPQZLA.VP2', 'V V C', qrn*qrn, lvec2)
        lvec3=lvec2
    else
        lvec3=lvec
    endif
    qrinfo=-999
! ---- QZ EXPERT (EQUILIBRAGE)
    if (lqze) then
! ON NE CALCULE L'ERREUR QUE SUR LES
! VALEURS PROPRES, CAR C'EST TROP COUTEUX EN MEMOIRE POUR LES VECTEURS
! PROPRES ET LES RESULTATS SONT SOUVENT INEXPLOITABLES (GROSSES VALEURS)
        kbal='B'
        ksens='E'
        call wkvect('&&VPQZLA.QRRCONDE', 'V V R', qrn, ics1)
        if ((lkr) .and. (.not.lc) .and. (.not.lnsr) .and. (.not.lnsm)) then
! RECHERCHE DE LA TAILLE OPTIMALE POUR L'ESPACE DE TRAVAIL
            call dggevx(kbal, 'N', 'V', ksens, qrn,&
                        zr(iqrn), qrn, zr( lqrn), qrn, zr(qrar),&
                        zr(qrai), zr(qrba), zr(qrvl), ldvl, zr( lvec3),&
                        qrn, ilo, ihi, zr(ilscal), zr(irscal),&
                        abnrm, bbnrm, zr(icscal), zr(ivscal), zr(kqrn),&
                        qrlwo, zi4(iiscal), bwork, qrinfo)
! CREATION DU VECTEUR DE TRAVAIL OPTIMALE, DESTRUCTION DU PRECEDENT
! ET RESOLUTION
            if (qrinfo .eq. 0) then
                qrlwo = int(zr(kqrn))
                qrlwor = int(zr(kqrn))
! PATCH POUR MKL INTEL 11.1 : MKL DIT 10*N, NETLIB DIT 12*N
                if (qrlwo .lt. (12*qrn)) then
                    qrlwo = int(12*qrn)
                    qrlwor = 12*qrn
                endif
! FIN PATCH
                call jedetr('&&VPQZLA.QR.WORK')
                call wkvect('&&VPQZLA.QR.WORK', 'V V R', qrlwor, kqrn2)
                call dggevx(kbal, 'N', 'V', ksens, qrn,&
                            zr(iqrn), qrn, zr( lqrn), qrn, zr(qrar),&
                            zr(qrai), zr(qrba), zr(qrvl), ldvl, zr(lvec3),&
                            qrn, ilo, ihi, zr(ilscal), zr(irscal),&
                            abnrm, bbnrm, zr(icscal), zr(ivscal), zr(kqrn2),&
                            qrlwo, zi4( iiscal), bwork, qrinfo)
            endif
        else
            call zggevx(kbal, 'N', 'V', ksens, qrn,&
                        zc(iqrn), qrn, zc( lqrn), qrn, zc(qrar),&
                        zc(qrba), zc(qrvl), ldvl, zc(lvec3), qrn,&
                        ilo, ihi, zr(ilscal), zr(irscal), abnrm,&
                        bbnrm, zr( icscal), zr(ivscal), zc(kqrn), qrlwo,&
                        zr(kqrnr), zi4(iiscal), bwork, qrinfo)
            if (qrinfo .eq. 0) then
                qrlwo = int(dble(zc(kqrn)))
                qrlwor = int(dble(zc(kqrn)))
! PATCH POUR MKL INTEL 11.1 : MKL DIT 4*N, NETLIB DIT 2*N
                if (qrlwo .lt. (4*qrn)) then
                    qrlwo = int(4*qrn)
                    qrlwor = 4*qrn
                endif
! FIN PATCH
                call jedetr('&&VPQZLA.QR.WORK')
                call wkvect('&&VPQZLA.QR.WORK', 'V V C', qrlwor, kqrn2)
                call zggevx(kbal, 'N', 'V', ksens, qrn,&
                            zc(iqrn), qrn, zc( lqrn), qrn, zc(qrar),&
                            zc(qrba), zc(qrvl), ldvl, zc(lvec3), qrn,&
                            ilo, ihi, zr(ilscal), zr(irscal), abnrm,&
                            bbnrm, zr( icscal), zr(ivscal), zc(kqrn2), qrlwo,&
                            zr(kqrnr), zi4( iiscal), bwork, qrinfo)
            endif
        endif
! --- SI TOUT VA BIEN ON CALCUL UN MAJORANT DE L'ERREUR SUR LES
!     VALEURS PROPRES ET LES ANGLES DE VECTEURS PROPRES
        if (qrinfo .eq. 0) then
            abnorm=sqrt(abnrm*abnrm+bbnrm*bbnrm)
            do 70 i = 1, qrn
                im1=i-1
                raux=zr(icscal+im1)
                if (abs(raux) .lt. prec1) ASSERT(.false.)
                zr(icscal+im1)=prec*abnorm/raux
70          continue
        endif
!
! ----  QZ SIMPLE
    else if (typeqz(1:9).eq.'QZ_SIMPLE') then
        if ((lkr) .and. (.not.lc) .and. (.not.lnsr) .and. (.not.lnsm)) then
            call dggev('N', 'V', qrn, zr(iqrn), qrn,&
                       zr(lqrn), qrn, zr( qrar), zr(qrai), zr(qrba),&
                       zr(qrvl), ldvl, zr(lvec3), qrn, zr(kqrn),&
                       qrlwo, qrinfo)
            if (qrinfo .eq. 0) then
                qrlwo = int(zr(kqrn))
                qrlwor = int(zr(kqrn))
                call jedetr('&&VPQZLA.QR.WORK')
                call wkvect('&&VPQZLA.QR.WORK', 'V V R', qrlwor, kqrn2)
                call dggev('N', 'V', qrn, zr(iqrn), qrn,&
                           zr(lqrn), qrn, zr(qrar), zr(qrai), zr(qrba),&
                           zr(qrvl), ldvl, zr(lvec3), qrn, zr(kqrn2),&
                           qrlwo, qrinfo)
            endif
        else
            call zggev('N', 'V', qrn, zc(iqrn), qrn,&
                       zc(lqrn), qrn, zc( qrar), zc(qrba), zc(qrvl),&
                       ldvl, zc(lvec3), qrn, zc(kqrn), qrlwo,&
                       zr(kqrnr), qrinfo)
            if (qrinfo .eq. 0) then
                qrlwo = int(dble(zc(kqrn)))
                qrlwor = int(dble(zc(kqrn)))
                call jedetr('&&VPQZLA.QR.WORK')
                call wkvect('&&VPQZLA.QR.WORK', 'V V C', qrlwor, kqrn2)
                call zggev('N', 'V', qrn, zc(iqrn), qrn,&
                           zc(lqrn), qrn, zc(qrar), zc(qrba), zc(qrvl),&
                           ldvl, zc(lvec3), qrn, zc(kqrn2), qrlwo,&
                           zr(kqrnr), qrinfo)
            endif
        endif
!
! ----  QR
    else if (typeqz(1:5).eq.'QZ_QR') then
! ---- CONFIGURATION ILLICITE
        if (lc .or. lnsm .or. lnsr .or. (.not.lkr)) ASSERT(.false.)
        call dsygv(1, 'V', 'U', qrn, zr(iqrn),&
                   qrn, zr(lqrn), qrn, zr(lvalpr), zr(kqrn),&
                   qrlwo, qrinfo)
        if (qrinfo .eq. 0) then
            qrlwo = int(zr(kqrn))
            qrlwor = int(zr(kqrn))
            call jedetr('&&VPQZLA.QR.WORK')
            call wkvect('&&VPQZLA.QR.WORK', 'V V R', qrlwor, kqrn2)
            call dsygv(1, 'V', 'U', qrn, zr(iqrn),&
                       qrn, zr(lqrn), qrn, zr(lvalpr), zr(kqrn2),&
                       qrlwo, qrinfo)
        endif
    else
! ---- OPTION INVALIDE
        ASSERT(.false.)
    endif
!
!-------------------------------------
! ------------------------------------
! TRAITEMENT  DES ERREURS DANS LAPACK
! ------------------------------------
!-------------------------------------
    vali(1)=qrinfo
    if (vali(1) .ne. 0) then
        call utmess('F', 'ALGELINE5_68', si=vali(1))
    endif
    call jeexin('&&VPQZLA.QR.WORK', iret)
    if (iret .ne. 0) call jedetr('&&VPQZLA.QR.WORK')
!
! POUR DEBUG
    if ((ldebug) .or. (niv.ge.2)) then
        write(ifm,*)'******** DONNEES BRUTES SORTANT DE LAPACK ********'
        if (lqze) write(ifm,*)'NORME L1 DE A/B (LAPACK) ',abnrm,bbnrm
        write(ifm,*)'LKR/LC/LNSR/LNSM/LNSA/QRN/NFREQ ',lkr,lc,lnsr,&
        lnsm, lnsa,qrn,nfreq
        write(ifm,*)
        do 900 i = 1, qrn
            im1=i-1
            if ((lkr) .and. (.not.lc) .and. (.not.lnsr) .and. (.not.lnsm)) then
                fr=zr(qrba+im1)
                if (abs(fr) .gt. prec) then
                    f1=zr(qrar+im1)/fr
                    f2=zr(qrai+im1)/fr
                else
                    f1=1.d+70
                    f2=1.d+70
                endif
                f1=freqom(f1)
                f2=freqom(f2)
                if (i .eq. 1) write(ifm,*)'I / (ALPHAR,ALPHAI) / BETA / (FREQR,FREQI)'
                if (lqze) then
                    call utmess('I', 'ALGELINE7_13', sr=zr(icscal+ im1))
                endif
                write(ifm,910)i,zr(qrar+im1),zr(qrai+im1),zr(qrba+im1)&
                ,f1,f2
            else
                freq=zc(qrba+im1)
                if (abs(freq) .gt. prec) then
                    freq=zc(qrar+im1)/freq
                else
                    freq=1.d+70
                endif
                if (i .eq. 1) then
                    if (lc) then
                        write(ifm,*)'I / (ALPHAR,ALPHAI) / (BETAR,BETAI) / '//&
     &                      '(LAMBDAI/2*PI, -LAMBDAR/ABS(LAMBDA))'
                    else
                        write(ifm,*)'I / (ALPHAR,ALPHAI) / (BETAR,BETAI) / '//&
     &                      '(FREQR, FREQI/(2*FREQR))'
                    endif
                endif
                if (lqze) then
                    call utmess('I', 'ALGELINE7_13', sr=zr(icscal+ im1))
                endif
                if (lc) then
                    write(ifm,912)i,dble(zc(qrar+im1)),dimag(zc(qrar+&
                    im1)), dble(zc(qrba+im1)),dimag(zc(qrba+im1)),&
                    dimag(freq)/depi,-dble(freq)/abs(freq)
                else
                    write(ifm,912)i,dble(zc(qrar+im1)),dimag(zc(qrar+&
                    im1)), dble(zc(qrba+im1)),dimag(zc(qrba+im1)),&
                    freqom(dble(freq)), dimag(freq)/(2.d0*dble(freq))
                endif
            endif
900      continue
        910   format(i4,1x,e12.5,e12.5,e12.5,1x,e12.5,e12.5)
        912   format(i4,1x,e12.5,e12.5,1x,e12.5,e12.5,1x,e12.5,e12.5)
    endif
! FIN DEBUG
!
!-----------------------------------
! ----------------------------------
! POST-TRAITEMENTS ET VERIFICATIONS
! ----------------------------------
!-----------------------------------
!
    decal = 0
! ---- SI SYSTEME NON SYM REEL, TRAITEMENT DES PARTIES IMAGINAIRES
    if ((typeqz(1:5).ne.'QZ_QR') .and. (lkr) .and. (.not.lc) .and. (.not.lnsm) .and.&
        (.not.lnsr)) then
        do 50 i = 1, qrn
            im1=i-1
            raux=abs(zr(qrai+im1))
            if (raux .gt. omecor) then
                vali(1)=i
                valr(1)=zr(qrar+im1)
                valr(2)=zr(qrai+im1)
                kmsg='A'
                call utmess(kmsg, 'ALGELINE5_51', si=vali(1), nr=2, valr=valr)
            endif
            if ((raux.ne.0.d0) .and. (niv.ge.2)) then
                vali(1)=i
                valr(1)=zr(qrar+im1)
                valr(2)=zr(qrai+im1)
                kmsg='I'
                call utmess(kmsg, 'ALGELINE5_51', si=vali(1), nr=2, valr=valr)
            endif
50      continue
    endif
!
!---------------------------------------------------------
! ----  ON TESTE LES MODES VALIDES
! ----  1/ ADEQUATION /ALPHA/, /BETA/ VS //A// ET //B//
! ----  2/ ADEQUATION /BETA/ PROCHE DE ZERO ET DDL BLOQUE
!---------------------------------------------------------
    if ((typeqz(1:5).ne.'QZ_QR') .and. (lkr) .and. (.not.lc) .and. (.not.lnsm) .and.&
        (.not.lnsr)) then
! ---- GENERALISE REEL SYM MAIS PAS SPD
        do 55 i = 1, qrn
            im1=i-1
            if (abs(zr(qrba+im1)) .gt. prec) then
                raux=sqrt(zr(qrar+im1)**2+zr(qrai+im1)**2)
                rauxi=abs(zr(qrba+im1))
                if ((raux.gt.anorm) .or. (rauxi.gt.bnorm)) then
                    vali(1)=i
                    valr(1)=raux
                    valr(2)=anorm
                    valr(3)=rauxi
                    valr(4)=bnorm
                    kmsg='A'
                    call utmess(kmsg, 'ALGELINE5_61', si=vali(1), nr=4, valr=valr)
                endif
                zr(lvalpr+im1-decal) = zr(qrar+im1)/zr(qrba+im1)
                call dcopy(qrn, zr(lvec3+im1*qrn), 1, zr(lvec+(im1- decal)*qrn), 1)
                if (lqze) zr(ics1+im1-decal)=zr(icscal+im1)
            else
                decal = decal+1
                if (niv .ge. 2) then
                    call utmess('I', 'ALGELINE7_11', si=i)
                    valr(1)=zr(qrar+im1)
                    valr(2)=zr(qrba+im1)
                    call utmess('I', 'ALGELINE7_12', nr=2, valr=valr)
                    if (lqze) then
                        call utmess('I', 'ALGELINE7_13', sr=zr( icscal+im1))
                    endif
                    call utmess('I', 'ALGELINE7_14')
                endif
            endif
55      continue
        else if ((typeqz(1:5).ne.'QZ_QR').and.((.not.lkr).or.(lc).or.&
    (lnsm).or.(lnsr))) then
! ---- GENERALISE COMPLEXE SYM OU NON, REEL NON SYM
! ---- QUADRATIQUE REEL ET COMPLEXE, SYM OU NON
        if (lc) call wkvect('&&VPQZLA.VP4', 'V V C', qrn*qrn, lvec4)
        do 155 i = 1, qrn
            im1=i-1
            if (abs(zc(qrba+im1)) .gt. prec) then
                raux=abs(zc(qrar+im1))
                rauxi=abs(zc(qrba+im1))
                if ((raux.gt.anorm) .or. (rauxi.gt.bnorm)) then
                    vali(1)=i
                    valr(1)=raux
                    valr(2)=anorm
                    valr(3)=rauxi
                    valr(4)=bnorm
                    kmsg='A'
                    call utmess(kmsg, 'ALGELINE5_61', si=vali(1), nr=4, valr=valr)
                endif
                zc(lvalpr+im1-decal) = zc(qrar+im1)/zc(qrba+im1)
                if (lc) then
                    call zcopy(qrn, zc(lvec3+im1*qrn), 1, zc(lvec4+( im1-decal)*qrn), 1)
                else
                    call zcopy(qrn, zc(lvec3+im1*qrn), 1, zc(lvec+(im1- decal)*qrn), 1)
                endif
                if (lqze) zr(ics1+im1-decal)=zr(icscal+im1)
            else
                decal = decal+1
                if (niv .ge. 2) then
                    if (abs(zc(qrba+im1)) .gt. prec) then
                        freq=zc(qrar+im1)/zc(qrba+im1)
                    else
                        freq=1.d+70*cun
                    endif
                    call utmess('I', 'ALGELINE7_11', si=i)
                    if (abs(freq) .gt. prec) then
                        valr(1)=dimag(freq)/depi
                        valr(2)=-dble(freq)/abs(freq)
                    else
                        valr(1)=0.d0
                        valr(2)=1.d0
                    endif
                    call utmess('I', 'ALGELINE7_15', nr=2, valr=valr)
                    if (lqze) then
                        call utmess('I', 'ALGELINE7_13', sr=zr( icscal+im1))
                    endif
                    call utmess('I', 'ALGELINE7_14')
                endif
            endif
155      continue
        if (lc) call jedetr('&&VPQZLA.VP2')
    else if (typeqz(1:5).eq.'QZ_QR') then
        if (lqze) ASSERT(.false.)
!     --- POST-TRAITEMENT POUR QR ---
        do 57 i = 1, qrn
            im1=i-1
            if ((zr(lvalpr+im1).lt.prec3) .or. (zr(lvalpr+im1).gt.prec2)) then
                decal = decal+1
                if (niv .ge. 2) then
                    call utmess('I', 'ALGELINE7_11', si=i)
                    call utmess('I', 'ALGELINE7_16', sr=zr(lvalpr+im1))
                    call utmess('I', 'ALGELINE7_14')
                endif
            else
                zr(lvalpr+im1-decal) = zr(lvalpr+im1)
                call dcopy(qrn, zr(iqrn+im1*qrn), 1, zr(lvec+(im1- decal)*qrn), 1)
            endif
57      continue
    endif
!
! ----  NBRE DE MODES RETENUS
    nconv = qrn-decal
!
! ---- RESULTAT DU TEST UNITAIRE
    if (ltest) then
        write(ifm,*)'*******RESULTATS DU TEST UNITAIRE VPQZLA *********'
        write(ifm,*)' --> ON DOIT TROUVER LAMBDA(I)=I'
        do 66 i = 1, nconv
            if (lkr .and. (.not.lc) .and. (.not.lnsm) .and. (.not.lnsr)) then
                write(ifm,*)'I/LAMBDA(I) ',i,zr(lvalpr-1+i)
            else
                write(ifm,*)'I/LAMBDA(I) ',i,zc(lvalpr-1+i)
            endif
66      continue
    endif
!
!-------------------------------------
! ----  ON TESTE LES MODES VALIDES
! ----  1/ NBRE TOTAL DE MODES TROUVES
!-------------------------------------
    if ((nconv/imult) .ne. neqact) then
        vali(1)=nconv/imult
        vali(2)=neqact
        if (lkr .and. (.not.lc) .and. (.not.lnsm) .and. (.not.lnsr)) then
            kmsg='E'
            flage=.true.
        else
            kmsg='I'
        endif
        call utmess(kmsg, 'ALGELINE5_62', ni=2, vali=vali)
    endif
!
!------------------------------------------------------------------
! -----------------------------------------------------------------
! SELECTION ET TRI DES MODES SUIVANT LES DESIRATAS DES UTILISATEURS
! -----------------------------------------------------------------
!------------------------------------------------------------------
!
! ---- INITS
    if (lkr .and. (.not.lc) .and. (.not.lnsm) .and. (.not.lnsr)) then
        call wkvect('&&VPQZLA.QR.VPGSKP1', 'V V R', qrn, ivp1)
        call wkvect('&&VPQZLA.QR.VPGSKP2', 'V V R', qrn*(qrn+1), ivp2)
    else
        if (.not.lc) then
            call wkvect('&&VPQZLA.QR.VPGSKP1', 'V V C', qrn, ivp1)
            call wkvect('&&VPQZLA.QR.VPGSKP2', 'V V R', qrn*(qrn+1), ivp2)
        endif
    endif
    if (lkr .and. (.not.lc) .and. (.not.lnsm) .and. (.not.lnsr)) then
! ---- GENERALISE SYM REEL
        call vpordo(0, 0, nconv, zr(lvalpr), zr(lvec),&
                    qrn)
        vpinf = omemin-prec
        vpmax = omemax+prec
        j=0
        if (optiof(1:5) .eq. 'BANDE') then
            do 80 i = 1, nconv
                vpcour = zr(lvalpr-1+i)
                if ((vpcour.ge.vpinf) .and. (vpcour.le.vpmax)) then
                    j = j+1
                    zr(lvalpr-1+j) = vpcour
                    call dcopy(qrn, zr(lvec+(i-1)*qrn), 1, zr(lvec+(j- 1)*qrn), 1)
                endif
80          continue
            nconv = j
            if (nconv .ne. nfreq) then
                vali(1)=nconv
                vali(2)=nfreq
                kmsg='E'
                flage=.true.
                call utmess(kmsg, 'ALGELINE5_63', ni=2, vali=vali)
            endif
            do 81 i = 1, nconv
                zr(lvalpr-1+i) = zr(lvalpr-1+i) - omeshi
81          continue
            call vpordo(1, 0, nconv, zr(lvalpr), zr(lvec),&
                        qrn)
        else
            do 82 i = 1, nconv
                zr(lvalpr-1+i) = zr(lvalpr-1+i) - omeshi
82          continue
            call vpordo(1, 0, nconv, zr(lvalpr), zr(lvec),&
                        qrn)
            if (nconv .ge. nfreq) then
                nconv=nfreq
            else
                vali(1)=nfreq
                vali(2)=nconv
                kmsg='E'
                flage=.true.
                call utmess(kmsg, 'ALGELINE5_66', ni=2, vali=vali)
            endif
        endif
       call vpgsmm(qrn, nconv, zr(lvec), alpha, lmasse,&
                    2, zr(ivp1), ddlexc, zr(ivp2),zr(lvalpr),omecor)
!
    else if ((.not.lc).and.((lnsm).or.(lnsr).or.(.not.lkr))) then
! ---- GENERALISE COMPLEXE SYM OU NON, REEL NON SYM
! DECALAGE DU SHIFT HOMOGENE A CE QUI EST FAIT POUR SORENSEN
! STRATEGIE BIZARRE A REVOIR (CF VPSORC, VPFOPC, RECTFC, VPBOSC)
        call vpordc(1, 0, nconv, zc(lvalpr), zc(lvec),&
                    qrn)
        if (nconv .ge. nfreq) then
            nconv=nfreq
        else
            kmsg='F'
! --- PROBABLEMENT OPTION='TOUT' QUI PRESUPPOSE (SANS DOUTE A TORT
!     EN NON SYM) QUE NFREQ=NEQACT
            vali(1)=nfreq
            vali(2)=nconv
            if ((nfreq.eq.neqact) .and. (lnsm.or.lnsr)) then
                kmsg='I'
                nfreq=nconv
            endif
            call utmess(kmsg, 'ALGELINE5_66', ni=2, vali=vali)
        endif
!
        do 83 i = 1, nconv
            zc(lvalpr-1+i) = zc(lvalpr-1+i) + sigma
83      continue
!
    else if (lc) then
! ---- QUADRATIQUE SYM OU NON, REEL ET COMPLEXE
        call vpordc(1, 0, nconv, zc(lvalpr), zc(lvec4),&
                    qrn)
        do 89 i = 1, nconv
            do 88 j = 1, qrns2
! ---- REMPLISSAGE DU VECT PAR LA PARTIE BASSE DE VECTA
                zc(lvec+(i-1)*qrns2+j-1)=zc(lvec4+(i-1)*qrn+qrns2+j-1)
88          continue
89      continue
!
        call jedetr('&&VPQZLA.VP4')
    endif
    if (.not.lc) then
        call jedetr('&&VPQZLA.QR.VPGSKP1')
        call jedetr('&&VPQZLA.QR.VPGSKP2')
    endif
!
!------------------------------------------------------------------
! -----------------------------------------------------------------
! VERIFICATION DES ERREURS INVERSES DES MODES (DEVELOPPEURS)
! -----------------------------------------------------------------
!------------------------------------------------------------------
    if (ldebug) then
        write(ifm,*)'******** DONNEES LAPACK APRES TRI/REORTHO ********'
        if (lkr .and. (.not.lc) .and. (.not.lnsm) .and. (.not.lnsr)) then
            iauxh=qrn
            call wkvect('&&VPQZLA.TAMPON.PROV_1', 'V V R', iauxh, iaux1)
            call wkvect('&&VPQZLA.TAMPON.PROV_2', 'V V R', iauxh, iaux2)
        else
            if (lc) then
                iauxh=qrns2
                call wkvect('&&VPQZLA.TAMPON.PROV_3', 'V V C', iauxh, iaux3)
            else
                iauxh=qrn
            endif
            call wkvect('&&VPQZLA.TAMPON.PROV_1', 'V V C', iauxh, iaux1)
            call wkvect('&&VPQZLA.TAMPON.PROV_2', 'V V C', iauxh, iaux2)
        endif
        do 91 i = 1, nconv
            call jerazo('&&VPQZLA.TAMPON.PROV_1', iauxh, 1)
            call jerazo('&&VPQZLA.TAMPON.PROV_2', iauxh, 1)
            if (lc) call jerazo('&&VPQZLA.TAMPON.PROV_3', iauxh, 1)
            if (lkr .and. (.not.lc) .and. (.not.lnsm) .and. (.not.lnsr)) then
                fr=zr(lvalpr-1+i)+omeshi
                freq=fr*cun
                call mrmult('ZERO', lraide, zr(lvec+iauxh*(i-1)), zr(iaux1), 1,&
                            .false.)
                anorm1=dnrm2(iauxh,zr(iaux1),1)
                call mrmult('ZERO', lmasse, zr(lvec+iauxh*(i-1)), zr(iaux2), 1,&
                            .false.)
                call daxpy(iauxh, -fr, zr(iaux2), 1, zr(iaux1),&
                           1)
                anorm2=dnrm2(iauxh,zr(iaux1),1)
            else
                if (lc) then
                    freq = zc(lvalpr-1+i)
                else
                    freq = zc(lvalpr-1+i)-sigma
                endif
                call mcmult('ZERO', lraide, zc(lvec+iauxh*(i-1)), zc(iaux1), 1,&
                            .false.)
                anorm1=dznrm2(iauxh,zc(iaux1),1)
                call mcmult('ZERO', lmasse, zc(lvec+iauxh*(i-1)), zc(iaux2), 1,&
                            .false.)
                if (lc) then
                    call mcmult('ZERO', lamor, zc(lvec+iauxh*(i-1)), zc(iaux3), 1,&
                                .false.)
                    call zaxpy(iauxh, freq, zc(iaux3), 1, zc(iaux1),&
                               1)
                    freq2 = freq*freq
                    call zaxpy(iauxh, freq2, zc(iaux2), 1, zc(iaux1),&
                               1)
                else
                    call zaxpy(iauxh, -freq, zc(iaux2), 1, zc(iaux1),&
                               1)
                endif
                anorm2=dznrm2(iauxh,zc(iaux1),1)
            endif
            if (abs(freq) .gt. omecor) then
                if (anorm1 .gt. prec) then
                    anorm3=anorm2/anorm1
                else
                    anorm3= 1.d+70
                endif
            else
                anorm3=abs(freq)*anorm2
            endif
            if (lkr .and. (.not.lc) .and. (.not.lnsm) .and. (.not.lnsr)) then
                write(ifm,921)i,freqom(dble(freq)),anorm3
            else if (lc) then
                write(ifm,922)i,dimag(freq)/depi,-dble(freq)/abs(freq)&
                , anorm3
            else
                write(ifm,923)i,freqom(dble(freq)), dimag(freq)/(2.d0*&
                dble(freq)), anorm3
            endif
91      continue
        921   format('I/FREQ/ERREUR INVERSE ASTER',i4,1x,e12.5,1x,e12.5)
        922   format('I/LAMBDA/ERREUR INVERSE ASTER',i4,1x,e12.5,e12.5,1x,&
     &         e12.5)
        923   format('I/FREQ/ERREUR INVERSE ASTER',i4,1x,e12.5,e12.5,1x,&
     &         e12.5)
        call jedetr('&&VPQZLA.TAMPON.PROV_1')
        call jedetr('&&VPQZLA.TAMPON.PROV_2')
        if (lc) call jedetr('&&VPQZLA.TAMPON.PROV_3')
    endif
    call matfpe(1)
    call jedema()
end subroutine
