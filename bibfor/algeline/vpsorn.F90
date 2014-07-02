subroutine vpsorn(lmasse, ldynfa, nbeq, nbvect, nfreq,&
                  tolsor, vect, resid, workd, workl,&
                  lonwl, selec, dsor, fshift, vaux,&
                  workv, ddlexc, ddllag, neqact, maxitr,&
                  ifm, niv, priram, alpha, omecor,&
                  nconv, flage, solveu)
!---------------------------------------------------------------------
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
!
!     SUBROUTINE ASTER ORCHESTRANT LA METHODE DE SORENSEN: UN ARNOLDI
!     AVEC REDEMARRAGE IMPLICITE VIA QR (VERSION ARPACK 2.4).
!---------------------------------------------------------------------
!     PARTANT DU PROBLEME GENERALISE AUX VALEURS PROPRES
!                           (A)*X = LAMBDA*(B)*X
!     AVEC
!      - LES MATRICES REELLES SYMETRIQUES (A) ET (B), CORRESPONDANT AUX
!     MATRICES DE RAIDEUR ET A CELLE DE MASSE (RESP. RAIDEUR GEOMETRIQUE
!     , EN FLAMBEMENT),
!      - LE REEL, LAMBDA, CORRESPONDANT AU CARRE DE LA PULSATION (RESP.
!     L'OPPOSE DE LA CHARGE CRITIQUE, EN FLAMBEMENT),
!      - LE OU LES VECTEURS PROPRES REELS ASSOCIES, X, (A- ET B- ORTHOGO
!        NAUX ENTRE EUX AINSI QU'AVEC CEUX DES AUTRES VALEURS PROPRES).
!
!     ON RESOUT LE PROBLEME STANDARD
!                             (OP)*X =  MU*X
!     AVEC
!       - L'OPERATEUR SHIFTE (OP) = INV((A)-SIGMA*(B))*(B),
!       - LE 'SHIFT' REEL SIGMA,
!       - LA VALEUR PROPRE MU = 1/(LAMBDA-SIGMA),
!       - LE OU LES MEMES VECTEURS PROPRES QU'INITIALEMENT, X.
!   ------------------------------------------------------------------
!   CETTE METHODE, PARTANT D'UNE FACTORISATION DE TYPE ARNOLDI D'ORDRE
!   M=K+P DU PROBLEME, PILOTE UN RESTART A L'ORDRE K SUR P NOUVELLES
!   ITERATIONS. CE RESTART PERMET D'AMELIORER LES K PREMIERES VALEURS
!   PROPRES SOUHAITEES, LES P DERNIERES SERVANT UNIQUEMENT AUX CALCULS
!   AUXILIAIRES.
!   ELLE PERMET
!     - DE MINIMISER LA TAILLE DU SOUS-ESPACE DE PROJECTION,
!     - D'EFFECTUER DES RESTARTS DE MANIERE TRANSPARENTE, EFFICACE ET
!       AVEC DES PRE-REQUIS MEMOIRE FIXES,
!     - DE MIEUX PRENDRE EN COMPTE LES MULTIPLICITES,
!     - DE TRAITER AVEC UN BON COMPROMIS LA STRATEGIE DE RE-ORTHONORMA
!       LISATION.
!   ------------------------------------------------------------------
!     PARAMETRES D'APPELS:
!
! IN  LMASSE : IS : DESCRIPTEUR MATRICE DE "MASSE".
! IN  LDYNFA : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"-SHIFT"MASSE"
!                     FACTORISEE.
! IN  NBEQ   : IS : DIMENSION DES VECTEURS.
! IN  NBVECT : IS : DIMENSION DE L'ESPACE DE PROJECTION.
! IN  NFREQ  : IS : NOMBRE DE VALEURS PROPRES DEMANDEES.
! IN  TOLSOR : R8 : NORME D'ERREUR SOUHAITEE (SI 0.D0 ALORS LA VALEUR
!                   PAR DEFAUT EST LA PRECISION MACHINE).
! IN  LONWL  : IS : TAILLE DU VECTEUR DE TRAVAIL WORKL.
! IN  FSHIFT : R8 : VALEUR DU SHIFT SIGMA EN OMEGA2.
! IN  DDLEXC : IS : DDLEXC(1..NBEQ) VECTEUR POSITION DES DDLS BLOQUES.
! IN  DDLLAG : IS : DDLLAG(1..NBEQ) VECTEUR POSITION DES LAGRANGES.
! IN  NEQACT : IS : NOMBRE DE DDLS ACTIFS.
! IN  MAXITR : IS : NOMBRE MAXIMUM DE RESTARTS.
! IN  IFM    : IS : UNITE LOGIQUE D'IMPRESSION DES .MESS
! IN  NIV    : IS : NIVEAU D'IMPRESSION
! IN  PRIRAM : IS : PRIRAM(1..8) VECTEUR NIVEAU D'IMPRESSION ARPACK
! IN  ALPHA  : R8 : PARAMETRE VPGSKP D'ORTHONORMALISATION.
! IN  OMECOR : R8 : OMEGA2 DE CORPS RIGIDE
!
! OUT VECT   : R8 : VECT(1..NBEQ,1..NBVECT) MATRICE DES
!                   VECTEURS D'ARNOLDI.
! OUT RESID  : R8 : RESID(1..NBEQ) VECTEUR RESIDU.
! OUT WORKD  : R8 : WORKD(1..3*NBEQ) VECTEUR DE TRAVAIL PRIMAIRE IRAM
! OUT WORKL  : R8 : WORKL(1..LONWL) VECTEUR DE TRAVAIL SECONDAIRE IRAM
! OUT SELEC  : LS : SELEC(1..NBVECT) VECTEUR DE TRAVAIL POUR DNEUPD.
! OUT DSOR   : R8 : DSOR(1..NFREQ+1,1..2) MATRICE DES VALEURS PROPRES.
! OUT VAUX   : R8 : VAUX(1..NBEQ) VECTEUR DE TRAVAIL POUR VPSORN.
! OUT WORKV  : R8 : WORKV(1..3*NBVECT) VECTEUR DE TRAVAIL POUR DNEUPD
!                     ET VPGSKP.
! OUT NCONV  : IS : NOMBRE DE MODES CONVERGES.
! OUT FLAGE  : LO : FLAG PERMETTANT DE GERER LES IMPRESSIONS
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dnaupd.h"
#include "asterfort/dneupd.h"
#include "asterfort/mrmult.h"
#include "asterfort/resoud.h"
#include "asterfort/utmess.h"
#include "asterfort/vpgsmm.h"
#include "asterfort/vpordo.h"
!
    integer :: lmasse, ldynfa, nbeq, nbvect, nfreq, lonwl, ddlexc(nbeq), ddllag(nbeq), neqact
    integer :: maxitr, ifm, niv, priram(8), nconv
    real(kind=8) :: tolsor, vect(nbeq, nbvect), resid(nbeq), workd(3*nbeq), workl(lonwl)
    real(kind=8) :: dsor(nfreq+1, 2), fshift, vaux(nbeq), workv(3*nbvect), alpha, omecor
    aster_logical :: selec(nbvect), flage
    character(len=19) :: solveu
!
!
! DECLARATION VARIABLES LOCALES
!
! POUR LE FONCTIONNEMENT GLOBAL
    integer :: i, j
    complex(kind=8) :: cbid
    real(kind=8) :: varaux, varaux1
    integer :: iret
!
! POUR ARPACK
    integer :: ido, info, ishfts, mode, iparam(11), ipntr(14), vali(5)
    real(kind=8) :: sigmar, sigmai, valr(2)
    aster_logical :: rvec
    character(len=1) :: bmat, kbid
    character(len=2) :: which
    character(len=19) :: k19bid, matass, chcine, criter
!
    integer :: logfil, ndigit, mgetv0, mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
    common /debug/&
     &  logfil, ndigit, mgetv0,&
     &  mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
    cbid = dcmplx(0.d0, 0.d0)
!------------------------------------------------------------------
! INITIALISATION POUR ARPACK
!
! NIVEAU D'IMPRESSION ARPACK
    ndigit = -3
    logfil = ifm
    mgetv0 = priram(1)
    mnaupd = priram(2)
    mnaup2 = priram(3)
    mnaitr = priram(4)
    mneigh = priram(5)
    mnapps = priram(6)
    mngets = priram(7)
    mneupd = priram(8)
!
! FONCTIONNEMENT D'ARPACK
    ido = 0
    info = 0
    ishfts = 1
    mode = 3
    sigmar = fshift
    sigmai = 0.d0
    rvec = .true.
    bmat = 'G'
    which = 'LM'
    iparam(1) = ishfts
    iparam(3) = maxitr
    iparam(4) = 1
    iparam(7) = mode
!
! INIT. OBJETS ASTER
    matass=zk24(zi(ldynfa+1))(1:19)
    chcine=' '
    criter=' '
    k19bid=' '
!------------------------------------------------------------------
! BOUCLE PRINCIPALE
!
 20 continue
!
! CALCUL DES VALEURS PROPRES DE (OP)
    call dnaupd(ido, bmat, nbeq, which, nfreq,&
                tolsor, resid, nbvect, vect, nbeq,&
                iparam, ipntr, workd, workl, lonwl,&
                info, neqact, abs(alpha))
!
! NOMBRE DE MODES CONVERGES
    nconv = iparam(5)
!
! GESTION DES FLAGS D'ERREURS
    if ((info.eq.1) .and. (niv.ge.1)) then
        vali (1) = maxitr
        call utmess('I', 'ALGELINE6_89', si=vali(1))
    else if (info.eq.2) then
        call utmess('F', 'ALGELINE3_72')
    else if ((info.eq.3).and.(niv.ge.1)) then
        call utmess('I', 'ALGELINE6_90')
    else if (info.eq.-7) then
        call utmess('F', 'ALGELINE3_73')
    else if (info.eq.-8) then
        call utmess('F', 'ALGELINE3_74')
    else if (info.eq.-9) then
        call utmess('F', 'ALGELINE3_75')
    else if ((info.eq.-9999).and.(niv.ge.1)) then
        call utmess('I', 'ALGELINE6_91')
    else if (info.lt.0) then
        vali (1) = info
        call utmess('F', 'ALGELINE5_48', si=vali(1))
    endif
!
!
!---------------------------------------------------------------------
! ZONE GERANT LA 'REVERSE COMMUNICATION' VIA IDO
!
    if (ido .eq. -1) then
! CALCUL DU Y = (OP)*X INITIAL
! 1/ CALCUL D'UN ELT. INITIAL X REPONDANT AU C.I. DE LAGRANGE
! 2/ CALCUL DE Y = (OP)* X AVEC DDL CINEMATIQUEMENT BLOQUES
! X <- X*DDL_LAGRANGE
        do j = 1, nbeq
            vaux(j) = workd(ipntr(1)+j-1) * ddllag(j)
        end do
! X <- (INV((A)-SIGMA*(B))*X)*DDL_LAGRANGE
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, vaux, [cbid],&
                    criter, .false._1, 0, iret)
        do j = 1, nbeq
            workd(ipntr(1)+j-1) = vaux(j) * ddllag(j)
        end do
! X <- (OP)*(X*DDL_BLOQUE)
        call mrmult('ZERO', lmasse, workd(ipntr(1)), vaux, 1,&
                    .false._1)
        do j = 1, nbeq
            vaux(j) = vaux(j) * ddlexc(j)
        end do
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, vaux, [cbid],&
                    criter, .false._1, 0, iret)
! RETOUR VERS DNAUPD
        do j = 1, nbeq
            workd(ipntr(2)+j-1) = vaux(j)
        end do
        goto 20
!
    else if (ido .eq. 1) then
! CALCUL DU Y = (OP)*X CONNAISSANT DEJA (B)*X (EN FAIT ON CONNAIT
! SEULEMENT (ID)*X VIA IDO= 2 CAR PRODUIT SCALAIRE= L2)
! X <- (B)*X*DDL_BLOQUE
        call mrmult('ZERO', lmasse, workd(ipntr(3)), vaux, 1,&
                    .false._1)
        do j = 1, nbeq
            vaux(j) = vaux(j) * ddlexc(j)
        end do
! X <- (OP)*X
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, vaux, [cbid],&
                    criter, .false._1, 0, iret)
! RETOUR VERS DNAUPD
        do j = 1, nbeq
            workd(ipntr(2)+j-1) = vaux(j)
        end do
        goto 20
!
    else if (ido .eq. 2) then
! X <- X*DDL_BLOQUE  (PRODUIT SCALAIRE= L2)
        do j = 1, nbeq
            workd(ipntr(2)+j-1)=workd(ipntr(1)+j-1)*ddlexc(j)
        end do
! RETOUR VERS DNAUPD
        goto 20
!
! GESTION DES MODES CONVERGES
    else if (ido .eq. 99) then
        if (nconv .lt. nfreq) then
            vali (1) = nconv
            vali (2) = nfreq
            call utmess('E', 'ALGELINE5_49', ni=2, vali=vali)
            flage = .true.
        else if (nconv.gt.nfreq) then
            vali(1)=nconv
            vali(2)=nfreq
            call utmess('I', 'ALGELINE5_50', ni=2, vali=vali)
            nconv=nfreq
        endif
    else
        ASSERT(.false.)
    endif
!--------------------------------------------------------------------
! CALCUL DES MODES PROPRES APPROCHES DU PB INITIAL
!
    info = 0
    call dneupd(rvec, 'A', selec, dsor, dsor(1, 2),&
                vect, nbeq, sigmar, sigmai, workv,&
                bmat, nbeq, which, nfreq, tolsor,&
                resid, nbvect, vect, nbeq, iparam,&
                ipntr, workd, workl, lonwl, info)
!
! GESTION DES FLAGS D'ERREURS
    if (info .eq. 1) then
        call utmess('F', 'ALGELINE3_74')
    else if (info.eq.-7) then
        call utmess('F', 'ALGELINE3_73')
    else if (info.eq.-8) then
        call utmess('F', 'ALGELINE3_76')
    else if (info.eq.-9) then
        call utmess('F', 'ALGELINE3_77')
    else if (info.eq.-14) then
        call utmess('F', 'ALGELINE3_78')
    else if (info.lt.0) then
        vali (1) = info
        call utmess('F', 'ALGELINE5_48', si=vali(1))
    endif
!--------------------------------------------------------------------
! TESTS ET POST-TRAITEMENTS
!
! POUR TEST
!      DO 59 J=1,NCONV
!       WRITE(IFM,*) '******** VALEUR DE RITZ N ********',J
!       WRITE(IFM,*) 'RE: LANDAJ/ FJ INIT',DSOR(J,1),&
!    &                   FREQOM(DSOR(J,1))
!       WRITE(IFM,*) 'IM: LANDAJ/ FJ INIT',DSOR(J,2),&
!    &                   FREQOM(DSOR(J,2))
!  59 CONTINUE
!
!
! VERIFICATIONS DES VALEURS PROPRES
    do j = 1, nconv
        varaux=abs(dsor(j,2))
        varaux1=abs(dsor(j,1))
        if ((varaux1.gt.1.0d+3*omecor) .and. (varaux.gt.1.0d-2*varaux1)) then
            vali (1) = j
            valr (1) = dsor(j,1)
            valr (2) = dsor(j,2)
            call utmess('A', 'ALGELINE5_51', si=vali(1), nr=2, valr=valr)
        endif
    end do
!
! REMISE EN FORMES DES MODES PROPRES SELON FORMAT OP0045
    do i = 1, nconv
        dsor(i,1) = dsor(i,1) - sigmar
        dsor(i,2) = dsor(i,2) - sigmai
    end do
!
! TRI DES MODES PROPRES PAR RAPPORT AU NCONV DSOR(I)
    call vpordo(1, 0, nconv, dsor, vect,&
                nbeq)
!
! RE-ORTHONORMALISATION SUIVANT IGS PAR RAPPORT A B
    call vpgsmm(nbeq, nconv, vect, alpha, lmasse,&
                1, vaux, ddlexc, workv, dsor,&
                omecor)
!
! FIN DE VPSORN
!
end subroutine
