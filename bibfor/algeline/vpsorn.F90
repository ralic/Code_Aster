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
    include 'jeveux.h'
!
    include 'asterfort/dnaupd.h'
    include 'asterfort/dneupd.h'
    include 'asterfort/mrmult.h'
    include 'asterfort/resoud.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vpgskp.h'
    include 'asterfort/vpordo.h'
    integer :: lmasse, ldynfa, nbeq, nbvect, nfreq, lonwl, ddlexc(nbeq)
    integer :: ddllag(nbeq), neqact, maxitr, ifm, niv, priram(8), nconv
    real(kind=8) :: tolsor, vect(nbeq, nbvect), resid(nbeq), workd(3*nbeq)
    real(kind=8) :: workl(lonwl), dsor(nfreq+1, 2), fshift, vaux(nbeq)
    real(kind=8) :: workv(3*nbvect), alpha, omecor
    logical :: selec(nbvect), flage
    character(len=19) :: solveu
!
!
! DECLARATION VARIABLES LOCALES
!
! POUR LE FONCTIONNEMENT GLOBAL
    integer :: i, j
    complex(kind=8) :: cbid
    real(kind=8) :: varaux
    integer :: iret
!
! POUR ARPACK
    integer :: ido, info, ishfts, mode, iparam(11), ipntr(14), vali(5)
    real(kind=8) :: sigmar, sigmai, valr(2)
    logical :: rvec
    character(len=1) :: bmat, kbid
    character(len=2) :: which
    character(len=19) :: k19bid, matass, chcine, criter
!
    integer :: logfil, ndigit, mgetv0, mnaupd, mnaup2, mnaitr, mneigh, mnapps
    integer :: mngets, mneupd
    common /debug/&
     &  logfil, ndigit, mgetv0,&
     &  mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
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
    matass=zk24(zi(ldynfa+1))
    chcine=' '
    criter=' '
    k19bid=' '
!------------------------------------------------------------------
! BOUCLE PRINCIPALE
!
20  continue
!
! CALCUL DES VALEURS PROPRES DE (OP)
    call dnaupd(ido, bmat, nbeq, which, nfreq,&
                tolsor, resid, nbvect, vect, nbeq,&
                iparam, ipntr, workd, workl, lonwl,&
                info, neqact, alpha)
!
! NOMBRE DE MODES CONVERGES
    nconv = iparam(5)
!
! GESTION DES FLAGS D'ERREURS
    if ((info.eq.1) .and. (niv.ge.1)) then
        write(ifm,*)
        write(ifm,*)'<VPSORN/DNAUPD 1> NOMBRE MAXIMAL D''ITERATIONS'
        write(ifm,*)' NMAX_ITER_SOREN = ',maxitr,' A ETE ATTEINT !'
        write(ifm,*)
    else if (info.eq.2) then
        call u2mess('F', 'ALGELINE3_72')
    else if ((info.eq.3).and.(niv.ge.1)) then
        write(ifm,*)
        write(ifm,*)'<VPSORN/DNAUPD 3> AUCUN SHIFT NE PEUT ETRE'//&
        ' APPLIQUE'
        write(ifm,*)
    else if (info.eq.-7) then
        call u2mess('F', 'ALGELINE3_73')
    else if (info.eq.-8) then
        call u2mess('F', 'ALGELINE3_74')
    else if (info.eq.-9) then
        call u2mess('F', 'ALGELINE3_75')
    else if ((info.eq.-9999).and.(niv.ge.1)) then
        write(ifm,*)
        write(ifm,*)'<VPSORN/DNAUPD -9999> PROBLEME FACTORISATION'//&
        ' D''ARNOLDI'
        write(ifm,*)
    else if (info.lt.0) then
        vali (1) = info
        call u2mesg('F', 'ALGELINE5_48', 0, ' ', 1,&
                    vali, 0, 0.d0)
    endif
!
! GESTION DES MODES CONVERGES
    if (ido .eq. 99) then
        if (nconv .lt. nfreq) then
            vali (1) = nconv
            vali (2) = nfreq
            vali (3) = info
            vali (4) = nbvect
            vali (5) = maxitr
            valr (1) = tolsor
            call u2mesg('E', 'ALGELINE5_49', 0, ' ', 5,&
                        vali, 1, valr)
            flage = .true.
        endif
    endif
!
!---------------------------------------------------------------------
! ZONE GERANT LA 'REVERSE COMMUNICATION' VIA IDO
!
    if (ido .eq. -1) then
! CALCUL DU Y = (OP)*X INITIAL
! 1/ CALCUL D'UN ELT. INITIAL X REPONDANT AU C.I. DE LAGRANGE
! 2/ CALCUL DE Y = (OP)* X AVEC DDL CINEMATIQUEMENT BLOQUES
! X <- X*DDL_LAGRANGE
        do 25 j = 1, nbeq
            vaux(j) = workd(ipntr(1)+j-1) * ddllag(j)
25      continue
! X <- (INV((A)-SIGMA*(B))*X)*DDL_LAGRANGE
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, vaux, cbid,&
                    criter, .false., 0, iret)
        do 30 j = 1, nbeq
            workd(ipntr(1)+j-1) = vaux(j) * ddllag(j)
30      continue
! X <- (OP)*(X*DDL_BLOQUE)
        call mrmult('ZERO', lmasse, workd(ipntr(1)), vaux, 1,&
                    .false.)
        do 35 j = 1, nbeq
            vaux(j) = vaux(j) * ddlexc(j)
35      continue
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, vaux, cbid,&
                    criter, .false., 0, iret)
! RETOUR VERS DNAUPD
        do 40 j = 1, nbeq
            workd(ipntr(2)+j-1) = vaux(j)
40      continue
        goto 20
!
    else if (ido .eq. 1) then
! CALCUL DU Y = (OP)*X CONNAISSANT DEJA (B)*X (EN FAIT ON CONNAIT
! SEULEMENT (ID)*X VIA IDO= 2 CAR PRODUIT SCALAIRE= L2)
! X <- (B)*X*DDL_BLOQUE
        call mrmult('ZERO', lmasse, workd(ipntr(3)), vaux, 1,&
                    .false.)
        do 45 j = 1, nbeq
            vaux(j) = vaux(j) * ddlexc(j)
45      continue
! X <- (OP)*X
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, vaux, cbid,&
                    criter, .false., 0, iret)
! RETOUR VERS DNAUPD
        do 50 j = 1, nbeq
            workd(ipntr(2)+j-1) = vaux(j)
50      continue
        goto 20
!
    else if (ido .eq. 2) then
! X <- X*DDL_BLOQUE  (PRODUIT SCALAIRE= L2)
        do 55 j = 1, nbeq
            workd(ipntr(2)+j-1)=workd(ipntr(1)+j-1)*ddlexc(j)
55      continue
! RETOUR VERS DNAUPD
        goto 20
!
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
        call u2mess('F', 'ALGELINE3_74')
    else if (info.eq.-7) then
        call u2mess('F', 'ALGELINE3_73')
    else if (info.eq.-8) then
        call u2mess('F', 'ALGELINE3_76')
    else if (info.eq.-9) then
        call u2mess('F', 'ALGELINE3_77')
    else if (info.eq.-14) then
        call u2mess('F', 'ALGELINE3_78')
    else if (info.lt.0) then
        vali (1) = info
        call u2mesg('F', 'ALGELINE5_48', 0, ' ', 1,&
                    vali, 0, 0.d0)
    endif
!--------------------------------------------------------------------
! TESTS ET POST-TRAITEMENTS
!
! POUR TEST
!      DO 59 J=1,NCONV
!       WRITE(IFM,*) '******** VALEUR DE RITZ N ********',J
!       WRITE(IFM,*) 'RE: LANDAJ/ FJ INIT',DSOR(J,1),
!    &                   FREQOM(DSOR(J,1))
!       WRITE(IFM,*) 'IM: LANDAJ/ FJ INIT',DSOR(J,2),
!    &                   FREQOM(DSOR(J,2))
!  59 CONTINUE
!
!
! VERIFICATIONS DES VALEURS PROPRES
    do 60 j = 1, nconv
        varaux = abs(dsor(j,2))
        if (varaux .gt. omecor) then
            vali (1) = j
            valr (1) = dsor(j,1)
            valr (2) = dsor(j,2)
            call u2mesg('A', 'ALGELINE5_51', 0, ' ', 1,&
                        vali, 2, valr)
        else if ((varaux.ne.0.d0).and.(niv.ge.1)) then
            write(ifm,*)'<VPSORN/DNEUPD 0> LA VALEUR PROPRE NUMERO ',&
            j
            write(ifm,*)'A UNE PARTIE IMAGINAIRE NON NULLE'
            write(ifm,*)'RE(VP) = ',dsor(j,1)
            write(ifm,*)'IM(VP) = ',dsor(j,2)
            write(ifm,*)'--> CE PHENOMENE NUMERIQUE EST FREQUENT'
            write(ifm,*)'--> SUR LES PREMIERES VALEURS PROPRES'
            write(ifm,*)'--> LORSQUE LE SPECTRE RECHERCHE EST'
            write(ifm,*)'--> TRES ETENDU (EN PULSATION) '
        endif
60  end do
!
! REMISE EN FORMES DES MODES PROPRES SELON FORMAT OP0045
    do 65 i = 1, nconv
        dsor(i,1) = dsor(i,1) - sigmar
        dsor(i,2) = dsor(i,2) - sigmai
65  end do
!
! TRI DES MODES PROPRES PAR RAPPORT AU NCONV DSOR(I)
    call vpordo(1, 0, nconv, dsor, vect,&
                nbeq)
!
! RE-ORTHONORMALISATION SUIVANT IGS PAR RAPPORT A B
    call vpgskp(nbeq, nconv, vect, alpha, lmasse,&
                1, vaux, ddlexc, workv)
!
! FIN DE VPSORN
!
end subroutine
