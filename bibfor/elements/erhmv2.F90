subroutine erhmv2(axi, perman, deltat, dimdep, dimdef,&
                  nmec, np1, np2, ndim, nno,&
                  nnos, nnom, npg, nddls, nddlm,&
                  dimuel, ipoids, ivf, idfde, ipoid2,&
                  ivf2, idfde2, geom, fovo, deplp,&
                  deplm, sielnp, sielnm, nbcmp, biot,&
                  unsurm, fpx, fpy, frx, fry,&
                  yamec, addeme, yap1, addep1, yap2,&
                  addep2, yate, addete, tm2h1v)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
!  ERREUR EN HYDRO-MECANIQUE - TERME VOLUMIQUE - DIMENSION 2
!  **        *     *                 *                     *
! =====================================================================
!    - FONCTION REALISEE:  CALCUL DES TERMES VOLUMIQUES MECANIQUE ET
!      HYDRAULIQUE DE L'ESTIMATEUR D'ERREUR EN RESIDU POUR LES
!      MODELISATIONS HM SATUREES
!
! ENTREE :
! -------
!
! IN AXI     : AXISYMETRIQUE OU NON ?
! IN PERMAN  : PERMANENT OU NON ?
! IN DELTAT  : PAS DE TEMPS (SI INSTATIONNAIRE)
! IN DIMDEP  : DIMENSION DES DEPLACEMENTS
! IN DIMDEF  : DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IN NMEC    : = NDIM SI YAMEC, 0 SINON
! IN NP1     : = 1 SI YAP1, 0 SINON
! IN NP2     : = 1 SI YAP2, 0 SINON
! IN NDIM    : DIMENSION DE L'ESPACE
! IN NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN NNOS    : NOMBRE DE NOEUDS SOMMETS DE L'ELEMENT
! IN NNOM    : NOMBRE DE NOEUDS MILIEUX DE L'ELEMENT
! IN NPG     : NB DE POINTS DE GAUSS    POUR CLASSIQUE(=NPI)
!                    SOMMETS            POUR LUMPEE   (=NPI=NNOS)
!                    POINTS DE GAUSS    POUR REDUITE  (<NPI)
! IN NDDLS   : NOMBRE DE DDLS SUR LES SOMMETS
! IN NDDLM   : NOMBRE DE DDLS SUR LES POINTS MILIEUX
! IN DIMUEL  : NOMBRE DE DDLS TOTAL DE L'ELEMENT
! IN IPOIDS  : ADRESSE DANS ZR DU TABLEAU POIDS(IPG)
!              POUR LES FONCTIONS DE FORME P2
! IN IVF     : ADRESSE JEVEUX DES FONCTIONS DE FORME QUADRATIQUES
! IN IDFDE   : ADRESSE DANS ZR DU TABLEAU DFF(IDIM,INO,IPG)
!              POUR LES FONCTIONS DE FORME P2
! IN IPOID2  : ADRESSE DANS ZR DU TABLEAU POIDS(IPG)
!              POUR LES FONCTIONS DE FORME P1
! IN IVF2    : ADRESSE JEVEUX DES FONCTIONS DE FORME LINEAIRES
! IN IDFDE2  : ADRESSE DANS ZR DU TABLEAU DFF(IDIM,INO,IPG)
!              POUR LES FONCTIONS DE FORME P1
! IN GEOM    : TABLEAU DES COORDONNEES
! IN FOVO    : TABLEAU DES FORCES VOLUMIQUES SELON LA DIMENSION
! IN DEPLP   : TABLEAU DES DEPLACEMENTS GENERALISES A L'INSTANT ACTUEL
! IN DEPLM   : TABLEAU DES DEPLACEMENTS GENERALISES A L'INSTANT
!              PRECEDENT
! IN SIELNP  : CONTRAINTES AUX NOEUDS PAR ELEMENT A L'INSTANT ACTUEL
! IN SIELNM  : CONTRAINTES AUX NOEUDS PAR ELEMENT A L'INSTANT PRECEDENT
! IN NBCMP   : NOMBRE DE CONTRAINTES GENERALISEES PAR NOEUD
! IN BIOT    : VALEUR DU COEFFICIENT DE BIOT
! IN FPX     : VALEUR DE LA FORCE DE PESANTEUR SELON X
! IN FPY     : VALEUR DE LA FORCE DE PESANTEUR SELON Y
! IN FRX     : VALEUR DE LA FORCE DE ROTATION SELON X
! IN FRY     : VALEUR DE LA FORCE DE ROTATION SELON Y
!
! SORTIE :
! -------
!
! OUT TM2H1V : TABLEAU CONTENANT LES TERMES VOLUMIQUES
!              (2 POUR LA MECANIQUE, 1 POUR L'HYDRAULIQUE)
!  1. TSIVOM : RESIDU DE LA MECANIQUE
!  2. TDEVOM : RESIDU DE LA DERIVEE TEMPORELLE DE LA MECA
!  3. TSIVOH : RESIDU DE L'HYDRAULIQUE
!
!   -------------------------------------------------------------------
! aslint: disable=W1306,W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
!
    include 'asterc/r8miem.h'
    include 'asterfort/cabthm.h'
    include 'asterfort/u2mess.h'
    logical :: axi, perman
    integer :: dimuel
    integer :: ndim, nno, nnos, nnom, dimdep, dimdef, nmec, np1, np2
    integer :: nbcmp, npg, nddls, nddlm, ipoids, ivf, idfde
    integer :: ipoid2, ivf2, idfde2
    integer :: yamec, addeme, yate, addete, yap1, addep1, yap2, addep2
    real(kind=8) :: deltat, biot, unsurm
    real(kind=8) :: deplp(nno*dimdep), deplm(nno*dimdep)
    real(kind=8) :: fovo(ndim)
    real(kind=8) :: geom(ndim, nno)
    real(kind=8) :: fpx, fpy
    real(kind=8) :: frx(9), fry(9)
    real(kind=8) :: sielnp(90), sielnm(90)
    real(kind=8) :: dfdi(nno, 3), dfdi2(nnos, 3)
    real(kind=8) :: b(dimdef, dimuel)
!
! DECLARATION SORTIES
!
    real(kind=8) :: tm2h1v(3)
!
! DECLARATION VARIABLES LOCALES
!
    real(kind=8) :: grapxp, grapyp, dsxp, dsyp, dsxm, dsym, forx, fory
    real(kind=8) :: grapxm, grapym, pressp, pressm
    real(kind=8) :: poids, poids2, ovfl
    real(kind=8) :: sigxxp, sigxyp, sigyyp, sigxxm, sigxym, sigyym
    real(kind=8) :: dsxxxp, dsxyyp, dsxyxp, dsyyyp, dsxxxm, dsxyym, dsxyxm
    real(kind=8) :: dsyyym
    real(kind=8) :: divuxp, divuyp, divuxm, divuym
    real(kind=8) :: divup, divum, ter11, ter12
    integer :: ipi, kpi, nn, ii, iaux, jaux
!
! =====================================================================
! 1. INITIALISATION
! =====================================================================
!
    ovfl = r8miem()
!
    do 10, ii = 1 , 3
!
    tm2h1v(ii) = 0.d0
!
    10 end do
!
! =====================================================================
! 2. ------ BOUCLE SUR LES POINTS DE GAUSS ---------------------------
! =====================================================================
!
    do 20 , ipi = 1,npg
!
    kpi = ipi
!
! =====================================================================
! 2.1. --- CALCUL DE LA MATRICE B AU POINT D'INTEGRATION --------------
! =====================================================================
!
    call cabthm(nddls, nddlm, nno, nnos, nnom,&
                dimuel, dimdef, ndim, kpi, ipoids,&
                ipoid2, ivf, ivf2, idfde, idfde2,&
                dfdi, dfdi2, geom, poids, poids2,&
                b, nmec, yamec, addeme, yap1,&
                addep1, yap2, addep2, yate, addete,&
                np1, np2, axi)
!
! =====================================================================
! 2.2. ------ RECHERCHE DU GRADIENT DE LA PRESSION AU POINT DE GAUSS --
! EN THEORIE, ON PEUT FAIRE LA MULTIPLICATION DE LA MATRICE B PAR LE
! CHAMP DE DEPLACEMENT DEPLA POUR TOUTES LES COMPOSANTES DE DEPLA. EN
! EFFET, TOUTES LES VALEURS DE DEPLA QUI CORRESPONDENT A UN AUTRE DDL
! QUE P1 SERONT MULTIPLIEES PAR 0. IL EST PLUS ECONOMIQUE DE NE FAIRE
! QUE LES MULTIPLICATIONS 'UTILES', C'EST-A-DIRE CELLES QUI SONT LIEES
! A P1. DANS LE TABLEAU DEPLP, ELLES COMMENCENT A L'ADRESSE IAUX. ON
! LES RETROUVE ENSUITE ESPACEES DE DIMDEP, DIMENSION DU VECTEUR
! DEPLACEMENT. ON TERMINE AU NOMBRE DE NOEUDS SOMMETS MULTIPLIE PAR
! DIMDEP.
! LES TERMES DANS LA MATRICE B SE TROUVENT A L'ADRESSE DITE DES
! DEFORMATIONS GENERALISEES, ADDEP1,AUGMENTEE DE LA DIMENSION EN COURS.
!
! EXEMPLE POUR DU THM EN TRIA6 :
! DEPLA :
!  UX1 UY1 P11 T1 UX2 UY2 P12 T2 UX3 UY3 P13 T3 UX4 UY4 UX5 UY5 UX6 UY6
!    1   2   3  4   5   6   7  8   9  10  11 12  13  14  15  16  17  18
!         ON A DIMDEP =  4 = 2 + 1 + 1 ( UX, UY, P1, T )
!              IAUX   =  3 = 2 + 1     ( NDIM + 1      )
!              JAUX   = 12 = 3*4       ( NNOS*DIMDEP   )
!    LA BOUCLE 22 NN = IAUX, JAUX, DIMDEP, EXPLORE DONC LES
!    POSITIONS 3, 7 ET 11 (P11, P12 ET P13). CQFD.
! =====================================================================
!
    grapxp = 0.d0
    grapyp = 0.d0
!
    grapxm = 0.d0
    grapym = 0.d0
!
    iaux = ndim + 1
    jaux = nnos*dimdep
!
    do 30 , nn = iaux, jaux, dimdep
!
    grapxp = grapxp + b(addep1+1,nn)*deplp(nn)
    grapyp = grapyp + b(addep1+2,nn)*deplp(nn)
!
    if (.not. perman) then
!
        grapxm = grapxm + b(addep1+1,nn)*deplm(nn)
        grapym = grapym + b(addep1+2,nn)*deplm(nn)
!
    endif
!
30  continue
!
! =====================================================================
! 2.3. --------- CALCUL DE LA DIVERGENCE DES CONTRAINTES MECANIQUES ---
!    ON CALCULE LES DERIVEES DES CONTRAINTES SUR LE POINT DE GAUSS
!    COURANT AVEC LA FORMULE CLASSIQUE DES ELEMENTS FINIS :
!                  SOMME(VAL-NOEUD_I*WI)
!    LES VALEURS AUX NOEUDS SONT DANS SIELNP
! =====================================================================
!
    dsxxxp = 0.d0
    dsxyyp = 0.d0
    dsxyxp = 0.d0
    dsyyyp = 0.d0
!
    dsxxxm = 0.d0
    dsxyym = 0.d0
    dsxyxm = 0.d0
    dsyyym = 0.d0
!
    do 40 , ii = 1 , nno
!
    iaux = nbcmp*(ii-1)
!
    sigxxp = sielnp(iaux+1)
    sigyyp = sielnp(iaux+2)
    sigxyp = sielnp(iaux+4)
!
    dsxxxp = dsxxxp + sigxxp * dfdi(ii,1)
    dsxyyp = dsxyyp + sigxyp * dfdi(ii,2)
    dsyyyp = dsyyyp + sigyyp * dfdi(ii,2)
    dsxyxp = dsxyxp + sigxyp * dfdi(ii,1)
!
    if (.not. perman) then
!
        sigxxm = sielnm(iaux+1)
        sigyym = sielnm(iaux+2)
        sigxym = sielnm(iaux+4)
!
        dsxxxm = dsxxxm + sigxxm * dfdi(ii,1)
        dsxyym = dsxyym + sigxym * dfdi(ii,2)
        dsyyym = dsyyym + sigyym * dfdi(ii,2)
        dsxyxm = dsxyxm + sigxym * dfdi(ii,1)
!
    endif
!
40  continue
!
! LA DIVERGENCE DU TENSEUR DES CONTRAINTES EST UN VECTEUR
! DE COMPOSANTES :
!
    dsxp = dsxxxp + dsxyyp
    dsyp = dsxyxp + dsyyyp
!
    if (.not. perman) then
!
        dsxm = dsxxxm + dsxyym
        dsym = dsxyxm + dsyyym
!
    endif
!
! =====================================================================
! 2.4. ------ ASSEMBLAGE DES 3 TERMES : -------------------------------
!           FORCES MECANIQUES : VOLUMIQUES + PESANTEUR + ROTATION
!           CONTRAINTES MECANIQUES
!           GRADIENT DE PRESSION
! =====================================================================
!
    forx = fovo(1) + fpx + frx(kpi)
    fory = fovo(2) + fpy + fry(kpi)
!
    tm2h1v(1) = tm2h1v(1) + poids* ( ( forx + dsxp - biot * grapxp )**2 + ( fory + dsyp - biot * &
                &grapyp )**2 )
!
    if (.not. perman) then
!
        tm2h1v(2) = tm2h1v(2) + poids* ( ( dsxp - dsxm - biot * ( grapxp - grapxm ))**2 + ( dsyp &
                    &- dsym - biot * ( grapyp - grapym ))**2 )
!
    endif
!
! =====================================================================
! 2.5. TERME VOLUMIQUE DE L'HYDRAULIQUE (CF DOC R)
!
! ====================================================================
! 2.5.1. EN PERMANENT
!
!        TOUS CES TERMES (DIVERGENCE DU FLUX HYDRAULIQUE) SONT DES
!        DERIVEES SECONDES DE TERMES DISCRETISES EN DEGRE 1,
!        DONC SONT STRUCTURELLEMENT NULS.
!        ON POURRAIT LES CALCULER QUAND MEME ET RETROUVER CES VALEURS
!        NULLES MAIS ON NE CALCULERA RIEN DU TOUT.
! =====================================================================
! 2.5.2. EN INSTATIONNAIRE
!
!        COMME POUR LE PERMANENT, LES TERMES DE DIVERGENCE DU FLUX
!        HYDRAULIQUE NE SONT PAS CALCULES.
! =====================================================================
!
    if (.not. perman) then
!
        divuxp = 0.d0
        divuyp = 0.d0
        divuxm = 0.d0
        divuym = 0.d0
!
        do 50 , ii = 1 , nno
!
        if (ii .le. nnos) then
            iaux = dimdep*(ii-1)
        else
            iaux = (dimdep-1)*ii + nnos - 2
        endif
!
        divuxp = divuxp + deplp(iaux+1) * dfdi(ii,1)
        divuyp = divuyp + deplp(iaux+2) * dfdi(ii,2)
        divuxm = divuxm + deplm(iaux+1) * dfdi(ii,1)
        divuym = divuym + deplm(iaux+2) * dfdi(ii,2)
!
50      continue
!
        divup = divuxp + divuyp
        divum = divuxm + divuym
!
        pressp = 0.d0
        pressm = 0.d0
!
        iaux = ndim + 1
        jaux = nnos*dimdep
!
        do 60 , nn = iaux, jaux, dimdep
!
        pressp = pressp + b(addep1,nn) * deplp(nn)
        pressm = pressm + b(addep1,nn) * deplm(nn)
!
60      continue
!
        if (deltat .gt. ovfl) then
!
            ter11 = biot * ( divup - divum )/deltat
            ter12 = unsurm * ( pressp - pressm )/deltat
!
            tm2h1v(3) = tm2h1v(3) + poids2 * ( ter11 + ter12 )**2
        else
            call u2mess('F', 'INDICATEUR_31')
        endif
!
    endif
!
!
    20 end do
!
! FIN BOUCLE SUR LES POINTS DE GAUSS
!
end subroutine
