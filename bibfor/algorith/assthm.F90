subroutine assthm(nno, nnos, nnom, npg, npi,&
                  ipoids, ipoid2, ivf, ivf2, idfde,&
                  idfde2, geom, crit, deplm, deplp,&
                  contm, contp, varim, varip, defgem,&
                  defgep, drds, drdsr, dsde, b,&
                  dfdi, dfdi2, r, sigbar, c,&
                  ck, cs, matuu, vectu, rinstm,&
                  rinstp, option, imate, mecani, press1,&
                  press2, tempe, dimdef, dimcon, dimuel,&
                  nbvari, nddls, nddlm, nmec, np1,&
                  np2, ndim, compor, typmod, axi,&
                  perman, modint, codret)
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! aslint: disable=W1504
    implicit none
!
!
#include "asterc/r8prem.h"
#include "asterfort/cabthm.h"
#include "asterfort/equthm.h"
#include "asterfort/equthp.h"
#include "asterfort/lceqvn.h"
#include "asterfort/matini.h"
#include "asterfort/pmathm.h"
#include "asterfort/rcvala.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: dimmat, npg, ipoid2, ivf2, idfde2, dimuel, nnom
    parameter    (dimmat=120)
    integer :: nno, nnos, npi, ipoids, ivf, idfde, imate, dimdef, dimcon
    integer :: nbvari, nddls, nddlm, nmec, np1, np2, ndim, codret
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: yamec, yap1, yap2, yate
    integer :: addeme, addep1, addep2, addete, ii, jj
    integer :: kpi, ipi
    integer :: i, j, n, k, kji, nvim, nbcomp
    real(kind=8) :: dfdi(nno, 3), dfdi2(nnos, 3)
    real(kind=8) :: geom(ndim, nno), crit(*), poids, poids2
    real(kind=8) :: deplp(dimuel), deplm(dimuel)
    real(kind=8) :: contm(dimcon*npi), contp(dimcon*npi)
    real(kind=8) :: varim(nbvari*npi), varip(nbvari*npi)
    real(kind=8) :: matuu(dimuel*dimuel), matri(dimmat, dimmat)
    real(kind=8) :: rinstp, rinstm, a(2), as(2), ak(2), vectu(dimuel)
    real(kind=8) :: defgem(dimdef), defgep(dimdef)
    real(kind=8) :: drds(dimdef+1, dimcon), drdsr(dimdef, dimcon)
    real(kind=8) :: dsde(dimcon, dimdef), b(dimdef, dimuel)
    real(kind=8) :: r(dimdef+1), sigbar(dimdef), c(dimdef)
    real(kind=8) :: dt, ta, ta1, rthmc, ck(dimdef), cs(dimdef)
    logical :: axi, perman
    integer :: codmes(1)
    character(len=3) :: modint
    character(len=8) :: typmod(2)
    character(len=16) :: option, compor(*), thmc, loi
    character(len=24) :: valk(2)
!
! =====================================================================
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN MECANIQUE DES MILIEUX POREUX AVEC COUPLAGE THM
!.......................................................................
! =====================================================================
! IN AXI       AXISYMETRIQUE?
! IN TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
! IN MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! IN NNO       NB DE NOEUDS DE L'ELEMENT
! IN NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
! IN NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
! IN NDDLS     NB DE DDL SUR LES SOMMETS
! IN NDDLM     NB DE DDL SUR LES MILIEUX
! IN NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
! IN NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
!                    SOMMETS             POUR LUMPEE   (=NPI=NNOS)
!                    POINTS DE GAUSS     POUR REDUITE  (<NPI)
! IN NDIM      DIMENSION DE L'ESPACE
! IN DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
! IN DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! IN DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IN IVF       FONCTIONS DE FORMES QUADRATIQUES
! IN IVF2      FONCTIONS DE FORMES LINEAIRES
! =====================================================================
! IN  DFDE    : DERIVEE DES FTION FORME QUAD ELEMENT DE REFERENCE
! IN  DFDN    : DERIVEE DES FTION FORME QUAD ELEMENT DE REFERENCE
! IN  DFDK    : DERIVEE DES FTION FORME QUAD ELEMENT DE REFERENCE
! IN  DFDE2   : DERIVEE DES FTION FORME LINE ELEMENT DE REFERENCE
! IN  DFDN2   : DERIVEE DES FTION FORME LINE ELEMENT DE REFERENCE
! IN  DFDK2   : DERIVEE DES FTION FORME LINE ELEMENT DE REFERENCE
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX + THETA
! IN  DEPLP   : DEPLACEMENT A L INSTANT PLUS
! IN  DEPLM   : DEPLACEMENT A L INSTANT MOINS
! IN  RINSTM  : INSTANT PRECEDENT
! IN  RINSTP  : INSTANT COURANT
! IN  MECANI  : TABLEAU CONTENANT
!               YAMEC = MECA(1), YAMEC = 1 >> IL Y A UNE EQUATION MECANI
!               ADDEME = MECA(2), ADRESSE DANS LES TABLEAUX DES DEFORMAT
!               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
!               DEFORMATIONS CORRESPONDANT A LA MECANIQUE
!               ADCOME = MECA(3), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
!               CONTRAINTES CORRESPONDANT A LA MECANIQUE
!               NDEFME = MECA(4), NOMBRE DE DEFORMATIONS MECANIQUES
!               NCONME = MECA(5), NOMBRE DE CONTRAINTES MECANIQUES
! IN  PRESS1    : TABLEAU CONTENANT
!               YAP1 = PRESS1(1), YAP1 = 1 >> IL Y A UNE EQUATION DE PRE
!               NBPHA1=PRESS1(2) NOMBRE DE PHASES POUR LE CONSTITUANT 1
!               ADDEP1 = PRESS1(3), ADRESSE DANS LES TABLEAUX DES DEFORM
!               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
!               DEFORMATIONS CORRESPONDANT A LA PREMIERE PRESSION
!               ADCP11=PRESS1(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
!               CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
!               PREMIER CONSTITUANT
!               ADCP12=PRESS1(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
!               CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
!               PREMIER CONSTITUANT
!               NDEFP1 = PRESS1(6), NOMBRE DE DEFORMATIONS PRESSION 1
!               NCONP1 = PRESS1(7), NOMBRE DE CONTRAINTES POUR
!               CHAQUE PHASE DU CONSTITUANT 1
! IN  PRESS2    : TABLEAU CONTENANT
!               YAP2 = PRESS2(1), YAP2 = 1 >> IL Y A UNE EQUATION DE PRE
!               NBPHA1=PRESS2(2) NOMBRE DE PHASES POUR LE CONSTITUANT 1
!               ADDEP2 = PRESS2(3), ADRESSE DANS LES TABLEAUX DES DEFORM
!               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
!               DEFORMATIONS CORRESPONDANT A LA PREMIERE PRESSION
!               ADCP21=PRESS2(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
!               CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
!               SECOND CONSTITUANT
!               ADCP22=PRESS2(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
!               CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
!               SECOND CONSTITUANT
!               NDEFP2 = PRESS2(6), NOMBRE DE DEFORMATIONS PRESSION 2
!               NCONP2 = PRESS2(7), NOMBRE DE CONTRAINTES POUR
!               CHAQUE PHASE DU CONSTITUANT 2
!
! IN  TEMPE    : TABLEAU CONTENANT
!               YATE = TEMPE(1), YAMEC = 1 >> IL Y A UNE EQUATION THERMI
!               ADDETE = TEMPE(2), ADRESSE DANS LES TABLEAUX DES DEFORMA
!               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
!               DEFORMATIONS CORRESPONDANT A LA THERMIQUE
!               ADCOTE = TEMPE(3), ADRESSE DANS LES TABLEAUX DES CONTRAI
!               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
!               CONTRAINTES CORRESPONDANT A LA THERMIQUE
!               NDEFTE = TEMPE(4), NOMBRE DE DEFORMATIONS THERMIQUES
!               NCONTE = TEMPE(5), NOMBRE DE CONTRAINTES THERMIQUES
! OUT CODRET  : CODE RETOUR LOIS DE COMPORTEMENT
! OUT DFDI    : DERIVEE DES FCT FORME
! OUT CONTP   : CONTRAINTES
! OUT VARIP   : VARIABLES INTERNES
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!......................................................................
!
    if (nddls*nno .gt. dimmat) then
        call u2mess('F', 'ALGORITH_33')
    endif
!
    if (dimuel .gt. dimmat) then
        call u2mess('F', 'ALGORITH_33')
    endif
! =====================================================================
! --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU -------------
! =====================================================================
    yamec = mecani(1)
    addeme = mecani(2)
    yap1 = press1(1)
    addep1 = press1(3)
    yap2 = press2(1)
    addep2 = press2(3)
    yate = tempe(1)
    addete = tempe(2)
! =====================================================================
! --- CALCUL DE CONSTANTES TEMPORELLES --------------------------------
! =====================================================================
    dt = rinstp-rinstm
    ta= crit(4)
    ta1 = 1.d0-ta
! =====================================================================
! --- CREATION DES MATRICES DE SELECTION ------------------------------
! --- (MATRICES DIAGONALES) C,D,F,CS,DS,FS ----------------------------
! --- CREATION DE L'OPERATEUR A, AS -----------------------------------
! --- CES MATRICES SELECTIONNENT LES COMPOSANTES UTILES POUR ----------
! --- POUR CHAQUE TYPE DE POINT D'INTEGRATION -------------------------
! =====================================================================
! --- POUR LES METHODES CLASSIQUE ET LUMPEE ---------------------------
! --- A,C,D,F NE COMPORTENT QUE DES 1 ---------------------------------
! =====================================================================
! --- INITIALISATION --------------------------------------------------
! =====================================================================
    do 201 i = 1, dimdef
        c(i) = 1.d0
        cs(i) = 1.d0
201  end do
    a(1)= 1.d0
    a(2)= 1.d0
    as(1) = 1.d0
    as(2) = 1.d0
! =====================================================================
! --- SI INTEGRATION REDUITE, ON MET A 0 CERTAINS COEFFICIENTS --------
! =====================================================================
    if (modint .eq. 'RED') then
        if (yamec .eq. 1) then
            do 203 i = 1, ndim
                cs(addeme-1+i) = 0.d0
203          continue
            do 213 i = 1, 6
                cs(addeme-1+ndim+i) = 0.d0
213          continue
        endif
        if (yap1 .eq. 1) then
            c(addep1) = 0.d0
            do 204 i = 1, ndim
                cs(addep1-1+1+i) = 0.d0
204          continue
        endif
        if (yap2 .eq. 1) then
            c(addep2) = 0.d0
            do 206 i = 1, ndim
                cs(addep2-1+1+i) = 0.d0
206          continue
        endif
        if (yate .eq. 1) then
            a(2) = 0.d0
            as(1) = 0.d0
            do 207 i = 1, ndim
                cs(addete-1+1+i) = 0.d0
207          continue
        endif
    endif
! ======================================================================
! --- FIN CALCUL C,D,F -------------------------------------------------
! ======================================================================
! --- INITIALISATION DE VECTU, MATUU A 0 SUIVANT OPTION ----------------
! ======================================================================
    if (option(1:9) .ne. 'RIGI_MECA') then
        do 1 i = 1, dimuel
            vectu(i)=0.d0
 1      continue
    endif
! ======================================================================
! --- INITIALISATION DF(MATUU) ET MATRI --------------------------------
! ======================================================================
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        do 3 i = 1, dimuel*dimuel
            matuu(i)=0.d0
 3      continue
!
        call matini(dimmat, dimmat, 0.d0, matri)
!
    endif
! ======================================================================
! --- CALCUL POUR CHAQUE POINT D'INTEGRATION: BOUCLE SUR KPI -----------
! ======================================================================
    loi = ' '
    call rcvala(imate, ' ', 'THM_INIT', 0, ' ',&
                0.d0, 1, 'COMP_THM', rthmc, codmes,&
                1)
    thmc = compor(8)
    if ((rthmc-1.0d0) .lt. r8prem()) then
        loi = 'LIQU_SATU'
    else if ((rthmc-2.0d0).lt.r8prem()) then
        loi = 'GAZ'
    else if ((rthmc-3.0d0).lt.r8prem()) then
        loi = 'LIQU_VAPE'
    else if ((rthmc-4.0d0).lt.r8prem()) then
        loi = 'LIQU_VAPE_GAZ'
    else if ((rthmc-5.0d0).lt.r8prem()) then
        loi = 'LIQU_GAZ'
    else if ((rthmc-6.0d0).lt.r8prem()) then
        loi = 'LIQU_GAZ_ATM'
    else if ((rthmc-9.0d0).lt.r8prem()) then
        loi = 'LIQU_AD_GAZ_VAPE'
    else if ((rthmc-10.0d0).lt.r8prem()) then
        loi = 'LIQU_AD_GAZ'
    endif
    if (thmc .ne. loi) then
        valk(1) = loi
        valk(2) = thmc
        call u2mesk('F', 'ALGORITH_34', 2, valk)
    endif
! =====================================================================
! --- BOUCLE SUR LES POINTS D'INTEGRATION -----------------------------
! =====================================================================
    do 10 ipi = 1, npi
        kpi = ipi
! =====================================================================
! --- CALCUL DE LA MATRICE B AU POINT D'INTEGRATION -------------------
! =====================================================================
        call cabthm(nddls, nddlm, nno, nnos, nnom,&
                    dimuel, dimdef, ndim, kpi, ipoids,&
                    ipoid2, ivf, ivf2, idfde, idfde2,&
                    dfdi, dfdi2, geom, poids, poids2,&
                    b, nmec, yamec, addeme, yap1,&
                    addep1, yap2, addep2, yate, addete,&
                    np1, np2, axi)
! =====================================================================
! --- CALCUL DES DEFORMATIONS GENERALISEES E=BU -----------------------
! =====================================================================
        do 108 i = 1, dimdef
            defgem(i)=0.d0
            defgep(i)=0.d0
            do 109 n = 1, dimuel
                defgem(i)=defgem(i)+b(i,n)*deplm(n)
                defgep(i)=defgep(i)+b(i,n)*deplp(n)
109          continue
108      continue
! ======================================================================
! --- APPEL A LA ROUTINE EQUTHP OU EQUTHM ------------------------------
! ======================================================================
! --- CALCUL DES CONTRAINTES (VIRTUELLES ET GENERALISEES) --------------
! --- ET DE LEURS DERIVEES ---------------------------------------------
! ======================================================================
        if (perman) then
            call equthp(imate, option, ndim, compor, typmod,&
                        kpi, npg, dimdef, dimcon, nbvari,&
                        defgem, contm((kpi-1)*dimcon+1), varim((kpi-1)*nbvari+1), defgep,&
                        contp((kpi-1)*dimcon+1), varip((kpi-1)*nbvari+1), mecani, press1, press2,&
                        tempe, crit, rinstm, rinstp, r,&
                        drds, dsde, codret)
        else
            call equthm(imate, option, ta, ta1, ndim,&
                        compor, typmod, kpi, npg, dimdef,&
                        dimcon, nbvari, defgem, contm((kpi-1)* dimcon+1),&
                        varim((kpi-1)*nbvari+1), defgep, contp((kpi-1)* dimcon+1),&
                        varip((kpi-1)*nbvari+1), mecani, press1, press2, tempe,&
                        crit, rinstm, rinstp, dt, r,&
                        drds, dsde, codret)
! ======================================================================
! --- ATTENTION CI-DESSOUS IL N'Y A PAS D'IMPACT DE CALCUL -------------
! --- ON RECOPIE POUR LA METHODE D'INTEGRATION SELECTIVE LES CONTRAINTES
! --- CALCULEES AUX POINTS DE GAUSS SUR LES NOEUDS POUR DES QUESTIONS DE
! --- POST-TRAITEMENT --------------------------------------------------
! ======================================================================
! --- POUR LES VARIABLES INTERNES ON PASSE PAR COMPOR POUR RECUPERER ---
! --- L'INFORMATION SUR LE NOMBRE DE VI DE LA LOI MECA CORRESPONDANTE --
! --- CETTE IDENTIFICATION SE FAIT NORMALEMENT AU NIVEAU DE NVITHM -----
! --- ATTENTION : NBCOMP EST LE NOMBRE DE VARIABLES DANS LA CARTE COMPOR
! --- DE GRANDEUR_SIMPLE AVANT LA DEFINITION DU NOMBRE DE VARIABLES ----
! --- INTERNES ASSOCIEES AUX RELATIONS DE COMPORTEMENT POUR LA THM -----
! --- SA VALEUR EST A REPRENDRE A L'IDENTIQUE DE LA ROUTINE NVITHM -----
! --- A CE JOUR ELLE VAUT : PARAMETER ( NBCOMP = 7 + 9 ) ---------------
! ======================================================================
            if (mecani(1) .eq. 1) then
                if (kpi .gt. npg) then
                    do 110 i = 1, 6
                        contp((kpi-1)*dimcon+i) = contp((kpi-npg-1)* dimcon+i)
110                  continue
                    nbcomp = 9 + 7
                    read (compor(nbcomp+4),'(I16)') nvim
                    do 112 i = 1, nvim
                        varip((kpi-1)*nbvari+i) = varip((kpi-npg-1)* nbvari+i)
112                  continue
                endif
            endif
        endif
        if (codret .ne. 0) then
            goto 9000
        endif
! ======================================================================
! --- CONTRIBUTION DU POINT D'INTEGRATION KPI A LA MATRICE TANGENTE ET -
! --- AU RESIDU --------------------------------------------------------
! ----------------------------------------------------------------------
! --- MATRICE TANGENTE : REMPLISSAGE EN NON SYMETRIQUE -----------------
! ======================================================================
! --- CHOIX DU JEU DE MATRICES ADAPTE AU POINT D'INTEGRATION -----------
! --- SI KPI<NPG ALORS ON EST SUR UN POINT DE GAUSS: CK = C  -----------
! --- SINON ON EST SUR UN SOMMET                   : CK = CS -----------
! ======================================================================
        if (kpi .le. npg) then
            call lceqvn(dimdef, c, ck)
            call lceqvn(2, a, ak)
        else
            call lceqvn(dimdef, cs, ck)
            call lceqvn(2, as, ak)
        endif
! ======================================================================
! --- CALCUL DE MATUU (MATRI) ------------------------------------------
! --- ON MODIFIE LA 7EME LIGNE (TERME EN TEMPERATURE) DE LA MATRICE ----
! --- DE MANIERE A L'ADAPTER AU PI -------------------------------------
! ======================================================================
        if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            do 199 i = 1, dimdef
                do 198 j = 1, dimcon
                    drdsr(i,j)=drds(i,j)
198              continue
199          continue
!
            if (yate .eq. 1) then
                do 200 i = 1, dimcon
                    drdsr(addete,i) = ak(1)*drds(addete,i) + ak(2)* drds(dimdef+1,i)
200              continue
            endif
! ======================================================================
! --- ON ASSEMBLE: DF=BT.CK.DRDSR.DK.DSDE.FK.B.POIDS -------------------
! ======================================================================
            call pmathm(dimmat, dimdef, dimcon, dimuel, dsde,&
                        drdsr, ck, b, poids, matri)
        endif
! ======================================================================
! --- CALCUL DE VECTUU -------------------------------------------------
! ======================================================================
! --- ON SELECTIONNE LES COMPOSANTES UTILES DE R POUR CE PI ------------
! ======================================================================
        if ((option(1:9).eq.'FULL_MECA' .or. option(1:9) .eq.'RAPH_MECA')) then
            do 20 i = 1, dimdef
                sigbar(i) = ck(i)*r(i)
20          continue
! ======================================================================
! --- ON SELECTIONNE LA BONNE COMPOSANTE 7 POUR CE PI ------------------
! ======================================================================
            if (yate .eq. 1) then
                sigbar(addete) = ak(1)*r(addete) + ak(2)*r(dimdef+1)
            endif
! ======================================================================
! --- ON ASSEMBLE R=BT.SIGBAR.POIDS ------------------------------------
! ======================================================================
            do 117 i = 1, dimuel
                do 118 k = 1, dimdef
                    vectu(i)=vectu(i)+b(k,i)*sigbar(k)*poids
118              continue
117          continue
        endif
10  end do
! ======================================================================
! --- SORTIE DE BOUCLE SUR LES POINTS D'INTEGRATION --------------------
! ======================================================================
    if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        kji=1
        do 115 ii = 1, dimuel
            do 116 jj = 1, dimuel
                matuu(kji) = matri(ii,jj)
                kji= kji + 1
116          continue
115      continue
    endif
! ======================================================================
9000  continue
! ======================================================================
end subroutine
