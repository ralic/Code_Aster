subroutine equthp(imate, option, ndim, compor, typmod,&
                  kpi, npg, dimdef, dimcon, nbvari,&
                  defgem, congem, vintm, defgep, congep,&
                  vintp, mecani, press1, press2, tempe,&
                  crit, rinstm, rinstp, r, drds,&
                  dsde, retcom)
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRP_20
! TOLE CRP_21
! ======================================================================
    implicit none
! ======================================================================
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!     EN MECANIQUE DES MILIEUX POREUX PARTIELLEMENT SATURES
!     AVEC COUPLAGE THM 3D
!
!     VERSION PERMANENTE DE EQUTHM
!
!.......................................................................
! ARGUMENTS D'ENTREE
! C. CHAVANT "ARCHITECTURE THM", 05/01/99 P. 18
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
!                                 (RESI_INTE_PAS == ITEDEC )
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  OPTION  : OPTION DE CALCUL
! IN  DIMDEF  : DIMENSION DU TABLEAU DES DEFORMATIONS GENERALISEES
!               AU POINT DE GAUSS
! IN  DIMCON  : DIMENSION DU TABLEAU DES CONTRAINTES GENERALISEES
!               AU POINT DE GAUSS
! IN  NBVARI  : NOMBRE TOTAL DE VARIABLES INTERNES "MECANIQUES"
! IN  DEFGEP  : TABLEAU DES DEFORMATIONS GENERALISEES
!               AU POINT DE GAUSS AU TEMPS PLUS
! IN  DEFGEM  : TABLEAU DES DEFORMATIONS GENERALISEES
!               AU POINT DE GAUSS AU TEMPS MOINS
!             : EPSXY = (DV/DX+DU/DY)/SQRT(2)
! IN  CONGEM  : TABLEAU DES CONTRAINTES GENERALISEES
!               AU POINT DE GAUSS AU TEMPS MOINS
! IN  VINTM   : TABLEAU DES VARIABLES INTERNES (MECANIQUES ET
!               HYDRAULIQUES)AU POINT DE GAUSS AU TEMPS MOINS
! IN  MECANI  : TABLEAU CONTENANT
!               YAMEC = MECA(1), YAMEC=1 : IL Y A UNE EQUATION MECANIQUE
!               ADDEME = MECA(2), ADRESSE DANS LES TABLEAUX DES DEFORMAT
!               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
!               DEFORMATIONS CORRESPONDANT A LA MECANIQUE
!               ADCOME = MECA(3), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
!               CONTRAINTES CORRESPONDANT A LA MECANIQUE
!               NDEFME = MECA(4), NOMBRE DE DEFORMATIONS MECANIQUES
!               NCONME = MECA(5), NOMBRE DE CONTRAINTES MECANIQUES
! IN  PRESS1    : TABLEAU CONTENANT
!               YAP1 = PRESS1(1), YAP1 = 1 >> IL Y A UNE PREMIERE
!               EQUATION DE PRESSION
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
!               YAP2 = PRESS2(1), YAP2 = 1 >> IL Y A UNE SECONDE
!               EQUATION DE PRESSION
!               ADDEP2 = PRESS2(3), ADRESSE DANS LES TABLEAUX DES DEFORM
!               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
!               DEFORMATIONS CORRESPONDANT A LA SECONDE PRESSION
!               NBPHA2=PRESS2(2) NOMBRE DE PHASES POUR LE CONSTITUANT 2
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
! IN  TEMPE    : TABLEAU CONTENANT
!               YATE = TEMPE(1), YATE=1 : IL Y A UNE EQUATION THERMIQUE
!               ADDETE = TEMPE(2), ADRESSE DANS LES TABLEAUX DES DEFORMA
!               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
!               DEFORMATIONS CORRESPONDANT A LA THERMIQUE
!               ADCOTE = TEMPE(3), ADRESSE DANS LES TABLEAUX DES CONTRAI
!               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
!               CONTRAINTES CORRESPONDANT A LA THERMIQUE
!               NDEFTE = TEMPE(4), NOMBRE DE DEFORMATIONS THERMIQUES
!               NCONTE = TEMPE(5), NOMBRE DE CONTRAINTES THERMIQUES
! OUT CONGEP  : TABLEAU DES CONTRAINTES GENERALISEES
!               AU POINT DE GAUSS AU TEMPS PLUS
!             : SIGXY = LE VRAI
! OUT VINTP   : TABLEAU DES VARIABLES INTERNES (MECANIQUES ET HYDRAULIQU
!               AU POINT DE GAUSS AU TEMPS PLUS
! OUT R       : TABLEAU DES RESIDUS
! OUT DRDE    : TABLEAU DE LA MATRICE TANGENTE AU POINT DE GAUSS
! OUT         : RETCOM RETOUR DES LOIS DE COMPORTEMENT
! ======================================================================
!   -------------------------------------------------------------------
!     SUBROUTINE APPELLEE :
!       DEDIEE A EQUTHP : COMTHM
!     FONCTIONS INTRINSEQUES :
!       SQRT.
!   -------------------------------------------------------------------
!
    include 'asterfort/comthm.h'
    integer :: imate, ndim, nbvari, kpi, npg, dimdef, dimcon, retcom, ibid
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: yamec, addeme, adcome, yate, addete, adcote, i, j
    integer :: yap1, nbpha1, addep1, adcp11, adcp12
    integer :: yap2, nbpha2, addep2, adcp21, adcp22
    real(kind=8) :: defgem(1:dimdef), defgep(1:dimdef), congem(1:dimcon)
    real(kind=8) :: congep(1:dimcon), vintm(1:nbvari), vintp(1:nbvari)
    real(kind=8) :: r(1:dimdef+1), drds(1:dimdef+1, 1:dimcon), pesa(3)
    real(kind=8) :: dsde(1:dimcon, 1:dimdef), crit(*), rinstp, rinstm, rbid
    real(kind=8) :: rbid1(6, 14, 6), rbid2(14, 6)
    real(kind=8) :: deux, rac2
    parameter   (deux = 2.d0)
    logical :: perman
    character(len=8) :: typmod(2)
    character(len=16) :: option, compor(*)
! ======================================================================
! --- INITIALISATIONS DES VARIABLES DEFINISSANT LE PROBLEME ------------
! ======================================================================
    perman = .true.
    rac2 = sqrt(deux)
    yamec = mecani(1)
    addeme = mecani(2)
    adcome = mecani(3)
    yap1 = press1(1)
    nbpha1 = press1(2)
    addep1 = press1(3)
    adcp11 = press1(4)
    adcp12 = press1(5)
    yap2 = press2(1)
    nbpha2 = press2(2)
    addep2 = press2(3)
    adcp21 = press2(4)
    adcp22 = press2(5)
    yate = tempe(1)
    addete = tempe(2)
    adcote = tempe(3)
!
    ibid = 0
    rbid = 0.d0
! ============================================================
! --- COMME CONGEM CONTIENT LES VRAIES CONTRAINTES ET --------
! --- COMME PAR LA SUITE ON TRAVAILLE AVEC SQRT(2)*SXY -------
! --- ON COMMENCE PAR MODIFIER LES CONGEM EN CONSEQUENCE -----
! ============================================================
    if (yamec .eq. 1) then
        do 100 i = 4, 6
            congem(adcome+i-1)= congem(adcome+i-1)*rac2
100      continue
    endif
! ============================================================
! --- INITIALISATION DES TABLEAUX A ZERO ---------------------
! --- ET DU TABLEAU CONGEP A CONGEM --------------------------
! ============================================================
    if ((option .eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        do 1 i = 1, dimcon
            congep(i)=congem(i)
 1      continue
    endif
!
    do 2 i = 1, dimdef
        do 3 j = 1, dimcon
            drds(i,j)=0.d0
            dsde(j,i)=0.d0
 3      continue
        r(i)=0.d0
 2  end do
! ======================================================================
! --- INITIALISATION DE LA COMPOSANTE ADDITIONNELLE DE R ET DF ---------
! --- DUE A LA DECOMPOSITION DU TERME EN TEMPERATURE (GAUSS & SOMMET): -
! --- UNE PARTIE DE LA COMPOSANTE 7 DE R(ET LA LIGNE 7 DE DF) S'INTEGRE-
! --- AU SOMMET ET L'AUTRE AU PT DE GAUSS ------------------------------
! --- ON DECIDE D'ENVOYER LA COMPOSANTE R7SOMMET A L'ADRESSE DIMDEF+1 --
! --- ET DE LAISSER R7GAUSS A L'ADRESSE ADDETE -------------------------
! ======================================================================
    r(dimdef+1)=0.d0
!
    do 800 j = 1, dimcon
        drds(dimdef+1,j)=0.d0
800  end do
!
!
    call comthm(option, perman, .false., ibid, rbid1,&
                rbid2, imate, typmod, compor, crit,&
                rinstm, rinstp, ndim, dimdef, dimcon,&
                nbvari, yamec, yap1, yap2, yate,&
                addeme, adcome, addep1, adcp11, adcp12,&
                addep2, adcp21, adcp22, addete, adcote,&
                defgem, defgep, congem, congep, vintm,&
                vintp, dsde, pesa, retcom, kpi,&
                npg, rbid, rbid)
    if (retcom .ne. 0) then
        goto 9000
    endif
! ======================================================================
! --- CALCUL DE LA CONTRAINTE VIRTUELLE R ------------------------------
! ======================================================================
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RAPH_MECA')) then
! ======================================================================
! --- SI PRESENCE DE MECANIQUE -----------------------------------------
! ======================================================================
        if (yamec .eq. 1) then
!
!  CONTRIBUTIONS A R2 INDEPENDANTE DE YAP1 , YAP2 ET YATE
!  CONTRAINTES SIGPRIMPLUS PAGE 33
!
            do 6 i = 1, 6
                r(addeme+ndim+i-1)= r(addeme+ndim+i-1) +congep(&
                adcome-1+i)
 6          continue
!  SCALAIRE SIGPPLUS MULTIPLIE PAR LE TENSEUR UNITE PAGE 33
            do 7 i = 1, 3
                r(addeme+ndim-1+i)=r(addeme+ndim-1+i) +congep(&
                adcome+6)
 7          continue
        endif
! ======================================================================
! --- SI PRESENCE DE PRESS1 --------------------------------------------
! ======================================================================
        if (yap1 .eq. 1) then
!
!  CONTRIBUTIONS A R4 DEPENDANTES DE YAP1
            do 12 i = 1, ndim
                r(addep1+i)=r(addep1+i)+congep(adcp11+i-1)
12          continue
            if (nbpha1 .gt. 1) then
                do 13 i = 1, ndim
                    r(addep1+i)=r(addep1+i)+congep(adcp12+i-1)
13              continue
            endif
! ======================================================================
! --- ON NE PASSE JAMAIS DANS CETTE BOUCLE -----------------------------
! --- ON LA LAISSE POUR UNE EVENTUELLE MODELISATION THM PERMANENTE -----
! ======================================================================
            if (yate .eq. 1) then
!
!   CONTRIBUTIONS A R7 !!SOMMET!! DEPENDANTES DE YAP1
!   PRODUITS ENTHALPIE MASSIQUE - APPORTS MASSE FLUIDE
                r(dimdef+1)= r(dimdef+1)-congep(adcp11)*congep(adcp11+&
                ndim+1)
!
!        PRODUITS ENTHALPIE MASSIQUE - APPORTS MASSE FLUIDE
!        CONTRIBUTION SECONDE PHASE DU FLUIDE 1
                if (nbpha1 .gt. 1) then
                    r(dimdef+1)= r(dimdef+1)-congep(adcp12)*congep(&
                    adcp12+ndim+1)
                endif
!
!    CONTRIBUTION A R7 !!GAUSS!!
!    PRODUIT SCALAIRE GRAVITE VECTEURS COURANTS DE MASSE FLUIDE
!        PESA . MFL11
                do 14 i = 1, ndim
                    r(addete)=r(addete)+congep(adcp11+i)*pesa(i)
14              continue
!        PESA . MFL12
                if (nbpha1 .gt. 1) then
                    do 15 i = 1, ndim
                        r(addete)=r(addete)+congep(adcp12+i)*pesa(i)
15                  continue
                endif
!
!    CONTRIBUTIONS A R8 DEPENDANTES DE YAP1
!    PRODUITS ENTHALPIE MASSIQUE - VECTEURS COURANT DE MASSE FLUIDE
                do 16 i = 1, ndim
                    r(addete+i)= r(addete+i)+congep(adcp11+ndim+1)*&
                    congep(adcp11+i)
16              continue
                if (nbpha1 .gt. 1) then
!        PRODUITS ENTHALPIE MASSIQUE - VECTEURS COURANT DE MASSE FLUID
!        CONTRIBUTION SECONDE PHASE DU FLUIDE 1
                    do 17 i = 1, ndim
                        r(addete+i)= r(addete+i)+congep(adcp12+ndim+1)&
                        *congep(adcp12+i)
17                  continue
                endif
!
            endif
! ======================================================================
!
        endif
! ======================================================================
! --- SI PRESENCE DE PRESS2 --------------------------------------------
! ======================================================================
        if (yap2 .eq. 1) then
!
!    CONTRIBUTIONS A R6 DEPENDANTES DE YAP2
            do 18 i = 1, ndim
                r(addep2+i)=r(addep2+i)+congep(adcp21+i-1)
18          continue
            if (nbpha2 .gt. 1) then
                do 19 i = 1, ndim
                    r(addep2+i)=r(addep2+i)+congep(adcp22+i-1)
19              continue
            endif
! ======================================================================
! --- ON NE PASSE JAMAIS DANS CETTE BOUCLE -----------------------------
! --- ON LA LAISSE POUR UNE EVENTUELLE MODELISATION THM PERMANENTE -----
! ======================================================================
            if (yate .eq. 1) then
!
!    CONTRIBUTIONS A R7GAUSS
!    PRODUIT SCALAIRE GRAVITE VECTEURS COURANTS DE MASSE FLUIDE
!         PESA . MFL21
                do 20 i = 1, ndim
                    r(addete)=r(addete)+congep(adcp21+i)*pesa(i)
20              continue
!
!         PESA . MFL22
                if (nbpha2 .gt. 1) then
                    do 21 i = 1, ndim
                        r(addete)=r(addete)+congep(adcp22+i)*pesa(i)
21                  continue
                endif
!
!    CONTRIBUTIONS A R8 DEPENDANTES DE YAP2
!    PRODUITS ENTHALPIE MASSIQUE - VECTEURS COURANT DE MASSE FLUIDE
                do 22 i = 1, ndim
                    r(addete+i)= r(addete+i)+congep(adcp21+ndim+1)*&
                    congep(adcp21+i)
22              continue
!
!          PRODUITS ENTHALPIE MASSIQUE - VECTEURS COURANT
!          DE MASSE FLUIDE
!          CONTRIBUTION SECONDE PHASE DU FLUIDE 1
                if (nbpha2 .gt. 1) then
                    do 23 i = 1, ndim
                        r(addete+i)= r(addete+i)+congep(adcp22+ndim+1)&
                        *congep(adcp22+i)
23                  continue
                endif
            endif
! ======================================================================
        endif
! ======================================================================
! --- SI PRESENCE DE THERMIQUE -----------------------------------------
! ======================================================================
        if (yate .eq. 1) then
!
!   CONTRIBUTION A R8 INDEPENDANTE DE YAMEC , YAP1 , YAP2
!         >>>>   VECTEUR COURANT DE CHALEUR
            do 24 i = 1, ndim
                r(addete+i)=r(addete+i)+congep(adcote+i)
24          continue
        endif
    endif
! ======================================================================
! --- CALCUL DES MATRICES DERIVEES CONSTITUTIVES DE DF -----------------
! ======================================================================
    if ((option(1:9) .eq. 'RIGI_MECA') .or. (option(1:9) .eq. 'FULL_MECA')) then
! ======================================================================
! --- SI PRESENCE DE MECANIQUE -----------------------------------------
! ======================================================================
        if (yamec .eq. 1) then
!
!    CONTRIBUTIONS A DR/DS INDEPENDANTES DE YAP1 ET YATE
!    CALCUL DE DR2/DS :
!    DR2DS:DERIVEES PAR RAPPORT AUX CONTRAINTES SIGPRIMPLUSIJ
!    TABLEAU 6 - 6 : ON N'ECRIT QUE LES TERMES NON NULS
!    (1 SUR DIAGONALE)
!
            do 25 i = 1, 6
                drds(addeme+ndim-1+i,adcome+i-1)= drds(addeme+ndim-1+&
                i,adcome+i-1)+1.d0
25          continue
!    DR2DS:DERIVEES PAR RAPPORT AU SCALAIRE SIGPPLUS
!    >> TENSEUR ISOTROPE : ON N'ECRIT QUE LES
!    TROIS PREMIERS TERMES = 1
!
            do 26 i = 1, 3
                drds(addeme+ndim-1+i,adcome+6)= drds(addeme+ndim-1+i,&
                adcome+6)+1.d0
26          continue
        endif
! ======================================================================
! --- SI PRESENCE DE PRESS1 --------------------------------------------
! ======================================================================
        if (yap1 .eq. 1) then
!
!     DR4P11:DERIVEE / COURANTM11PLUS  (VECTEUR COURANT MASSE FLUIDE)
            do 28 i = 1, ndim
                drds(addep1+i,adcp11+i-1)= drds(addep1+i,adcp11+i-1)+&
                1.d0
28          continue
!
            if (nbpha1 .gt. 1) then
!
!     DR4P12:DERIVEE / COURANTM12PLUS  (VECTEUR COURANT MASSE FLUIDE)
                do 30 i = 1, ndim
                    drds(addep1+i,adcp12+i-1)= drds(addep1+i,adcp12+i-&
                    1)+1.d0
30              continue
            endif
        endif
! ======================================================================
! --- SI PRESENCE DE PRESS2 --------------------------------------------
! ======================================================================
        if (yap2 .eq. 1) then
!
!     DR6P21:DERIVEE / COURANTM21PLUS  (VECTEUR COURANT MASSE FLUIDE)
            do 32 i = 1, ndim
                drds(addep2+i,adcp21+i-1)= drds(addep2+i,adcp21+i-1)+&
                1.d0
32          continue
!
            if (nbpha2 .gt. 1) then
!
!     DR6P22:DERIVEE / COURANTM22PLUS  (VECTEUR COURANT MASSE FLUIDE)
                do 34 i = 1, ndim
                    drds(addep2+i,adcp22+i-1)= drds(addep2+i,adcp22+i-&
                    1)+1.d0
34              continue
            endif
        endif
! ======================================================================
! --- SI PRESENCE DE THERMIQUE -----------------------------------------
! ======================================================================
        if (yate .eq. 1) then
!
!     DR7SOMMET/DT:DERIVEE / QPRIMPLUS  (APPORT DE CHALEUR  REDUIT )
            drds(dimdef+1,adcote)=drds(dimdef+1,adcote)-1.d0
!
!     DR8DT:DERIVEE / QPLUS  (VECTEUR COURANT DE CHALEUR)
            do 35 i = 1, ndim
                drds(addete+i,adcote+i)=drds(addete+i,adcote+i)+1.d0
35          continue
!
            if (yap1 .eq. 1) then
!
!     DR7SOMMET/P11:DERIVEE / M11PLUS  (APPORT MASSE FLUIDE 1)
!
                drds(dimdef+1,adcp11)= drds(dimdef+1,adcp11)-congep(&
                adcp11+ndim+1)
!
!     DR7GAUSS/P11:DERIVEE/COURANTM11PLUS : VECTEUR COURANT MASSE FLUIDE
                do 351 i = 1, ndim
                    drds(addete,adcp11+i)=drds(addete,adcp11+i)+pesa(&
                    i)
351              continue
!
!     DR8P11:DERIVEE/HM11PLUS  (ENTHALPIE MASSIQUE DU FLUIDE 1)
                do 36 i = 1, ndim
                    drds(addete+i,adcp11+ndim+1)=drds(addete+i,adcp11+&
                    ndim+1) +congep(adcp11+i)
36              continue
!
!     DR8P11:DERIVEE/COURANTM11PLUS : VECTEUR COURANT MASSE FLU1
                do 37 i = 1, ndim
                    drds(addete+i,adcp11+i)=drds(addete+i,adcp11+i)&
                    +congep(adcp11+ndim+1)
37              continue
!
                if (nbpha1 .gt. 1) then
!
!  R7GAUSS/P12:DERIVEE/COURANTM12PLUS : VECTEUR COURANT MASSE FL1 PH 2
                    do 371 i = 1, ndim
                        drds(addete,adcp12+i)=drds(addete,adcp12+i)+&
                        pesa(i)
371                  continue
!
!     DR8P12:DERIVEE/HM12PLUS (ENTHALPIE MASSIQUE DU FLUIDE 1 PHASE 2)
                    do 38 i = 1, ndim
                        drds(addete+i,adcp12+ndim+1)= drds(addete+i,&
                        adcp12+ndim+1)+congep(adcp12+i)
38                  continue
!
!     DR8P12:DERIVEE/COURANTM12PLUS : VECTEUR COURANT MASSE FL 1 PHASE 2
                    do 39 i = 1, ndim
                        drds(addete+i,adcp12+i)=drds(addete+i,adcp12+&
                        i) +congep(adcp12+ndim+1)
39                  continue
                endif
            endif
!
            if (yap2 .eq. 1) then
!
!  DR7GAUSS/P11:DERIVEE/COURANTM11PLUS : VECTEUR COURANT MASSE FLUIDE 1
                do 391 i = 1, ndim
                    drds(addete,adcp21+i)=drds(addete,adcp21+i)+pesa(&
                    i)
391              continue
!
!     DR8P21:DERIVEE/HM21PLUS  (ENTHALPIE MASSIQUE DU FLUIDE 2)
                do 40 i = 1, ndim
                    drds(addete+i,adcp21+ndim+1)=drds(addete+i,adcp21+&
                    ndim+1) +congep(adcp21+i)
40              continue
!
!     DR8P21:DERIVEE/COURANTM21PLUS : VECTEUR COURANT MASSE FLUIDE 2
                do 41 i = 1, ndim
                    drds(addete+i,adcp21+i)=drds(addete+i,adcp21+i)&
                    +congep(adcp21+ndim+1)
41              continue
!
                if (nbpha2 .gt. 1) then
!
! DR7GAUSS/P22:DERIVEE/COURANTM12PLUS:VECTEUR COURANT MASSE FL 2 PH 2
                    do 411 i = 1, ndim
                        drds(addete,adcp22+i)=drds(addete,adcp22+i)+&
                        pesa(i)
411                  continue
!     DR8P22:DERIVEE/HM22PLUS (ENTHALPIE MASSIQUE DU FLUIDE 2 PHASE 2)
                    do 42 i = 1, ndim
                        drds(addete+i,adcp22+ndim+1)= drds(addete+i,&
                        adcp22+ndim+1)+congep(adcp22+i)
42                  continue
!
!     DR8P22:DERIVEE/COURANTM22PLUS : VECTEUR COURANT MASSE FL 2 PHASE 2
                    do 43 i = 1, ndim
                        drds(addete+i,adcp22+i)=drds(addete+i,adcp22+&
                        i) +congep(adcp22+ndim+1)
43                  continue
                endif
            endif
!
        endif
!
    endif
! ======================================================================
! --- FIN DU CALCUL DE DF ----------------------------------------------
! ======================================================================
! --- COMME CONGEP DOIT FINALEMENT CONTENIR LES VRAIES CONTRAINTES -----
! --- ET COMME ON A TRAVAILLE AVEC SQRT(2)*SXY -------------------------
! --- ON MODIFIE LES CONGEP EN CONSEQUENCE -----------------------------
! ======================================================================
    if ((yamec.eq.1) .and. ((option .eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA'))) then
        do 110 i = 4, 6
            congep(adcome+i-1)= congep(adcome+i-1)/rac2
110      continue
    endif
! ======================================================================
9000  continue
! ======================================================================
end subroutine
