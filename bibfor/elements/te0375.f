      SUBROUTINE TE0375(OPTION,NOMTE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2009   AUTEUR GNICOLAS G.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE DELMAS J.DELMAS
C TOLE CRP_20
C
C     BUT:
C       CALCUL DE L'INDICATEUR D'ERREUR EN MECANIQUE 3D AVEC LA
C       METHODE DES RESIDUS EXPLICITES.
C       OPTION : 'ERRE_ELEM_SIGM'
C
C REMARQUE : LES PROGRAMMES SUIVANTS DOIVENT RESTER TRES SIMILAIRES
C            TE0368, TE0375, TE0377, TE0378, TE0382, TE0497
C
C ----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16 OPTION,NOMTE
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C DECLARATION VARIABLES LOCALES
C
      INTEGER IFM,NIV
      INTEGER IADZI,IAZK24
      INTEGER IBID,IAUX,IRET,ITAB(7)
      INTEGER NOE(9,6,4)
      INTEGER IGEOM,JTIME
      INTEGER IERR, IVOIS
      INTEGER IMATE
      INTEGER IAD
      INTEGER IFOVR, IFOVF
      INTEGER IPES,IROT
      INTEGER IREF1,IREF2
      INTEGER NDIM
      INTEGER NNO , NNOS , NPG , IPOIDS, IVF , IDFDE , JGANO
      INTEGER NDIMF
      INTEGER NNOF, NNOSF, NPGF, IPOIDF, IVFF, IDFDXF, JGANOF
      INTEGER NNO2, NNOS2, NPG2, IPOID2, IVF2, IDFDX2, JGANO2
      INTEGER NBCMP
      INTEGER IPG
      INTEGER IPGF
      INTEGER NBF
      INTEGER TYMVOL,NDEGRE,IFA,TYV

      REAL*8 R8BID,R8BID2,R8BID3(3),R8BID4(3)
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),HK,POIDS
      REAL*8 FPX,FPY,FPZ
      REAL*8 FRX(27),FRY(27),FRZ(27)
      REAL*8 FOVO(3)
      REAL*8 DSX,DSY,DSZ
      REAL*8 ERREST,NOR,NORSIG,SIGCAL,NUEST,COEFF
      REAL*8 TER1,TER2,TER3,HF,INTE,INST
      REAL*8 NX(9),NY(9),NZ(9),JACO(9)
      REAL*8 CHX(9),CHY(9),CHZ(9)
      REAL*8 DSG11(9),DSG22(9),DSG33(9),DSG12(9),DSG13(9),DSG23(9)
      REAL*8 SIG11(9),SIG22(9),SIG12(9),SIG33(9),SIG13(9),SIG23(9)
      REAL*8 RHO,VALRES(1)

      CHARACTER*2 CODRET(2)
      CHARACTER*8 TYPMAV, ELREFE
      CHARACTER*8 ELREFF, ELREFB
      CHARACTER*8 NOMPAR(1)
      CHARACTER*16 PHENOM
      CHARACTER*24 VALK(2)

      LOGICAL YAPR, YARO

C --- INITIALISATION DU TABLEAU DES NUMEROS DE NOEUDS FACE PAR FACE ----
C
C     NOE (IN,IFA,TYMVOL) : IN     : NUMERO DU NOEUD DANS LA FACE
C                           IFA    : NUMERO DE LA FACE
C                           TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
C                                    1 : HEXAEDRE A 8,20 ET 27 NOEUDS
C                                    2 : PENTAEDRE A 6 ET 15 NOEUDS
C                                    3 : TETRAEDRE A 4 ET 10 NOEUDS
C                                    4 : PYRAMIDE A 5 ET 13 NOEUDS
C     VOIR TE003 POUR LES EXPLICATIONS DETAILLEES
C
      DATA NOE/1,4,3,2,12,11,10, 9,21, 1,2,6,5, 9,14,17,13,22,
     >         2,3,7,6,10,15,18,14,23, 3,4,8,7,11,16,19,15,24,
     >         4,1,5,8,12,13,20,16,25, 5,6,7,8,17,18,19,20,26,
     >         1,3,2,9,8,7, 3*0,       4,5,6,13,14,15, 3*0,
     >         1,2,5,4, 7,11,13,10, 0, 2,3,6,5,8,12,14,11,0,
     >         1,4,6,3,10,15,12, 9, 0,  9*0,
     >         1,3,2,7,6, 5, 3*0,      2,3,4,6,10,9, 3*0,
     >         3,1,4,7,8,10, 3*0,      1,2,4,5, 9,8, 3*0,
     >         9*0,                    9*0,
     >         1,2,5,6,11,10, 3*0,     2,3,5,7,12,11, 3*0,
     >         3,4,5,8,13,12, 3*0,     4,1,5,9,10,13, 3*0,
     >         1,4,3,2,9,8,7,6, 0,     9*0 /
C
C ----------------------------------------------------------------------
 1000 FORMAT(A,' :',(6(1X,1PE17.10)))
 2000 FORMAT(A,10I8)
C ----------------------------------------------------------------------
C 1 -------------- GESTION DES DONNEES ---------------------------------
C ----------------------------------------------------------------------
      CALL JEMARQ()

      CALL INFNIV(IFM,NIV)

C 1.1. --- LES INCONTOURNABLES
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVOISIN','L',IVOIS)
C
      CALL JEVECH('PTEMPSR','L',JTIME)
      INST=ZR(JTIME-1+1)
C
      CALL JEVECH('PERREUR','E',IERR)

C 1.2. --- LES CARACTERISTIQUES DE LA MAILLE EN COURS

      CALL TECAEL(IADZI,IAZK24)
      VALK(1)=ZK24(IAZK24-1+3)
      VALK(2)=OPTION

      CALL ELREF1(ELREFE)

      IF ( NIV.GE.2 ) THEN
      WRITE(IFM,*) ' '
      WRITE(IFM,*) '================================================='
      WRITE(IFM,*) ' '
      WRITE(IFM,*) 'MAILLE NUMERO', ZI(IADZI),', DE TYPE ', ELREFE
      ENDIF

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C 1.3. --- CHAMP DE CONTRAINTES

      CALL TECACH('OOO','PCONTNO',3,ITAB,IRET)
      IAD=ITAB(1)
      NBCMP=ITAB(2)/NNO
C
C 1.4. --- CARTES DE PESANTEUR ET ROTATION
C
      CALL TECACH('ONN','PPESANR',1,ITAB,IRET)
      IF ( ITAB(1).NE.0 ) THEN
        CALL JEVECH('PPESANR','L',IPES)
        YAPR = .TRUE.
      ELSE
        YAPR = .FALSE.
      ENDIF
      CALL TECACH('ONN','PROTATR',1,ITAB,IRET)
      IF ( ITAB(1).NE.0 ) THEN
        CALL JEVECH('PROTATR','L',IROT)
        YARO = .TRUE.
      ELSE
        YARO = .FALSE.
      ENDIF
C
C 1.5. --- FORCES VOLUMIQUES EVENTUELLES
C          VALEURS REELLES ?
      CALL TECACH('ONN','PFRVOLU',1,IFOVR,IRET)
C          OU FONCTIONS ?
      IF ( IFOVR.EQ.0 ) THEN
        CALL TECACH('ONN','PFFVOLU',1,IFOVF,IRET)
      ELSE
        IFOVF = 0
      ENDIF
CGN      WRITE(IFM,2000) 'IFOVR', IFOVR
CGN      WRITE(IFM,2000) 'IFOVF', IFOVF
C
C 1.6. --- FORCES ET PRESSIONS AUX BORDS
C
      CALL JEVECH('PFORCE','L',IREF1)
C
      CALL JEVECH('PPRESS','L',IREF2)
C
C 1.7. --- MATERIAU SI BESOIN
C
      IF ( YAPR .OR. YARO ) THEN
C
        CALL JEVECH('PMATERC','L',IMATE)
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        NOMPAR(1)='RHO'
        CALL RCVALA ( ZI(IMATE), ' ', PHENOM, 1, ' ', R8BID,
     >                1, NOMPAR, VALRES, CODRET, 'FM')
        RHO = VALRES(1)
CGN        WRITE(IFM,1000) 'RHO', RHO
C
      ENDIF

C ----------------------------------------------------------------------
C 2 -------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
C ----------------------------------------------------------------------
C
C 2.1. --- CALCUL DU DIAMETRE HK DE LA MAILLE ----
C
      CALL UTHK(NOMTE,IGEOM,HK,NDIM,ITAB,IBID,IBID,IBID,NIV,IFM)
C
C 2.2. --- CALCUL DE LA FORCE DE PESANTEUR ---
C
      IF ( YAPR ) THEN
        FPX=RHO*ZR(IPES)*ZR(IPES+1)
        FPY=RHO*ZR(IPES)*ZR(IPES+2)
        FPZ=RHO*ZR(IPES)*ZR(IPES+3)
      ELSE
        FPX=0.D0
        FPY=0.D0
        FPZ=0.D0
      ENDIF
CGN      WRITE(IFM,1000) 'P',FPX,FPY,FPZ
C
C 2.3. --- CALCUL DE LA FORCE DE ROTATION ---
C
      IF ( YARO ) THEN
        CALL RESR3D (ZR(IROT),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     >               FRX,FRY,FRZ)
      ELSE
        DO 23 , IPG = 1 , NPG
          FRX(IPG) = 0.D0
          FRY(IPG) = 0.D0
          FRZ(IPG) = 0.D0
   23   CONTINUE
      ENDIF
CGN      WRITE(IFM,1000) 'R X',(FRX(IPG),IPG = 1 , NPG)
CGN      WRITE(IFM,1000) 'R Y',(FRY(IPG),IPG = 1 , NPG)
CGN      WRITE(IFM,1000) 'R Z',(FRZ(IPG),IPG = 1 , NPG)
C
C 2.4. --- CALCUL DE LA FORCE VOLUMIQUE EVENTUELLE ---
C
      IF ( IFOVR.NE.0 ) THEN
        FOVO(1) = ZR(IFOVR  )
        FOVO(2) = ZR(IFOVR+1)
        FOVO(3) = ZR(IFOVR+2)
C
      ELSEIF ( IFOVF.NE.0 ) THEN
        NOMPAR(1) = 'INST'
        R8BID3(1) = INST
C       SI UNE COMPOSANTE N'A PAS ETE DECRITE, ASTER AURA MIS PAR
C       DEFAUT LA FONCTION NULLE &FOZERO. ON LE REPERE POUR
C       IMPOSER LA VALEUR 0 SANS FAIRE DE CALCULS INUTILES
        DO 24 , IBID = 1 , NDIM
          IF ( ZK8(IFOVF+IBID-1)(1:7).EQ.'&FOZERO' ) THEN
            FOVO(IBID) = 0.D0
          ELSE
            CALL FOINTE('FM',ZK8(IFOVF+IBID-1),1,NOMPAR,R8BID3,
     >                   FOVO(IBID),IRET)
          ENDIF
   24   CONTINUE
CGN        WRITE(IFM,*) 'F X : ',ZK8(IFOVF),FOVO(1)
CGN        WRITE(IFM,*) 'F Y : ',ZK8(IFOVF+1),FOVO(2)
CGN        WRITE(IFM,*) 'F Z : ',ZK8(IFOVF+2),FOVO(3)
      ENDIF
C
C 2.5. --- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS ---
C
      TER1 = 0.D0
      NORSIG = 0.D0
C
      DO 25 , IPG = 1 , NPG
C
C ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X, /Y ET /Z -----
C
        CALL DFDM3D(NNO,IPG,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA --------------
C
        CALL ERMEV3(NNO,IPG,IVF,IAD,NBCMP,DFDX,DFDY,DFDZ,
     >              DSX,DSY,DSZ,NOR)
C
C ------- CUMUL
C
        R8BID3(1) = FPX + FRX(IPG) + DSX
        R8BID3(2) = FPY + FRY(IPG) + DSY
        R8BID3(3) = FPZ + FRZ(IPG) + DSZ
C
C ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE EVENTUEL ---------------
C
        IF ( IFOVR.NE.0 .OR. IFOVF.NE.0 ) THEN
C
CGN          WRITE(IFM,1000) 'F X', FOVO(1)
CGN          WRITE(IFM,1000) 'F Y', FOVO(2)
CGN          WRITE(IFM,1000) 'F Z', FOVO(3)
          R8BID3(1) = R8BID3(1) + FOVO(1)
          R8BID3(2) = R8BID3(2) + FOVO(2)
          R8BID3(3) = R8BID3(3) + FOVO(3)
C
        ENDIF
C
C ------- CUMUL DU TERME D'ERREUR
C
        TER1 = TER1
     >       + ( R8BID3(1)**2 + R8BID3(2)**2 + R8BID3(3)**2 ) * POIDS
        IF ( NIV.GE.2 ) THEN
          WRITE(IFM,1000) 'POIDS', POIDS
          WRITE(IFM,1000) 'A2 + B2 + C2',
     >                    R8BID3(1)**2 + R8BID3(2)**2 + R8BID3(3)**2
          WRITE(IFM,1000) '==> TER1    ', TER1
        ENDIF
C
C ------- CALCUL DE LA NORME DE SIGMA SUR L'ELEMENT --------------------
C
        NORSIG = NORSIG + NOR*POIDS
C
   25 CONTINUE
C
      IF (TER1.LT.0.D0 ) THEN
        CALL U2MESK('A','INDICATEUR_9',2,VALK)
        GOTO 9999
      ENDIF
C
      TER1=HK*SQRT(TER1)
C
C ----------------------------------------------------------------------
C ------------ FIN DU CALCUL DU PREMIER TERME DE L'ERREUR --------------
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C 3. ------- CALCUL DES DEUXIEME ET TROISIEME TERMES DE L'ERREUR -------
C ----------------------------------------------------------------------
C
C 3.1. ---- INFORMATIONS SUR LA MAILLE COURANTE : ----------------------
C       TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
C       NDEGRE : DEGRE DE L'ELEMENT
C       NBF    : NOMBRE DE FACES DE LA MAILLE VOLUMIQUE
C       ELREFF : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 1
C       ELREFB : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 2
C      --- REMARQUE : ON IMPOSE UNE FAMILLE DE POINTS DE GAUSS
C
      CALL ELREF7 ( ELREFE,
     >              TYMVOL, NDEGRE, NBF, ELREFF, ELREFB )
CGN      WRITE(6,*) 'TYPE MAILLE VOLUMIQUE COURANTE :',TYMVOL
C --- CARACTERISTIQUES DES FACES DE BORD DE LA FAMILLE 1 ---------------
      CALL ELREF4 ( ELREFF, 'NOEU',
     >              NDIMF, NNOF, NNOSF, NPGF, IPOIDF, IVFF,
     >              IDFDXF, JGANOF )
CGN      WRITE(IFM,2000) 'NDIMF',NDIMF
CGN      WRITE(IFM,2000) 'NNOSF,NNOF,NPGF',NNOSF,NNOF,NPGF
CGN      WRITE(IFM,1000) 'IPOIDF', (ZR(IPOIDF+IFA),IFA=0,NPGF-1)
C
C --- COMPLEMENT EVENTUEL POUR LES MAILLES QUI ONT 2 TYPES DE ---
C --- MAILLES DE BORD (PENTAEDRE, PYRAMIDE) ---
C
      IF ( ELREFB(1:1).NE.' ' ) THEN
         CALL ELREF4 ( ELREFB, 'NOEU',
     >                 NDIMF, NNO2, NNOS2, NPG2, IPOID2, IVF2,
     >                 IDFDX2, JGANO2 )
CGN       WRITE(IFM,2000) 'NDIMF,NNO2',NDIMF,NNO2
CGN       WRITE(IFM,2000) 'NNOS2,NPG2',NNOS2,NPG2
CGN       WRITE(IFM,1000) 'IPOID2', (ZR(IPOID2+IFA),IFA=0,NPG2-1)
      ENDIF
C
C 3.2. --- BOUCLE SUR LES FACES DE LA MAILLE VOLUMIQUE --------------
C
      TER2 = 0.D0
      TER3 = 0.D0
      DO 320 , IFA = 1 , NBF
C
C ------TEST DU TYPE DE VOISIN -----------------------------------------
C
        TYV=ZI(IVOIS+7+IFA)
C
        IF ( TYV.NE.0 ) THEN
C
C ------- RECUPERATION DU TYPE DE LA MAILLE VOISINE
C
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',TYV),TYPMAV)
          IF ( NIV.GE.2  ) THEN
            WRITE(IFM,1003)  IFA, ZI(IVOIS+IFA), TYPMAV
 1003 FORMAT (I2,'-EME FACE DE NUMERO',I10,' ==> TYPMAV = ', A)
            WRITE(IFM,1000) 'TER2', TER2
            WRITE(IFM,1000) 'TER3', TER3
          ENDIF
C
C --- QUAND ON ARRIVE AUX FACES QUAD DES PENTAEDRES OU DES PYRAMIDES ---
C --- IL FAUT REMPLACER LES CARACTERISTIQUES DE LA FAMILLE 1         ---
C --- PAR CELLES DE LA FAMILLE 2                                     ---
C
          IF ( ( TYMVOL.EQ.2 .AND. IFA.GE.3 ) .OR.
     >         ( TYMVOL.EQ.4 .AND. IFA.GE.5 ) ) THEN
C
            NNOF   = NNO2
            NPGF   = NPG2
            NNOSF  = NNOS2
            IPOIDF = IPOID2
            IDFDXF = IDFDX2
C
          ENDIF
CGN      WRITE(IFM,*) '. NPGF =', NPGF
C
C ----- CALCUL DU DIAMETRE HF DE LA FACE ----------
C
          IBID=0
          CALL UTHK(NOMTE,IGEOM,HF,IBID,NOE,NNOSF,TYMVOL,IFA,NIV,IFM)
C
C ------- CALCUL DE NORMALES ET JACOBIENS AUX POINTS DE GAUSS ----------
C
          IAUX = IFA
          CALL CALNOR ('3D', IGEOM,
     >                  IBID, IBID, IBID, R8BID,
     >                  NNOF, NPGF, NOE, IAUX, TYMVOL, IDFDXF,
     >                  JACO, NX, NY, NZ,
     >                  R8BID3, R8BID4, R8BID2 )
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
C --------------- LE BORD VOISIN EST UN VOLUME -------------------------
C ----------------------------------------------------------------------
C
          IF ( TYPMAV(1:4).EQ.'HEXA' .OR.
     >         TYPMAV(1:4).EQ.'PENT' .OR.
     >         TYPMAV(1:4).EQ.'TETR' .OR.
     >         TYPMAV(1:4).EQ.'PYRA' ) THEN
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS ------------------
C ------- CE CHAMP DSGXX EST EXPRIME SUR LES NOEUDS DE LA FACE ---------
C
            CALL ERMES3(NOE,IFA,TYMVOL,NNOF,TYPMAV,
     >                  IREF1,IVOIS,IAD,NBCMP,
     >                  DSG11,DSG22,DSG33,DSG12,DSG13,DSG23)
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
C ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
C
            DO 321 , IPGF = 1 , NPGF
              CHX(IPGF) = 0.D0
              CHY(IPGF) = 0.D0
              CHZ(IPGF) = 0.D0
  321       CONTINUE
C
            CALL INTEGA(NPGF,
     >                  JACO,ZR(IPOIDF),
     >                  CHX,CHY,CHZ,
     >                  DSG11,DSG22,DSG33,DSG12,DSG13,DSG23,
     >                  NX,NY,NZ,
     >                  INTE)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            IF ( INTE.LT.0.D0 ) THEN
              CALL U2MESK('A','INDICATEUR_9',2,VALK)
              GOTO 9999
            ENDIF
C
            TER2=TER2+0.5D0*SQRT(HF)*SQRT(INTE)
            IF ( NIV.GE.2 ) THEN
              WRITE(IFM,1000) 'VOLU INTE', INTE
              WRITE(IFM,1000) '==> TER2 ', TER2
            ENDIF
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU TROISIEME TERME DE L'ERREUR ----------------
C --------------- LE BORD VOISIN EST UNE FACE --------------------------
C ----------------------------------------------------------------------
C
          ELSEIF ( TYPMAV(1:4).EQ.'QUAD' .OR.
     >             TYPMAV(1:4).EQ.'TRIA' ) THEN
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES ----------------
C
            CALL ERMEB3(NOE,IFA,TYMVOL,NNOF,
     >                  IREF1,IREF2,IVOIS,IGEOM,IAD,
     >                  NBCMP,INST,
     >                  NX,NY,NZ,
     >                  SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,
     >                  CHX,CHY,CHZ)
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
C ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
C
            CALL INTEGA(NPGF,
     >                  JACO,ZR(IPOIDF),
     >                  CHX,CHY,CHZ,
     >                  SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,
     >                  NX,NY,NZ,
     >                  INTE)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            IF ( INTE.LT.0.D0 ) THEN
              CALL U2MESK('A','INDICATEUR_9',2,VALK)
              GOTO 9999
            ENDIF
C
CGN       WRITE(IFM,*) '==> INTE', INTE
            TER3=TER3+SQRT(HF)*SQRT(INTE)
            IF ( NIV.GE.2 ) THEN
              WRITE(IFM,1000) 'SURF INTE', INTE
              WRITE(IFM,1000) '==> TER3 ', TER3
            ENDIF
C
C ----------------------------------------------------------------------
C --------------- CURIEUX ----------------------------------------------
C ----------------------------------------------------------------------
C
          ELSE
C
            VALK(1)=TYPMAV(1:4)
            CALL U2MESK('F','INDICATEUR_10',1,VALK)
C
          ENDIF
C
        ENDIF
C
  320 CONTINUE
C
C ----------------------------------------------------------------------
C ------- FIN DU CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR -----
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C 4. ------- MISE EN MEMOIRE DES DIFFERENTS TERMES DE L'ERREUR ---------
C ----------------------------------------------------------------------
C
      IF (NDEGRE.EQ.2) THEN
        COEFF=SQRT(96.D0)
      ELSE IF (NDEGRE.EQ.1) THEN
        COEFF=SQRT(24.D0)
      ENDIF
C
      ERREST=(TER1+TER2+TER3)/COEFF
      SIGCAL=SQRT(NORSIG)
      NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
C
      ZR(IERR)=ERREST
      ZR(IERR+1)=NUEST
      ZR(IERR+2)=SIGCAL
C
      ERREST=TER1/COEFF
      NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
C       TERMRE       TERMR2
      ZR(IERR+3)=ERREST
      ZR(IERR+4)=NUEST
C
      ERREST=TER3/COEFF
      NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
C       TERMNO     TERMN2
      ZR(IERR+5)=ERREST
      ZR(IERR+6)=NUEST
C
      ERREST=TER2/COEFF
      NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
C       TERMSA       TERMS2
      ZR(IERR+7)=ERREST
      ZR(IERR+8)=NUEST
C       DIAMETRE
      ZR(IERR+9)=HK
C
9999  CONTINUE
C
      CALL JEDEMA()
C
      END
