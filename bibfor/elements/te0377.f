      SUBROUTINE TE0377 (OPTION,NOMTE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       CALCUL DE L'INDICATEUR D'ERREUR EN MECANIQUE 2D AVEC LA
C       METHODE DES RESIDUS EXPLICITES.
C       OPTION : 'ERME_ELEM'
C
C REMARQUE : LES PROGRAMMES SUIVANTS DOIVENT RESTER TRES SIMILAIRES
C            TE0368, TE0375, TE0377, TE0378, TE0382, TE0497
C
C ----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      CHARACTER*16 OPTION,NOMTE
C
C
C
C DECLARATION VARIABLES LOCALES
C
      INTEGER IFM,NIV
      INTEGER IADZI,IAZK24
      INTEGER IBID,IAUX,IRET,ITAB(7)
      INTEGER IGEOM,JTIME
      INTEGER IERR, IVOIS
      INTEGER IMATE
      INTEGER IAD
      INTEGER IFOVR, IFOVF
      INTEGER IPES,IROT
      INTEGER IREF1,IREF2
      INTEGER NDIM
      INTEGER NNO , NNOS , NPG , IPOIDS, IVF , IDFDE , JGANO
      INTEGER NNOF, NPGF
      INTEGER NBCMP
      INTEGER IPG
      INTEGER IPGF
      INTEGER NBF
      INTEGER TYMVOL,NDEGRE,IFA,TYV

      REAL*8 R8BID,R8BID3(3)
      REAL*8 DFDX(9),DFDY(9),HK,POIDS
      REAL*8 FPX,FPY
      REAL*8 FRX(9),FRY(9)
      REAL*8 FOVO(2)
      REAL*8 DSX,DSY
      REAL*8 ERREST,NOR,NORSIG,SIGCAL,NUEST,COEFF
      REAL*8 TER1,TER2,TER3,HF,INTE,INST
      REAL*8 NX(3),NY(3),JACO(3),ORIEN
      REAL*8 CHX(3),CHY(3)
      REAL*8 SG11(3),SG22(3),SG12(3)
      REAL*8 TX(3),TY(3)
      REAL*8 SIG11(3),SIG22(3),SIG12(3)
      REAL*8 E,NU,RHO,VALRES(3)

      INTEGER ICODRE(2)
      CHARACTER*3 TYPNOR
      CHARACTER*8 TYPMAV, ELREFE
      CHARACTER*8 ELREFF, ELREFB
      CHARACTER*8 NOMPAR(3)
      CHARACTER*16 PHENOM
      CHARACTER*24 VALK(2)

      LOGICAL YAPR, YARO
C
C ----------------------------------------------------------------------
C ----- NORME CALCULEE : SEMI-H1 (H1) ou ENERGIE (NRJ) -----------------
C ----------------------------------------------------------------------
C
      DATA TYPNOR / 'NRJ' /
C
C ----------------------------------------------------------------------
 1000 FORMAT(A,' :',(6(1X,1PE17.10)))
C ----------------------------------------------------------------------
C 1 -------------- GESTION DES DONNEES ---------------------------------
C ----------------------------------------------------------------------
      CALL JEMARQ()

      CALL INFNIV(IFM,NIV)

C 1.1. --- LES INCONTOURNABLES
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVOISIN','L',IVOIS)
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
      IF ( YAPR .OR. YARO .OR. TYPNOR.EQ.'NRJ' ) THEN
C
        CALL JEVECH('PMATERC','L',IMATE)
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)
        IBID = 0
        IF ( TYPNOR.EQ.'NRJ' ) THEN
          IBID = IBID +1
          NOMPAR(IBID) = 'E'
          IBID = IBID +1
          NOMPAR(IBID) = 'NU'
        ENDIF
        IF ( YAPR .OR. YARO ) THEN
          IBID = IBID +1
          NOMPAR(IBID) = 'RHO'
        ENDIF
C
        CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ',PHENOM,0,' ',R8BID,
     &                IBID, NOMPAR, VALRES, ICODRE, 1)
C
        IF ( TYPNOR.EQ.'NRJ' ) THEN
          E  = VALRES(1)
          NU = VALRES(2)
        ENDIF
        IF ( YAPR .OR. YARO ) THEN
          RHO = VALRES(IBID)
        ENDIF
CGN        WRITE(IFM,1000) 'RHO, E, NU', RHO, E, NU
C
      ENDIF

C ----------------------------------------------------------------------
C 2 -------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
C ----------------------------------------------------------------------
C
C 2.1. --- CALCUL DU DIAMETRE HK DE LA MAILLE ----
C
      CALL UTHK(NOMTE,ZR(IGEOM),HK,NDIM,ITAB,IBID,IBID,IBID,NIV,IFM)
C
C 2.2. --- CALCUL DE LA FORCE DE PESANTEUR ---
C
      IF ( YAPR ) THEN
        FPX=RHO*ZR(IPES)*ZR(IPES+1)
        FPY=RHO*ZR(IPES)*ZR(IPES+2)
      ELSE
        FPX=0.D0
        FPY=0.D0
      ENDIF
CGN      WRITE(IFM,1000) 'P',FPX,FPY,FPZ
C
C 2.3. --- CALCUL DE LA FORCE DE ROTATION ---
C
      IF ( YARO ) THEN
        CALL RESROT (ZR(IROT),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     &               FRX,FRY)
      ELSE
        DO 23 , IPG = 1 , NPG
          FRX(IPG) = 0.D0
          FRY(IPG) = 0.D0
   23   CONTINUE
      ENDIF
CGN      WRITE(IFM,1000) 'R X',(FRX(IPG),IPG = 1 , NPG)
CGN      WRITE(IFM,1000) 'R Y',(FRY(IPG),IPG = 1 , NPG)
C
C 2.4. --- CALCUL DE LA FORCE VOLUMIQUE EVENTUELLE ---
C
      IF ( IFOVR.NE.0 ) THEN
        FOVO(1) = ZR(IFOVR  )
        FOVO(2) = ZR(IFOVR+1)
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
     &                   FOVO(IBID),IRET)
          ENDIF
   24   CONTINUE
CGN        WRITE(IFM,*) 'F X : ',ZK8(IFOVF),FOVO(1)
CGN        WRITE(IFM,*) 'F Y : ',ZK8(IFOVF+1),FOVO(2)
      ENDIF
C
C 2.5. --- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS ---
C
      TER1 = 0.D0
      NORSIG = 0.D0
C
      DO 25 , IPG = 1 , NPG
C
C ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X ET /Y ---------
C
        CALL DFDM2D (NNO,IPG,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
C
C ------- CALCUL DE L'ORIENTATION DE LA MAILLE -------------------------
C
        CALL UTJAC(.TRUE.,ZR(IGEOM),IPG,IDFDE,0,IBID,NNO,ORIEN)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA --------------
C
        IAUX=IVF+(IPG-1)*NNO
        IBID = 1
        CALL ERMEV2(NNO,IGEOM,ZR(IAUX),ZR(IAD),NBCMP,DFDX,DFDY,
     &              POIDS,IBID,
     &              DSX,DSY,NOR)
C
C ------- CUMUL
C
        R8BID3(1) = FPX + FRX(IPG) + DSX
        R8BID3(2) = FPY + FRY(IPG) + DSY
C
C ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE EVENTUEL ---------------
C
        IF ( IFOVR.NE.0 .OR. IFOVF.NE.0 ) THEN
C
CGN          WRITE(IFM,1000) 'F X', FOVO(1)
CGN          WRITE(IFM,1000) 'F Y', FOVO(2)
          R8BID3(1) = R8BID3(1) + FOVO(1)
          R8BID3(2) = R8BID3(2) + FOVO(2)
C
        ENDIF
C
C ------- CUMUL DU TERME D'ERREUR
C
        TER1 = TER1
     &       + ( R8BID3(1)**2 + R8BID3(2)**2 ) * POIDS
        IF ( NIV.GE.2 ) THEN
          WRITE(IFM,1000) 'POIDS', POIDS
          WRITE(IFM,1000) 'A2 + B2 ', R8BID3(1)**2 + R8BID3(2)**2
          WRITE(IFM,1000) '==> TER1', TER1
        ENDIF
C
C ------- CALCUL DE LA NORME DE SIGMA SUR L'ELEMENT --------------------
C
        NORSIG = NORSIG + NOR*POIDS
C
   25 CONTINUE
C
      IF (TYPNOR(1:2).EQ.'H1') THEN
C       NORME H1
        TER1=HK*SQRT(TER1)
      ELSE IF (TYPNOR.EQ.'NRJ') THEN
C       NORME EN ENERGIE
        TER1=(HK**2)*ABS(TER1)
      ENDIF
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
     &              TYMVOL, NDEGRE, NBF, ELREFF, ELREFB )
CGN      WRITE(6,*) 'TYPE MAILLE VOLUMIQUE COURANTE :',TYMVOL
C --- CARACTERISTIQUES DES FACES DE BORD -------------------------------
C     ON EST TENTE DE FAIRE L'APPEL A ELREF4 COMME EN 3D MAIS C'EST EN
C     FAIT INUTILE CAR ON N'A BESOIN QUE DE NNOF ET NPGF.
C     CELA TOMBE BIEN CAR L'APPEL MARCHE RAREMENT ...
C
       IF ( NDEGRE.EQ.1 ) THEN
         NNOF = 2
       ELSE
         NNOF = 3
       ENDIF
       NPGF = NNOF
CGN      CALL ELREF4 ( ELREFF, 'RIGI',
CGN     >              NDIMF, NNOF, NNOSF, NPGF, IPOIDF, IVFF,
CGN     >              IDFDXF, JGANOF )
CGN      WRITE(IFM,2000) 'NDIMF',NDIMF
CGN      WRITE(IFM,2000) 'NNOSF,NNOF,NPGF',NNOSF,NNOF,NPGF
CGN      WRITE(IFM,1000) 'IPOIDF', (ZR(IPOIDF+IFA),IFA=0,NPGF-1)
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
          ENDIF
C
C ----- CALCUL DE NORMALES, TANGENTES ET JACOBIENS AUX POINTS DE GAUSS
C
          IAUX = IFA
          CALL CALNOR ( '2D' , ZR(IGEOM),
     &                  IAUX, NNOS, NNOF, ORIEN,
     &                  IBID, IBID, ITAB, IBID, IBID, IBID,
     &                  JACO, NX, NY, R8BID3,
     &                  TX, TY, HF )
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
C --------------- LE BORD VOISIN EST UN VOLUME -------------------------
C ----------------------------------------------------------------------
C
          IF ( TYPMAV(1:4).EQ.'TRIA' .OR.
     &         TYPMAV(1:4).EQ.'QUAD' ) THEN
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS ------------------
C
            IAUX = IFA
            CALL ERMES2(IAUX,ELREFE,TYPMAV,IREF1,IVOIS,IAD,NBCMP,
     &                  SG11,SG22,SG12)
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C ------- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE NEWTON-COTES ----
C ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
C ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
C
            DO 321 , IPGF = 1 , NPGF
              CHX(IPGF) = 0.D0
              CHY(IPGF) = 0.D0
  321       CONTINUE
C
            CALL INTENC(NNOF,JACO,CHX,CHY,SG11,SG22,SG12,NX,NY,INTE)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            IF ( INTE.LT.0.D0 ) THEN
              CALL U2MESK('A','INDICATEUR_9',2,VALK)
              GOTO 9999
            ENDIF
C
            IF (TYPNOR(1:2).EQ.'H1') THEN
C             NORME H1
              TER2=TER2+0.5D0*SQRT(HF)*SQRT(INTE)
            ELSE IF (TYPNOR.EQ.'NRJ') THEN
C             NORME EN ENERGIE
              TER2=TER2+0.5D0*HF*INTE
            ENDIF
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
          ELSEIF ( TYPMAV(1:3).EQ.'SEG' ) THEN
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES ----------------
C
            IAUX = IFA
            CALL ERMEB2 (IAUX,IREF1,IREF2,IVOIS,IGEOM,IAD,
     &                   ELREFE,NBCMP,INST,NX,NY,TX,TY,
     &                   SIG11,SIG22,SIG12,CHX,CHY)
C
C ------- CALCUL DE L'INTEGRALE SUR LE BORD ----------------------------
C
            CALL INTENC(NNOF,JACO,CHX,CHY,SIG11,SIG22,SIG12,NX,NY,INTE)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            IF ( INTE.LT.0.D0 ) THEN
              CALL U2MESK('A','INDICATEUR_9',2,VALK)
              GOTO 9999
            ENDIF
C
            IF (TYPNOR(1:2).EQ.'H1') THEN
C             NORME H1
              TER3=TER3+SQRT(HF)*SQRT(INTE)
            ELSE IF (TYPNOR.EQ.'NRJ') THEN
C             NORME EN ENERGIE
              TER3=TER3+HF*INTE
            ENDIF
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
      IF (TYPNOR(1:2).EQ.'H1') THEN
C
        IF (NDEGRE.EQ.2) THEN
          COEFF=SQRT(96.D0)
        ELSE
          COEFF=SQRT(24.D0)
        ENDIF
C
C      NORME H1
        ERREST=(TER1+TER2+TER3)/COEFF
        SIGCAL=SQRT(NORSIG)
        IF ((ERREST**2+NORSIG).NE.0.D0) THEN
          NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
        ELSE
          NUEST=0.D0
        ENDIF
C
        ZR(IERR)=ERREST
        ZR(IERR+1)=NUEST
        ZR(IERR+2)=SIGCAL
C
        ERREST=TER1/COEFF
        IF ((ERREST**2+NORSIG).NE.0.D0) THEN
          NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
        ELSE
          NUEST=0.D0
        ENDIF
C
        ZR(IERR+3)=ERREST
        ZR(IERR+4)=NUEST
C
        ERREST=TER3/COEFF
        IF ((ERREST**2+NORSIG).NE.0.D0) THEN
          NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
        ELSE
          NUEST=0.D0
        ENDIF
C
        ZR(IERR+5)=ERREST
        ZR(IERR+6)=NUEST
C
        ERREST=TER2/COEFF
        IF ((ERREST**2+NORSIG).NE.0.D0) THEN
          NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
        ELSE
          NUEST=0.D0
        ENDIF
C
        ZR(IERR+7)=ERREST
        ZR(IERR+8)=NUEST
C
      ELSE IF (TYPNOR.EQ.'NRJ') THEN
C
        IF (NDEGRE.EQ.2) THEN
          COEFF=SQRT(96.D0*E/(1-NU))
        ELSE
          COEFF=SQRT(24.D0*E/(1-NU))
        ENDIF
C
C      NORME EN ENERGIE
        ERREST=SQRT(TER1+TER2+TER3)/COEFF
        SIGCAL=SQRT(NORSIG)
        IF ((ERREST**2+NORSIG).NE.0.D0) THEN
          NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
        ELSE
          NUEST=0.D0
        ENDIF
C
        ZR(IERR)=ERREST
        ZR(IERR+1)=NUEST
        ZR(IERR+2)=SIGCAL
C
        ERREST=SQRT(TER1)/COEFF
        IF ((ERREST**2+NORSIG).NE.0.D0) THEN
          NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
        ELSE
          NUEST=0.D0
        ENDIF
C
        ZR(IERR+3)=ERREST
        ZR(IERR+4)=NUEST
C
        ERREST=SQRT(TER3)/COEFF
        IF ((ERREST**2+NORSIG).NE.0.D0) THEN
          NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
        ELSE
          NUEST=0.D0
        ENDIF
C
        ZR(IERR+5)=ERREST
        ZR(IERR+6)=NUEST
C
        ERREST=SQRT(TER2)/COEFF
        IF ((ERREST**2+NORSIG).NE.0.D0) THEN
          NUEST=100.D0*SQRT(ERREST**2/(ERREST**2+NORSIG))
        ELSE
          NUEST=0.D0
        ENDIF
C
        ZR(IERR+7)=ERREST
        ZR(IERR+8)=NUEST
C
      ENDIF
C       DIAMETRE
      ZR(IERR+9)=HK
C
 9999 CONTINUE
C
      CALL JEDEMA()
C
      END
