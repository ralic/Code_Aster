      SUBROUTINE NMSH1(FAMI,OPTION,TYPMOD,FORMAL,
     &                   NDIM,NNO,NPG,IW,IVF,VFF,IDFF,GEOMI,DFF,
     &                   COMPOR,MATE,LGPG,CRIT,ANGMAS,
     &                   INSTM,INSTP,
     &                   DEPLM,DEPLD,SIGM,VIM,
     &                   SIGP,VIP,FINT,MATUU,CODRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/01/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21 CRS_1404

       IMPLICIT NONE
       INTEGER       NDIM,NNO,NPG,MATE,LGPG,CODRET,IW,IDFF
       CHARACTER*8   TYPMOD(*)
       CHARACTER*(*) FAMI
       CHARACTER*16  OPTION, COMPOR(*),FORMAL(*)
       REAL*8        GEOMI(*),DFF(NNO,*),CRIT(*),INSTM,INSTP
       REAL*8        VFF(NNO,NPG)
       REAL*8        ANGMAS(3)
       REAL*8        DEPLM(*), DEPLD(*),SIGM(2*NDIM,NPG)
       REAL*8        VIM(LGPG,NPG),SIGP(2*NDIM,NPG),VIP(LGPG,NPG)
       REAL*8        MATUU(*), FINT(*)

C ----------------------------------------------------------------------
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_*, RAPH_MECA ET FULL_MECA_*
C           EN GRANDES DEFORMATIONS 2D (D_PLAN ET AXI) ET 3D
C ----------------------------------------------------------------------
C IN  OPTION  : OPTION DE CALCUL
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IW      : PTR. POIDS DES POINTS DE GAUSS
C IN  VFF     : VALEUR  DES FONCTIONS DE FORME
C IN  IDFF    : PTR. DERIVEE DES FONCTIONS DE FORME ELEMENT DE REF.
C IN  GEOMI   : COORDONNEES DES NOEUDS (CONFIGURATION INITIALE)
C MEM DFF     : ESPACE MEMOIRE POUR LA DERIVEE DES FONCTIONS DE FORME
C               DIM :(NNO,3) EN 3D, (NNO,4) EN AXI, (NNO,2) EN D_PLAN
C IN  COMPOR  : COMPORTEMENT
C IN  MATE    : MATERIAU CODE
C IN  LGPG    : DIMENSION DU VECTEUR DES VAR. INTERNES POUR 1 PT GAUSS
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C IN  INSTM   : VALEUR DE L'INSTANT T-
C IN  INSTP   : VALEUR DE L'INSTANT T+
C IN  DEPLM   : DEPLACEMENT EN T-
C IN  DEPLD   : INCREMENT DE DEPLACEMENT ENTRE T- ET T+
C IN  SIGM    : CONTRAINTES DE CAUCHY EN T-
C IN  VIM     : VARIABLES INTERNES EN T-
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA_*)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA_*)
C OUT FINT    : FORCES INTERIEURES (RAPH_MECA ET FULL_MECA_*)
C OUT MATUU    : MATR. DE RIGIDITE NON SYM. (RIGI_MECA_* ET FULL_MECA_*)
C OUT IRET    : CODE RETOUR DE L'INTEGRATION DE LA LDC
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      LOGICAL GRAND, AXI, RESI, RIGI
      INTEGER LIJ(3,3),VIJ(3,3),IA,JA,NA,IB,JB,NB,G,KK,OS,IJA,I,J
      INTEGER NDDL,NDU,VU(3,27),IVF,N,KL,M,J1,KKD,IVASH2
      INTEGER COD(27),N6,ND,NBCIN,NUMCIN(2)
        COMMON /TDIM/  N6 , ND
      REAL*8 GEOMM(3*27),GEOMP(3*27)
      REAL*8 FM(3,3),FMP(3,3),FMA(3,3),FD(3,3),FDA(3,3),FDMT(3,3)
      REAL*8 FDM(3,3),FDAT(3,3),FMAM(3,3), FTA(3,3), FTAT(3,3)
      REAL*8 DEPLDA(3*27), DEPLT(3*27)
      REAL*8 SIG(6),SIGMM(3,3),SIGMAM(6),CONTM(3,3),TAU(3,3),SIGG(6)
      REAL*8 ETDPNV(6),EDPN1(3,3),PRODF(3,3),ETDPN1(3,3)
      REAL*8 JM,JD,JP,R,POIDS, XESSAI(6)
      REAL*8 PFF(6,NNO,NNO)
      REAL*8 COEF
      REAL*8 DSIDEP(6,6),TMP2
      REAL*8 RAC2,T1,T2,ALPHA
      REAL*8 DEF(6,NNO,3),TMP1
      REAL*8 TAMPON(10),TAUP(6)
      REAL*8 TBID(6),RBID,R8BID,ID(3,3),TRAV(10,27),WORK(9),R8VIDE
      REAL*8 ESP(9),EPSM(6),EPSMM(6)
      REAL*8 VIMM(3,3), VARI(3,3), VARIM(7)
      REAL*8 VIPM(3,3), VARIP(3,3), VARIPV(6)
      REAL*8 FMPM(3,3),L(3,3),LT(3,3),W(3,3)
      REAL*8 RP(3,3),RPA(3,3),VW(3)
      REAL*8 RPAT(3,3),ETDM(3,3),ETDV(6),LAMBP(3,3)
      REAL*8 RPT(3,3),KRON(6)
      PARAMETER (GRAND = .TRUE.)
      DATA    VIJ  / 1, 4, 5,
     &               4, 2, 6,
     &               5, 6, 3 /
      DATA ID/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
C ---------------------------------------------------------------------

C -----------------------------DECLARATION-----------------------------
      IF (FORMAL(1).NE.'GDEF_HYPO_ELAS') THEN
         CALL ASSERT(.FALSE.)
      END IF
      
      RBID = R8VIDE()
      RAC2 = SQRT(2.D0)
      NDDL = NDIM*NNO
      NDU  = NDIM
C     ALPHA : INDICE POUR CONF INTERMEDIAIRE COMPRIS ENTRE 0 ET 1
      ALPHA =1.D0/2.D0
      N6=6
      ND=3

C     AFFECTATION DES VARIABLES LOGIQUES  OPTIONS ET MODELISATION
      AXI  = TYPMOD(1).EQ.'AXIS'
      RESI = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
      CALL VICIN0(COMPOR,NBCIN,NUMCIN)

C--------------------------INITIALISATION------------------------

C     INTIALISATION DU VECTEUR TBID AVEC LA VALEUR RBID
      CALL R8INIR(6,RBID,TBID,1)

C     MISE A ZERO TAUP[6]=0, DSIDEP[54]=0, TAMPON[10]=0, COD[27]=0
      CALL R8INIR(6 ,0.D0,TAUP,1)
      CALL R8INIR(36,0.D0,DSIDEP,1)
      CALL R8INIR(10,0.D0,TAMPON,1)

      DO 9 I=1,27
        COD(I)=0
   9  CONTINUE

C    INITIALISATION TABLEAU VU CONTENANT LES INDICES POUR LE COUPLE(I,N)
C     si AXI='OUI' VU(1,N)=VU(3,N)
      CALL NMGPIN(NDIM,NNO,AXI,VU)

C-----------------------------TEST AVANT CALCUL---------------------

C     TEST SUR LE NOMBRE DE NOEUDS SI TEST NON VERIFIE MESSAGE ERREUR
      CALL ASSERT (NNO.LE.27)
      IF (TYPMOD(1).EQ.'C_PLAN') CALL U2MESS('F','ALGORITH8_1')
      IF (AXI) NDU = 3

C------------------------------DEPLACEMENT ET GEOMETRIE-------------

C    DETERMINATION DES CONFIGURATIONS EN T- (GEOMM) ET T+ (GEOMP)
      CALL DCOPY(NDDL,GEOMI,1,GEOMM,1)
      CALL DAXPY(NDDL,1.D0,DEPLM,1,GEOMM,1)
      CALL DCOPY(NDDL,GEOMM,1,GEOMP,1)
      IF (RESI) THEN
         CALL DAXPY(NDDL,1.D0,DEPLD,1,GEOMP,1)
C        DEPLT : DEPLACEMENT TOTAL ENTRE CONF DE REF ET INSTANT T_N+1
         DO 20 I=1,NNO*NDIM
            DEPLT(I) = DEPLM(I) + DEPLD(I)
 20      CONTINUE
      ELSE
         DO 21 I=1,NNO*NDIM
            DEPLT(I) = DEPLM(I)
            DEPLD(I) = 0.D0
 21      CONTINUE
      ENDIF


C****************************BOUCLE SUR LES POINTS DE GAUSS************

C - CALCUL POUR CHAQUE POINT DE GAUSS
      DO 10 G=1,NPG

C---CALCUL DE F_N, F_N+1 ET F_(N+ALPHA) PAR RAPPORT A GEOMI GEOM INITIAL
         CALL CALGF(NDIM,NNO,AXI,NPG,GEOMI,G,IW,VFF,IDFF,
     &                 DEPLM,DEPLT,GRAND,ALPHA,R,POIDS,DFF,FM,FMP,
     &                 FMA)

C----------------CALCUL DE f_(n+1) ET f_(n+alpha) PAR RAPPORT A GEOM T-
         CALL CALPF(NDIM,NNO,AXI,NPG,GEOMM,G,IW,VFF,IDFF,
     &                 DEPLD,GRAND,ALPHA,R,DFF,FD,DEPLDA,FDA)

C--------------------------------CALCUL DE e_(n+1)--------------
         CALL CALE(NDIM,FD,ID,FDM,FDMT,PRODF,EDPN1)

C--------------------------------CALCUL DE e~_(n+alpha)-----------
         CALL CALET(NDIM,FM,FMA,FMP,EDPN1,FMAM,FTA,
     &                 ETDPN1,JM,JP)
C------------------------CALCUL DE L,W ET DE R SI VMIS_CINE_LINE------
         CALL CACINA (NDIM,NNO,NPG,LGPG,AXI,GRAND,COMPOR,GEOMM,G,IW,
     &                   VFF,IDFF,FM,FMA,DEPLD,INSTM,INSTP,VIM(1,G),RP,
     &                   RPA,LAMBP)

C---------------------TRANSFORMATION DES ARG D ENTRE SUBROUTINE NMCOMP

         CALL NMGEOM(NDIM,NNO,.FALSE.,GRAND,GEOMI,G,IW,
     &         IVF,IDFF,DEPLM,R8BID,DFF,FM,EPSM,R8BID)
         CALL PREP2(NDIM,NPG,G,RPA,ETDPN1,SIGM,JM,FDA,
     &           RP,RPAT,ETDM,ETDPNV,SIGMAM,RPT,EPSM,EPSMM)

C  **************  COMPORTEMENTS AVEC ECROUISSAGE CINEMATIQUE**********
C-----------------------TRANSFORMATION DE X : VARIABLE INTERNE---------
         IF (NBCIN.GT.0) THEN
               CALL VICIN2(NDIM,G,NPG,LGPG,VIM,RPT,NBCIN,NUMCIN)
         ENDIF
         
C************************APPEL A LA LOI DE COMPORTEMENT**********
         CALL NMCOMP(FAMI,G,1,3,TYPMOD,MATE,COMPOR,CRIT,
     &             INSTM,INSTP,
     &             EPSMM,ETDPNV,SIGMAM,VIM(1,G),OPTION,
     &             ANGMAS,TAMPON,
     &             TAUP,VIP(1,G),DSIDEP,COD(G))

C       TEST SUR LES CODES RETOUR DE LA LOI DE COMPORTEMENT
         IF(COD(G).NE.0) THEN
            IF (RESI) THEN
               CODRET = 1
               GOTO 9999
            ENDIF
         ENDIF

C        SUPPRESSION DES RACINES DE 2
         IF (RESI) CALL DSCAL(3,1.D0/RAC2, TAUP(4),1)

C********************CONTRAINTE ET FORCES INTERIEURES******************

        IF (RESI) THEN

C         CONTRAINTE DE CAUCHY A PARTIR DE KIRCHHOFF
C         PASSAGE DE TAU_(n+1) A CONTRAINTE DE CAUCHY
          CALL DCOPY(2*NDIM,TAUP,1,SIGP(1,G),1)
          COEF=1.D0/JP
          CALL DSCAL(2*NDIM,COEF,SIGP(1,G),1)

          IF (FORMAL(1).EQ.'GDEF_HYPO_ELAS') THEN
            IVASH2=LGPG-9+1
            CALL DAXPY(9,-1.D0,ID,1,LAMBP,1)
            CALL DCOPY(9,LAMBP,1,VIP(IVASH2,G),1)
          END IF
C         Pour le calcul des forces internes.
C         La configuration de calcul de LIJ est A VERIFIER !!!

          CALL DFDMIP(NDIM,NNO,AXI,GEOMP,G,IW,VFF(1,G),IDFF,R,POIDS,DFF)
          CALL NMMALU(NNO,AXI,R,VFF(1,G),DFF,LIJ)

C         VECTEUR FINT
          DO 300 NA=1,NNO
          DO 310 IA=1,NDU
C         ATTENTION IA=1,NDU
            KK = VU(IA,NA)
            T1 = 0
            DO 320 JA = 1,NDU
              T2 = TAUP(VIJ(IA,JA))
              T1 = T1 + T2*DFF(NA,LIJ(IA,JA))
 320        CONTINUE
            FINT(KK) = FINT(KK) + POIDS*T1
 310      CONTINUE
 300      CONTINUE
        END IF


C *********************MATRICE TANGENTE(SYMETRIQUE)********************
C  REM : ON DUPLIQUE LES CAS 2D ET 3D POUR EVITER DE PERDRE TROP EN
C         TERME DE TEMPS DE CALCULS

        IF (RIGI) THEN
          CALL DFDMIP(NDIM,NNO,AXI,GEOMP,G,IW,VFF(1,G),IDFF,R,POIDS,DFF)
          CALL NMMALU(NNO,AXI,R,VFF(1,G),DFF,LIJ)
           DO 125 N=1,NNO
            DO 126 M=1,N
             PFF(1,N,M) =  DFF(N,1)*DFF(M,1)
             PFF(2,N,M) =  DFF(N,2)*DFF(M,2)
             PFF(4,N,M) =(DFF(N,1)*DFF(M,2)+DFF(N,2)*DFF(M,1))/RAC2
             IF(NDIM.EQ.2) THEN
               PFF(3,N,M) =  0.D0
               PFF(5,N,M) =  0.D0
               PFF(6,N,M) =  0.D0
             ELSE
               PFF(3,N,M) =   DFF(N,3)*DFF(M,3)
               PFF(5,N,M) =  (DFF(N,1)*DFF(M,3)+DFF(N,2)*DFF(M,1))/RAC2
               PFF(6,N,M) =  (DFF(N,2)*DFF(M,3)+DFF(N,3)*DFF(M,2))/RAC2
             END IF
 126        CONTINUE
 125       CONTINUE

           DO 40 N=1,NNO
            DO 30 I=1,NDIM
             DEF(1,N,I) =  ID(I,1)*DFF(N,1)
             DEF(2,N,I) =  ID(I,2)*DFF(N,2)
             DEF(4,N,I) = (ID(I,1)*DFF(N,2) + ID(I,2)*DFF(N,1))/RAC2
             IF(NDIM.EQ.2) THEN
               DEF(3,N,I) =  0.D0
               DEF(5,N,I) =  0.D0
               DEF(6,N,I) =  0.D0
             ELSE
               DEF(3,N,I) =  ID(I,3)*DFF(N,3)
               DEF(5,N,I) =  (ID(I,1)*DFF(N,3) + ID(I,3)*DFF(N,1))/RAC2
               DEF(6,N,I) =  (ID(I,2)*DFF(N,3) + ID(I,3)*DFF(N,2))/RAC2
             END IF
C 5.2.5 - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1

 30         CONTINUE
 40        CONTINUE
           IF (AXI) THEN
            DO 50 N=1,NNO
             DEF(3,N,1) = ID(3,3)*ZR(IVF+N+(G-1)*NNO-1)/R
 50         CONTINUE
           ENDIF

           DO 160 N=1,NNO
            DO 150 I=1,NDIM
             DO 151,KL=1,2*NDIM
              SIG(KL)=0.D0
              SIG(KL)=SIG(KL)+DEF(1,N,I)*DSIDEP(1,KL)
              SIG(KL)=SIG(KL)+DEF(2,N,I)*DSIDEP(2,KL)
              SIG(KL)=SIG(KL)+DEF(3,N,I)*DSIDEP(3,KL)
              SIG(KL)=SIG(KL)+DEF(4,N,I)*DSIDEP(4,KL)
              IF(NDIM.EQ.3) THEN
                SIG(KL)=SIG(KL)+DEF(5,N,I)*DSIDEP(5,KL)
                SIG(KL)=SIG(KL)+DEF(6,N,I)*DSIDEP(6,KL)
              END IF
151          CONTINUE
             DO 140 J=1,NDIM
              DO 130 M=1,N
               IF (M.EQ.N) THEN
                J1 = I
               ELSE
                J1 = NDIM
               ENDIF

C              RIGIDITE GEOMETRIQUE

               IF (OPTION(1:4).EQ.'RIGI') THEN
                 SIGG(1)=SIGMAM(1)
                 SIGG(2)=SIGMAM(2)
                 SIGG(3)=SIGMAM(3)
                 SIGG(4)=SIGMAM(4)
                 IF(NDIM.EQ.3) THEN
                   SIGG(5)=SIGMAM(5)
                   SIGG(6)=SIGMAM(6)
                 END IF
                ELSE
                 SIGG(1)=SIGP(1,G)
                 SIGG(2)=SIGP(2,G)
                 SIGG(3)=SIGP(3,G)
                 SIGG(4)=SIGP(4,G)
                 IF(NDIM.EQ.3) THEN
                   SIGG(5)=SIGP(5,G)
                   SIGG(6)=SIGP(6,G)
                 END IF
                ENDIF

               TMP1 = 0.D0
               IF (I.EQ.J) THEN
                TMP1 = PFF(1,N,M)*SIGG(1)
     &              + PFF(2,N,M)*SIGG(2)
     &              + PFF(3,N,M)*SIGG(3)
     &              + PFF(4,N,M)*SIGG(4)
                IF(NDIM.EQ.3) THEN
                  TMP1=TMP1+ PFF(5,N,M)*SIGG(5)
     &               + PFF(6,N,M)*SIGG(6)
                END IF
C TERME DE CORRECTION AXISYMETRIQUE (NMGR2D)
                 IF (AXI .AND. I.EQ.1) THEN
                  TMP1=TMP1+ZR(IVF+N+(G-1)*NNO-1)*
     &            ZR(IVF+M+(G-1)*NNO-1)/(R*R)*SIGG(3)
                 END IF
               ENDIF

C 5.4.2    - RIGIDITE ELASTIQUE

               TMP2=0.D0
               TMP2=TMP2+SIG(1)*DEF(1,M,J)
               TMP2=TMP2+SIG(2)*DEF(2,M,J)
               TMP2=TMP2+SIG(3)*DEF(3,M,J)
               TMP2=TMP2+SIG(4)*DEF(4,M,J)
               IF(NDIM.EQ.3) THEN
                 TMP2=TMP2+SIG(5)*DEF(5,M,J)
                 TMP2=TMP2+SIG(6)*DEF(6,M,J)
               END IF
C 5.4.3    - STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
               IF (J.LE.J1) THEN
                KKD = (NDIM*(N-1)+I-1) * (NDIM*(N-1)+I) /2
                KK = KKD + NDIM*(M-1)+J
                MATUU(KK) = MATUU(KK) + (TMP1+TMP2)*POIDS
               END IF

 130          CONTINUE
 140         CONTINUE
 150        CONTINUE
 160       CONTINUE

        END IF

 10   CONTINUE

C - SYNTHESE DES CODES RETOURS
      CALL CODERE(COD,NPG,CODRET)

 9999 CONTINUE
      END
