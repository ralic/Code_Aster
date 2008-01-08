      SUBROUTINE NSISOT(OPTION,TYPMOD,COMPOR,NDIM,IMATE,IMATSE,DEPS,
     & SIGMS,VARMS,VARM,SIGM,VARP,SIPAS,SIGP,SIGPS,VARPS,STYPSE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/01/2008   AUTEUR COURTOIS M.COURTOIS 
C TOLE CRP_20
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C

      IMPLICIT NONE
      INTEGER            NDIM,IMATE,IMATSE
      CHARACTER*16      OPTION,COMPOR(4)
      CHARACTER*24      STYPSE
      CHARACTER*8       TYPMOD(*)
      REAL*8    DEPS(*),SIGMS(*),VARMS(*),VARPS(*)
      REAL*8    VARM(*),SIGM(*),VARP(*),SIPAS(*),SIGP(*),SIGPS(*)
           
C ----------------------------------------------------------------------
C     INTEGRATION DES LOIS ELASTIQUE ET VON MISES ISOTROPE SENSIBLES 
C     POUR LES ELEMENTS ISOPARAMETRIQUES ET LES COQUES 
C     EN PETITES DEFORMATIONS
C
C IN  OPTION  : OPTION DEMANDEE 
C IN  COMPOR  : COMPORTEMENT
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  DEPS    : INCR. DE DEFORMATION 
C IN  SIGMS   : CONTRAINTES SENSIBLES A L'INSTANT -
C IN  VARMS   : VAR INTERNES SENSIBLES A L'INSTANT -
C IN  VARM    : VAR INTERNES A L'INSTANT -
C IN  SIGM    : CONTRAINTES A L'INSTANT -
C IN  VARP    : VAR INTERNES A L'INSTANT +
C IN  SIGP    : CONTRAINTES A L'INSTANT +
C IN  STYPSE  : SOUS-TYPE DE SENSIBILITE
C OUT SIGPS   : CONTRAINTES SENSIBLES A L'INSTANT +
C OUT VARPS   : VAR INTERNES SENSIBLES A L'INSTANT +
C 
C   ATTENTION : 
C      - POUR L'OPTION MECA_SENS_MATE, DEPS EST UN
C        CHAMPS DIRECT (NON DERIVE),SIGMS SONT LES CONTRAINTES
C        SENSIBLES A L'INSTANT -, SIGPS SONT LES CONTRAINTES
C        SENSIBLES PARTIELLES A L'INSTANT +
C      - POUR L'OPTION MECA_SENS_CHAR, DEPS EST UN
C        CHAMPS DIRECT (NON DERIVE),SIGMS SONT LES CONTRAINTES
C        SENSIBLES A L'INSTANT -, SIGPS SONT LES CONTRAINTES
C        SENSIBLES PARTIELLES A L'INSTANT +
C      - POUR L'OPTION MECA_SENS_RAPH, DEPS EST UN
C        CHAMPS SENSIBLE (DERIVE),SIGMS SONT LES CONTRAINTES
C        SENSIBLES A L'INSTANT -,SIPAS SONT LES CONTRAINTES
C        SENSIBLES PARTIELLES A L'INSTANT +, SIGPS SONT LES
C        CONTRAINTES SENSIBLES A L'INSTANT +
C 
C ----------------------------------------------------------------------
C
C----- COMMON NECESSAIRE A VON_MISES ISOTROPE C_PLAN :
C      COMMON COMMUN A NMCRI1 ET NMISOT
      COMMON /RCONM1/DEUXMU,NU,E,SIGY,RPRIM,PM,SIGEL,LINE
      REAL*8 SIGMDV(6)
      REAL*8 DDESMO,SEUIL,SIELEQ,SIGEL(6),PM,LINE,DX
      REAL*8 TEMP,TRSIGM,DSIEEQ,DDESDV(6)
      REAL*8 VALRES(3),DELTAP,DPMIN,TROIMU
      REAL*8 E,NU,TROISK,RPRIM,DSY,DEUXMU
      REAL*8 DSDE,SIGY,TRDSIM,DRPRIM,TRDSIP
      REAL*8 KRON(6),SOUTOT,RPTM,TRSIGP,A,RP,COEF      
      REAL*8 DTROIK,DDEUMU,DSIDEP(6,6),VALPAP(3)
      REAL*8 ES,NUS,DSDES,SIGYS
      REAL*8 APUIS, NPUIS, UNSURN, COCO, R8PREM
      REAL*8 APUISS, NPUISS
      REAL*8 NU2M, NU2P, TRACE, DLAMDB, LAMDB 
      REAL*8 PREC, PRECR, VAL0, XAP, DP0
      REAL*8 FDDX(6), DENO, DDX
      REAL*8 DDXDE, DDXDNU, DDXDSI, DDXDET
      REAL*8 COE1, COE2, COE3, COE4
      REAL*8 NUM, DEN, NUMP, DENP, PROD
      REAL*8 DPDE, DPDNU, DPDSI, DPDET
      INTEGER NDIMSI,K,I,L,NITER,IRET
      CHARACTER*2 BL2,FB2,CODRET(3)
      CHARACTER*8 NOMRES(3)
      CHARACTER*8 NOMPAR(3)
      CHARACTER*24 BLAN24
      PARAMETER ( BLAN24 = '                        ' )
      DATA KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C     -- 1 INITIALISATIONS
C     --------------------
      NDIMSI = 2*NDIM

      BL2 = '  '
      FB2 = 'F '

      IF (COMPOR(1)(1:14).EQ.'VMIS_ISOT_TRAC') THEN
        CALL U2MESK('F','SENSIBILITE_15',1,COMPOR(1))
      ENDIF

      IF (COMPOR(1)(1:9).EQ.'VMIS_ISOT') THEN
        DELTAP = VARP(1) - VARM(1)
      ELSE
        DELTAP = 0.D0
      ENDIF
      DPMIN = 1.D-15

C     -- 2 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'

      CALL RCVALA(IMATE,' ','ELAS',3,NOMPAR,VALPAP,2,NOMRES(1),
     &           VALRES(1), CODRET(1),FB2)
      CALL RCVALA(IMATE,' ','ELAS',3,NOMPAR,VALPAP,1,NOMRES(3),
     &           VALRES(3), CODRET(3),BL2)
      IF (CODRET(3).NE.'OK') VALRES(3) = 0.D0
      E = VALRES(1)
      NU = VALRES(2)
      DEUXMU = E/ (1.D0+NU)
      TROISK = E/ (1.D0-2.D0*NU)

C     -- 3 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
      IF (COMPOR(1)(1:14).EQ.'VMIS_ISOT_LINE') THEN
        NOMRES(1) = 'D_SIGM_E'
        NOMRES(2) = 'SY'
        CALL RCVALA(IMATE,' ','ECRO_LINE',3,NOMPAR,VALPAP,2,NOMRES,
     &             VALRES, CODRET,FB2)
        DSDE = VALRES(1)
        SIGY = VALRES(2)
        RPRIM = DSDE*E/ (E-DSDE)
      ELSEIF (COMPOR(1)(1:14).EQ.'VMIS_ISOT_PUIS') THEN
        NOMRES(1)='SY'
        NOMRES(2)='A_PUIS'
        NOMRES(3)='N_PUIS'
        CALL RCVALA(IMATE,' ','ECRO_PUIS',3,NOMPAR,VALPAP,3,NOMRES,
     &             VALRES, CODRET,FB2)
        SIGY = VALRES(1)
        APUIS = VALRES(2)
        NPUIS = VALRES(3)
        COCO   = E/APUIS/SIGY
        UNSURN = 1.D0/NPUIS
        IF (VARM(1).GT.R8PREM()) THEN
             RPRIM = UNSURN * SIGY * COCO * (COCO*VARM(1))**(UNSURN-1)
        ELSE
             RPRIM = SIGY * COCO 
        ENDIF
      ENDIF

C     ------------------------------------
C     --- PARAMETRES MATERIAUX DERIVES ---
C     ------------------------------------

      DRPRIM = 0.D0
      DTROIK = 0.D0
      DDEUMU = 0.D0
      DSY = 0.D0

      IF (STYPSE.NE.BLAN24) THEN

        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'
        CALL RCVALA(IMATSE,' ','ELAS',0,' ',0.D0,2,
     &           NOMRES(1),VALRES(1),CODRET(1),'FM')
        CALL RCVALA(IMATSE,' ','ELAS',0,' ',0.D0,1,NOMRES(3),
     &           VALRES(3), CODRET(3),BL2)
        IF (CODRET(3).NE.'OK') VALRES(3) = 0.D0
        ES = VALRES(1)
        NUS = VALRES(2)

        IF (COMPOR(1)(1:14).EQ.'VMIS_ISOT_LINE') THEN
          NOMRES(1) = 'D_SIGM_E'
          NOMRES(2) = 'SY'
          CALL RCVALA(IMATSE,' ','ECRO_LINE',3,NOMPAR,VALPAP,2,
     &             NOMRES,VALRES, CODRET,FB2)
          DSDES = VALRES(1)
          SIGYS = VALRES(2)
        ELSEIF (COMPOR(1)(1:14).EQ.'VMIS_ISOT_PUIS') THEN
          NOMRES(1)='SY'
          NOMRES(2)='DA_PUIS'
          NOMRES(3)='DN_PUIS'
          CALL RCVALA(IMATSE,' ','ECRO_PUIS',3,NOMPAR,VALPAP,3,
     &             NOMRES,VALRES, CODRET,FB2)
          SIGYS = VALRES(1)
          APUISS = VALRES(2)
          NPUISS= VALRES(3)
        ENDIF

        DTROIK = (ES*(1.D0-2.D0*NU)+2.D0*E*NUS)/
     &          ((1.D0-2.D0*NU)*(1.D0-2.D0*NU))
        DDEUMU = (ES*(1.D0+NU)-E*NUS)/((1.D0+NU)*(1.D0+NU))
        IF (COMPOR(1)(1:14).EQ.'VMIS_ISOT_LINE') THEN
          DRPRIM=(E*E*DSDES-ES*DSDE*DSDE)/((E-DSDE)*(E-DSDE))
          DSY = SIGYS
        ELSEIF (COMPOR(1)(1:14).EQ.'VMIS_ISOT_PUIS') THEN
          DRPRIM= (  UNSURN/E * ES
     &              - UNSURN/APUIS * APUISS
     &              - UNSURN * (1 + UNSURN*LOG(COCO*VARM(1))) * NPUISS
     &              + (1-UNSURN)/SIGY * SIGYS ) * RPRIM
          DSY = SIGYS
        ENDIF
      
      ENDIF


C     ------------------------------------
C     --- CALCUL DES DERIVEES ---
C     ------------------------------------

      IF (OPTION(1:14).EQ.'MECA_SENS_MATE') THEN

        DDESMO = 0.D0
        IF (TYPMOD(1).EQ.'C_PLAN') THEN
C   HYPOTHESE DE CONTRAINTES PLANES POUR LES PLAQUES
          DEPS(3)=-(NU/(1.D0-NU))*(DEPS(1)+DEPS(2))
        ENDIF
        DO 20 K = 1,3
          DDESMO = DDESMO + DEPS(K)
   20   CONTINUE
        DDESMO = DDESMO/3.D0
        DO 30 K = 1,NDIMSI
          DDESDV(K) = DEPS(K) - DDESMO*KRON(K)
   30   CONTINUE

       IF ((COMPOR(1)(1:4).EQ.'VMIS').AND.
     &      (DELTAP.GT.DPMIN)) THEN
         TRSIGM = 0.D0
         DO 40 K = 1,3
           TRSIGM = TRSIGM + SIGM(K)
   40    CONTINUE
         TRSIGM = TRSIGM/3.D0
         SIELEQ = 0.D0
         DO 50 K = 1,NDIMSI
           SIGMDV(K) = SIGM(K) - TRSIGM*KRON(K)
           SIGEL(K) = SIGMDV(K) + DEUXMU*DDESDV(K)
           SIELEQ = SIELEQ + SIGEL(K)**2
   50    CONTINUE
         SIELEQ = SQRT(1.5D0*SIELEQ)
         TROIMU = 1.5D0*DEUXMU
         IF (COMPOR(1)(1:9).EQ.'VMIS_ISOT') THEN
           SEUIL = SIELEQ - RPRIM*VARM(1) - SIGY
           RPTM = RPRIM + 1.5D0*DEUXMU
         ENDIF

         TEMP = 0.D0
         DO 60 K = 1,NDIMSI
           TEMP = TEMP + DDESDV(K)*SIGEL(K)
   60    CONTINUE
        IF (SIELEQ.EQ.0.0D0) THEN
          DSIEEQ = 0.D0
        ELSE
          DSIEEQ = 1.5D0*DDEUMU*TEMP/SIELEQ
        ENDIF

         TRDSIM = 0.D0
         DO 70 K = 1,3
           TRDSIM = TRDSIM + SIGMS(K)
   70    CONTINUE
         TRDSIM = TRDSIM/3.D0

         SOUTOT = 0.D0
         DO 90 K = 1,NDIMSI
           SOUTOT = SOUTOT + SIGEL(K)*SIGMS(K)
   90    CONTINUE

       ENDIF
C === CALCUL DE (D DELTA SIG / D PHI) ===

        IF ((COMPOR(1)(1:4).NE.'VMIS').OR.
     &      (DELTAP.LE.DPMIN)) THEN
           IF (TYPMOD(1).EQ.'C_PLAN') THEN
C  LE COEFFICIENT DTROIK DE LA LOI DERIVEE CHANGE POUR LA PARTIE NUS
             DTROIK = ES/(1.D0-2.D0*NU)+E*NUS*(2.D0+NU)/
     &               ((1.D0-2.D0*NU)*(1.D0+NU)*(1.D0-NU))
           ENDIF
           DO 100 I = 1,NDIMSI
              SIGPS(I) = SIGMS(I) + DDEUMU*DDESDV(I) +
     &                   DTROIK*DDESMO*KRON(I)
  100      CONTINUE
        ELSE
            IF (SIELEQ.EQ.0.0D0) THEN
              DO 110 I = 1,NDIMSI
                SIGPS(I) = SIGMS(I)
110           CONTINUE
            ELSE
            IF (TYPMOD(1).EQ.'C_PLAN') THEN
C  DTROIK CHANGE A CAUSE DES CONTRAINTES PLANES
C             DTROIK = ES/(1.D0-2.D0*NU)+E*NUS*(2.D0+NU)/
C     &               ((1.D0-2.D0*NU)*(1.D0+NU)*(1.D0-NU))
              PM = VARM(1)
              IF (COMPOR(1)(10:14) .EQ. '_LINE') THEN
                RP = SIGY +RPRIM*(PM+DELTAP)
C              ELSEIF (COMPOR(1)(10:14) .EQ. '_PUIS') THEN
C                RP=SIGY+SIGY*(E*(PM+DELTAP)/ALFAFA/SIGY)**UNSURN
              ENDIF
              DX = 3.D0*(1.D0-2.D0*NU)*SIGEL(3)*DELTAP/
     &            (E*DELTAP+2.D0*(1.D0-NU)*RP)
              COE1 = 3.D0*(1.D0-2.D0*NU)
              COE2 = DSDE*DSDE/((E-DSDE)*(E-DSDE))
              COE3 = E*E/((E-DSDE)*(E-DSDE))
              COE4 = RPRIM+TROIMU
              PROD = 0.D0
              DO 108 I = 1,NDIMSI
                PROD = PROD + DDESDV(I)/(1.D0+NU)
     &                      * (SIGMDV(I)+DEUXMU*DDESDV(I))
  108         CONTINUE
              PROD = PROD * 1.5D0/SIELEQ
              DPDE = (PROD + VARM(1)*COE2 - RPRIM*VARMS(1))/COE4
     &                -(SIELEQ - SIGY - RPRIM*VARM(1))
     &                *(1.5D0*DDEUMU-COE2)/(COE4*COE4)
              DPDNU = (PROD - RPRIM*VARMS(1))/COE4
     &                -(SIELEQ - SIGY - RPRIM*VARM(1))
     &                *(1.5D0*DDEUMU)/(COE4*COE4)
              DPDSI = (-1.D0 - RPRIM*VARMS(1))/COE4
              DPDET = (-VARM(1)*COE3 - RPRIM*VARMS(1))/COE4
     &                -(SIELEQ - SIGY - RPRIM*VARM(1))*COE3/(COE4*COE4)
C
              NUM = COE1*DELTAP*SIGEL(3)
              DEN = E*DELTAP+2.D0*(1.D0-NU)*(RPRIM*VARP(1)+SIGY)
C   DERIVEE PAR RAPPORT A E
              NUMP = COE1*(DELTAP*DDESDV(3)/(1.D0+NU)
     &               + SIGEL(3)*DPDE)
              DENP = DELTAP+E*DPDE+2.D0*(1.D0-NU)*
     &               (RPRIM*(DPDE+VARMS(1))-VARP(1)*COE2)
              DDXDE = (DEN*NUMP-NUM*DENP)/(DEN*DEN)
C   DERIVEE PAR RAPPORT A NU
              NUMP = COE1*(-DELTAP*DDESDV(3)*E/((1.D0+NU)*(1.D0+NU))
     &               + SIGEL(3)*DPDNU)-6.D0*DELTAP*SIGEL(3)
              DENP = E*DPDNU-2.D0*(RPRIM*VARP(1)+SIGY)
     &               +2.D0*(1.D0-NU)*RPRIM*(DPDNU+VARMS(1))
              DDXDNU = (DEN*NUMP-NUM*DENP)/(DEN*DEN)
C   DERIVEE PAR RAPPORT A SIGY
              NUMP = COE1*SIGEL(3)*DPDSI
              DENP = E*DPDSI+2.D0*(1.D0-NU)*(RPRIM*(VARMS(1)+DPDSI)
     &               +1.D0)
              DDXDSI = (DEN*NUMP-NUM*DENP)/(DEN*DEN)
C   DERIVEE PAR RAPPORT A DSDE
              NUMP = COE1*SIGEL(3)*DPDET
              DENP = E*DPDET+2.D0*(1.D0-NU)*(RPRIM*(VARMS(1)+DPDET)
     &               +COE3*(VARM(1)+DELTAP))
              DDXDET = (DEN*NUMP-NUM*DENP)/(DEN*DEN)
C ======================================================================
              DDX = DDXDE*ES + DDXDNU*NUS + DDXDSI*SIGYS + DDXDET*DSDES
              FDDX(1) = -DDX/3.D0
              FDDX(2) = -DDX/3.D0
              FDDX(3) = 2.D0*DDX/3.D0
              DO 111 I = 4,6
                FDDX(I) = 0.D0
111           CONTINUE
C   HYPOTHESE DE CONTRAINTES PLANES POUR LES PLAQUES
              DDESMO   =DDESMO   +DX/3.D0
              DDESDV(1)=DDESDV(1)-DX/3.D0
              DDESDV(2)=DDESDV(2)-DX/3.D0
              DDESDV(3)=DDESDV(3)+DX*2.D0/3.D0
              DO 112 I = 1,NDIMSI
                SIGPS(I) = SIGMS(I)
     &                 +DDEUMU*DDESDV(I) + DEUXMU*FDDX(I) -
     &                 1.5D0*DDEUMU*SEUIL/ (RPTM*SIELEQ)*SIGEL(I) -
     &                 TROIMU* (DSIEEQ-DSY-DRPRIM*VARM(1))/
     &                 (RPTM*SIELEQ)*SIGEL(I) + TROIMU*SEUIL*
     &                 ((DRPRIM+1.5D0*DDEUMU)*SIELEQ+
     &                 (RPRIM+TROIMU)*DSIEEQ)/
     &                 ((RPTM*SIELEQ)* (RPTM*SIELEQ))*SIGEL(I) -
     &                 TROIMU*SEUIL/ (RPTM*SIELEQ)*
     &                 (DDEUMU*DDESDV(I) + DEUXMU*FDDX(I))
     &                 +DTROIK*DDESMO*KRON(I)
     &                 +TROISK*DDX/3.D0*KRON(I)
     &                 -TROIMU/ (RPTM*SIELEQ)* (1.D0- (SEUIL/SIELEQ))*
     &                 1.5D0/SIELEQ*SIGEL(I)*SOUTOT -
     &                 TROIMU*SEUIL/ (RPTM*SIELEQ)*
     &                 (SIGMS(I)-(1.D0/3.D0)*(SIGMS(1)+SIGMS(2)+
     &                 SIGMS(3))*KRON(I))
     &                 +TROIMU*RPRIM/ (RPTM*SIELEQ)*SIGEL(I)*VARMS(1)
112           CONTINUE
         ELSE
              DO 113 I = 1,NDIMSI
                SIGPS(I) = SIGMS(I)
     &                 +DDEUMU*DDESDV(I) -
     &                 1.5D0*DDEUMU*SEUIL/ (RPTM*SIELEQ)*SIGEL(I) -
     &                 TROIMU* (DSIEEQ-DSY-DRPRIM*VARM(1))/
     &                 (RPTM*SIELEQ)*SIGEL(I) + TROIMU*SEUIL*
     &                 ((DRPRIM+1.5D0*DDEUMU)*SIELEQ+
     &                 (RPRIM+TROIMU)*DSIEEQ)/
     &                 ((RPTM*SIELEQ)* (RPTM*SIELEQ))*SIGEL(I) -
     &                 TROIMU*SEUIL/ (RPTM*SIELEQ)*
     &                 (DDEUMU*DDESDV(I))
     &                 +DTROIK*DDESMO*KRON(I)
     &                 -TROIMU/ (RPTM*SIELEQ)* (1.D0- (SEUIL/SIELEQ))*
     &                 1.5D0/SIELEQ*SIGEL(I)*SOUTOT -
     &                 TROIMU*SEUIL/ (RPTM*SIELEQ)*
     &                 (SIGMS(I)-(1.D0/3.D0)*(SIGMS(1)+SIGMS(2)+
     &                 SIGMS(3))*KRON(I))
     &                 +TROIMU*RPRIM/ (RPTM*SIELEQ)*SIGEL(I)*VARMS(1)
113           CONTINUE
            ENDIF
            ENDIF
        ENDIF

      ELSEIF (OPTION(1:14).EQ.'MECA_SENS_CHAR'.OR.
     &        OPTION(1:14).EQ.'MECA_SENS_EPAI') THEN

        DDESMO = 0.D0
        DO 21 K = 1,3
          DDESMO = DDESMO + DEPS(K)
   21   CONTINUE
        DDESMO = DDESMO/3.D0
        DO 31 K = 1,NDIMSI
          DDESDV(K) = DEPS(K) - DDESMO*KRON(K)
   31   CONTINUE

       IF ((COMPOR(1)(1:4).EQ.'VMIS').AND.
     &      (DELTAP.GT.DPMIN)) THEN
       
         TRSIGM = 0.D0
         DO 41 K = 1,3
           TRSIGM = TRSIGM + SIGM(K)
   41    CONTINUE
         TRSIGM = TRSIGM/3.D0
         SIELEQ = 0.D0
         DO 51 K = 1,NDIMSI
           SIGMDV(K) = SIGM(K) - TRSIGM*KRON(K)
           SIGEL(K) = SIGMDV(K) + DEUXMU*DDESDV(K)
           SIELEQ = SIELEQ + SIGEL(K)**2
   51    CONTINUE
         SIELEQ = SQRT(1.5D0*SIELEQ)
         TROIMU = 1.5D0*DEUXMU
         IF (COMPOR(1)(1:9).EQ.'VMIS_ISOT') THEN
           SEUIL = SIELEQ - RPRIM*VARM(1) - SIGY
           RPTM = RPRIM + 1.5D0*DEUXMU
         ENDIF

         TEMP = 0.D0
         DO 61 K = 1,NDIMSI
           TEMP = TEMP + DDESDV(K)*SIGEL(K)
   61    CONTINUE
        IF (SIELEQ.EQ.0.0D0) THEN
          DSIEEQ = 0.D0
        ELSE
          DSIEEQ = 1.5D0*DDEUMU*TEMP/SIELEQ
        ENDIF

         TRDSIM = 0.D0
         DO 71 K = 1,3
           TRDSIM = TRDSIM + SIGMS(K)
   71    CONTINUE
         TRDSIM = TRDSIM/3.D0

         SOUTOT = 0.D0
         DO 91 K = 1,NDIMSI
           SOUTOT = SOUTOT + SIGEL(K)*SIGMS(K)
   91    CONTINUE

       ENDIF
C === CALCUL DE (D DELTA SIG / D PHI) ===

        IF ((COMPOR(1)(1:4).NE.'VMIS').OR.
     &      (DELTAP.LE.DPMIN)) THEN
          DO 101 I = 1,NDIMSI
            SIGPS(I) = SIGMS(I)
  101     CONTINUE
        ELSE
          IF (SIELEQ.EQ.0.0D0) THEN
            DO 310 I = 1,NDIMSI
              SIGPS(I) = SIGMS(I)
310         CONTINUE
          ELSE
            DO 311 I = 1,NDIMSI
              SIGPS(I) = SIGMS(I)
     &                 +0.D0
     &                 +0.D0
     &                 -TROIMU/ (RPTM*SIELEQ)* (1.D0- (SEUIL/SIELEQ))*
     &                 1.5D0/SIELEQ*SIGEL(I)*SOUTOT -
     &                 TROIMU*SEUIL/ (RPTM*SIELEQ)*
     &                 (SIGMS(I)- (1.D0/3.D0)* (SIGMS(1)+SIGMS(2)+
     &                 SIGMS(3))*KRON(I))
     &                 +TROIMU*RPRIM/ (RPTM*SIELEQ)*SIGEL(I)*VARMS(1)
311         CONTINUE
          ENDIF
        ENDIF

C CALCUL DES DERIVEES DES CONTRAINTES ET VARIABLES INTERNES

      ELSEIF (OPTION(1:14).EQ.'MECA_SENS_RAPH') THEN

        IF ((COMPOR(1)(1:4).NE.'VMIS').OR.
     &      (DELTAP.LE.DPMIN)) THEN
            DDESMO = 0.D0
            IF (TYPMOD(1).EQ.'C_PLAN') THEN
C   HYPOTHESE DE CONTRAINTES PLANES POUR LES PLAQUES
              DEPS(3)=-NU/(1.D0-NU)*(DEPS(1)+DEPS(2))
            ENDIF
            DO 120 K = 1,3
              DDESMO = DDESMO + DEPS(K)
  120       CONTINUE
            DDESMO = DDESMO/3.D0
            DO 130 K = 1,NDIMSI
              DDESDV(K) = DEPS(K) - DDESMO*KRON(K)
  130       CONTINUE
            DO 140 I = 1,NDIMSI
              SIGPS(I) = SIPAS(I) + DEUXMU*DDESDV(I) +
     &                   TROISK*DDESMO*KRON(I)
  140       CONTINUE

            VARPS(1) = VARMS(1)

        ELSE

C ----ON RECALCULE L'OPERATEUR TANGENT....
          
C ----POUR LES CONTRAINTES

          RP = SIGY + RPRIM*VARP(1)
          TRSIGP = (1.D0/3.D0)* (SIGP(1)+SIGP(2)+SIGP(3))

          A = 1.D0 + 1.5D0*DEUXMU* DELTAP/RP
          COEF = - (1.5D0*DEUXMU)**2/ (1.5D0*DEUXMU+RPRIM)/RP**2*
     &           (1.D0- DELTAP*RPRIM/RP)/A
          DO 160 K = 1,NDIMSI
            DO 150 L = 1,NDIMSI
              DSIDEP(K,L) = COEF* (SIGP(K)-TRSIGP*KRON(K))*
     &                      (SIGP(L)-TRSIGP*KRON(L))
  150       CONTINUE
  160     CONTINUE
 
         DO 180 K = 1,3
            DO 170 L = 1,3
              DSIDEP(K,L) = DSIDEP(K,L) + (TROISK/3.D0-DEUXMU/ (3.D0*A))
  170       CONTINUE
  180     CONTINUE
          DO 190 K = 1,NDIMSI
            DSIDEP(K,K) = DSIDEP(K,K) + DEUXMU/A
  190     CONTINUE


          DO 210 K = 1,NDIMSI
            SOUTOT = SIPAS(K)
            DO 200 L = 1,NDIMSI
              SOUTOT = SOUTOT + DSIDEP(K,L)*DEPS(L)
  200       CONTINUE
            SIGPS(K) = SOUTOT
  210     CONTINUE

C ----POUR LES VARIABLES INTERNES
          
          TRDSIP = (1.D0/3.D0)* (SIGPS(1)+SIGPS(2)+SIGPS(3))
          
          SOUTOT=0.D0
          DO 220 I=1,NDIMSI
            SOUTOT=SOUTOT+
     &        ((SIGPS(I)-TRDSIP*KRON(I))*(SIGP(I)-TRSIGP*KRON(I)))
  220     CONTINUE

          VARPS(1)=(1.5D0*SOUTOT/RP-DRPRIM*VARP(1)-DSY)/RPRIM

        ENDIF

      ENDIF

C FIN ----------------------------------------------------------------

      END
