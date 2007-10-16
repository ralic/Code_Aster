      SUBROUTINE NMECMI (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
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
C TOLE CRP_7
C TOLE CRP_20
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            KPG,KSP,NDIM,IMATE,IRET,IRET0,IRET1,IRET2
      CHARACTER*(*)      FAMI(*)
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       COMPOR(3),OPTION
      REAL*8             CRIT(6),INSTAM,INSTAP,TP2,LINE
      REAL*8             DEPS(6),PREC,DX,DEUXMU
      REAL*8             SIGM(6),VIM(8),SIGP(6),VIP(8),DSIDEP(6,6)
C ----------------------------------------------------------------------
C     REALISE LA LOI DE VON MISES ISOTROPE ET ELASTIQUE POUR LES
C     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
C IN  INSTAP  : INSTANT DU CALCUL
C IN  DEPS    : INCREMENT DE DEFORMATION
C               SI C_PLAN DEPS(3) EST EN FAIT INCONNU (ICI:0)
C                 =>  ATTENTION LA PLACE DE DEPS(3) EST ALORS UTILISEE.
C IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
C OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
C OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
C
C               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
C OUT IRET    : CODE RETOUR DE  L'INTEGRATION DE LA LOI DE VOM MISES
C                   IRET=0 => PAS DE PROBLEME
C                   IRET=1 => ABSENCE DE CONVERGENCE LORS DE
C                                        L'INTEGRATION DE LA LOI
C
C----- COMMONS NECESSAIRES A VON_MISES ISOTROPE C_PLAN :
C      COMMONS COMMUNS A NMCRI1 ET NMECMI
      COMMON /RCONM5/DEUXMU,TROISK,SIGY,RPRIM,PM,SIGEL,TP2,LINE,PRAG,XM
      COMMON /KCONM1/IMATE2,JPROL2,JVALE2,NBVAL2
      REAL*8   NMCRI5
      EXTERNAL NMCRI5
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      LOGICAL     CPLAN,PLASTI
      REAL*8      DEPSTH(6),VALRES(3),PM,VAL0,XP(6),PLAST,RESU
      REAL*8      DEPSMO,SIGMMO,E,NU,TROISK,RPRIM,RP,HP,GP,G1,RPM
      REAL*8      SIELEQ,SIGEPS,SEUIL,DP,COEF,DSDE,SIGY,XM(6),SIGEDV(6)
      REAL*8      KRON(6),DEPSDV(6),SIGMDV(6),SIGPDV(6),SIGDV(6),DUM,CC
      REAL*8      EM,NUM,TROIKM,DEUMUM,RBID,SIGMP(6),SIGEL(6)
      REAL*8      HSG,PP,PRAG,PRAGM,PRECR,TM,TP,EPSTHE
      INTEGER     NDIMSI,JPROLM,JVALEM,NBVALM,JPROL2,JVALE2,NBVAL2
      INTEGER     JPROLP,JVALEP,NBVALP,K,L,NITER,IMATE2
      CHARACTER*2 FB2, CODRET(3)
      CHARACTER*8 NOMRES(3),TYPE
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C DEB ------------------------------------------------------------------
C
C     -- 1 INITIALISATIONS :
C     ----------------------
      CPLAN =  TYPMOD(1) .EQ. 'C_PLAN'
      NDIMSI = 2*NDIM
      IMATE2=IMATE
C
C MISE AU FORMAT DES CONTRAINTES DE RAPPEL
C
      PM = VIM(1)
      PLAST = VIM(2)
      DO 11 K=1,3
       XM(K) = VIM(K+2)
  11  CONTINUE
      DO 10 K=4,NDIMSI
        XM(K) = VIM(K+2)*SQRT(2.D0)
  10  CONTINUE
      DP=0.D0
C
      FB2 = 'F '
C
      IF (.NOT.( COMPOR(1)(1:9) .EQ. 'VMIS_ECMI' )) THEN
            CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      ENDIF


C
C     -- 2 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
      NOMRES(1)='E'
      NOMRES(2)='NU'
      NOMRES(3)='ALPHA'
C
      IF (COMPOR(1)(1:14) .EQ. 'VMIS_ECMI_TRAC' ) THEN
         CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,1,
     &                 NOMRES(2),VALRES(2),CODRET(2), FB2 )
         NUM = VALRES(2)
         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',
     &                0.D0,1,NOMRES(2),VALRES(2),CODRET(2), FB2 )
         NU = VALRES(2)
      ELSE
         CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',
     &                0.D0,2,NOMRES(1),VALRES(1),CODRET(1), FB2 )
         EM  = VALRES(1)
         NUM = VALRES(2)
         DEUMUM = EM/(1.D0+NUM)
         TROIKM = EM/(1.D0-2.D0*NUM)
         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',
     &                0.D0,2,NOMRES(1),VALRES(1),CODRET(1), FB2 )
         E      = VALRES(1)
         NU     = VALRES(2)
         DEUXMU = E/(1.D0+NU)
         TROISK = E/(1.D0-2.D0*NU)
      ENDIF
      CALL VERIFT(FAMI,KPG,KSP,'T',IMATE,'ELAS',1,EPSTHE,IRET0)
C
C     -- 3 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
        NOMRES(1)='C'
        CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','PRAGER',0,' ',
     &                0.D0,1,NOMRES,VALRES,CODRET,'FM')
        PRAG=VALRES(1)
        CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','PRAGER',0,' ',
     &                0.D0,1,NOMRES,VALRES,CODRET,'FM')
        PRAGM=VALRES(1)
        LINE=0.D0
        IF (COMPOR(1)(10:14) .EQ. '_LINE') THEN
          LINE=1.D0
          NOMRES(1)='D_SIGM_EPSI'
          NOMRES(2)='SY'
          CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ECRO_LINE',0,' ',
     &                0.D0,2,NOMRES,VALRES,CODRET,'FM')
          DSDE=VALRES(1)
          SIGY=VALRES(2)
          RPRIM    = DSDE*E/(E-DSDE)-1.5D0*PRAG
          RPM      = RPRIM*PM +SIGY
        ELSE

          CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,TM,IRET2)
          CALL RCTYPE(IMATE,1,'TEMP',TM,RESU,TYPE)
          IF ((TYPE.EQ.'TEMP').AND.(IRET2.EQ.1)) 
     &        CALL U2MESS('F','CALCULEL_31')
          CALL RCTRAC(IMATE,'TRACTION','SIGM',RESU,JPROLM,
     &                JVALEM,NBVALM,EM)
          DEUMUM = EM/(1.D0+NUM)
          TROIKM = EM/(1.D0-2.D0*NUM)
  
          CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,TP,IRET1)
          CALL RCTYPE(IMATE,1,'TEMP',TP,RESU,TYPE)
          IF ((TYPE.EQ.'TEMP').AND.(IRET1.EQ.1)) 
     &        CALL U2MESS('F','CALCULEL_31')
          CALL RCTRAC(IMATE,'TRACTION','SIGM',RESU,JPROLP,
     &                JVALEP,NBVALP,E)
          DEUXMU = E/(1.D0+NU)
          TROISK = E/(1.D0-2.D0*NU)

          CALL RCFON2('S',JPROLP,JVALEP,NBVALP,SIGY,DUM,DUM,
     &                DUM,DUM,DUM,DUM,DUM,DUM)
          CALL RCFON2('V',JPROLP,JVALEP,NBVALP,RBID,RBID,
     &                RBID,PM,RPM,RPRIM,PRAG,RBID,RBID)
        ENDIF
C
C     -- 4 CALCUL DE DEPSMO ET DEPSDV :
C     --------------------------------
      COEF = EPSTHE
      IF (CPLAN) DEPS(3)=-NU/(1.D0-NU)*(DEPS(1)+DEPS(2))
     &                +(1.D0+NU)/(1.D0-NU)*COEF
      DEPSMO = 0.D0
      DO 110 K=1,3
        DEPSTH(K)   = DEPS(K) -COEF
        DEPSMO = DEPSMO + DEPSTH(K)
 110  CONTINUE
      DEPSMO = DEPSMO/3.D0
      DO 111 K=4,NDIMSI
        DEPSTH(K) = DEPS(K)
 111  CONTINUE
      DO 115 K=1,NDIMSI
        DEPSDV(K)   = DEPSTH(K) - DEPSMO * KRON(K)
 115  CONTINUE
C
C     -- 5 CALCUL DE SIGMP :
C     ----------------------
      SIGMMO = 0.D0
      DO 113 K =1,3
        SIGMMO = SIGMMO + SIGM(K)
 113  CONTINUE
      SIGMMO = SIGMMO /3.D0
      DO 114 K=1,NDIMSI
        SIGMP(K)=DEUXMU/DEUMUM*(SIGM(K)-SIGMMO*KRON(K)) +
     &           TROISK/TROIKM*SIGMMO*KRON(K)
114   CONTINUE
C
C     -- 6 CALCUL DE SIGMMO, SIGMDV, SIGEL, SIELEQ ET SEUIL :
C     -------------------------------------------------------
      SIGMMO = 0.D0
      DO 116 K =1,3
        SIGMMO = SIGMMO + SIGMP(K)
 116  CONTINUE
      SIGMMO = SIGMMO /3.D0
      SIELEQ = 0.D0
      DO 117 K = 1,NDIMSI
        SIGMDV(K) = SIGMP(K)- SIGMMO * KRON(K)
        IF (PRAGM.NE.0.D0) THEN
           XM(K)=XM(K)*PRAG/PRAGM
        ENDIF
        SIGEL(K)=SIGMDV(K)+DEUXMU*DEPSDV(K)-XM(K)
        SIELEQ     = SIELEQ + SIGEL(K)**2
 117  CONTINUE
      SIELEQ     = SQRT(1.5D0*SIELEQ)
      SEUIL      = SIELEQ - RPM
      HP=1.D0
      GP=1.D0
C
C     -- 7 CALCUL DE SIGP,SIGPDV,VIP,DP,RP:
C     -------------------------------------
C
      IF ( OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA'     ) THEN
C
C       -- 7.1 CALCUL DE DP (ET DX SI C_PLAN) :
C       -------------------------------------------
         IF (SEUIL.LE.0.D0) THEN
            PLAST = 0.D0
            DP = 0.D0
            RP=RPM
         ELSE
            PLAST = 1.D0
            IF (CPLAN) THEN
               PREC= CRIT(3)
               NITER=NINT(CRIT(1))
               JPROL2 = JPROLP
               JVALE2 = JVALEP
               NBVAL2 = NBVALP
               VAL0 = NMCRI5(0.D0)
               PRECR = PREC * SIGY
C
C             CALCUL DE L'APPROXIMATION : DP SANS CONTRAINTE PLANE
C
               IF (COMPOR(1)(10:14) .EQ. '_LINE') THEN
                  DP0 = SIELEQ - SIGY - RPRIM * PM
                  DP0 = DP0 / (RPRIM+1.5D0*(DEUXMU+PRAG))
               ELSE
                  CALL RCFON2('E',JPROLP,JVALEP,NBVALP,RBID,E,NU,
     &                        PM,RP,RPRIM,PRAG,SIELEQ,DP0)
               ENDIF
               XAP = DP0
               CALL ZEROFO(NMCRI5,VAL0,XAP,PRECR,NITER,DP,IRET)
               IF(IRET.EQ.1) GOTO 9999
               IF (LINE.GE.0.5D0) THEN
                  RP = SIGY +RPRIM*(PM+DP)
               ELSE
                  CALL RCFON2('V',JPROLP,JVALEP,NBVALP,RBID,RBID,RBID,
     &                        PM+DP,RP,RPRIM,PRAG,RBID,RBID)
               ENDIF
            ELSE
               IF (COMPOR(1)(10:14) .EQ. '_LINE') THEN
                  DP = SIELEQ - SIGY - RPRIM * PM
                  DP = DP / (RPRIM+1.5D0*(DEUXMU+PRAG))
                  RP = SIGY +RPRIM*(PM+DP)
               ELSE
                  CALL RCFON2('E',JPROLP,JVALEP,NBVALP,RBID,E,NU,
     &                        PM,RP,RPRIM,PRAG,SIELEQ,DP)
               ENDIF
            ENDIF
         ENDIF
         PP = PM + DP
         GP=1.D0+1.5D0*PRAG*DP/RP
         HP=GP+1.5D0*DEUXMU*DP/RP
         PLASTI=(PLAST.GE.0.5D0)
C
C         -- 7.2 CALCUL DE SIGP :
C         -----------------------
         IF (CPLAN.AND.PLASTI) THEN
            HSG=HP/GP
            DX=  (HSG-1.D0)* SIGEL(3)
            DX=DX/( DEUXMU/1.5D0 + TROISK*HSG/3.D0 )
            DEPSMO   =DEPSMO   +DX/3.D0
            DEPSDV(1)=DEPSDV(1)-DX/3.D0
            DEPSDV(2)=DEPSDV(2)-DX/3.D0
            DEPSDV(3)=DEPSDV(3)+DX*2.D0/3.D0
         ENDIF
         DO 160 K = 1,NDIMSI
            SIGEDV(K) = SIGMDV(K) + DEUXMU * DEPSDV(K)
            G1=1.5D0*PRAG*DP/RP/HP
            XP(K)=XM(K)*(1.D0-G1)+G1*SIGEDV(K)
            SIGPDV(K) = SIGEDV(K)*GP/HP+XM(K)*1.5D0*DEUXMU*DP/RP/HP
            SIGP(K)  = SIGPDV(K) + (SIGMMO + TROISK*DEPSMO)*KRON(K)
 160     CONTINUE
      ENDIF
C
C     -- 8 CALCUL DE DSIDEP(6,6) :
C     ----------------------------
      IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG'.OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA'         ) THEN
C
         PLASTI=(PLAST.GE.0.5D0)
         IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG' ) THEN
C         - - OPTION='RIGI_MECA_TANG' => SIGMA(T)
            DO 118 K=1,NDIMSI
               SIGDV(K) = SIGMDV(K)-XM(K)
 118        CONTINUE
            RP = RPM
         ELSE
C         - - OPTION='FULL_MECA' => SIGMA(T+DT)
            DO 119 K=1,NDIMSI
               SIGDV(K) = SIGPDV(K)-XP(K)
 119        CONTINUE
         ENDIF
C
C       -- 8.1 PARTIE PLASTIQUE:
         DO 100 K=1,NDIMSI
            DO 101 L=1,NDIMSI
               DSIDEP(K,L) =  0.D0
 101        CONTINUE
 100     CONTINUE
C
         SIGEPS = 0.D0
         DO 170 K = 1,NDIMSI
            SIGEPS = SIGEPS + SIGDV(K)*DEPSDV(K)
 170     CONTINUE
         IF(PLASTI.AND.SIGEPS.GE.0.D0) THEN
            CC=-(1.5D0*DEUXMU)**2/( 1.5D0*(DEUXMU+PRAG)+RPRIM )/RP**2
     &             *(1.D0 - DP*RPRIM/RP )/HP
            DO 135 K=1,NDIMSI
              DO 135 L=1,NDIMSI
                DSIDEP(K,L) =  CC*SIGDV(K)*SIGDV(L)
 135        CONTINUE
         ENDIF
C
C       -- 8.2 PARTIE ELASTIQUE:
         DO 130 K=1,3
            DO 131 L=1,3
               DSIDEP(K,L)=DSIDEP(K,L)+TROISK/3.D0-DEUXMU/3.D0*GP/HP
 131        CONTINUE
 130     CONTINUE
         DO 120 K=1,NDIMSI
            DSIDEP(K,K) = DSIDEP(K,K) + DEUXMU*GP/HP
 120     CONTINUE
C
C       -- 8.3 CORRECTION POUR LES CONTRAINTES PLANES :
         IF (CPLAN) THEN
            DO 136 K=1,NDIMSI
               IF (K.EQ.3) GO TO 136
               DO 137 L=1,NDIMSI
                  IF (L.EQ.3) GO TO 137
                  DSIDEP(K,L)=DSIDEP(K,L)
     &                      - 1.D0/DSIDEP(3,3)*DSIDEP(K,3)*DSIDEP(3,L)
 137           CONTINUE
 136        CONTINUE
         ENDIF
      ENDIF
C
C MISE AU FORMAT DES CONTRAINTES DE RAPPEL
C
      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA')     THEN
         VIP(1)=PP
         VIP(2)=PLAST
         DO 31 K=1,3
            VIP(K+2) = XP(K)
 31      CONTINUE
         DO 30 K=4,NDIMSI
            VIP(K+2) = XP(K)/SQRT(2.D0)
 30      CONTINUE
      END IF
C
9999  CONTINUE
      END
