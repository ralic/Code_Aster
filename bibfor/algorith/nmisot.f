      SUBROUTINE NMISOT (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP,DEMU,CINCO,IRET)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_7 CRP_20
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            NDIM,IMATE,KPG,KSP,IRET
      CHARACTER*(*)      FAMI
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       COMPOR,OPTION
      REAL*8             CRIT(3),LINE
      REAL*8             DEPS(6),DX,DEUXMU,DEMU,CINCO
      REAL*8             SIGM(6),VIM(2),SIGP(6),VIP(2),DSIDEP(6,6)
C ----------------------------------------------------------------------
C     REALISE LA LOI DE VON MISES ISOTROPE ET ELASTIQUE POUR LES
C     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
C
C IN  KPG,KSP  : NUMERO DU (SOUS)POINT DE GAUSS
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
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
C OUT DEUXMU,CINCO : POUR ELEMEMTS INCOMPRESSIBLES
C OUT IRET    : CODE RETOUR DE L'INTEGRATION DE LA LOI DE VOM MISES
C               = 1  => PAS DE PROBLEME
C               = 0  => ECHEC DANS L'INTEGRATION DE LA LOI
C
C----- COMMONS NECESSAIRES A VON_MISES ISOTROPE C_PLAN :
C      COMMONS COMMUNS A NMCRI1 ET NMISOT
      COMMON /RCONM1/DEUXMU,NU,E,SIGY,RPRIM,PM,SIGEL,LINE
      COMMON /KCONM1/IMATE2,JPROL2,JVALE2,NBVAL2
      REAL*8   NMCRI1
      EXTERNAL NMCRI1
C----- COMMONS NECESSAIRES A VON_MISES ISOTROPE ECROUISSAGE PUISSANCE :
C      COMMONS COMMUNS A NMCRI2 ET NMISOT
      COMMON /RCONM2/ALFAFA,UNSURN,SIELEQ
      REAL*8   NMCRI2
      EXTERNAL NMCRI2
C
      LOGICAL     CPLAN,PLASTI,INCO,DECH
      REAL*8      DEPSTH(6),VALRES(3),EPSTHE,PM,CO,VAL0
      REAL*8      DEPSMO,SIGMMO,E,NU,TROISK,RPRIM,RP,AIRERP
      REAL*8      SIELEQ,SIGEPS,SEUIL,DP,COEF,DSDE,SIGY,HYDRM,HYDRP
      REAL*8      KRON(6),DEPSDV(6),SIGMDV(6),SIGPDV(6),SIGDV(6),DUM
      REAL*8      EM,NUM,TROIKM,DEUMUM,RBID,SIGMP(6),SIGEL(6),A,RBID2
      REAL*8      SECHM,SECHP,SREF,TP,DEFAM(6),DEFAP(6)
      INTEGER     NDIMSI,JPROLM,JVALEM,NBVALM,JPROL2,JVALE2,NBVAL2
      INTEGER     IMATE2,JPROLP,JVALEP,NBVALP,K,L,NITER,IBID
      INTEGER     IRET2, IRET3, IRET4, IRET0,IRET5
      INTEGER ICODRE(3)
      CHARACTER*6       EPSA(6)
      CHARACTER*8 NOMRES(3)
      CHARACTER*8 NOMPAR(3),TYPE
      REAL*8      VALPAM(3),VALPAP(3),RESU,R8MIEM,VALRM(2)
      REAL*8      BENDOM,BENDOP,KDESSM,KDESSP,RAC2
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA EPSA   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',
     &              'EPSAYZ'/
C DEB ------------------------------------------------------------------
C
C     -- 1 INITIALISATIONS :
C     ----------------------
C     CES VARIABLES SONT RECOPIEES DANS LE COMMON PLUS LOIN,
C     EN LES INITIALISANT A ZERO, ON DEVRAIT VOIR ASSEZ VITE
C     SI NMCRI1 UTILISE LE COMMON SANS QUE L'ON SOIT PASSER PAR RCTRAC.
      NBVALP = 0
      JPROLP = 0
      JVALEP = 0
C
      CPLAN =  TYPMOD(1) .EQ. 'C_PLAN'
      INCO  =  TYPMOD(2) .EQ. 'INCO'
      DECH  =  OPTION(11:14).EQ.'ELAS'
      IF (INCO) THEN
        CO = 0.D0
      ELSE
        CO = 1.D0
      ENDIF
      NDIMSI = 2*NDIM
      IMATE2 = IMATE
      RAC2 = SQRT(2.D0)
C
C
      IF (.NOT.( COMPOR(1:4) .EQ. 'ELAS'.OR.
     &     COMPOR(1:9) .EQ. 'VMIS_ISOT' )) THEN
            CALL U2MESK('F','ALGORITH4_50',1,COMPOR)
      ENDIF
C
C     -- 2 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
      NOMRES(1)='E'
      NOMRES(2)='NU'
C
      NOMPAR(1) = 'TEMP'
      CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,TM,IRET3)
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,TP,IRET4)
      VALPAM(1) = TM
      VALPAP(1) = TP

      CALL RCVARC(' ','HYDR','-',FAMI,KPG,KSP,HYDRM,IRET2)
      IF (IRET2.NE.0) HYDRM=0.D0
      CALL RCVARC(' ','HYDR','+',FAMI,KPG,KSP,HYDRP,IRET2)
      IF (IRET2.NE.0) HYDRP=0.D0
      CALL RCVARC(' ','SECH','-',FAMI,KPG,KSP,SECHM,IRET2)
      IF (IRET2.NE.0) SECHM=0.D0
      CALL RCVARC(' ','SECH','+',FAMI,KPG,KSP,SECHP,IRET2)
      IF (IRET2.NE.0) SECHP=0.D0
      CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,SREF,IRET2)
      IF (IRET2.NE.0) SREF=0.D0
C
      DO 19 K=1,6
         DEFAM(K) = 0.D0
         DEFAP(K) = 0.D0
 19   CONTINUE

      DO 20 K=1,NDIMSI
        CALL RCVARC(' ',EPSA(K),'-',FAMI,KPG,KSP,DEFAM(K),IRET5)
        IF (IRET5.NE.0) DEFAM(K)=0.D0

        CALL RCVARC(' ',EPSA(K),'+',FAMI,KPG,KSP,DEFAP(K),IRET5)
        IF (IRET5.NE.0) DEFAP(K)=0.D0
 20   CONTINUE
C
C MISE AU FORMAT DES TERMES NON DIAGONAUX
C
      DO 105 K=4,NDIMSI
         DEFAM(K) = DEFAM(K)*RAC2
         DEFAP(K) = DEFAP(K)*RAC2
 105  CONTINUE

      IF (COMPOR(1:14) .EQ. 'VMIS_ISOT_TRAC' ) THEN
         CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,
     &                 1,NOMRES(2),VALRES(2),ICODRE(2), 2)
         NUM = VALRES(2)
         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,
     &                 1,NOMRES(2),VALRES(2),ICODRE(2), 2)
         NU = VALRES(2)
      ELSE
         CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,
     &                 2,NOMRES(1),VALRES(1),ICODRE(1), 2)
         EM  = VALRES(1)
         NUM = VALRES(2)
         DEUMUM = EM/(1.D0+NUM)
         IF (INCO) THEN
           TROIKM = DEUMUM
         ELSE
           TROIKM = EM/(1.D0-2.D0*NUM)
         ENDIF
         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,
     &                 2,NOMRES(1),VALRES(1),ICODRE(1), 2)
         E      = VALRES(1)
         NU     = VALRES(2)
         IF (INCO) THEN
           DEUXMU = 2.D0*E/3.D0
           TROISK = DEUXMU
         ELSE
           DEUXMU = E/(1.D0+NU)
           TROISK = E/(1.D0-2.D0*NU)
         ENDIF
      ENDIF
      CALL VERIFT(FAMI,KPG,KSP,'T',IMATE,'ELAS',1,EPSTHE,IRET0)
C
C --- RETRAIT ENDOGENE ET RETRAIT DE DESSICCATION
C
      NOMRES(1)='B_ENDOGE'
      NOMRES(2)='K_DESSIC'
      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(1),VALRES(1),ICODRE(1), 0)
      IF ( ICODRE(1) .NE.0    ) VALRES(1) = 0.D0
      BENDOM = VALRES(1)
C
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(1),VALRES(1),ICODRE(1), 0)
      IF ( ICODRE(1) .NE.0    ) VALRES(1) = 0.D0
      BENDOP = VALRES(1)
C
      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(2),VALRES(2),ICODRE(2), 0)
      IF ( ICODRE(2) .NE.0    ) VALRES(2) = 0.D0
      KDESSM = VALRES(2)
C
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(2),VALRES(2),ICODRE(2), 0)
      IF ( ICODRE(2) .NE.0    ) VALRES(2) = 0.D0
      KDESSP = VALRES(2)
C
C     -- 3 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
      IF ( COMPOR(1:9) .EQ. 'VMIS_ISOT') THEN
        LINE=0.D0
        PLASTI=(VIM(2).GE.0.5D0)
        IF (COMPOR(10:14) .EQ. '_LINE') THEN
          LINE=1.D0
          NOMRES(1)='D_SIGM_EPSI'
          NOMRES(2)='SY'
          CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ECRO_LINE',0,' ',0.D0,
     &                          2,NOMRES,VALRES,ICODRE, 2)
          DSDE=VALRES(1)
          SIGY=VALRES(2)
          IF ((E-DSDE).LT.R8MIEM()) THEN
             VALRM(1)=DSDE
             VALRM(2)=E
             CALL U2MESG('F','COMPOR1_54',0,' ',0,IBID,2,VALRM)
          ELSE
             RPRIM    = DSDE*E/(E-DSDE)
          ENDIF
          RP       = RPRIM*VIM(1)+SIGY
        ELSEIF (COMPOR(10:14) .EQ. '_PUIS') THEN
          LINE=-1.D0
          NOMRES(1)='SY'
          NOMRES(2)='A_PUIS'
          NOMRES(3)='N_PUIS'
          CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ECRO_PUIS',0,' ',0.D0,
     &                          3,NOMRES,VALRES,ICODRE, 2)
          SIGY   = VALRES(1)
          ALFAFA = VALRES(2)
          COCO   = E/ALFAFA/SIGY
          UNSURN = 1.D0/VALRES(3)
          RP     = SIGY * (COCO*VIM(1))**UNSURN + SIGY
          IF (VIM(1).GT.R8PREM()) THEN
             RPRIM    = UNSURN * SIGY * COCO * (COCO*VIM(1))**(UNSURN-1)
          ELSE
             RPRIM    = E
          ENDIF
        ELSE
          NOMPAR(2)='SECH'
          VALPAM(2)=SECHM
          NOMPAR(3)='HYDR'
          VALPAM(3)=HYDRM
          CALL RCTYPE(IMATE,3,NOMPAR,VALPAM,RESU,TYPE)

          IF ((TYPE.EQ.'TEMP').AND.(IRET3.EQ.1))
     &        CALL U2MESS('F','CALCULEL_31')
          CALL RCTRAC(IMATE,1,'SIGM',TM,JPROLM,JVALEM,
     &                NBVALM,EM)
          DEUMUM = EM/(1.D0+NUM)
          IF (INCO) THEN
            TROIKM = DEUMUM
          ELSE
            TROIKM = EM/(1.D0-2.D0*NUM)
          ENDIF
          NOMPAR(2)='SECH'
          VALPAP(2)=SECHP
          NOMPAR(3)='HYDR'
          VALPAP(3)=HYDRP
          CALL RCTYPE(IMATE,3,NOMPAR,VALPAP,RESU,TYPE)
          IF ((TYPE.EQ.'TEMP').AND.(IRET4.EQ.1))
     &        CALL U2MESS('F','CALCULEL_31')
          CALL RCTRAC(IMATE,1,'SIGM',RESU,JPROLP,JVALEP,
     &                NBVALP,E)
          CALL RCFONC('S',1,JPROLP,JVALEP,NBVALP,SIGY,DUM,
     &                DUM,DUM,DUM,DUM,DUM,DUM,DUM)
          CALL RCFONC('V',1,JPROLP,JVALEP,NBVALP,RBID,RBID,
     &                RBID,VIM(1),RP,RPRIM,AIRERP,RBID,RBID)
          IF (INCO) THEN
            DEUXMU = 2.D0*E/3.D0
            TROISK = DEUXMU
          ELSE
            DEUXMU = E/(1.D0+NU)
            TROISK = E/(1.D0-2.D0*NU)
          ENDIF
        ENDIF
      ELSE
C       -- CAS : COMPOR = 'ELAS'
        RP=0.D0
        PLASTI=.FALSE.
      ENDIF
      DEMU = DEUXMU
      IF (INCO) THEN
        CINCO =(1.D0-2.D0*NU)/NU
      ENDIF
C
C     -- 4 CALCUL DE DEPSMO ET DEPSDV :
C     --------------------------------
      COEF = EPSTHE
     &     - BENDOP*HYDRP        + BENDOM*HYDRM
     &     - KDESSP*(SREF-SECHP) + KDESSM*(SREF-SECHM)
      IF (CPLAN) DEPS(3)=-NU/(1.D0-NU)*(DEPS(1)+DEPS(2))
     &                +(1.D0+NU)/(1.D0-NU)*COEF
     & + NU*(DEFAP(1)-DEFAM(1)+DEFAP(2)-DEFAM(2))/(1.D0-NU)
     & + DEFAP(3)-DEFAM(3)
      DEPSMO = 0.D0
      DO 110 K=1,3
        DEPSTH(K)   = DEPS(K) -COEF
     &                -(DEFAP(K)-DEFAM(K))
        DEPSTH(K+3) = DEPS(K+3)-(DEFAP(K+3)-DEFAM(K+3))
        DEPSMO = DEPSMO + DEPSTH(K)
 110  CONTINUE
      DEPSMO = DEPSMO/3.D0
      DO 115 K=1,NDIMSI
        DEPSDV(K)   = DEPSTH(K) - DEPSMO * KRON(K)*CO
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
        SIGEL(K)  = SIGMDV(K) + DEUXMU * DEPSDV(K)
        SIELEQ     = SIELEQ + SIGEL(K)**2
 117  CONTINUE
      SIELEQ     = SQRT(1.5D0*SIELEQ)
      SEUIL      = SIELEQ - RP
C
C     -- 7 CALCUL DE SIGP,SIGPDV,VIP,DP,RP:
C     -------------------------------------
      DP=0.D0
      IF ( OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA'     ) THEN
C

        IF (COMPOR(1:4) .EQ. 'ELAS') THEN
          DO 145 K = 1,NDIMSI
            SIGP(K) = SIGMP(K)+DEUXMU*DEPSDV(K)+CO*TROISK*DEPSMO*KRON(K)
 145      CONTINUE
C
C       -- 7.1 CALCUL DE DP (ET DX SI C_PLAN) :
C       -------------------------------------------
        ELSE IF (COMPOR(1:9) .EQ. 'VMIS_ISOT') THEN
          IF (SEUIL.LE.0.D0) THEN
            VIP(2) = 0.D0
            DP = 0.D0
          ELSE
            VIP(2) = 1.D0
            PM = VIM(1)
            IF (CPLAN) THEN
              NITER=NINT(CRIT(1))
              JPROL2 = JPROLP
              JVALE2 = JVALEP
              NBVAL2 = NBVALP
              VAL0 = NMCRI1(0.D0)
              PRECR = CRIT(3) * SIGY
C
C             CALCUL DE L'APPROXIMATION : DP SANS CONTRAINTE PLANE
C
              IF (COMPOR(10:14) .EQ. '_LINE') THEN
                DP0 = SIELEQ - SIGY - RPRIM * PM
                DP0 = DP0 / (RPRIM+1.5D0*DEUXMU)
              ELSEIF (COMPOR(10:14) .EQ. '_PUIS') THEN
                DP0 = (SIELEQ - RP)/(1.5D0*DEUXMU)
              ELSE
                CALL RCFONC('E',1,JPROLP,JVALEP,NBVALP,RBID,E,
     &                      NU,PM,RP,RPRIM,AIRERP,SIELEQ,DP0)
              ENDIF
              XAP = DP0
              CALL ZEROFO(NMCRI1,VAL0,XAP,PRECR,NITER,DP,IRET,IBID)
              IF(IRET.EQ.1) GOTO 9999
              IF (LINE.GT.0.5D0) THEN
                RP = SIGY +RPRIM*(PM+DP)
              ELSEIF (LINE.LT.-0.5D0) THEN
                RP=SIGY+SIGY*(E*(PM+DP)/ALFAFA/SIGY)**UNSURN
              ELSE
                CALL RCFONC('V',1,JPROLP,JVALEP,NBVALP,RBID,
     &                      RBID,RBID,PM+DP,RP,RBID2,AIRERP,RBID,RBID)
              ENDIF
              DX = 3.D0*(1.D0-2.D0*NU)*SIGEL(3)*DP/(E*DP+2.D0*
     &                                                (1.D0-NU)*RP)
            ELSE
              IF (COMPOR(10:14) .EQ. '_LINE') THEN
                DP = SIELEQ - SIGY - RPRIM * PM
                DP = DP / (RPRIM+1.5D0*DEUXMU)
                RP = SIGY +RPRIM*(PM+DP)
              ELSEIF (COMPOR(10:14) .EQ. '_PUIS') THEN
                DP0 = (SIELEQ - RP)/(1.5D0*DEUXMU)
C               AMELIORATION DE LA PREDICTION DE DP EN ESTIMANT
C               RPRIM(PM+DP0)
                RPRIM0 = UNSURN*SIGY*COCO * (COCO*(PM+DP0))**(UNSURN-1)
                DP0 = DP0 / (1+RPRIM0/1.5D0/DEUXMU)
                XAP   = DP0
                VAL0  = NMCRI2(0.D0)
                PRECR = CRIT(3) * SIGY
                NITER = NINT(CRIT(1))
                CALL ZEROFO(NMCRI2,VAL0,XAP,PRECR,NITER,DP,IRET,IBID)
                IF(IRET.EQ.1) GOTO 9999
                CALL ECPUIS(E,SIGY,ALFAFA,UNSURN,PM,DP,RP,RPRIM)
              ELSE
                CALL RCFONC('E',1,JPROLP,JVALEP,NBVALP,RBID,E,
     &                      NU,VIM(1),RP,RPRIM,AIRERP,SIELEQ,DP)
              ENDIF
            ENDIF
          ENDIF
          VIP(1) = VIM(1) + DP
          PLASTI=(VIP(2).GE.0.5D0)
C
C         -- 7.2 CALCUL DE SIGP :
C         -----------------------
          IF (CPLAN.AND.PLASTI) THEN
            DEPSMO   =DEPSMO   +DX/3.D0
            DEPSDV(1)=DEPSDV(1)-DX/3.D0
            DEPSDV(2)=DEPSDV(2)-DX/3.D0
            DEPSDV(3)=DEPSDV(3)+DX*2.D0/3.D0
          ENDIF
          DO 160 K = 1,NDIMSI
            SIGPDV(K) = SIGMDV(K) + DEUXMU * DEPSDV(K)
            SIGPDV(K) = SIGPDV(K)*RP/(RP+1.5D0*DEUXMU*DP)
            SIGP(K)  = SIGPDV(K) + (SIGMMO + CO*TROISK*DEPSMO)*KRON(K)
 160      CONTINUE
C
        ENDIF
      ENDIF
C
C     -- 8 CALCUL DE DSIDEP(6,6) :
C     ----------------------------
      IF ( OPTION(1:10) .EQ. 'RIGI_MECA_'.OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA'         ) THEN
C
        IF ( OPTION(1:10) .EQ. 'RIGI_MECA_' ) THEN
C         - - OPTION='RIGI_MECA_TANG' => SIGMA(T)
          RP=0.D0
          DO 118 K=1,NDIMSI
            SIGDV(K) = SIGMDV(K)
            RP = RP + SIGDV(K)**2
 118      CONTINUE
          RP = SQRT(1.5D0*RP)
        ELSE
C         - - OPTION='FULL_MECA' => SIGMA(T+DT)
          IF (COMPOR(1:9) .EQ. 'VMIS_ISOT') THEN
            DO 119 K=1,NDIMSI
              SIGDV(K) = SIGPDV(K)
 119        CONTINUE
          ENDIF
        ENDIF
C
C       -- 8.1 PARTIE PLASTIQUE:
        DO 100, K=1,NDIMSI
          DO 101, L=1,NDIMSI
            DSIDEP(K,L) =  0.D0
 101      CONTINUE
 100    CONTINUE
C
        A=1.D0
        IF (.NOT.DECH) THEN
          IF (COMPOR(1:9) .EQ. 'VMIS_ISOT') THEN
            SIGEPS = 0.D0
            DO 170 K = 1,NDIMSI
              SIGEPS = SIGEPS + SIGDV(K)*DEPSDV(K)
 170        CONTINUE
            IF(PLASTI.AND.SIGEPS.GE.0.D0) THEN
              A = 1.D0+1.5D0*DEUXMU*DP/RP
              COEF = -  (1.5D0 * DEUXMU)**2/(1.5D0*DEUXMU+RPRIM)/RP**2
     &               *(1.D0 - DP*RPRIM/RP )/A
              DO 135 K=1,NDIMSI
                DO 135 L=1,NDIMSI
                  DSIDEP(K,L) =  COEF*SIGDV(K)*SIGDV(L)
 135          CONTINUE
            ENDIF
          ENDIF
        ENDIF
C
C       -- 8.2 PARTIE ELASTIQUE:
        DO 130 K=1,3
          DO 131 L=1,3
            DSIDEP(K,L) = DSIDEP(K,L)+CO*(TROISK/3.D0-DEUXMU/(3.D0*A))
 131      CONTINUE
 130    CONTINUE
        DO 120 K=1,NDIMSI
          DSIDEP(K,K) = DSIDEP(K,K) + DEUXMU/A
 120    CONTINUE
C
C       -- 8.3 CORRECTION POUR LES CONTRAINTES PLANES :
        IF (CPLAN) THEN
          DO 136 K=1,NDIMSI
            IF (K.EQ.3) GO TO 136
            DO 137 L=1,NDIMSI
              IF (L.EQ.3) GO TO 137
              DSIDEP(K,L)=DSIDEP(K,L)
     &          - 1.D0/DSIDEP(3,3)*DSIDEP(K,3)*DSIDEP(3,L)
 137        CONTINUE
 136      CONTINUE
        ENDIF
      ENDIF

C
 9999 CONTINUE
C FIN ------------------------------------------------------------------
      END
