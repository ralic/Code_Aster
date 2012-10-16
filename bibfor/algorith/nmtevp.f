      SUBROUTINE NMTEVP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP,DEMU,CINCO,IRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/10/2012   AUTEUR PROIX J-M.PROIX 
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
C TOLE CRP_7
C RESPONSABLE SFAYOLLE S.FAYOLLE
      IMPLICIT NONE
      INTEGER NDIM,IMATE,KPG,KSP,IRET

      REAL*8 CRIT(6),INSTAM,INSTAP
      REAL*8 DEPS(6),DEUXMU,DEMU,CINCO
      REAL*8 SIGM(6),VIM(5),SIGP(6),VIP(5),DSIDEP(6,6)

      CHARACTER*(*) FAMI
      CHARACTER*8   TYPMOD(*)
      CHARACTER*16  COMPOR(3),OPTION
C ----------------------------------------------------------------------
C     INTEGRATION DE LA LOI DE JOHNSON-COOK
C     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
C
C IN  KPG,KSP  : NUMERO DU (SOUS)POINT DE GAUSS
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
C OUT DEUXMU,CINCO : POUR ELEMEMTS INCOMPRESSIBLES
C OUT IRET    : CODE RETOUR DE L'INTEGRATION DE LA LOI DE VOM MISES
C               = 1  => PAS DE PROBLEME
C               = 0  => ECHEC DANS L'INTEGRATION DE LA LOI
C
      COMMON /RCONM9/ACOOK,BCOOK,CCOOK,NPUIS,MPUIS,
     &               EPSP0,TROOM,TMELT,TP,DINST,SIELEQ,DEUXMU,RPRIM,PM
      REAL*8   NMCRI9
      EXTERNAL NMCRI9
C
      LOGICAL PLASTI,INCO,DECH

      INTEGER NDIMSI
      INTEGER K,L,NITER,IBID,I
      INTEGER IRET3, IRET4, IRET0,IRET5

      REAL*8 DEPSTH(6),VALRES(8),EPSTHE,PM,CO,DP0,TM,RPRIM0,PRECR
      REAL*8 DEPSMO,SIGMMO,E,NU,TROISK,RPRIM,RP
      REAL*8 SIELEQ,SIGEPS,SEUIL,DP,COEF,SIGY
      REAL*8 KRON(6),DEPSDV(6),SIGMDV(6),SIGPDV(6),SIGDV(6)
      REAL*8 EM,NUM,TROIKM,DEUMUM,SIGMP(6),SIGEL(6),A
      REAL*8 TP,DEFAM(6),DEFAP(6)
      REAL*8 RAC2,DPMAX,FMAX,COEF1
      REAL*8 SIGPMO,DINST
      REAL*8 ACOOK,BCOOK,CCOOK,NPUIS,MPUIS,EPSP0,TROOM,TMELT
      REAL*8 ALPHA, DIRR, DTE, DGDTSG, DKDTSK, KHI, TPDSDT, EPSPET

      INTEGER     CODRET(8),ITER
      CHARACTER*6 EPSA(6)
      CHARACTER*8 NOMRES(8)
      CHARACTER*16  METH

      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA EPSA   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',
     &              'EPSAYZ'/
C DEB ------------------------------------------------------------------
C
C     -- 1 INITIALISATIONS :
C     ----------------------
      INCO  =  TYPMOD(2) .EQ. 'INCO'
      DECH  =  OPTION(11:14).EQ.'ELAS'
      IF (INCO) THEN
        CO = 0.D0
      ELSE
        CO = 1.D0
      ENDIF
      NDIMSI = 2*NDIM
      RAC2 = SQRT(2.D0)
C
C     -- 2 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
      NOMRES(1)='E'
      NOMRES(2)='NU'
C
      CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,TM,IRET3)
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,TP,IRET4)
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

      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,
     +            2,NOMRES(1),VALRES(1),CODRET(1), 1 )
      EM  = VALRES(1)
      NUM = VALRES(2)
      DEUMUM = EM/(1.D0+NUM)
      IF (INCO) THEN
        TROIKM = DEUMUM
      ELSE
        TROIKM = EM/(1.D0-2.D0*NUM)
      ENDIF
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,
     +            2,NOMRES(1),VALRES(1),CODRET(1), 1 )
      E  = VALRES(1)
      NU = VALRES(2)

      IF (INCO) THEN
        DEUXMU = 2.D0*E/3.D0
        TROISK = DEUXMU
      ELSE
        DEUXMU = E/(1.D0+NU)
        TROISK = E/(1.D0-2.D0*NU)
      ENDIF
      CALL VERIFT(FAMI,KPG,KSP,'T',IMATE,'ELAS',1,EPSTHE,IRET0)

      IF (IRET4.EQ.0) THEN
        NOMRES(1)='ALPHA'
         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,
     +                 1,NOMRES(1),VALRES(1),CODRET(1), 0 )
        IF ( CODRET(1) .NE. 0 ) VALRES(1) = 0.D0
        ALPHA = VALRES(1)
      ENDIF
C
C     -- 3 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
      PLASTI=(VIM(2).GE.0.5D0)

      NOMRES(1)='A'
      NOMRES(2)='B'
      NOMRES(3)='N_PUIS'
      NOMRES(4)='C'
      NOMRES(5)='M_PUIS'
      NOMRES(6)='EPSP0'
      NOMRES(7)='TROOM'
      NOMRES(8)='TMELT'
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ECRO_COOK',0,' ',0.D0,
     &                      3,NOMRES,VALRES,CODRET, 1 )
      ACOOK = VALRES(1)
      BCOOK = VALRES(2)
      NPUIS = VALRES(3)

      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ECRO_COOK',0,' ',0.D0,
     &                      5,NOMRES(4),VALRES(4),CODRET(4), 0 )

      IF ( CODRET(4) .NE. 0 ) THEN
        VALRES(4) = 0.D0
        VALRES(6) = 1.D0
      ENDIF
      CCOOK = VALRES(4)
      EPSP0  = VALRES(6)
      IF ( CODRET(5) .NE. 0 ) THEN
        VALRES(5) = 0.D0
        VALRES(7) = -1.D0
        VALRES(8) = 1.D0
      ENDIF
      MPUIS = VALRES(5)
      TROOM  = VALRES(7)
      TMELT  = VALRES(8)
      DINST = INSTAP-INSTAM

      IF(IRET3 .NE.0 .AND. IRET4 .NE.0)THEN
        TM = TROOM
        TP = TROOM
      ENDIF

      CALL ECCOOK(ACOOK,BCOOK,CCOOK,NPUIS,MPUIS,
     &           EPSP0,TROOM,TMELT,TM,VIM(4),VIM(1),VIM(3),RP,RPRIM)


      DEMU = DEUXMU
      IF (INCO) THEN
        CINCO =(1.D0-2.D0*NU)/NU
      ENDIF
C
C     -- 4 CALCUL DE DEPSMO ET DEPSDV :
C     --------------------------------

      DEPSMO = 0.D0
      DO 110 K=1,3
        DEPSTH(K)   = DEPS(K) -EPSTHE
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

C=======================================================================
C     -- 7 CALCUL DE SIGP,SIGPDV,VIP,DP,RP:
C=======================================================================

      DP=0.D0
      IF ( OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA'     ) THEN

C       -- 7.1 CALCUL DE DP :

          IF (SEUIL.LE.0.D0) THEN
             DP = 0.D0
             VIP(2) = 0.D0
             VIP(3)=DP
             VIP(4)=DINST
          ELSE

             PM = VIM(1)

             DP0 = (SIELEQ - RP)/(1.5D0*DEUXMU)
             CALL ECCOOK(ACOOK,BCOOK,CCOOK,NPUIS,MPUIS,
     &              EPSP0,TROOM,TMELT,TP,DINST,PM,DP0,SIGY,RPRIM0)
             PRECR = CRIT(3) * SIGY
             NITER = NINT(CRIT(1))
C     -------------------------------------------------------
C
C ---        F0 < 0 , ON CHERCHE DPMAX PAS TROP GRAND TEL QUE FMAX < 0
C
             FMAX=SIGY
             DPMAX=DP0
             COEF1=2.D0
             IF (ABS(FMAX).LE.PRECR) THEN
                DP = DPMAX
                ITER=1
                GOTO 50
             ELSEIF (FMAX.GT.0.D0) THEN
C               FMAX > 0.
C               VERIFICATION QUE DPMAX N'EST PAS TROP GRAND.BRACKETTING
                DO 31 I = 1, NITER
                   DPMAX = DPMAX/COEF1
                   FMAX=NMCRI9(DPMAX)
                   IF (ABS(FMAX).LE.PRECR) THEN
                      DP = DPMAX
                      ITER=I
                      GOTO 50
                   ELSEIF (FMAX.LT.0.D0) THEN
C                     ON RECALCULE LA VALEUR PRECEDENTE DE DPMAX
                      DPMAX = DPMAX*COEF1
                      FMAX=NMCRI9(DPMAX)
                      GOTO 21
                   ENDIF
  31            CONTINUE
                GOTO 21

             ELSE
C               FMAX <0. On augmente DPMAX jusqu'à ce que F(DPMAX) > 0
                DO 32 I = 1, NITER
                   FMAX=NMCRI9(DPMAX)
                   IF (ABS(FMAX).LE.PRECR) THEN
                      DP = DPMAX
                      ITER=I
                      GOTO 50
                   ELSEIF (FMAX.GT.0.D0) THEN
                      GOTO 21
                   ELSE
                      DPMAX = DPMAX*COEF1
                   ENDIF
  32            CONTINUE
                CALL U2MESS('A','ALGORITH6_79')
                GOTO 21
             ENDIF
             
   21        CONTINUE


C     -------------------------------------------------------
C            RESOLUTION 1D
C     -------------------------------------------------------
C            RECUPERATION DE L'ALGORITHME DE RESOLUTION 1D
             CALL UTLCAL('VALE_NOM',METH,CRIT(6))
C     -------------------------------------------------------
             CALL ZEROFR(2,METH,NMCRI9,0.D0,DPMAX,PRECR,NITER,DP,IRET,
     &                   ITER)

  50         CONTINUE

             VIP(2)=ITER
             
             IF(IRET.EQ.1) GOTO 9999
             
             CALL ECCOOK(ACOOK,BCOOK,CCOOK,NPUIS,MPUIS,
     &              EPSP0,TROOM,TMELT,TP,DINST,PM,DP,RP,RPRIM)
             VIP(3)=DP
             VIP(4)=DINST
             
          ENDIF
          
          VIP(1) = VIM(1) + DP
          PLASTI=(VIP(2).GE.0.5D0)
C
C         -- 7.2 CALCUL DE SIGP :
C         -----------------------

          SIELEQ = 0.D0
          SIGPMO = SIGMMO + CO*TROISK*DEPSMO
          DO 160 K = 1,NDIMSI
            SIGPDV(K) = SIGMDV(K) + DEUXMU * DEPSDV(K)
            SIGPDV(K) = SIGPDV(K)*RP/(RP+1.5D0*DEUXMU*DP)
            SIELEQ    = SIELEQ + SIGPDV(K)**2
            SIGP(K)   = SIGPDV(K) + SIGPMO*KRON(K)
 160      CONTINUE
          SIELEQ = SQRT(1.5D0*SIELEQ)
C
C         -- 7.3 CALCUL DES DISSIPATIONS :
C         --------------------------------
C         -- 7.3.1 CALCUL DES DISSIPATIONS IRREVERSIBLES :
C         ------------------------------------------------
C         FACTEUR DE TAYLOR-QUINNEY
          KHI = 0.9D0
          DIRR = KHI * SIELEQ * DP/DINST
C
C         -- 7.3.2 CALCUL DES DISSIPATIONS THERMOELASTIQUE :
C         --------------------------------------------------
          DTE  = 0.D0
          IF((E.NE.EM).OR.(NU.NE.NUM))THEN
             DGDTSG = (DEUXMU-DEUMUM)/(TP-TM)/DEUXMU
             DKDTSK = (TROISK-TROIKM)/(TP-TM)/TROISK
          ELSE
             DGDTSG = 0.D0
             DKDTSK = 0.D0
          ENDIF
          
          DO 180 K = 1, NDIMSI
             IF(PLASTI)THEN
                EPSPET = (DEPS(K)-3.D0*SIGPDV(K)*DP/(2.D0*SIELEQ))/DINST
             ELSE
                EPSPET = DEPS(K)/DINST
             ENDIF
             TPDSDT=TP*(DGDTSG*SIGP(K)
     &            +KRON(K)*((DKDTSK-DGDTSG)*SIGPMO-TROISK*ALPHA))
             DTE = DTE + TPDSDT*EPSPET
 180      CONTINUE
          VIP(5) = DTE + DIRR
C
      ENDIF

C=======================================================================
C     -- 8 CALCUL DE DSIDEP(6,6) :
C=======================================================================
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
          IF (COMPOR(1)(1:5) .EQ. 'VMIS_') THEN
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
          IF (COMPOR(1)(1:5) .EQ. 'VMIS_') THEN
            SIGEPS = 0.D0
            DO 170 K = 1,NDIMSI
              SIGEPS = SIGEPS + SIGDV(K)*DEPSDV(K)
 170        CONTINUE
            IF(PLASTI.AND.SIGEPS.GE.0.D0) THEN
              A = 1.D0+1.5D0*DEUXMU*DP/RP
              COEF = -  (1.5D0 * DEUXMU)**2/(1.5D0*DEUXMU+RPRIM)/RP**2
     +               *(1.D0 - DP*RPRIM/RP )/A
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
      ENDIF
C=======================================================================

C
 9999 CONTINUE
C FIN ------------------------------------------------------------------
      END
