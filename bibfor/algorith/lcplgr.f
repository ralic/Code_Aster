      SUBROUTINE LCPLGR(COMPOR, NDIM  , OPTION, IMATE , CARCRI,
     &                  TREF  , TM    , TP    , EPSM  , DEPS  ,
     &                  SIGM  , VIM   , R     , CHAMP , LAGR  ,
     &                  VIP   , ENER  , DPDA  , PONDER, SIGP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C TOLE CRP_20

      IMPLICIT NONE
      INTEGER      NDIM, IMATE
      REAL*8       CARCRI(*), TREF, TM, TP, EPSM(6), DEPS(6)
      REAL*8       VIM(*), SIGM(6)
      REAL*8       R, CHAMP(0:3), LAGR(0: NDIM), VIP(*), SIGP(6)
      REAL*8       DPDA(0:3, 0:3)
      REAL*8       ENER, PONDER(0:3)
      CHARACTER*16 OPTION, COMPOR

C ----------------------------------------------------------------------
C              LOI DE COMPORTEMENT PLASTIQUE A GRADIENT
C ----------------------------------------------------------------------
C IN  COMPOR  NOM DU COMPORTEMENT
C IN  NDIM    DIMENSION DE L'ESPACE
C IN  OPTION  OPTION DE CALCUL (RAPH_MECA OU DECO_LOCA)
C IN  IMATE   NATURE DU MATERIAU
C IN  CARCRI  CRITERE DE CONVERGENCE (1: MAX ITERATIONS,  3: PRECISION)
C IN  TREF    TEMPERATURE DE REFERENCE
C IN  TM      TEMPERATURE EN T-
C IN  TP      TEMPERATURE EN T+
C IN  EPSM    CHAMP DE DEFORMATION EN T-
C IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION
C IN  SIGM    CONTRAINTES EN T-
C IN  VIM     VARIABLES INTERNES EN T-
C IN  R       PARAMETRE DE PENALISATION                  (DECO_LOCA)
C IN  CHAMP   CHAMP DE PLASTICITE LISSE                  (DECO_LOCA)
C IN  LAGR    MULTIPLICATEURS DE LAGRANGE (/LC SUR GRAD) (DECO_LOCA)
C VAR VIP     VARIABLES INTERNES EN T+
C              IN  ESTIMATION (ITERATION PRECEDENTE)     (DECO_LOCA)
C              OUT CALCULEES                             (DECO_LOCA)
C              IN  VALEURS EN T+                         (RAPH_MECA)
C OUT ENER    ENERGIE                                    (DECO_LOCA)
C OUT DPDA    MATRICE TANGENTE                           (DECO_LOCA)
C OUT PONDER  COEFFICIENT DE PONDERATION                 (DECO_LOCA)
C OUT SIGP    CONTRAINTES EN T+                          (RAPH_MECA)
C ----------------------------------------------------------------------

      LOGICAL     LINE
      CHARACTER*2 CODRET(6)
      CHARACTER*8 NOMRES(6)
      INTEGER     NDIMSI, K
      INTEGER     JPROLP, JVALEP, NBVALP, CAS
      REAL*8      VALRES(6), EPSTH, TROISK, DEUXMU, TROIMU, DEUMUM, NU
      REAL*8      AIRERP, RBID, RP, RPRIM, RPM, EF
      REAL*8      H, SY, LC, LC2
      REAL*8      EPM(6), EPMH, EPSDV(6), EPSH, SIGTR(6), STREQ, SIGH
      REAL*8      COEF, SEQ
      REAL*8      P0M, PGM(3), DP0, P0, PG(3), PG2
      REAL*8      A0TR, AGTR(3), FTR
      REAL*8      ENEREL, ENERPL, ENERDI

      REAL*8      R8NRM2, R8DOT

      REAL*8      KRON(6)
      DATA        KRON /1.D0, 1.D0, 1.D0, 0.D0, 0.D0, 0.D0/
C ----------------------------------------------------------------------



C -- INITIALISATION

      NDIMSI = 2*NDIM
      IF (OPTION.NE.'RAPH_MECA' .AND. OPTION.NE.'DECO_LOCA')
     & CALL UTMESS('F','LCPLGR','OPTION '//OPTION//' NON PREVUE (DVLP)')



C ======================================================================
C                      CARACTERISTIQUES MATERIAU
C ======================================================================


C -- COEFFICIENT DE CISAILLEMENT EN T-

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      
      CALL RCVALA(IMATE,' ','ELAS',1,'TEMP',TM,2,NOMRES,VALRES,
     &           CODRET,'F ' )
      DEUMUM = VALRES(1) / (1+VALRES(2))


C -- CARACTERISTIQUES MATERIAU EN T+

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
      NOMRES(4) = 'D_SIGM_EPSI'
      NOMRES(5) = 'SY'
      NOMRES(6) = 'LONG_CARA'

      CALL RCVALA (IMATE, ' ','ELAS', 1,'TEMP',TP,2,NOMRES,
     &             VALRES,CODRET, 'F ' )
      CALL RCVALA (IMATE, ' ','ELAS', 1,'TEMP',TP,1,NOMRES(3),
     &             VALRES(3),CODRET(3), '  ' )
      IF ( CODRET(3) .NE. 'OK' ) VALRES(3) = 0.D0
      CALL RCVALA(IMATE,' ', 'NON_LOCAL', 1, 'TEMP',TP,1,NOMRES(6),
     &            VALRES(6), CODRET(6), 'F ')


      IF (COMPOR .EQ. 'VMIS_ISOT_LINE') THEN
        LINE = .TRUE.
        CALL RCVALA(IMATE,' ', 'ECRO_LINE', 1,'TEMP',TP,2, NOMRES(4),
     &            VALRES(4), CODRET(4), 'F ' )
        H      = VALRES(4)*VALRES(1) / (VALRES(1) - VALRES(4))
        SY     = VALRES(5)

      ELSE
        LINE = .FALSE.
        CALL RCTRAC(IMATE,'TRACTION','SIGM',TP,JPROLP,JVALEP,
     &                NBVALP,VALRES(1))
        CALL RCFONC('S','TRACTION',JPROLP,JVALEP,NBVALP,SY,RBID,
     &                RBID,RBID,RBID,RBID,RBID,RBID,RBID)     
        CALL RCFONC('V','TRACTION',JPROLP,JVALEP,NBVALP,RBID,RBID,
     &                RBID,VIM(1),RPM,H,AIRERP,RBID,RBID)
        RPM = RPM - SY
      END IF

      NU     = VALRES(2)
      EPSTH  = VALRES(3) * (TP - TREF)      
      TROISK = VALRES(1) / (1.D0-2.D0*VALRES(2))
      DEUXMU = VALRES(1) / (1.D0+VALRES(2))
      TROIMU = 1.5D0 * DEUXMU
      LC     = 2*VALRES(6)/SQRT(13.D0)
      LC2    = LC*LC



C ======================================================================
C                  PARTIE MECANIQUE DU COMPORTEMENT
C ======================================================================


C -- CONTRAINTES ELASTIQUES

C    DEFORMATION PLASTIQUE EN T-
      DO 10 K = 1, NDIMSI
        EPM(K) = EPSM(K) - SIGM(K)/DEUMUM
 10   CONTINUE
      EPMH = (EPM(1) + EPM(2) + EPM(3)) / 3
      DO 20 K = 1, 3
        EPM(K) = EPM(K) - EPMH
 20   CONTINUE

C    DEVIATEUR ET TRACE DES DEFORMATIONS EN T+
      EPSH = (EPSM(1)+EPSM(2)+EPSM(3) + DEPS(1)+DEPS(2)+DEPS(3)) / 3
      DO 30 K = 1, NDIMSI
        EPSDV(K) = (EPSM(K) + DEPS(K)) - EPSH*KRON(K)
 30   CONTINUE

C    DEVIATEUR ET TRACE DES CONTRAINTES ELASTIQUES
      DO 40 K = 1, NDIMSI
        SIGTR(K) = DEUXMU * (EPSDV(K) - EPM(K))
 40   CONTINUE
      STREQ = SQRT(1.5D0) * R8NRM2(NDIMSI, SIGTR,1)
      SIGH  = TROISK * (EPSH - EPSTH)


C -- TENSEUR DES CONTRAINTES

      IF (OPTION.EQ.'RAPH_MECA') THEN

C      INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
        DP0 = VIP(1) - VIM(1)

C      CONTRAINTE EQUIVALENTE
        SEQ = STREQ - TROIMU*DP0
        IF (SEQ.LT.0) SEQ = 0.D0

C      TENSEUR DES CONTRAINTES
        COEF = 0.D0
        IF (STREQ .NE. 0.D0) COEF = SEQ/STREQ
        DO 110 K = 1, NDIMSI
          SIGP(K) = SIGH*KRON(K) + COEF*SIGTR(K)
 110    CONTINUE

        GOTO 9999
      END IF



C ======================================================================
C                   PARTIE NON LOCALE DU COMPORTEMENT
C ======================================================================


C -- INTIALISATION

      P0M = VIM(1)
      CALL R8COPY(NDIM, VIM(2),1, PGM,1)


C -- PREDICTION ELASTIQUE

      IF (LINE) THEN
        A0TR = -H*P0M + LAGR(0) + R*H*(CHAMP(0)-P0M)
      ELSE
        A0TR = -RPM   + LAGR(0) + R*H*(CHAMP(0)-P0M)
      END IF
      DO 200 K = 1,NDIM
        AGTR(K) = -H*LC2*PGM(K) + LAGR(K) + R*H*LC2*(CHAMP(K)-PGM(K))
 200  CONTINUE


C -- EVOLUTION DU GRADIENT
      COEF = (1+R)*H*LC2
      DO 205 K = 1, NDIM
        PG(K) = PGM(K) + AGTR(K)/COEF
 205  CONTINUE                      
      PG2 = R8DOT(NDIM, PG,1, PG,1)
      
      
C -- SOLUTION ELASTIQUE

      FTR = STREQ+A0TR-SY 
      IF ( FTR .LE. 0 ) THEN
        CAS   = 0
        DP0   = 0
        SEQ   = STREQ
        GOTO 5000
      END IF


C -- CAS DISSIPATIF 

      IF (LINE) THEN

        DP0 = FTR/(TROIMU+R*H+H)
        SEQ = STREQ - TROIMU*DP0
        CAS = 1

        IF (SEQ.LT.0) THEN
          DP0 = (A0TR-SY)/(R*H+H)
          SEQ = 0
          CAS = 2
        END IF

        RPRIM = H
      ELSE

        COEF = 2.D0*(1.D0+NU)/3.D0
        EF = (R*H+TROIMU) * COEF 
        CALL RCFONC('E','TRACTION',JPROLP,JVALEP,NBVALP,RBID,EF,NU,
     &               P0M, RP, RPRIM, AIRERP, STREQ+A0TR+RPM, DP0)
        SEQ = STREQ - TROIMU*DP0
        CAS = 1

        IF (SEQ .LT. 0) THEN
          EF = R*H * COEF
          CALL RCFONC('E','TRACTION',JPROLP,JVALEP,NBVALP,RBID,EF,NU,
     &                 P0M, RP, RPRIM, AIRERP, A0TR+RPM, DP0)
          SEQ = 0
          CAS = 2
        END IF

        RP = RP - SY
      END IF



C -- STOCKAGE DES VARIABLES INTERNES

 5000 CONTINUE
       
      P0   = P0M + DP0
      VIP(1) = P0
      CALL R8COPY(NDIM, PG,1, VIP(2),1)
      VIP(5) = 0.D0
      VIP(6) = CAS


C -- CALCUL DE L'ENERGIE

      IF (LINE) THEN
        AIRERP = 0.5D0 * H * P0**2
      ELSE
        AIRERP = AIRERP - SY*P0
      END IF
      ENEREL = 1.5D0 * SIGH**2/TROISK + 1/3.D0 * SEQ**2/DEUXMU
      ENERPL = AIRERP + 0.5D0 * H*LC2 * PG2
      ENERDI = SY*DP0
      ENER   = ENEREL + ENERPL + ENERDI


C -- CALCUL DES COEFFICIENTS DE PONDERATION

      PONDER(0) = H
      DO 500 K = 1,NDIM
        PONDER(K) = H*LC2
 500  CONTINUE



C ======================================================================
C                      CALCUL DE LA MATRICE TANGENTE
C ======================================================================


      CALL R8INIR(16, 0.D0, DPDA,1)


C -- TERMES DPG / DAG

      DO 600 K = 1,NDIM
        DPDA(K,K) = R/(1+R)
 600  CONTINUE
 
 
C -- TERME DP0 / DA0

      IF (CAS .EQ. 0) THEN
        DPDA(0,0) = 0

      ELSE IF (CAS .EQ. 1) THEN
        DPDA(0,0) = R*H / (R*H + TROIMU + RPRIM)

      ELSE 
        DPDA(0,0) = R*H / (R*H + RPRIM)

      END IF
   
   
 9999 CONTINUE
      END
