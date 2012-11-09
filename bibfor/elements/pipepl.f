      SUBROUTINE PIPEPL(NDIM  ,COMPOR,TYPMOD,TAU   ,MATE  ,
     &                  SIGM  ,VIM   ,EPSP  ,EPSD  ,A0    ,
     &                  A1    ,A2    ,A3    ,ETAS  )
C
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
C
      IMPLICIT NONE
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       COMPOR
      INTEGER            NDIM, MATE
      REAL*8             EPSP(6), EPSD(6), TAU
      REAL*8             VIM(2), SIGM(6)
      REAL*8             A0, A1, A2, A3,ETAS
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS)
C
C LOI DE COMPORTEMENT PLASTIQUE VMIS_ISOT_*
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  COMPOR : NOM DU COMPORTEMENT
C IN  TYPMOD : TYPE DE MODELISATION
C IN  TAU    : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
C IN  MATE   : NATURE DU MATERIAU
C IN  SIGM   : CONTRAINTE EN T-
C IN  VIM    : VARIABLES INTERNES EN T-
C IN  EPSP   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
C IN  EPSD   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
C OUT A0     : LINEARISATION DU CRITERE : FEL = A0 + A1*ETA
C OUT A1     : IDEM A0
C OUT A2     : IDEM A0 POUR LA 2E SOLUTION EVENTUELLE. R8VIDE SINON
C OUT A3     : IDEM A1 POUR LA 2E SOLUTION EVENTUELLE. R8VIDE SINON
C OUT ETAS   : SI PAS DE SOLUTION : LE MINIMUM. R8VIDE SINON
C
C ----------------------------------------------------------------------
C
      INTEGER     NBRES
      PARAMETER   (NBRES=4)
      INTEGER ICODRE(NBRES)
      CHARACTER*8 NOMRES(NBRES),FAMI,POUM
      REAL*8      VALRES(NBRES)
C
      LOGICAL     CPLAN
      INTEGER     NDIMSI, K, NRAC, JPROL, JVALE, NBVALE,KPG,SPT
      REAL*8      SIGMH, EPSPH, EPSDH, S0H, S1H, S0(6), S1(6)
      REAL*8      KRON(6)
      REAL*8      P0, P1, P2, ETA, RAC(2)
      REAL*8      YOUNG, NU, DEUXMU, RP, R8BID,H,ET,SY
      REAL*8      R8VIDE,DDOT
C
      DATA        KRON /1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C
C ----------------------------------------------------------------------
C

C -- OPTION ET MODELISATION
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      NDIMSI = 2*NDIM
      CPLAN  = (TYPMOD(1).EQ.'C_PLAN  ')
C
      IF (CPLAN) CALL U2MESS('F','PILOTAGE_1')

C -- LECTURE DES CARACTERISTIQUES

      IF (COMPOR .EQ. 'VMIS_ISOT_LINE') THEN

        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'SY'
        NOMRES(4) = 'D_SIGM_EPSI'
        CALL RCVALB(FAMI,KPG,SPT,POUM,MATE ,' '   ,'ELAS',0 ,' ',
     &              0.D0 ,2 ,NOMRES,VALRES,ICODRE,  2)
        CALL RCVALB(FAMI,KPG,SPT,POUM,MATE ,' '   ,'ECRO_LINE',0 ,' ',
     &              0.D0 ,2 ,NOMRES(3)  ,VALRES(3),ICODRE(3), 2)
        YOUNG  = VALRES(1)
        NU     = VALRES(2)
        SY     = VALRES(3)
        ET     = VALRES(4)
        H      = YOUNG*ET/(YOUNG-ET)
        RP     = SY + H*VIM(1)

      ELSE
        CALL RCVALB(FAMI,KPG,SPT,POUM,MATE  ,' ','ELAS',0 ,' ' ,
     &              0.D0 ,1 ,'NU' ,NU   ,ICODRE, 2)
        CALL RCTRAC(MATE  ,1,'SIGM',0.D0  ,JPROL ,
     &              JVALE ,NBVALE   ,YOUNG)
        CALL RCFONC('V'   ,1,JPROL ,JVALE ,NBVALE,
     &              R8BID ,R8BID     ,R8BID ,VIM(1),RP    ,
     &              R8BID ,R8BID     ,R8BID ,R8BID )
      END IF

      DEUXMU = YOUNG/(1.D0+NU)
C
C ======================================================================
C                CALCUL DES DEFORMATIONS POUR LINEARISATION
C ======================================================================
C
C    PARTITION TRACE / DEVIATEUR

      SIGMH  = (SIGM(1)+SIGM(2)+SIGM(3))/3
      EPSPH  = (EPSP(1)+EPSP(2)+EPSP(3))/3
      EPSDH  = (EPSD(1)+EPSD(2)+EPSD(3))/3

      S0H    = DEUXMU*EPSPH + SIGMH
      S1H    = DEUXMU*EPSDH
      DO 10 K = 1, NDIMSI
        S0(K) = SIGM(K) + DEUXMU*EPSP(K) - S0H*KRON(K)
        S1(K) = DEUXMU*EPSD(K) - S1H*KRON(K)
 10   CONTINUE


C    COEFFICIENTS DE LA FORME QUADRATIQUE DU CRITERE
C      FEL = SQRT(P0 + 2P1 ETA + P2 ETA**2) - 1

      P0     = DDOT(NDIMSI,S0,1,S0,1) * (1.5D0 / RP**2)
      P1     = DDOT(NDIMSI,S0,1,S1,1) * (1.5D0 / RP**2)
      P2     = DDOT(NDIMSI,S1,1,S1,1) * (1.5D0 / RP**2)


C    POINT A DEVIATEUR NUL : PAS DE PILOTAGE POSSIBLE
      IF (P2 .EQ. 0) THEN
        A0 = 0.D0
        A1 = 0.D0
        A2 = 0.D0
        A3 = 0.D0
        GOTO 9999
      END IF

C    RECHERCHE DES INTERSECTIONS ELLIPSE / DROITE
      CALL ZEROP2(2*P1/P2, (P0-(1+TAU)**2)/P2, RAC, NRAC)

C    PAS DE SOLUTION : POINT LE PLUS PROCHE
      IF (NRAC .EQ. 0) THEN
        ETAS   = - P1/P2

C    UNE OU DEUX SOLUTIONS : ON LINEARISE AUTOUR DES DEUX
      ELSE IF (NRAC.EQ.1) THEN
        ETA    = RAC(1)
        A1     = (P2*ETA+P1)/(1+TAU)
        A0     = TAU - A1*ETA
        A2     = R8VIDE()
        A3     = R8VIDE()
      ELSE
        ETA    = RAC(1)
        A1     = (P2*ETA+P1)/(1+TAU)
        A0     = TAU - A1*ETA
        ETA    = RAC(2)
        A3     = (P2*ETA+P1)/(1+TAU)
        A2     = TAU - A3*ETA
      ENDIF

 9999 CONTINUE
      END
