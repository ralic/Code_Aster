      SUBROUTINE DISIEF (NBT,NEQ,NNO,NC,PGL,KLV,DUL,SIM,
     &                   ILOGIC,KTY2,DULY,SIP,FONO,FOR2,FOR3)
C ----------------------------------------------------------------------
      IMPLICIT REAL * 8 (A-H,O-Z)
      INTEGER NBT,NEQ,ILOGIC,NNO,NC
      REAL*8  PGL(3,3),KLV(NBT),DUL(NEQ),SIM(NEQ),KTY2,DULY
      REAL*8  SIP(NEQ),FONO(NEQ)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/09/1999   AUTEUR F1BHHAJ J.ANGLES 
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
C     CALCUL DES EFFORTS GENERALISES (REPERE LOCAL)
C     ET DES FORCES NODALES (REPERE GLOBAL). COMME ON TRAITE DES
C     ELEMENTS DISCRETS, CES QUANTITES SONT EGALES, AU REPERE PRES.
C
C ----------------------------------------------------------------------
C
C IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       NNO    : NOMBRE DE NOEUDS DE L'ELEMENT (1 OU 2)
C       NC     : NOMBRE DE DDL PAR NOEUD
C       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
C       KLV    : MATRICE DE "RAIDEUR TANGENTE"
C       DUL    : INCREMENT DE DEPLACEMENT LOCAL
C       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
C       ILOGIC : VAUT 1 DANS LES CAS DU COMPORTEMENT "ARME" (ARMEMENT)
C       KTY2   :
C       DULY   :
C
C OUT : SIP    : EFFORTS GENERALISES ACTUALISES
C       FONO   : FORCES NODALES
C
C**************** DECLARATION DES VARIABLES LOCALES ********************
C
      REAL*8 KLC(144),FL(12)
C
C
C************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
      ZERO = 0.D0
      EPSI = 1.D-20
C
C --- DEMI-MATRICE KLV TRANSFORMEE EN MATRICE PLEINE KLC
C
      CALL VECMA (KLV,NBT,KLC,NEQ)
C
C --- CALCUL DE FL = KLC.DUL (INCREMENT D'EFFORT)
C
      CALL PMAVEC ('ZERO',NEQ,KLC,DUL,FL)
C
C --- EFFORTS GENERALISES AUX NOEUDS 1 ET 2 (REPERE LOCAL)
C         ---- ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
C         ---- POUR LES MECA_DIS_TR_L ET MECA_DIS_T_L
C
      IF ( NNO.EQ.1 ) THEN
         DO 90 I = 1,NEQ
            SIP(I)      = FL(I) + SIM(I)
            FL(I)       = FL(I) + SIM(I)
 90      CONTINUE
      ELSEIF ( NNO.EQ.2 ) THEN
         DO 100 I = 1,NC
            SIP(I)      = -FL(I)      + SIM(I)
            SIP(I+NC) =  FL(I+NC) + SIM(I+NC)
            FL(I)       =  FL(I)      - SIM(I)
            FL(I+NC)  =  FL(I+NC) + SIM(I+NC)
 100     CONTINUE
      ENDIF
C      WRITE(6,*) 'SIP FL',SIP,FL
C
C --- PETITE MODIF POUR LES ARMEMENTS
C
      IF (ILOGIC.EQ.1) THEN
         SIP(2) = SIM(2) + KTY2*DULY
         SIP(8) = SIM(8) + KTY2*DULY
         FL(2)  = -SIM(2) - KTY2*DULY
         FL(8) = SIM(8) + KTY2*DULY
      END IF
      IF (ILOGIC.EQ.2) THEN
         IF (NNO.EQ.1) THEN
            FL(1) = KTY2
            FL(2) = FOR2
            FL(3) = FOR3
            SIP(1) = KTY2
            SIP(2) = FOR2
            SIP(3) = FOR3
         ELSEIF (NNO.EQ.2) THEN
            FL(1) = -KTY2
            SIP(1) = KTY2
            FL(1+NC) = KTY2
            SIP(1+NC) = KTY2
            FL(2) = -FOR2
            SIP(2) = FOR2
            FL(2+NC) = FOR2
            SIP(2+NC) = FOR2
            FL(3) = -FOR3
            SIP(3) = FOR3
            FL(3+NC) = FOR3
            SIP(3+NC) = FOR3
         END IF
         IF (ABS(KTY2).LT.EPSI) THEN
            DO 1 N = 1,NEQ
             FL(N) = ZERO
             SIP(N) = ZERO
    1       CONTINUE
         END IF
      END IF
C      IF (NNO.EQ.2) WRITE(6,*) 'FL : ',(FL(N),N=1,NEQ)
C
C --- FORCES NODALES AUX NOEUDS 1 ET 2 (REPERE GLOBAL)
C
      IF (NC.NE.2) THEN
         CALL UTPVLG ( NNO, NC, PGL, FL, FONO )
      ELSE
         CALL UT2VLG ( NNO, NC, PGL, FL, FONO )
      ENDIF
C ----------------------------------------------------------------------
C
 9999 CONTINUE
      END
