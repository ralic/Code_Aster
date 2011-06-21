      SUBROUTINE DISIEF(NBT,NEQ,NNO,NC,PGL,
     &                  KLV,DUL,SIM,ILOGIC,DULY,
     &                  SIP,FONO,FORCE,DIMELE)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NBT,NEQ,ILOGIC,NNO,NC,DIMELE
      REAL*8   PGL(3,3),KLV(NBT),DUL(NEQ),SIM(NEQ),DULY
      REAL*8   SIP(NEQ),FONO(NEQ),FORCE(3)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/06/2011   AUTEUR PELLET J.PELLET 
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
C       DULY   :
C
C OUT : SIP    : EFFORTS GENERALISES ACTUALISES
C       FONO   : FORCES NODALES
C
C =============== DECLARATION DES VARIABLES LOCALES ====================
C
      INTEGER  N,I
      REAL*8   KLC(144),FL(12),ZERO,R8PREM
C
C ----------------------------------------------------------------------
C
      ZERO = 0.D0
C --- DEMI-MATRICE KLV TRANSFORMEE EN MATRICE PLEINE KLC
      CALL VECMA (KLV,NBT,KLC,NEQ)
C --- CALCUL DE FL = KLC.DUL (INCREMENT D'EFFORT)
      CALL PMAVEC ('ZERO',NEQ,KLC,DUL,FL)
C --- EFFORTS GENERALISES AUX NOEUDS 1 ET 2 (REPERE LOCAL)
C     ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
C     POUR LES MECA_DIS_TR_L ET MECA_DIS_T_L
      IF ( NNO.EQ.1 ) THEN
         DO 90 I = 1,NEQ
            SIP(I)      = FL(I) + SIM(I)
            FL(I)       = FL(I) + SIM(I)
 90      CONTINUE
      ELSEIF ( NNO.EQ.2 ) THEN
         DO 100 I = 1,NC
            SIP(I)      = -FL(I)    + SIM(I)
            SIP(I+NC)   =  FL(I+NC) + SIM(I+NC)
            FL(I)       =  FL(I)    - SIM(I)
            FL(I+NC)    =  FL(I+NC) + SIM(I+NC)
 100     CONTINUE
      ENDIF
C
C --- PETITE MODIF POUR LES ARMEMENTS
      IF (ILOGIC.EQ.1) THEN
         SIP(2) =  SIM(2) + FORCE(1)*DULY
         SIP(8) =  SIM(8) + FORCE(1)*DULY
         FL(2)  = -SIM(2) - FORCE(1)*DULY
         FL(8)  =  SIM(8) + FORCE(1)*DULY
      ENDIF
      IF (ILOGIC.EQ.2) THEN
         IF (NNO.EQ.1) THEN
            FL(1)  = FORCE(1)
            FL(2)  = FORCE(2)
            SIP(1) = FORCE(1)
            SIP(2) = FORCE(2)
            IF ( DIMELE.EQ.3 ) THEN
               FL(3)  = FORCE(3)
               SIP(3) = FORCE(3)
            ENDIF
         ELSEIF (NNO.EQ.2) THEN
            FL(1)     = -FORCE(1)
            SIP(1)    =  FORCE(1)
            FL(1+NC)  =  FORCE(1)
            SIP(1+NC) =  FORCE(1)
            FL(2)     = -FORCE(2)
            SIP(2)    =  FORCE(2)
            FL(2+NC)  =  FORCE(2)
            SIP(2+NC) =  FORCE(2)
            IF ( DIMELE.EQ.3 ) THEN
               FL(3)     = -FORCE(3)
               SIP(3)    =  FORCE(3)
               FL(3+NC)  =  FORCE(3)
               SIP(3+NC) =  FORCE(3)
            ENDIF
         ENDIF
         IF ( ABS(FORCE(1)).LT.R8PREM() ) THEN
            DO 10 N = 1,NEQ
               FL(N)  = ZERO
               SIP(N) = ZERO
10          CONTINUE
         ENDIF
      ENDIF
C
C --- FORCES NODALES AUX NOEUDS 1 ET 2 (REPERE GLOBAL)
      IF (NC.NE.2) THEN
         CALL UTPVLG ( NNO, NC, PGL, FL, FONO )
      ELSE
         CALL UT2VLG ( NNO, NC, PGL, FL, FONO )
      ENDIF
      END
