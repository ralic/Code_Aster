      SUBROUTINE IRRCVX ( FAMI, KPG, KSP, NMAT, MATER, SIG ,VIN,
     &                    SEUIL )
      IMPLICIT  NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*(*) FAMI
      INTEGER       KPG, KSP, NMAT
      REAL*8        MATER(NMAT,2),SIG(6),VIN(*),SEUIL

C --- BUT : CONVEXE ELASTO PLASTIQUE A T+DT POUR (SIGF , VIND) DONNES --
C ----------------------------------------------------------------------
C IN  : FAMI   :  FAMILLE DES POINTS DE GAUSS  -------------------------
C --- : KPG    :  NUMERO DU POINT DE GAUSS  ----------------------------
C --- : KSP    :  NUMERO DU SOUS POINT DE GAUSS ------------------------
C --- : SIG    :  CONTRAINTE A T+DT ------------------------------------
C --- : VIN    :  VARIABLES INTERNES A T -------------------------------
C --- : NMAT   :  DIMENSION MATER --------------------------------------
C --- : MATER  :  COEFFICIENTS MATERIAU A T+DT -------------------------
C OUT : SEUIL  :  SEUIL  ELASTICITE  A T+DT ----------------------------
C ----------------------------------------------------------------------
C ======================================================================

      REAL*8 IRRAD, IRRAF, ETAI, P, DEV(6),K,N,P0,ETAIS,LCNRTS
      REAL*8 PK,PENPE,KAPPA,R02,PE,SPE
      INTEGER IRET

C ==========================
        SEUIL=1.D0
        GOTO 9999
C ==========================

CC     RECUPERATION DE L IRRADIATION
C      CALL RCVARC('F','IRRA','-',FAMI,KPG,KSP,IRRAD,IRET)
C      CALL RCVARC('F','IRRA','+',FAMI,KPG,KSP,IRRAF,IRET)
CC VARIABLES INTERNES
C      P  = VIN(1)
CC PARAMETRES MATERIAUX
C      K     = MATER(7,2)
C      N     = MATER(8,2)
C      P0    = MATER(9,2)
C      KAPPA = MATER(10,2)
C      R02   = MATER(11,2)
C      PENPE = MATER(13,2)
C      PK    = MATER(14,2)
C      PE    = MATER(15,2)
C      SPE   = MATER(16,2)
C
C      IF ( (IRRAF-IRRAD).GT.0.D0) THEN
C        SEUIL=1.D0
C        GOTO 9999
C      ELSE IF ( (IRRAD-IRRAF).GT.0.D0) THEN
C        impression du message suivant :
C        PROBLEME DANS LA DEFINITION DE LA FLUENCE. 
C        ELLE DIMINUE AU COURS DU TEMPS!
C      ELSE
C         CALL LCDEVI( SIG, DEV )
C         IF      (P.LT.PK) THEN
C            SEUIL = LCNRTS(DEV) - KAPPA*R02
C         ELSE IF (P.LT.PE) THEN
C            SEUIL = LCNRTS(DEV) - ( SPE + PENPE*(P - PE) )
C         ELSE
C            SEUIL = LCNRTS(DEV) - K*((P + P0)**N)
C         ENDIF
C      ENDIF

9999  CONTINUE
      END
