      SUBROUTINE MDFLAM( DNORM,VITLOC,KNORM,COST,SINT,FLIM,FSEUIL,
     &                   RIGIFL,DEFPLA,FNORMA,FLOCAL,VNORM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C***********************************************************************
C 01/01/91    G.JACQUART AMV/P61 47 65 49 41
C***********************************************************************
C     FONCTION  : CALCULE LA DISTANCE NORMALE A L'OBSTACLE (<0 SI CHOC)
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C    DNORM          <--   DISTANCE NORMALE A L'OBSTACLE
C    VITLOC         <--   VITESSE DANS LE REPERE LOCAL
C    COST,SINT      <--   DIRECTION NORMALE A L'OBSTACLE
C    KNORM          <--   RAIDEUR NORMALE DE CHOC
C    FNORMA          -->  FORCE NORMALE DE CHOC  (MODULE)
C    FLOCAL          -->  FORCE NORMALE DE CHOC REP. LOCAL
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 VITLOC(3),FLOCAL(3),KNORM,FNORMA
      VNORM = VITLOC(2)*COST + VITLOC(3)*SINT
C
      IF (DEFPLA .LE. 0.D0) THEN
C     --- FLAMBAGE NON ENCORE RENCONTRE ---
         IF (-DNORM .LT. 0.D0) THEN
            FNORMA = 0.0D0
         ELSE
            IF (-DNORM .LT. FLIM/KNORM) THEN
               FNORMA = -KNORM*DNORM
            ELSE
C           --- DEBUT DU FLAMBAGE ---
               FNORMA = FLIM
               DEFPLA = FLIM/KNORM - FSEUIL/RIGIFL
               IF (DEFPLA .LE. 0.D0) DEFPLA = 1.D-20
            ENDIF
         ENDIF
      ELSE
C     --- LE FLAMBAGE A DEJA EU LIEU ---
         IF (-DNORM .LT. DEFPLA) THEN
            FNORMA = 0.0D0
         ELSE
            IF (VNORM.GT.0.D0 .OR.
     &           -DNORM.LE.(FSEUIL/RIGIFL+DEFPLA)) THEN
               FNORMA = -RIGIFL*(DNORM+DEFPLA)
               IF (FNORMA .LT. 0.D0) FNORMA = 0.D0
            ELSE
               FNORMA = FSEUIL
               DEFPLA = -DNORM - FSEUIL/RIGIFL
            ENDIF
         ENDIF
      ENDIF
      FLOCAL(1)=0.D0
      FLOCAL(2)=FNORMA*COST
      FLOCAL(3)=FNORMA*SINT
 9999 CONTINUE
      END
