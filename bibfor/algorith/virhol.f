      SUBROUTINE VIRHOL(NBVARI,VINTM,VINTP,ADVIHY,VIHRHO,RHO110,
     +            DP1,DP2,DPAD,CLIQ,DT,ALPLIQ,SIGNE,RHO11,RHO11M,RETCOM)
      IMPLICIT      NONE
      INTEGER       NBVARI,RETCOM,ADVIHY,VIHRHO
      REAL*8        VINTM(NBVARI),VINTP(NBVARI),RHO110,DP1,DP2,DPAD,DT
      REAL*8        CLIQ,SIGNE,ALPLIQ,RHO11,RHO11M
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C ======================================================================
C --- CALCUL ET STOCKAGE DE LA VARIABLE INTERNE DE LA MASSE ------------
C --- VOLUMIQUE DE L EAU -----------------------------------------------
C ======================================================================
      REAL*8        VARBIO,EPXMAX
      PARAMETER    (EPXMAX = 5.D0)
C ======================================================================
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C ======================================================================
C --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
C --- ET VERIFICATION DE LA COHERENCE ----------------------------------
C ======================================================================
      VARBIO = (DP2-SIGNE*DP1-DPAD)*CLIQ - 3.D0*ALPLIQ*DT
      IF (VARBIO.GT.EPXMAX) THEN
         RETCOM = 2
         GO TO 30
      ENDIF
      VINTP(ADVIHY+VIHRHO) =
     +              - RHO110 + (VINTM(ADVIHY+VIHRHO)+RHO110)*EXP(VARBIO)

      RHO11  = VINTP(ADVIHY+VIHRHO) + RHO110
      RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
C ======================================================================
 30   CONTINUE
C ======================================================================
C ======================================================================
      END
