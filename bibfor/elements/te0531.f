      SUBROUTINE TE0531(OPTION,NOMTE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
      IMPLICIT NONE

C      ROUTINE PROCHE DE TE0335 (3D)
C
C     BUT:       POUR LES ELEMENTS SHB8 , CALCUL DES
C                GRANDEURS EQUIVALENTES SUIVANTES
C                AUX POINTS DE GAUSS :
C                    POUR LES CONTRAINTES  A PARTIR DE SIGM_ELGA
C                DANS CET ORDRE :
C                . CONTRAINTES EQUIVALENTES  :
C                        . VON MISES                    (= 1 VALEUR)
C                        . TRESCA                       (= 1 VALEUR)
C                        . CONTRAINTES PRINCIPALES      (= 3 VALEURS)
C                        . VON-MISES * SIGNE (PRESSION) (= 1 VALEUR)
C                        . DIRECTION DES CONTRAINTES PRINCIPALES
C                                                      (=3*3 VALEURS)
C                        . TRACE                        (= 1 VALEUR)
C                        . TAUX DE TRIAXIALITE          (= 1 VALEUR)
C     OPTION :  'SIEQ_ELGA'
C     ENTREES :  OPTION : OPTION DE CALCUL
C                NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C     REMARQUE:  LA DERNIERE GRANDEUR EST UTILISE
C                PARTICULIEREMENT POUR DES CALCULS DE CUMUL DE
C                DOMMAGE EN FATIGUE
C ----------------------------------------------------------------------
      INCLUDE 'jeveux.h'
C-----------------------------------------------------------------------
      INTEGER IDFDE ,IPOIDS ,IVF ,JGANO ,NDIM ,NEQMAX ,NPGMAX 

C-----------------------------------------------------------------------
      PARAMETER (NPGMAX=27,NEQMAX=17)
C ----------------------------------------------------------------------
      INTEGER ICONT,IEQUIF
      INTEGER IDCP,KP,J,I
      INTEGER NNO,NCEQ,NPG,NNOS
      REAL*8 EQPG(NEQMAX*NPGMAX)
      CHARACTER*16 NOMTE,OPTION


      NCEQ = 17

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      CALL JEVECH('PCONTRR','L',ICONT)
      CALL JEVECH('PCONTEQ','E',IEQUIF)

      DO 10 I = 1,NCEQ*NPG
        EQPG(I) = 0.0D0
   10 CONTINUE

C -   CONTRAINTES EQUIVALENTES AUX POINTS DE GAUSS

      IF (OPTION(6:9).EQ.'ELGA') THEN

C -       DEFORMATIONS

        IF (OPTION(1:4).EQ.'SIEQ') THEN
          DO 50 KP = 1,NPG
            IDCP = (KP-1)*NCEQ
            CALL FGEQUI(ZR(ICONT+(KP-1)*6),'SIGM_DIR',3,EQPG(IDCP+1))
   50     CONTINUE
        END IF

C -       STOCKAGE

        DO 70 KP = 1,NPG
          DO 60 J = 1,NCEQ
            ZR(IEQUIF-1+ (KP-1)*NCEQ+J) = EQPG((KP-1)*NCEQ+J)
   60     CONTINUE
   70   CONTINUE

      END IF

      END
