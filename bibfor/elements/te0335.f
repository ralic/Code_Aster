      SUBROUTINE TE0335(OPTION,NOMTE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------
      IMPLICIT NONE

C     BUT:       POUR LES ELEMENTS ISOPARAMETRIQUES 3D , CALCUL DES
C                GRANDEURS EQUIVALENTES SUIVANTES
C                AUX POINTS DE GAUSS :
C                    POUR LES CONTRAINTES  A PARTIR DE SIEF_ELGA
C                                                   OU SIEF_ELGA
C                    POUR LES DEFORMATIONS A PARTIR DE EPSI_ELGA
C                                                   OU EPME_ELGA
C                    (POUR LES DEFORMATIONS HORS THERMIQUES)
C                AUX NOEUDS :
C                    POUR LES DEFORMATIONS A PARTIR DE EPSI_ELNO
C                                                   OU EPME_ELNO
C                    (POUR LES DEFORMATIONS HORS THERMIQUES)

C                DANS CET ORDRE :

C                . CONTRAINTES EQUIVALENTES  :
C                        . VON MISES                    (= 1 VALEUR)
C                        . TRESCA                       (= 1 VALEUR)
C                        . CONTRAINTES PRINCIPALES      (= 3 VALEURS)
C                        . VON-MISES * SIGNE (PRESSION) (= 1 VALEUR)
C                        . DIRECTION DES CONTRAINTES PRINCIPALES
C                                                       (=3*3 VALEURS)
C                        . TRACE                        (= 1 VALEUR)
C                        . TAUX DE TRIAXIALITE          (=1 VALEUR)
C               . DEFORMATIONS EQUIVALENTES  :
C                        . SECOND INVARIANT             (= 1 VALEUR)
C                        . DEFORMATIONS PRINCIPALES     (= 3 VALEURS)
C                        . 2EME INV. * SIGNE (1ER.INV.) (= 1 VALEUR)
C                        . DIRECTION DES DEFORMATIONS PRINCIPALES
C                                                       (=3*3 VALEURS)

C     OPTIONS :  'SIEQ_ELGA'
C                'EPEQ_ELNO'
C                'EPEQ_ELGA'
C                'EPMQ_ELNO'
C                'EPMQ_ELGA'

C     ENTREES :  OPTION : OPTION DE CALCUL
C                NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C     REMARQUE:  LA DERNIERE GRANDEUR EST UTILISE
C                PARTICULIEREMENT POUR DES CALCULS DE CUMUL DE
C                DOMMAGE EN FATIGUE
C                EQPG (CONT/DEF EQUIVALENT PG)
C                DIMENSIONNE  A  NEQMAX CMP MAX * 27 PG MAX
C                EQNO (CONT/DEF EQUIVALENT NOEUDS)
C                DIMENSIONNE  A  NEQMAX CMP MAX * 27 NO MAX
C ----------------------------------------------------------------------
      INCLUDE 'jeveux.h'
C-----------------------------------------------------------------------
      INTEGER IDFDE ,IPOIDS ,IVF ,JGANO ,NDIM ,NEQMAX ,NNOMAX 
      INTEGER NPGMAX 
C-----------------------------------------------------------------------
      PARAMETER (NPGMAX=27,NNOMAX=27,NEQMAX=17)
C ----------------------------------------------------------------------
      INTEGER IDEFO,ICONT,IEQUIF
      INTEGER IDCP,KP,J,I,INO
      INTEGER NNO,NCEQ,NPG,NNOS,NCMP
      INTEGER NDIM1,NBVA
      REAL*8 EQPG(NEQMAX*NPGMAX),EQNO(NEQMAX*NNOMAX)
      CHARACTER*6  TYPMOD
      CHARACTER*16 NOMTE,OPTION


      NBVA = 0

      CALL EQCARA(NOMTE,TYPMOD,NDIM1,NCEQ,NCMP,NBVA)

      IF (NBVA .EQ. 0) NBVA = 4

      IF (OPTION(1:4).EQ.'EPEQ') THEN
        NCEQ = 14
        NCMP = 5
      ELSE IF (OPTION(1:4).EQ.'EPMQ') THEN
        NCEQ = 5
        NCMP = 5
      ELSE IF (OPTION(1:4).EQ.'SIEQ') THEN
        NCEQ = 17
        NCMP = 7
      END IF

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)


      IF (OPTION(1:4).EQ.'EPEQ') THEN
        CALL JEVECH('PDEFORR','L',IDEFO)
        CALL JEVECH('PDEFOEQ','E',IEQUIF)
      ELSE IF (OPTION(1:4).EQ.'EPMQ') THEN
        CALL JEVECH('PDEFORR','L',IDEFO)
        CALL JEVECH('PDEFOEQ','E',IEQUIF)
      ELSE IF (OPTION(1:4).EQ.'SIEQ') THEN
        CALL JEVECH('PCONTRR','L',ICONT)
        CALL JEVECH('PCONTEQ','E',IEQUIF)
      END IF

      DO 10 I = 1,NCEQ*NPG
        EQPG(I) = 0.0D0
   10 CONTINUE
      DO 20 I = 1,NCMP*NNO
        EQNO(I) = 0.0D0
   20 CONTINUE

C -   DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX POINTS DE GAUSS

      IF (OPTION(6:9).EQ.'ELGA') THEN

C -       DEFORMATIONS

        IF (OPTION(1:4).EQ.'EPEQ') THEN
          DO 30 KP = 1,NPG
            IDCP = (KP-1)*NCEQ
            CALL FGEQUI(ZR(IDEFO+ (KP-1)*6),'EPSI_DIR',3,EQPG(IDCP+1))
   30     CONTINUE

C -       DEFORMATIONS HORS THERMIQUES

        ELSE IF (OPTION(1:4).EQ.'EPMQ') THEN
          DO 40 KP = 1,NPG
            IDCP = (KP-1)*NCEQ
            CALL FGEQUI(ZR(IDEFO+ (KP-1)*6),'EPSI',3,EQPG(IDCP+1))
   40     CONTINUE

C -       CONTRAINTES

        ELSE IF (OPTION(1:4).EQ.'SIEQ') THEN
          DO 50 KP = 1,NPG
            IDCP = (KP-1)*NCEQ
          CALL FGEQUI(ZR(ICONT+ (KP-1)*NBVA),'SIGM_DIR',3,EQPG(IDCP+1))
   50     CONTINUE
        END IF

C -       STOCKAGE

        DO 70 KP = 1,NPG
          DO 60 J = 1,NCEQ
            ZR(IEQUIF-1+ (KP-1)*NCEQ+J) = EQPG((KP-1)*NCEQ+J)
   60     CONTINUE
   70   CONTINUE

C -   DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX NOEUDS

      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN

C -       DEFORMATIONS

        IF (OPTION(1:4).EQ.'EPEQ') THEN
          DO 80 INO = 1,NNO
            IDCP = (INO-1)*NCMP
            CALL FGEQUI(ZR(IDEFO+ (INO-1)*6),'EPSI',3,EQNO(IDCP+1))
   80     CONTINUE

C -       DEFORMATIONS HORS THERMIQUES

        ELSE IF (OPTION(1:4).EQ.'EPMQ') THEN
          DO 90 INO = 1,NNO
            IDCP = (INO-1)*NCMP
            CALL FGEQUI(ZR(IDEFO+ (INO-1)*6),'EPSI',3,EQNO(IDCP+1))
   90     CONTINUE

        END IF

C -       STOCKAGE

          DO 120 INO = 1,NNO
            DO 110 J = 1,NCMP
              ZR(IEQUIF-1+ (INO-1)*NCMP+J) = EQNO((INO-1)*NCMP+J)
  110       CONTINUE
  120     CONTINUE
      END IF

      END
