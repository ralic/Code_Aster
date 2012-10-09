      SUBROUTINE TE0335(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 NOMTE,OPTION
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/10/2012   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE DELMAS J.DELMAS
C
C     BUT:
C       CALCULER LES GRANDEURS EQUIVALENTES SUIVANTES
C       . CONTRAINTES EQUIVALENTES               (= 17 VALEURS)
C          . VON MISES                               (= 1 VALEUR)
C          . TRESCA                                  (= 1 VALEUR)
C          . CONTRAINTES PRINCIPALES                 (= 3 VALEURS)
C          . VON-MISES * SIGNE (PRESSION)            (= 1 VALEUR)
C          . DIRECTIONS DES CONTRAINTES PRINCIPALES  (= 3*3 VALEURS)
C          . TRACE                                   (= 1 VALEUR)
C          . TAUX DE TRIAXIALITE                     (= 1 VALEUR)
C
C       . DEFORMATIONS EQUIVALENTES              (= 14 VALEURS)
C          . SECOND INVARIANT                        (= 1 VALEUR)
C          . DEFORMATIONS PRINCIPALES                (= 3 VALEURS)
C          . 2EME INV. * SIGNE (1ER.INV.)            (= 1 VALEUR)
C          . DIRECTIONS DES DEFORMATIONS PRINCIPALES (= 3*3 VALEURS)
C
C       AUX POINTS DE GAUSS ET AUX NOEUDS :
C       A PARTIR DE SIGM_ELGA ET SIGM_ELNO POUR LES CONTRAINTES
C       A PARTIR DE EPSI_ELGA ET EPSI_ELNO POUR LES DEFORMATIONS
C       A PARTIR DE EPME_ELGA ET EPME_ELNO POUR LES DEF. HORS THERMIQUE
C
C       OPTION : 'SIEQ_ELGA'
C                'SIEQ_ELNO'
C                'EPEQ_ELGA'
C                'EPEQ_ELNO'
C                'EPMQ_ELGA'
C                'EPMQ_ELNO'
C
C ----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'

      INTEGER NEQMAX,NNOMAX,NPGMAX,NEEQMX,NCEQMX
      PARAMETER (NPGMAX=27,NNOMAX=27,NEQMAX=17,NEEQMX=14,NCEQMX=17)

      INTEGER NDIM,NDIM1,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO
      INTEGER IDEFO,ICONT,IEQUIF
      INTEGER IRET,ITABIN(7),ITABOU(7),NBCMP,NCMPEQ
      INTEGER I,J,IDCP,KP,INO

      REAL*8 EQPG(NEQMAX*NPGMAX),EQNO(NEQMAX*NNOMAX)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

      CALL ELREF4(' ','RIGI',NDIM1,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      IF ((OPTION.EQ.'EPEQ_ELGA') .OR.
     &    (OPTION.EQ.'EPEQ_ELNO') .OR.
     &    (OPTION.EQ.'EPMQ_ELGA') .OR.
     &    (OPTION.EQ.'EPMQ_ELNO')) THEN
C
        CALL TECACH('OOO','PDEFORR',7,ITABIN,IRET)
        IDEFO=ITABIN(1)
        CALL TECACH('OOO','PDEFOEQ',7,ITABOU,IRET)
        CALL ASSERT(ITABOU(2)/ITABOU(3).EQ.NEEQMX)
C
      ELSEIF ((OPTION.EQ.'SIEQ_ELGA') .OR.
     &        (OPTION.EQ.'SIEQ_ELNO')) THEN
C
        CALL TECACH('OOO','PCONTRR',7,ITABIN,IRET)
        ICONT=ITABIN(1)
        CALL TECACH('OOO','PCONTEQ',7,ITABOU,IRET)
        CALL ASSERT(ITABOU(2)/ITABOU(3).EQ.NCEQMX)
C
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      CALL ASSERT(ITABIN(7).LE.1)
      CALL ASSERT(ITABOU(7).LE.1)
      NBCMP = ITABIN(2)/ITABIN(3)
      CALL ASSERT((NBCMP.EQ.1).OR.(NBCMP.EQ.4).OR.(NBCMP.EQ.6))
      IEQUIF = ITABOU(1)
      NCMPEQ = ITABOU(2)/ITABOU(3)

      IF (NBCMP.EQ.6) THEN
        NDIM = 3
      ELSEIF (NBCMP.EQ.4) THEN
        NDIM = 2
      ELSEIF (NBCMP.EQ.1) THEN
        NDIM = 1
      ENDIF
C
C ----------------------------------------------------------------
C --- DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX POINTS DE GAUSS
C ----------------------------------------------------------------
C
      IF (OPTION(6:9).EQ.'ELGA') THEN
C
        DO 10 I = 1,NCMPEQ*NPG
          EQPG(I) = 0.0D0
   10   CONTINUE
C
C ------ DEFORMATIONS :
C -------------------
        IF ((OPTION.EQ.'EPEQ_ELGA').OR.
     &      (OPTION.EQ.'EPMQ_ELGA')) THEN
          DO 20 KP = 1,NPG
            IDCP = (KP-1)*NCMPEQ
            CALL FGEQUI(ZR(IDEFO+(KP-1)*NBCMP),'EPSI_DIR',
     &                                                NDIM,EQPG(IDCP+1))
   20     CONTINUE
C
C ----- CONTRAINTES :
C -----------------
        ELSEIF (OPTION.EQ.'SIEQ_ELGA') THEN
          DO 30 KP = 1,NPG
            IDCP = (KP-1)*NCMPEQ
            CALL FGEQUI(ZR(ICONT+(KP-1)*NBCMP),'SIGM_DIR',
     &                                                NDIM,EQPG(IDCP+1))
   30     CONTINUE
        ENDIF
C
C ----- STOCKAGE :
C --------------
        DO 50 KP = 1,NPG
          DO 40 J = 1,NCMPEQ
            ZR(IEQUIF-1+(KP-1)*NCMPEQ+J) = EQPG((KP-1)*NCMPEQ+J)
   40     CONTINUE
   50   CONTINUE
C
C -------------------------------------------------------
C --- DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX NOEUDS
C -------------------------------------------------------
C
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
C
        DO 60 I = 1,NCMPEQ*NNO
          EQNO(I) = 0.0D0
   60   CONTINUE
C
C ------ DEFORMATIONS :
C -------------------
        IF ((OPTION.EQ.'EPEQ_ELNO').OR.
     &      (OPTION.EQ.'EPMQ_ELNO')) THEN
          DO 70 INO = 1,NNO
            IDCP = (INO-1)*NCMPEQ
            CALL FGEQUI(ZR(IDEFO+(INO-1)*NBCMP),'EPSI_DIR',
     &                                                NDIM,EQNO(IDCP+1))
   70     CONTINUE
C
C ----- CONTRAINTES :
C -----------------
        ELSEIF (OPTION.EQ.'SIEQ_ELNO') THEN
          DO 80 INO = 1,NNO
            IDCP = (INO-1)*NCMPEQ
            CALL FGEQUI(ZR(ICONT+(INO-1)*NBCMP),'SIGM_DIR',
     &                                                NDIM,EQNO(IDCP+1))
   80     CONTINUE
        ENDIF
C
C ----- STOCKAGE :
C --------------
        DO 110 INO = 1,NNO
          DO 90 J = 1,NCMPEQ
            ZR(IEQUIF+(INO-1)*NCMPEQ-1+J) = EQNO((INO-1)*NCMPEQ+J)
   90     CONTINUE
  110   CONTINUE
C
      ENDIF
C
      CALL JEDEMA()
C
      END
