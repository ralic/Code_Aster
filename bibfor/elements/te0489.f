      SUBROUTINE TE0489(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/09/2003   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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

C     BUT: CALCUL DES INDICATEURS LOCAUX DE DECHARGE
C                 ET DE PERTE DE RADIALITE POUR LES NORMES :
C              VMIS      : SECOND INVARIANT DU TENSEUR DES CONTRAINTES
C                          DEVIATORIQUES
C              TOTAL     : SECOND INVARIANT DU TENSEUR DES CONTRAINTES
C              VMIS_CINE : SECOND INVARIANT DU DEVIATEUR DU TENSEUR
C                          SIGMA - X
C                          OU SIGMA EST LE TENSEUR DES CONTRAINTES
C                          ET X     EST LE TENSEUR DE RAPPEL
C              TOTAL_CINE: SECOND INVARIANT DU TENSEUR SIGMA - X

C    ON NOTE SIGMA2 = SIGMA(M,T+DT)
C            SIGMA1 = SIGMA(M,T)
C            DSIGMA = SIGMA2 - SIGMA1

C    A)LES INDICATEURS LOCAUX DE DECHARGE :
C      I = (NORME(SIGMA2) - NORME(SIGMA1)/NORME(SIGMA2)
C               SONT CALCULES :
C     .AUX POINTS D'INTEGRATION POUR L'OPTION  'DCHA_ELGA_SIGM'
C     .AUX NOEUDS DES ELEMENTS  POUR L'OPTION  'DCHA_ELNO_SIGM'

C    B)LES INDICATEURS LOCAUX DE PERTE DE RADIALITE :
C      I = 1 - ABS(DSIGMA : SIGMA1)/(NORME(DSIGMA)*NORME(SIGMA1))
C               SONT CALCULES :
C     .AUX POINTS D'INTEGRATION POUR L'OPTION  'RADI_ELGA_SIGM'
C     .AUX NOEUDS DES ELEMENTS  POUR L'OPTION  'RADI_ELNO_SIGM'


C          ELEMENTS ISOPARAMETRIQUES 2D ET 3D

C          OPTIONS : 'DCHA_ELGA_SIGM'
C                    'DCHA_ELNO_SIGM'
C                    'RADI_ELGA_SIGM'
C                    'RADI_ELNO_SIGM'

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,ELREFA
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER NBSIGM,NBNOEU,NBNOSO,NBDIM,JTAB(3)
      INTEGER IRET,NPG1,IPOIDS,IVF,IDFDE,JGANO
      INTEGER MXCMEL,NBPGMX,NNO,NBSIG,IDNORM,NNOS,IDSIG1,IDSIG2,IDVAR1,
     &        IDVAR2,NBSIG2,NPG,I,K,NDIM,IGAU,INO,IDINLO
      PARAMETER (MXCMEL=162)
      PARAMETER (NBPGMX=27)
      REAL*8 SIGMA1(MXCMEL),SIGMA2(MXCMEL)
      REAL*8 INDLOG(NBPGMX),INDLON(NBPGMX)
      REAL*8 DSIGMA(MXCMEL),TRX1,TRX2,S1DSIG
      REAL*8 X1(MXCMEL),X2(MXCMEL)
      REAL*8 NORM1,NORM2,DNORM,R8PREM,TRSIG1,TRSIG2
      REAL*8 NORSIG,ZERO,UN,DEUX,UNTIER,ZERNOR
      CHARACTER*8 MODELI
      CHARACTER*24 NORME
C DEB ------------------------------------------------------------------

C ---- INITIALISATIONS :
C      ---------------
      ZERO = 0.0D0
      UN = 1.0D0
      DEUX = 2.0D0
      UNTIER = 1.0D0/3.0D0
      ZERNOR = 10.0D0*R8PREM()
      MODELI(1:2) = NOMTE(3:4)

      DO 10 I = 1,MXCMEL
        SIGMA1(I) = ZERO
        SIGMA2(I) = ZERO
        DSIGMA(I) = ZERO
        X1(I) = ZERO
        X2(I) = ZERO
   10 CONTINUE

      DO 20 I = 1,NBPGMX
        INDLOG(I) = ZERO
        INDLON(I) = ZERO
   20 CONTINUE


C ----     DIMENSION DE L'ELEMENT :
      NDIM = NBDIM(NOMTE)
      ELREFA = (NDIM.EQ.3) .AND. (NOMTE(3:4).NE.'FO')
C ----     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
      NBSIG = NBSIGM(MODELI)

C ---- RECUPERATION DE LA CARTE DEFINISSANT LA NORME ADOPTEE :
C      -----------------------------------------------------
      CALL JEVECH('PNEUK24','L',IDNORM)
      NORME = ZK24(IDNORM)

C ---- RECUPERATION DES COMPOSANTES DU TENSEUR DE CONTRAINTES
C ---- A L'INSTANT T :
C      -------------
      CALL TECACH('OOO','PCONTMR',3,JTAB,IRET)
      NPG = JTAB(3)
      IDSIG1 = JTAB(1)

C ---- RECUPERATION DES COMPOSANTES DU TENSEUR DE CONTRAINTES
C ---- A L'INSTANT T+DT :
C      ---------------
      CALL JEVECH('PCONTPR','L',IDSIG2)


      IF (ELREFA) THEN
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,
     &              JGANO)
        CALL ASSERT(NPG.EQ.NPG1)
      ELSE
        NNO = NBNOEU(' ')
        NNOS = NBNOSO(NOMTE)
      END IF



C ---- AFFECTATION DES VECTEURS DE TRAVAIL SIGMA1 ET SIGMA2
C ---- REPRESENTANT LES TENSEURS DE CONTRAINTES RESPECTIVEMENT
C ---- AUX INSTANTS T ET T+DT :
C      ----------------------
      K = 0

      DO 40 IGAU = 1,NPG
        DO 30 I = 1,NBSIG
          K = K + 1
          SIGMA1(I+ (IGAU-1)*NBSIG) = ZR(IDSIG1+K-1)
          SIGMA2(I+ (IGAU-1)*NBSIG) = ZR(IDSIG2+K-1)
   30   CONTINUE
   40 CONTINUE

C ---- DANS LE CAS D'UN ECROUISSAGE CINEMATIQUE
C ---- RECUPERATION DES COMPOSANTES DU TENSEUR DE RAPPEL
C ---- AUX INSTANTS T ET T+DT :
C      ----------------------
      IF (NORME(1:9).EQ.'VMIS_CINE' .OR.
     &    NORME(1:10).EQ.'TOTAL_CINE') THEN
        CALL JEVECH('PVARIMR','L',IDVAR1)
        CALL JEVECH('PVARIPR','L',IDVAR2)

C ----   AFFECTATION DES VECTEURS DE TRAVAIL X1 ET X2 REPRESENTANT
C ----   LES TENSEURS DE RAPPEL RESPECTIVEMENT AUX INSTANTS T
C ----   ET T+DT :
C        -------
        NBSIG2 = 7

        DO 60 IGAU = 1,NPG
          DO 50 I = 1,NBSIG
            X1(I+ (IGAU-1)*NBSIG) = ZR(IDVAR1+I+ (IGAU-1)*NBSIG2-1)
            X2(I+ (IGAU-1)*NBSIG) = ZR(IDVAR2+I+ (IGAU-1)*NBSIG2-1)
   50     CONTINUE
   60   CONTINUE
      END IF

C ---- CALCUL DU TENSEUR APPROPRIE DONT ON VA DETERMINER LA NORME
C ---- CAS DE LA NORME VMIS : CALCUL DES DEVIATEURS DES CONTRAINTES
C ---- SIGMA1 ET SIGMA2 :
C      ----------------
      IF (NORME.EQ.'VMIS') THEN

        DO 70 IGAU = 1,NPG

          TRSIG1 = SIGMA1(1+ (IGAU-1)*NBSIG) +
     &             SIGMA1(2+ (IGAU-1)*NBSIG) + SIGMA1(3+ (IGAU-1)*NBSIG)

          TRSIG2 = SIGMA2(1+ (IGAU-1)*NBSIG) +
     &             SIGMA2(2+ (IGAU-1)*NBSIG) + SIGMA2(3+ (IGAU-1)*NBSIG)

          SIGMA1(1+ (IGAU-1)*NBSIG) = SIGMA1(1+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG1
          SIGMA1(2+ (IGAU-1)*NBSIG) = SIGMA1(2+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG1
          SIGMA1(3+ (IGAU-1)*NBSIG) = SIGMA1(3+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG1

          SIGMA2(1+ (IGAU-1)*NBSIG) = SIGMA2(1+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG2
          SIGMA2(2+ (IGAU-1)*NBSIG) = SIGMA2(2+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG2
          SIGMA2(3+ (IGAU-1)*NBSIG) = SIGMA2(3+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG2
   70   CONTINUE

C ---- CAS DE LA NORME VMIS_CINE : CALCUL DES DEVIATEURS DES CONTRAINTES
C ---- (SIGMA1-X1) ET (SIGMA2-X2) :
C      -------------------------
      ELSE IF (NORME.EQ.'VMIS_CINE') THEN

        DO 80 IGAU = 1,NPG

          TRSIG1 = SIGMA1(1+ (IGAU-1)*NBSIG) +
     &             SIGMA1(2+ (IGAU-1)*NBSIG) + SIGMA1(3+ (IGAU-1)*NBSIG)

          TRSIG2 = SIGMA2(1+ (IGAU-1)*NBSIG) +
     &             SIGMA2(2+ (IGAU-1)*NBSIG) + SIGMA2(3+ (IGAU-1)*NBSIG)

          TRX1 = X1(1+ (IGAU-1)*NBSIG) + X1(2+ (IGAU-1)*NBSIG) +
     &           X1(3+ (IGAU-1)*NBSIG)

          TRX2 = X2(1+ (IGAU-1)*NBSIG) + X2(2+ (IGAU-1)*NBSIG) +
     &           X2(3+ (IGAU-1)*NBSIG)

          SIGMA1(1+ (IGAU-1)*NBSIG) = SIGMA1(1+ (IGAU-1)*NBSIG) -
     &                                X1(1+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG1-TRX1)
          SIGMA1(2+ (IGAU-1)*NBSIG) = SIGMA1(2+ (IGAU-1)*NBSIG) -
     &                                X1(2+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG1-TRX1)
          SIGMA1(3+ (IGAU-1)*NBSIG) = SIGMA1(3+ (IGAU-1)*NBSIG) -
     &                                X1(3+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG1-TRX1)
          SIGMA1(4+ (IGAU-1)*NBSIG) = SIGMA1(4+ (IGAU-1)*NBSIG) -
     &                                X1(4+ (IGAU-1)*NBSIG)

          SIGMA2(1+ (IGAU-1)*NBSIG) = SIGMA2(1+ (IGAU-1)*NBSIG) -
     &                                X2(1+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG2-TRX2)
          SIGMA2(2+ (IGAU-1)*NBSIG) = SIGMA2(2+ (IGAU-1)*NBSIG) -
     &                                X2(2+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG2-TRX2)
          SIGMA2(3+ (IGAU-1)*NBSIG) = SIGMA2(3+ (IGAU-1)*NBSIG) -
     &                                X2(3+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG2-TRX2)
          SIGMA2(4+ (IGAU-1)*NBSIG) = SIGMA2(4+ (IGAU-1)*NBSIG) -
     &                                X2(4+ (IGAU-1)*NBSIG)

          IF (NDIM.EQ.3) THEN
            SIGMA1(5+ (IGAU-1)*NBSIG) = SIGMA1(5+ (IGAU-1)*NBSIG) -
     &                                  X1(5+ (IGAU-1)*NBSIG)
            SIGMA1(6+ (IGAU-1)*NBSIG) = SIGMA1(6+ (IGAU-1)*NBSIG) -
     &                                  X1(6+ (IGAU-1)*NBSIG)

            SIGMA2(5+ (IGAU-1)*NBSIG) = SIGMA2(5+ (IGAU-1)*NBSIG) -
     &                                  X2(5+ (IGAU-1)*NBSIG)
            SIGMA2(6+ (IGAU-1)*NBSIG) = SIGMA2(6+ (IGAU-1)*NBSIG) -
     &                                  X2(6+ (IGAU-1)*NBSIG)
          END IF
   80   CONTINUE

C ---- CAS DE LA NORME TOTAL_CINE : CALCUL DES CONTRAINTES
C ---- (SIGMA1-X1) ET (SIGMA2-X2) :
C      -------------------------
      ELSE IF (NORME.EQ.'TOTAL_CINE') THEN

        DO 100 IGAU = 1,NPG
          DO 90 I = 1,NBSIG

            SIGMA1(I+ (IGAU-1)*NBSIG) = SIGMA1(I+ (IGAU-1)*NBSIG) -
     &                                  X1(I+ (IGAU-1)*NBSIG)
            SIGMA2(I+ (IGAU-1)*NBSIG) = SIGMA2(I+ (IGAU-1)*NBSIG) -
     &                                  X2(I+ (IGAU-1)*NBSIG)
   90     CONTINUE
  100   CONTINUE
      END IF

C ---- CALCUL DE L'INDICATEUR LOCAL DE DECHARGE :
C ----  I = (NORME(SIGMA2) - NORME(SIGMA1))/NORME(SIGMA2) :
C      --------------------------------------------------
      IF (OPTION.EQ.'DCHA_ELGA_SIGM' .OR.
     &    OPTION.EQ.'DCHA_ELNO_SIGM') THEN

C ----    CALCUL DE L'INDICATEUR LOCAL DE DECHARGE
C ----    AUX POINTS D'INTEGRATION :
C         ------------------------
        DO 110 IGAU = 1,NPG

C ----       CALCUL DU SECOND INVARIANT DU TENSEUR DES CONTRAINTES :
C            -----------------------------------------------------
          NORM1 = NORSIG(SIGMA1(1+ (IGAU-1)*NBSIG),NBSIG)
          NORM2 = NORSIG(SIGMA2(1+ (IGAU-1)*NBSIG),NBSIG)

C ----       DANS LE CAS OU NORME(SIGMA2) = 0 :
C ----       SI NORME(SIGMA1) = 0, ON MET L'INDICATEUR A 0
C ----       SINON IL Y A EU DECHARGE ET ON MET L'INDICATEUR A -1 :
C            ----------------------------------------------------
          IF (NORM2.LE.ZERNOR) THEN
            IF (NORM1.LE.ZERNOR) THEN
              INDLOG(IGAU) = ZERO
            ELSE
              INDLOG(IGAU) = -UN
            END IF
          ELSE
            INDLOG(IGAU) = (NORM2-NORM1)/NORM2
          END IF
  110   CONTINUE

C ----    CALCUL DE L'INDICATEUR LOCAL DE DECHARGE AUX NOEUDS :
C         ---------------------------------------------------
        IF (OPTION.EQ.'DCHA_ELNO_SIGM') THEN
          IF (ELREFA) THEN
            CALL PPGAN2(JGANO,1,INDLOG,INDLON)
          ELSE
            CALL PPGANO(NNOS,NPG,1,INDLOG,INDLON)
          END IF
        END IF

C ---- CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE :
C ----  I = 1- ABS(SIGMA1:DSIGMA)/(NORME(SIGMA1)*NORME(DSIGMA) :
C      -------------------------------------------------------
      ELSE IF (OPTION.EQ.'RADI_ELGA_SIGM' .OR.
     &         OPTION.EQ.'RADI_ELNO_SIGM') THEN

C ----    CALCUL DE DSIGMA = SIGMA2 - SIGMA1 :
C         ----------------------------------
        K = 0
        DO 130 IGAU = 1,NPG
          DO 120 I = 1,NBSIG
            K = K + 1
            DSIGMA(K) = SIGMA2(K) - SIGMA1(K)
  120     CONTINUE
  130   CONTINUE

C ----    CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE
C ----    AUX POINTS D'INTEGRATION :
C         ------------------------
        DO 160 IGAU = 1,NPG

C ----       CALCUL DU PRODUIT SIGMA1:(SIGMA2-SIGMA1) :
C            ----------------------------------------
          S1DSIG = ZERO
          DO 140 I = 1,3
            S1DSIG = S1DSIG + SIGMA1(I+ (IGAU-1)*NBSIG)*
     &               DSIGMA(I+ (IGAU-1)*NBSIG)
  140     CONTINUE

          DO 150 I = 4,NBSIG
            S1DSIG = S1DSIG + DEUX*SIGMA1(I+ (IGAU-1)*NBSIG)*
     &               DSIGMA(I+ (IGAU-1)*NBSIG)
  150     CONTINUE

C ----       CALCUL DU SECOND INVARIANT DES TENSEURS DES CONTRAINTES :
C            -------------------------------------------------------
          NORM1 = NORSIG(SIGMA1(1+ (IGAU-1)*NBSIG),NBSIG)
          DNORM = NORSIG(DSIGMA(1+ (IGAU-1)*NBSIG),NBSIG)

C ----       DANS LE CAS OU NORME(SIGMA1) = 0  OU NORME(DSIGMA) = 0 :
C ----       ON MET L'INDICATEUR A 0 :
C            -----------------------
          IF (NORM1.LE.ZERNOR .OR. DNORM.LE.ZERNOR) THEN
            INDLOG(IGAU) = ZERO
          ELSE IF (DNORM.LE.1.0D4*R8PREM()*NORM1) THEN
            INDLOG(IGAU) = ZERO
          ELSE
            INDLOG(IGAU) = UN - ABS(S1DSIG)/NORM1/DNORM
          END IF
  160   CONTINUE

C ---- CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE AUX NOEUDS :
C      -------------------------------------------------------------
        IF (OPTION.EQ.'RADI_ELNO_SIGM') THEN
          IF (ELREFA) THEN
            CALL PPGAN2(JGANO,1,INDLOG,INDLON)
          ELSE
            CALL PPGANO(NNOS,NPG,1,INDLOG,INDLON)
          END IF
        END IF

      END IF

C ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C ---- AVEC LE VECTEUR DES INDICATEURS LOCAUX :
C      --------------------------------------
      CALL JEVECH('PVARIMO','E',IDINLO)

      IF (OPTION(6:9).EQ.'ELGA') THEN
        DO 170 IGAU = 1,NPG
          ZR(IDINLO+IGAU-1) = INDLOG(IGAU)
  170   CONTINUE
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
        DO 180 INO = 1,NNO
          ZR(IDINLO+INO-1) = INDLON(INO)
  180   CONTINUE
      END IF

      END
