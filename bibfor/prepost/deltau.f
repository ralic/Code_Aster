      SUBROUTINE DELTAU(JRWORK, JNBPG, NBPGT, NBORDR, NMAINI, NBMAP,
     &                  NUMPAQ, TSPAQ, NOMMET, NOMCRI, CESR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE F1BHHAJ J.ANGLES
C TOLE  CRP_20
      IMPLICIT     NONE
      INTEGER      JRWORK, JNBPG, NBPGT, NBORDR, NMAINI, NUMPAQ, NBMAP
      INTEGER      TSPAQ
      CHARACTER*16 NOMCRI, NOMMET
      CHARACTER*19 CESR
C ---------------------------------------------------------------------
C BUT: DETERMINER LE PLAN INCLINE POUR LEQUEL DELTA_TAU EST MAXIMUM
C      POUR CHAQUE POINT DE GAUSS D'UN <<PAQUET>> DE MAILLES.
C ---------------------------------------------------------------------
C ARGUMENTS:
C JRWORK     IN    I  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                       L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                       ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                       DU <<PAQUET>> DE MAILLES.
C JNBPG      IN    I  : ADRESSE DU VECTEUR CONTENANT LE NOMBRE DE
C                       POINT DE GAUSS DE CHAQUE MAILLE DU MAILLAGE.
C NBPGT      IN    I  : NOMBRE TOTAL DE POINTS DE GAUSS A TRAITER.
C NBORDR     IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                       STRUCTURE DE DONNEES RESULTAT.
C NMAINI     IN    I  : NUMERO DE LA 1ERE MAILLE DU <<PAQUET>> DE
C                       MAILLES COURANT.
C NBMAP      IN    I  : NOMBRE DE MAILLES DANS LE <<PAQUET>> DE
C                       MAILLES COURANT.
C NUMPAQ     IN    I  : NUMERO DU PAQUET DE MAILLES COURANT.
C TSPAQ      IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                       COURANT.
C NOMMET     IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
C                       CIRCONSCRIT.
C NOMCRI     IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
C CESR       IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
C                       RESULTATS.
C
C REMARQUE :
C  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
C    MAILLES DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
C-----------------------------------------------------------------------

C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      I, J, K, KWORK, N, JCERD, JCERL, JCERV, JAD
      INTEGER      IRET, IMAP, ICESD, ICESL, ICESV, IBID
      INTEGER      IPG, TNECES, TDISP, JVECPG, JVECTN
      INTEGER      JVECTU, JVECTV, NBVEC, NGAM, DIM, TAB2(18)
      INTEGER      NBPG, SOMPGW, NBPGP, L, JDTAUM, JRESUN, MNMAX(2)
      INTEGER      JVPG1, JVPG2, LOR8EM, LOISEM
      INTEGER      JVECN2, JVECU2, JVECV2, JVECN1, JVECU1, JVECV1
      INTEGER       ICMP, ADRS
      INTEGER VALI(2)
C
      REAL*8       EPSILO, DGAM, GAMMA, PI, R8PI, DPHI, TAB1(18)
      REAL*8       COEPRE, GAMMAM, PHIM, DGAM2, DPHI2, PHI0, DTAUM(2)
      REAL*8       NXM(2), NYM(2), NZM(2)
      REAL*8       SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ, FXM(2), FYM(2)
      REAL*8       FZM(2), EPSXX, EPSYY, EPSZZ, EPSXY, EPSXZ, EPSYZ
      REAL*8       NORM(2), NORMAX(2), SNORM(2), EPSXM(2), EPSYM(2)
      REAL*8       EPSZM(2), EPNORM(2), EPNMAX(2), SEPNMX(2), NORMOY(2)
      REAL*8       EPNMOY(2), R8B, VALE, VALNU, C1, C2, VALA, VALB
      REAL*8       PHYDRO, PHYDRM
      REAL*8       COEFPA, SIGEQ(2), NRUPT(2), DOM(2), VRESU(24)
      REAL*8       R8MAEM
C
      INTEGER ICODRE, ICODWO
      CHARACTER*8    CHMAT1, NOMMAT
      CHARACTER*10 OPTIO
      CHARACTER*16 PHENOM
      CHARACTER*19 CHMAT, CESMAT
      LOGICAL      ENDUR
C
C-----------------------------------------------------------------------
C234567                                                              012
C-----------------------------------------------------------------------
      DATA  TAB1/ 180.0D0, 60.0D0, 30.0D0, 20.0D0, 15.0D0, 12.857D0,
     &             11.25D0, 10.588D0, 10.0D0, 10.0D0, 10.0D0, 10.588D0,
     &             11.25D0, 12.857D0, 15.0D0, 20.0D0, 30.0D0, 60.0D0 /
C
      DATA  TAB2/ 1, 3, 6, 9, 12, 14, 16, 17, 18, 18, 18, 17, 16, 14,
     &           12, 9, 6, 3 /
C
      EPSILO = 1.0D-7
      PI = R8PI()
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C CONSTRUCTION DU VECTEUR CONTENANT DELTA_TAU_MAX
C CONSTRUCTION DU VECTEUR CONTENANT LA VALEUR DU POINTEUR PERMETTANT
C              DE RETROUVER LE VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX

      CALL WKVECT('&&DELTAU.DTAU_MAX', 'V V R', 209, JDTAUM)
      CALL WKVECT('&&DELTAU.RESU_N', 'V V I', 209, JRESUN)

C CONSTRUCTION DU VECTEUR NORMAL SUR UNE DEMI SPHERE
C CONSTRUCTION DU VECTEUR U DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
C CONSTRUCTION DU VECTEUR V DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE

      CALL WKVECT( '&&DELTAU.VECT_NORMA', 'V V R', 630, JVECTN )
      CALL WKVECT( '&&DELTAU.VECT_TANGU', 'V V R', 630, JVECTU )
      CALL WKVECT( '&&DELTAU.VECT_TANGV', 'V V R', 630, JVECTV )

      CALL WKVECT( '&&DELTAU.VECT_NORMA1', 'V V R', 27, JVECN1 )
      CALL WKVECT( '&&DELTAU.VECT_TANGU1', 'V V R', 27, JVECU1 )
      CALL WKVECT( '&&DELTAU.VECT_TANGV1', 'V V R', 27, JVECV1 )
      CALL WKVECT( '&&DELTAU.VECT_NORMA2', 'V V R', 27, JVECN2 )
      CALL WKVECT( '&&DELTAU.VECT_TANGU2', 'V V R', 27, JVECU2 )
      CALL WKVECT( '&&DELTAU.VECT_TANGV2', 'V V R', 27, JVECV2 )

      CALL WKVECT( '&&DELTAU.VECTPG1', 'V V R', 18*NBORDR, JVPG1 )
      CALL WKVECT( '&&DELTAU.VECTPG2', 'V V R', 18*NBORDR, JVPG2 )

C OBTENTION DES ADRESSES '.CESD', '.CESL' ET '.CESV' DU CHAMP SIMPLE
C DESTINE A RECEVOIR LES RESULTATS : DTAUM, ....

      CALL JEVEUO(CESR//'.CESD','L',JCERD)
      CALL JEVEUO(CESR//'.CESL','E',JCERL)
      CALL JEVEUO(CESR//'.CESV','E',JCERV)

C RECUPERATION DU COEFFICIENT DE PRE-ECROUISSAGE DONNE PAR L'UTILISATEUR

      CALL GETVR8(' ','COEF_PREECROU',1,1,1,COEPRE,IRET)

C RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR

      CALL GETVID(' ','CHAM_MATER',1,1,1,CHMAT1,IRET)
      CHMAT = CHMAT1//'.CHAMP_MAT'
      CESMAT = '&&DELTAU.CESMAT'
      CALL CARCES(CHMAT,'ELEM',' ','V',CESMAT,IRET)
      CALL JEVEUO(CESMAT//'.CESD','L',ICESD)
      CALL JEVEUO(CESMAT//'.CESL','L',ICESL)
      CALL JEVEUO(CESMAT//'.CESV','L',ICESV)

      TNECES = 209*NBORDR*2
      CALL JEDISP(1, TDISP)
      TDISP =  (TDISP * LOISEM()) / LOR8EM()
      IF (TDISP .LT. TNECES ) THEN
         VALI (1) = TDISP
         VALI (2) = TNECES
         CALL U2MESG('F', 'PREPOST5_8',0,' ',2,VALI,0,0.D0)
      ELSE
         CALL WKVECT( '&&DELTAU.VECTPG', 'V V R', TNECES, JVECPG )
         CALL JERAZO( '&&DELTAU.VECTPG', TNECES, 1 )
      ENDIF

      DGAM = 10.0D0

      N = 0
      DIM = 627
      DO 300 J=1, 18
         GAMMA=(J-1)*DGAM*(PI/180.0D0)
         DPHI=TAB1(J)*(PI/180.0D0)
         NGAM=TAB2(J)
         PHI0=DPHI/2.0D0

         CALL VECNUV(1, NGAM, GAMMA, PHI0, DPHI, N, 1, DIM,
     &               ZR(JVECTN), ZR(JVECTU), ZR(JVECTV))

 300  CONTINUE

C CONSTRUCTION DU VECTEUR : CONTRAINTE = F(NUMERO D'ORDRE) EN CHAQUE
C POINT DE GAUSS DU PAQUET DE MAILLES.
      L = 1
      NBPG = 0
      NBPGP = 0
      KWORK = 0
      SOMPGW = 0

      DO 400 IMAP=NMAINI, NMAINI+(NBMAP-1)
         IF ( IMAP .GT. NMAINI ) THEN
           KWORK = 1
           SOMPGW = SOMPGW + ZI(JNBPG + IMAP-2)
         ENDIF
         NBPG = ZI(JNBPG + IMAP-1)
C SI LA MAILLE COURANTE N'A PAS DE POINTS DE GAUSS, LE PROGRAMME
C PASSE DIRECTEMENT A LA MAILLE SUIVANTE.
         IF (NBPG .EQ. 0) THEN
           GOTO 400
         ENDIF

         NBPGP = NBPGP + NBPG
         IF ( (L*INT(NBPGT/10.0D0)) .LT. NBPGP ) THEN
           WRITE(6,*)NUMPAQ,'   ',(NBPGP-NBPG)
           L = L + 1
         ENDIF

C RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE COURANTE
C ET DES PARAMETRES ASSOCIES AU CRITERE CHOISI POUR LA MAILLE COURANTE.

         OPTIO = 'DOMA_ELGA'
         CALL RNOMAT (ICESD, ICESL, ICESV, IMAP, NOMCRI, IBID, IBID,
     &                IBID, OPTIO, VALA, VALB, COEFPA, NOMMAT)

         CALL RCPARE( NOMMAT, 'FATIGUE', 'WOHLER', ICODWO )
         IF ( ICODWO .EQ. 1 ) THEN
            CALL U2MESK('F','FATIGUE1_90',1,NOMCRI(1:16))
         ENDIF

         DO 420 IPG=1, NBPG

C PROJECTION DE L'HISTORIQUE DU CISAILLEMENT DANS UN PLAN.

            CALL JERAZO('&&DELTAU.VECTPG', TNECES, 1)
            NBVEC = 209
            CALL TAURLO(NBVEC, JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &                  SOMPGW, JRWORK, TSPAQ, IPG, JVECPG)

C CALCUL DU MAX DES DELTA_TAU MAX ET DU VECTEUR NORMAL ASSOCIE POUR
C LE POINT DE GAUSS COURANT DE LA MAILLE COURANTE.

C 1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C    DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C    PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

            CALL JERAZO('&&DELTAU.DTAU_MAX', NBVEC, 1)
            CALL JERAZO('&&DELTAU.RESU_N', NBVEC, 1)

C 2/ CALCUL DU RAYON CIRCONSCRIT

            CALL RAYCIR(JVECPG, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)

C 3/ CALCUL DU 1ER MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

            DTAUM(1) = 0.0D0
            DTAUM(2) = 0.0D0
            MNMAX(1) = 1
            MNMAX(2) = 1

            DO 430 I=1, NBVEC
              IF ( ZR(JDTAUM + (I-1)) .GT. EPSILO ) THEN
                 IF ( (ZR(JDTAUM + (I-1))-DTAUM(1))/ZR(JDTAUM + (I-1))
     &                 .GT. EPSILO ) THEN
                    DTAUM(2) = DTAUM(1)
                    MNMAX(2) = MNMAX(1)
                    DTAUM(1) = ZR(JDTAUM + (I-1))
                    MNMAX(1) = I
                 ENDIF
                 IF ( ((ZR(JDTAUM + (I-1))-DTAUM(2))/ZR(JDTAUM + (I-1))
     &                 .GT. EPSILO)  .AND. (I .NE. MNMAX(1)) ) THEN
                    DTAUM(2) = ZR(JDTAUM + (I-1))
                    MNMAX(2) = I
                 ENDIF
              ENDIF
 430        CONTINUE

C 4/ PREMIER RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    ET DU MAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 2
C    DEGRES PRES).

            PHYDRO = 0.0D0
            PHYDRM = 0.0D0
            DIM = 27

            DO 440 K=1, 2
               NORM(K) = 0.0D0
               NORMAX(K) = 0.0D0
               SNORM(K) = 0.0D0
               EPNORM(K) = 0.0D0
               EPNMAX(K) = 0.0D0
               SEPNMX(K) = 0.0D0
               NXM(K) = ZR(JVECTN + (MNMAX(K)-1)*3)
               NYM(K) = ZR(JVECTN + (MNMAX(K)-1)*3 + 1)
               NZM(K) = ZR(JVECTN + (MNMAX(K)-1)*3 + 2)
               GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
               IF (GAMMAM .LT. 0.0D0) THEN
                  GAMMAM = GAMMAM + PI
               ENDIF

               IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &             (ABS(NXM(K)) .LT. EPSILO)) THEN
                 PHIM = 0.0D0
               ELSE
                 PHIM = ATAN2(ABS(NYM(K)),NXM(K))
               ENDIF
               IF (PHIM .LT. 0.0D0) THEN
                 PHIM = PHIM + PI
               ENDIF

               IF (ABS(GAMMAM) .LT. EPSILO) THEN
                  GAMMA = 5.0D0*(PI/180.0D0)
                  DPHI2 = 60.0D0*(PI/180.0D0)
                  PHI0 = 0.0D0
                  N = 0

                  CALL VECNUV(1, 6, GAMMA, PHI0, DPHI2, N, 1, DIM,
     &                        ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

                  GAMMA = 0.0D0
                  PHI0 = PI

                  CALL VECNUV(1, 1, GAMMA, PHI0, DPHI2, N, 1, DIM,
     &                        ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

                  NBVEC = 7
                  CALL TAURLO(NBVEC, JVECN2, JVECU2, JVECV2, NBORDR,
     &                        KWORK, SOMPGW, JRWORK, TSPAQ, IPG, JVPG2)
               ELSE
                  DGAM2 = 2.0D0*(PI/180.0D0)
                  DPHI2 = DGAM2/SIN(GAMMAM)
                  N = 0
                  DO 460 J=1, 3
                     GAMMA = GAMMAM + (J-2)*DGAM2

                     CALL VECNUV(1, 3, GAMMA, PHIM, DPHI2, N, 2, DIM,
     &                           ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

 460              CONTINUE

                  NBVEC = 9
                  CALL TAURLO(NBVEC, JVECN2, JVECU2, JVECV2, NBORDR,
     &                        KWORK, SOMPGW, JRWORK, TSPAQ, IPG, JVPG2)
               ENDIF

C 4-1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C     DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C     PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

               CALL JERAZO('&&DELTAU.DTAU_MAX', NBVEC, 1)
               CALL JERAZO('&&DELTAU.RESU_N', NBVEC, 1)

C 4-2/ CALCUL DU RAYON CIRCONSCRIT

               CALL RAYCIR(JVPG2, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)

C 4-3/ CALCUL DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

               DTAUM(K) = 0.0D0
               MNMAX(K) = 1

               DO 480 I=1, NBVEC
                  IF ( ZR(JDTAUM + (I-1)) .GT. DTAUM(K)) THEN
                     DTAUM(K) = ZR(JDTAUM + (I-1))
                     MNMAX(K) = I
                  ENDIF
 480           CONTINUE

C 5/ DEUXIEME RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    ET DU MAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 1
C    DEGRE PRES).

               NXM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3)
               NYM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3 + 1)
               NZM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3 + 2)
               GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
               IF (GAMMAM .LT. 0.0D0) THEN
                  GAMMAM = GAMMAM + PI
               ENDIF

               IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &             (ABS(NXM(K)) .LT. EPSILO)) THEN
                 PHIM = 0.0D0
               ELSE
                 PHIM = ATAN2(ABS(NYM(K)),NXM(K))
               ENDIF
               IF (PHIM .LT. 0.0D0) THEN
                 PHIM = PHIM + PI
               ENDIF

               IF (ABS(GAMMAM) .LT. EPSILO) THEN
                  GAMMA = 1.0D0*(PI/180.0D0)
                  DPHI2 = 60.0D0*(PI/180.0D0)
                  PHI0 = 0.0D0
                  N = 0

                  CALL VECNUV(1, 6, GAMMA, PHI0, DPHI2, N, 1, DIM,
     &                        ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

                  GAMMA = 0.0D0
                  PHI0 = PI

                  CALL VECNUV(1, 1, GAMMA, PHI0, DPHI2, N, 1, DIM,
     &                        ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

                  NBVEC = 7
                  CALL TAURLO(NBVEC, JVECN1, JVECU1, JVECV1, NBORDR,
     &                        KWORK, SOMPGW, JRWORK, TSPAQ, IPG, JVPG1)
               ELSE
                  DGAM2 = 1.0D0*(PI/180.0D0)
                  DPHI2 = DGAM2/SIN(GAMMAM)
                  N = 0
                  DO 510 J=1, 3
                     GAMMA = GAMMAM + (J-2)*DGAM2

                     CALL VECNUV(1, 3, GAMMA, PHIM, DPHI2, N, 2, DIM,
     &                        ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

 510              CONTINUE

                  NBVEC = 9
                  CALL TAURLO(NBVEC, JVECN1, JVECU1, JVECV1, NBORDR,
     &                        KWORK, SOMPGW, JRWORK, TSPAQ, IPG, JVPG1)
               ENDIF

C 5-1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C     DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C     PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

               CALL JERAZO('&&DELTAU.DTAU_MAX', NBVEC, 1)
               CALL JERAZO('&&DELTAU.RESU_N', NBVEC, 1)

C 5-2/ CALCUL DU RAYON CIRCONSCRIT

               CALL RAYCIR(JVPG1, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)

C 5-3/ CALCUL DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

               DTAUM(K) = 0.0D0
               MNMAX(K) = 1

               DO 530 I=1, NBVEC
                  IF ( ZR(JDTAUM + (I-1)) .GT. DTAUM(K)) THEN
                     DTAUM(K) = ZR(JDTAUM + (I-1))
                     MNMAX(K) = I
                  ENDIF
 530           CONTINUE

               NXM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3)
               NYM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3 + 1)
               NZM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3 + 2)
               GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
               IF (GAMMAM .LT. 0.0D0) THEN
                  GAMMAM = GAMMAM + PI
               ENDIF

               IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &             (ABS(NXM(K)) .LT. EPSILO)) THEN
                 PHIM = 0.0D0
               ELSE
                 PHIM = ATAN2(ABS(NYM(K)),NXM(K))
               ENDIF
               IF (PHIM .LT. 0.0D0) THEN
                 PHIM = PHIM + PI
               ENDIF

C CALCUL DE LA CONTRAINTE NORMALE MAXIMALE SUR LE PLAN CRITIQUE,
C DE LA CONTRAINTE NORMALE MOYENNE SUR LE PLAN CRITIQUE,
C DE LA DEFORMATION NORMALE MAXIMALE SUR LE PLAN CRITIQUE,
C DE LA DEFORMATION NORMALE MOYENNE SUR LE PLAN CRITIQUE.

               CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'E       ',
     &                     VALE,ICODRE,0)
               IF (ICODRE .EQ. 1) THEN
                  CALL U2MESS('F','PREPOST_11')
               ENDIF
               CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'NU      ',
     &                     VALNU,ICODRE,0)
               IF (ICODRE .EQ. 1) THEN
                  CALL U2MESS('F','PREPOST_12')
               ENDIF
               C1 = (1+VALNU)/VALE
               C2 = VALNU/VALE

               DO 540 I=1, NBORDR
                  ADRS = (I-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6
                  SIXX = ZR(JRWORK + ADRS + 0)
                  SIYY = ZR(JRWORK + ADRS + 1)
                  SIZZ = ZR(JRWORK + ADRS + 2)
                  SIXY = ZR(JRWORK + ADRS + 3)
                  SIXZ = ZR(JRWORK + ADRS + 4)
                  SIYZ = ZR(JRWORK + ADRS + 5)

C CALCUL DE LA PRESSION HYDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])

                  IF ( K .LT. 2 ) THEN

C ON CALCULE PHYDRM QU'UNE FOIS, PARCE QUE LA PRESSION HYDROSTATIQUE
C EST INVARIANTE PAR RAPPORT AU vect_n.

                     PHYDRO = (SIXX + SIYY + SIZZ)/3.0D0

                     IF (PHYDRO .GT. PHYDRM) THEN
                        PHYDRM = PHYDRO
                     ENDIF
                  ENDIF

                  EPSXX = C1*SIXX - C2*(SIXX + SIYY + SIZZ)
                  EPSYY = C1*SIYY - C2*(SIXX + SIYY + SIZZ)
                  EPSZZ = C1*SIZZ - C2*(SIXX + SIYY + SIZZ)
                  EPSXY = C1*SIXY
                  EPSXZ = C1*SIXZ
                  EPSYZ = C1*SIYZ

C CALCUL DE vect_F = [SIG].vect_n

                  FXM(K) = SIXX*NXM(K) + SIXY*NYM(K) + SIXZ*NZM(K)
                  FYM(K) = SIXY*NXM(K) + SIYY*NYM(K) + SIYZ*NZM(K)
                  FZM(K) = SIXZ*NXM(K) + SIYZ*NYM(K) + SIZZ*NZM(K)

C CALCUL DE NORM = vect_F.vect_n

                  NORM(K) = FXM(K)*NXM(K) + FYM(K)*NYM(K) +
     &                      FZM(K)*NZM(K)

                  IF (ABS(NORM(K)) .GT. NORMAX(K)) THEN
                     NORMAX(K) = NORM(K)
                  ENDIF

                  SNORM(K) = SNORM(K) + NORM(K)

C CALCUL DE vect_EPS = [EPS].vect_n

                  EPSXM(K) = EPSXX*NXM(K) + EPSXY*NYM(K) + EPSXZ*NZM(K)
                  EPSYM(K) = EPSXY*NXM(K) + EPSYY*NYM(K) + EPSYZ*NZM(K)
                  EPSZM(K) = EPSXZ*NXM(K) + EPSYZ*NYM(K) + EPSZZ*NZM(K)

C CALCUL DE EPSILON NORMALE = vect_EPS.vect_n

                  EPNORM(K) = EPSXM(K)*NXM(K) + EPSYM(K)*NYM(K) +
     &                        EPSZM(K)*NZM(K)

                  IF (ABS(EPNORM(K)) .GT. EPNMAX(K)) THEN
                     EPNMAX(K) = EPNORM(K)
                  ENDIF

                  SEPNMX(K) = SEPNMX(K) + EPNORM(K)
 540           CONTINUE

               NORMOY(K) = SNORM(K)/NBORDR
               EPNMOY(K) = SEPNMX(K)/NBORDR

C ---------------------------------------------------------------------
C       =============================================
C       /      CRITERES AVEC PLANS CRITIQUES        /
C       =============================================
C ---------------------------------------------------------------------


C 1/ CRITERE DE MATAKE
               IF (NOMCRI(1:14) .EQ. 'MATAKE_MODI_AC') THEN
                  IF ( NORMAX(K) .GT. 0.0D0 ) THEN
                     SIGEQ(K) = COEPRE*DTAUM(K) + (VALA*NORMAX(K))
                     SIGEQ(K) = SIGEQ(K)*COEFPA
                  ELSE
                     SIGEQ(K) = COEPRE*DTAUM(K)
                     SIGEQ(K) = SIGEQ(K)*COEFPA
                  ENDIF
               ENDIF

C 2/ CRITERE DE DANG VAN
               IF (NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AC') THEN
                  IF ( PHYDRM .GT. 0.0D0 ) THEN
                     SIGEQ(K) = COEPRE*DTAUM(K) + (VALA*PHYDRM)
                     SIGEQ(K) = SIGEQ(K)*COEFPA
                  ELSE
                     SIGEQ(K) = COEPRE*DTAUM(K)
                     SIGEQ(K) = SIGEQ(K)*COEFPA
                  ENDIF
               ENDIF

C PAS DE CRITERE DE FATEMI ET SOCIE EN ELASTIQUE ET AMPLITUDE CONSTANTE,
C CELA N'A PAS DE SENS.

C CALCUL DU NOMBRE DE CYCLES A LA RUPTURE ET DU DOMMAGE

               CALL RCCOME ( NOMMAT, 'FATIGUE', PHENOM, ICODRE )
               IF ( ICODRE .EQ. 1 ) CALL U2MESS('F','FATIGUE1_24')

               CALL LIMEND( NOMMAT,SIGEQ(K),'WOHLER',ENDUR)
               IF (ENDUR) THEN
                  NRUPT(K)=R8MAEM()
               ELSE
                  CALL RCVALE(NOMMAT,'FATIGUE',1,'SIGM    ',SIGEQ(K),
     &                        1,'WOHLER  ',NRUPT(K),ICODRE,1)
               ENDIF

               DOM(K) = 1.D0/NRUPT(K)
               NRUPT(K) = NINT(NRUPT(K))

 440        CONTINUE

C CONSTRUCTION D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
C POUR CHAQUE POINT DE GAUSS DE CHAQUE MAILLE MAX DE DTAU_MAX ET LE
C VECTEUR NORMAL ASSOCIE.

            VRESU(1) = DTAUM(1)
            VRESU(2) = NXM(1)
            VRESU(3) = NYM(1)
            VRESU(4) = NZM(1)
            VRESU(5) = NORMAX(1)
            VRESU(6) = NORMOY(1)
            VRESU(7) = EPNMAX(1)
            VRESU(8) = EPNMOY(1)
            VRESU(9) = SIGEQ(1)
            VRESU(10) = NRUPT(1)
            VRESU(11) = DOM(1)
            VRESU(12) = DTAUM(2)
            VRESU(13) = NXM(2)
            VRESU(14) = NYM(2)
            VRESU(15) = NZM(2)
            VRESU(16) = NORMAX(2)
            VRESU(17) = NORMOY(2)
            VRESU(18) = EPNMAX(2)
            VRESU(19) = EPNMOY(2)
            VRESU(20) = SIGEQ(2)
            VRESU(21) = NRUPT(2)
            VRESU(22) = DOM(2)
            VRESU(23) = 0.0D0
            VRESU(24) = 0.0D0

C AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE

            DO 550 ICMP=1, 24
               CALL CESEXI('C',JCERD,JCERL,IMAP,IPG,1,ICMP,JAD)

C              -- TOUTES LES MAILLES NE SAVENT PAS CALCULER LA FATIGUE :
               IF (JAD.EQ.0) THEN
                 CALL ASSERT (ICMP.EQ.1)
                 CALL ASSERT (IPG.EQ.1)
                 GOTO 400
               ENDIF
               JAD = ABS(JAD)
               ZL(JCERL - 1 + JAD) = .TRUE.
               ZR(JCERV - 1 + JAD) = VRESU(ICMP)

 550        CONTINUE

 420     CONTINUE
 400  CONTINUE

C MENAGE

      CALL DETRSD('CHAM_ELEM_S',CESMAT)

      CALL JEDETR('&&DELTAU.DTAU_MAX')
      CALL JEDETR('&&DELTAU.RESU_N')
      CALL JEDETR('&&DELTAU.VECT_NORMA')
      CALL JEDETR('&&DELTAU.VECT_TANGU')
      CALL JEDETR('&&DELTAU.VECT_TANGV')
      CALL JEDETR('&&DELTAU.VECT_NORMA1')
      CALL JEDETR('&&DELTAU.VECT_TANGU1')
      CALL JEDETR('&&DELTAU.VECT_TANGV1')
      CALL JEDETR('&&DELTAU.VECT_NORMA2')
      CALL JEDETR('&&DELTAU.VECT_TANGU2')
      CALL JEDETR('&&DELTAU.VECT_TANGV2')
      CALL JEDETR('&&DELTAU.VECTPG1')
      CALL JEDETR('&&DELTAU.VECTPG2')
      CALL JEDETR('&&DELTAU.VECTPG')
C
      CALL JEDEMA()
      END
