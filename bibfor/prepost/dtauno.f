      SUBROUTINE DTAUNO(JRWORK, LISNOE, NBNOT, NBORDR, NNOINI, NBNOP,
     &                  NUMPAQ, TSPAQ, NOMMET, NOMCRI, NOMMAI, CNSR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 16/10/2006   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER      JRWORK, NBNOT, LISNOE(NBNOT), NBORDR, NNOINI, NBNOP
      INTEGER      NUMPAQ, TSPAQ
      CHARACTER*8  NOMMAI
      CHARACTER*16 NOMCRI, NOMMET
      CHARACTER*19 CNSR
C ---------------------------------------------------------------------
C BUT: DETERMINER LE PLAN INCLINE POUR LEQUEL DELTA_TAU EST MAXIMUM
C      POUR CHAQUE NOEUD D'UN <<PAQUET>> DE NOEUDS.
C ---------------------------------------------------------------------
C ARGUMENTS:
C JRWORK     IN    I  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                       L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                       ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                       DU <<PAQUET>> DE MAILLES.
C LISNOE     IN    I  : LISTE COMPLETE DES NOEUDS A TRAITER.
C NBNOT      IN    I  : NOMBRE TOTAL DE NOEUDS A TRAITER.
C NBORDR     IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                       STRUCTURE DE DONNEES RESULTAT.
C NNOINI     IN    I  : NUMERO DU 1ER NOEUD DU <<PAQUET>> DE
C                       NOEUDS COURANT.
C NBNOP      IN    I  : NOMBRE DE NOEUDS DANS LE <<PAQUET>> DE
C                       NOEUDS COURANT.
C NUMPAQ     IN    I  : NUMERO DU PAQUET DE NOEUDS COURANT.
C TSPAQ      IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE NOEUDS
C                       COURANT.
C NOMMET     IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
C                       CIRCONSCRIT.
C NOMCRI     IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
C NOMMAI     IN    K8 : NOM UTILISATEUR DU MAILLAGE.
C CNSR       IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
C                       RESULTATS.
C
C REMARQUE :
C  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
C    NOEUDS DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
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
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      I, J, K, L, N, JCNRD, JCNRL, JCNRV, JAD
      INTEGER      IRET, NBMA, ADRMA, NUMA, IMAP, ICESD, ICESL, ICESV
      INTEGER      IAD, IBID, IVECT, IORDR, TNECES, TDISP, JVECNO
      INTEGER      JVECTN, JVECTU, JVECTV, NBVEC, NGAM, IDEB, DIM
      INTEGER      TAB2(18), INOP, NUNOE, IBIBD, JDTAUM, JRESUN
      INTEGER      MNMAX(2), JVNO1, JVNO2, LOR8EM, LOISEM, JTYPMA, JTYP
      INTEGER      JVECN2, JVECU2, JVECV2, JVECN1, JVECU1, JVECV1
      INTEGER      NBPAR, ICMP, ADRS, KWORK, SOMNOW, CNBNO
C
      REAL*8       EPSILO, DGAM, GAMMA, PI, R8PI, DPHI, TAB1(18)
      REAL*8       PHI0, COEPRE, GAMMAM, PHIM, DGAM2, DPHI2, DTAUM(2)
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
      CHARACTER*2  CODRET, CODWO
      CHARACTER*8  NOMPAR, NOMRES, CHMAT1, NOMMAT, KTYP, DIMK, K8B
      CHARACTER*10 OPTIO
      CHARACTER*16 PHENOM
      CHARACTER*19 CHMAT, CESMAT, NCNCIN
      CHARACTER*24 TYPMA
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

      CALL JEMARQ()

C CONSTRUCTION DU VECTEUR CONTENANT DELTA_TAU_MAX
C CONSTRUCTION DU VECTEUR CONTENANT LA VALEUR DU POINTEUR PERMETTANT
C              DE RETROUVER LE VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX

      CALL WKVECT('&&DTAUNO.DTAU_MAX', 'V V R', 209, JDTAUM)
      CALL WKVECT('&&DTAUNO.RESU_N', 'V V I', 209, JRESUN)

C CONSTRUCTION DU VECTEUR NORMAL SUR UNE DEMI SPHERE
C CONSTRUCTION DU VECTEUR U DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
C CONSTRUCTION DU VECTEUR V DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE

      CALL WKVECT( '&&DTAUNO.VECT_NORMA', 'V V R', 630, JVECTN )
      CALL WKVECT( '&&DTAUNO.VECT_TANGU', 'V V R', 630, JVECTU )
      CALL WKVECT( '&&DTAUNO.VECT_TANGV', 'V V R', 630, JVECTV )

      CALL WKVECT( '&&DTAUNO.VECT_NORMA1', 'V V R', 27, JVECN1 )
      CALL WKVECT( '&&DTAUNO.VECT_TANGU1', 'V V R', 27, JVECU1 )
      CALL WKVECT( '&&DTAUNO.VECT_TANGV1', 'V V R', 27, JVECV1 )
      CALL WKVECT( '&&DTAUNO.VECT_NORMA2', 'V V R', 27, JVECN2 )
      CALL WKVECT( '&&DTAUNO.VECT_TANGU2', 'V V R', 27, JVECU2 )
      CALL WKVECT( '&&DTAUNO.VECT_TANGV2', 'V V R', 27, JVECV2 )

      CALL WKVECT( '&&DTAUNO.VECTNO1', 'V V R', 18*NBORDR, JVNO1 )
      CALL WKVECT( '&&DTAUNO.VECTNO2', 'V V R', 18*NBORDR, JVNO2 )

C OBTENTION DES ADRESSES '.CESD', '.CESL' ET '.CESV' DU CHAMP SIMPLE
C DESTINE A RECEVOIR LES RESULTATS : DTAUM, ....

      CALL JEVEUO(CNSR//'.CNSD','L',JCNRD)
      CALL JEVEUO(CNSR//'.CNSL','E',JCNRL)
      CALL JEVEUO(CNSR//'.CNSV','E',JCNRV)

C RECUPERATION DU COEFFICIENT DE PRE-ECROUISSAGE DONNE PAR L'UTILISATEUR

      CALL GETVR8(' ','COEF_PREECROU',1,1,1,COEPRE,IRET)

C RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR

      CALL GETVID(' ','CHAM_MATER',1,1,1,CHMAT1,IRET)
      CHMAT = CHMAT1//'.CHAMP_MAT'
      CESMAT = '&&DTAUNO.CESMAT'
      CALL CARCES(CHMAT,'ELEM',' ','V',CESMAT,IRET)
      CALL JEVEUO(CESMAT//'.CESD','L',ICESD)
      CALL JEVEUO(CESMAT//'.CESL','L',ICESL)
      CALL JEVEUO(CESMAT//'.CESV','L',ICESV)

      TNECES = 209*NBORDR*2
      CALL JEDISP(1, TDISP)
      TDISP =  (TDISP * LOISEM()) / LOR8EM()
      IF (TDISP .LT. TNECES ) THEN
         CALL UTDEBM('F', 'DTAUNO.1', 'LA TAILLE MEMOIRE '//
     &       ' NECESSAIRE AU VECTEUR DE TRAVAIL DANS '//
     &       ' LEQUEL NOUS STOCKONS LES COMPOSANTES '//
     &       ' u ET v DU VECTEUR TAU EST TROP IMPORTANTE '//
     &       ' PAR RAPPORT A LA PLACE DISPONIBLE.')
         CALL UTIMPI('L', 'TAILLE DISPONIBLE : ', 1, TDISP)
         CALL UTIMPI('L', 'TAILLE NECESSAIRE : ', 1, TNECES)
         CALL UTFINM( )
      ELSE
         CALL WKVECT( '&&DTAUNO.VECTNO', 'V V R', TNECES, JVECNO )
         CALL JERAZO( '&&DTAUNO.VECTNO', TNECES, 1 )
      ENDIF

      DGAM = 10.0D0

      N = 0
      K = 1
      IDEB = 1
      DIM = 627
      DO 300 J=1, 18
         GAMMA=(J-1)*DGAM*(PI/180.0D0)
         DPHI=TAB1(J)*(PI/180.0D0)
         PHI0=DPHI/2.0D0
         NGAM=TAB2(J)

         CALL VECNUV(IDEB, NGAM, GAMMA, PHI0, DPHI, N, K, DIM,
     &               ZR(JVECTN), ZR(JVECTU), ZR(JVECTV))

 300  CONTINUE

C CONSTRUCTION DU VECTEUR : CONTRAINTE = F(NUMERO D'ORDRE) EN CHAQUE
C NOEUDS DU PAQUET DE MAILLES.
      L = 1
      CNBNO = 0
      KWORK = 0
      SOMNOW = 0

      NCNCIN = '&&DTAUNO.CNCINV'
      CALL CNCINV ( NOMMAI, IBID, 0, 'V', NCNCIN )

      TYPMA = NOMMAI//'.TYPMAIL'
      CALL JEVEUO( TYPMA, 'L', JTYPMA )

      DO 400 INOP=NNOINI, NNOINI+(NBNOP-1)

         IF ( INOP .GT. NNOINI ) THEN
            KWORK = 1
            SOMNOW = SOMNOW + 1
         ENDIF

         CNBNO = CNBNO + 1
         IF ( (L*INT(NBNOT/10.0D0)) .LT. CNBNO ) THEN
           WRITE(6,*)NUMPAQ,'   ',(CNBNO-1)
           L = L + 1
         ENDIF

C RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE OU AUX MAILLES
C QUI PORTENT LE NOEUD COURANT.

         NUNOE = LISNOE(INOP)
         CALL JELIRA( JEXNUM(NCNCIN,NUNOE), 'LONMAX', NBMA, K8B )
         CALL JEVEUO( JEXNUM(NCNCIN,NUNOE), 'L', ADRMA )

         K = 0
         OPTIO = 'DOMA_NOEUD'
         DO 410, I=1, NBMA
            CALL RNOMAT (ICESD, ICESL, ICESV, I, NOMCRI, ADRMA, JTYPMA,
     &                   K, OPTIO, VALA, VALB, COEFPA, NOMMAT)
 410     CONTINUE

         IF (K .EQ. 0) THEN
            CALL UTDEBM('A', 'DTAUNO.2', 'LE NOEUD TRAITE '//
     &                  'N''EST ASSOCIE A AUCUNE MAILLE VOLUMIQUE.')
            CALL UTIMPI('L','NUMERO DU NOEUD = ',1,NUNOE)
            CALL UTIMPI('L','NOMBRE DE MAILLES ATTACHEES AU NOEUD = ',
     &                   1,NBMA)
            CALL UTFINM( )
         ENDIF

         CALL JERAZO('&&DTAUNO.VECTNO', TNECES, 1)

         NBVEC = 209
         CALL TRLONO(NBVEC, JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &               SOMNOW, JRWORK, TSPAQ, JVECNO)

C CALCUL DU MAX DES DELTA_TAU MAX ET DU VECTEUR NORMAL ASSOCIE POUR
C LE NOEUD COURANT.

C 1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C    DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C    PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

         CALL JERAZO('&&DTAUNO.DTAU_MAX', NBVEC, 1)
         CALL JERAZO('&&DTAUNO.RESU_N', NBVEC, 1)

C 2/ CALCUL DU RAYON CIRCONSCRIT

         CALL RAYCIR(JVECNO, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)

C 3/ CALCUL DU 1ER MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

         DTAUM(1) = 0.0D0
         DTAUM(2) = 0.0D0
         MNMAX(1) = 1
         MNMAX(2) = 1

         DO 430 I=1, NBVEC
           IF ( ZR(JDTAUM + (I-1)) .GT. EPSILO ) THEN
              IF ( (ZR(JDTAUM + (I-1))-DTAUM(1))/ZR(JDTAUM + (I-1))
     &              .GT. EPSILO ) THEN
                 DTAUM(2) = DTAUM(1)
                 MNMAX(2) = MNMAX(1)
                 DTAUM(1) = ZR(JDTAUM + (I-1))
                 MNMAX(1) = I
              ENDIF
              IF ( ((ZR(JDTAUM + (I-1))-DTAUM(2))/ZR(JDTAUM + (I-1))
     &               .GT. EPSILO)  .AND. (I .NE. MNMAX(1)) ) THEN
                 DTAUM(2) = ZR(JDTAUM + (I-1))
                 MNMAX(2) = I
              ENDIF
           ENDIF
 430     CONTINUE

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
     &          (ABS(NXM(K)) .LT. EPSILO)) THEN
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
     &                     ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

               GAMMA = 0.0D0

               CALL VECNUV(1, 1, GAMMA, PI, DPHI2, N, 1, DIM,
     &                     ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

               NBVEC = 7
               CALL TRLONO(NBVEC, JVECN2, JVECU2, JVECV2, NBORDR,
     &                     KWORK, SOMNOW, JRWORK, TSPAQ, JVNO2)
            ELSE
               DGAM2 = 2.0D0*(PI/180.0D0)
               DPHI2 = DGAM2/SIN(GAMMAM)
               N = 0
               DO 460 J=1, 3
                  GAMMA = GAMMAM + (J-2)*DGAM2

                  CALL VECNUV(1, 3, GAMMA, PHIM, DPHI2, N, 2, DIM,
     &                        ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

 460           CONTINUE

               NBVEC = 9
               CALL TRLONO(NBVEC, JVECN2, JVECU2, JVECV2, NBORDR,
     &                     KWORK, SOMNOW, JRWORK, TSPAQ, JVNO2)
            ENDIF

C 4-1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C     DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C     PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

            CALL JERAZO('&&DTAUNO.DTAU_MAX', NBVEC, 1)
            CALL JERAZO('&&DTAUNO.RESU_N', NBVEC, 1)

C 4-2/ CALCUL DU RAYON CIRCONSCRIT

            CALL RAYCIR(JVNO2, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)

C 4-3/ CALCUL DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

            DTAUM(K) = 0.0D0
            MNMAX(K) = 1

            DO 480 I=1, NBVEC
               IF ( ZR(JDTAUM + (I-1)) .GT. DTAUM(K)) THEN
                  DTAUM(K) = ZR(JDTAUM + (I-1))
                  MNMAX(K) = I
               ENDIF
 480        CONTINUE

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
     &          (ABS(NXM(K)) .LT. EPSILO)) THEN
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
     &                     ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

               GAMMA = 0.0D0

               CALL VECNUV(1, 1, GAMMA, PI, DPHI2, N, 1, DIM,
     &                     ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

               NBVEC = 7
               CALL TRLONO(NBVEC, JVECN1, JVECU1, JVECV1, NBORDR,
     &                     KWORK, SOMNOW, JRWORK, TSPAQ, JVNO1)
            ELSE
               DGAM2 = 1.0D0*(PI/180.0D0)
               DPHI2 = DGAM2/SIN(GAMMAM)
               N = 0
               DO 510 J=1, 3
                  GAMMA = GAMMAM + (J-2)*DGAM2

                  CALL VECNUV(1, 3, GAMMA, PHIM, DPHI2, N, 2, DIM,
     &                        ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

 510           CONTINUE

               NBVEC = 9
               CALL TRLONO(NBVEC, JVECN1, JVECU1, JVECV1, NBORDR,
     &                     KWORK, SOMNOW, JRWORK, TSPAQ, JVNO1)
            ENDIF

C 5-1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C     DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C     PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

            CALL JERAZO('&&DTAUNO.DTAU_MAX', NBVEC, 1)
            CALL JERAZO('&&DTAUNO.RESU_N', NBVEC, 1)

C 5-2/ CALCUL DU RAYON CIRCONSCRIT

            CALL RAYCIR(JVNO1, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)

C 5-3/ CALCUL DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

            DTAUM(K) = 0.0D0
            MNMAX(K) = 1

            DO 530 I=1, NBVEC
               IF ( ZR(JDTAUM + (I-1)) .GT. DTAUM(K)) THEN
                  DTAUM(K) = ZR(JDTAUM + (I-1))
                  MNMAX(K) = I
               ENDIF
 530        CONTINUE

            NXM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3)
            NYM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3 + 1)
            NZM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3 + 2)
            GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
            IF (GAMMAM .LT. 0.0D0) THEN
               GAMMAM = GAMMAM + PI
            ENDIF

            IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &          (ABS(NXM(K)) .LT. EPSILO)) THEN
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
     &                  VALE,CODRET,'  ')
            IF (CODRET(1:2) .EQ. 'NO') THEN
               CALL U2MESS('F','PREPOST_11')
            ENDIF
            CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'NU      ',
     &                  VALNU,CODRET,'  ')
            IF (CODRET(1:2) .EQ. 'NO') THEN
               CALL U2MESS('F','PREPOST_12')
            ENDIF
            C1 = (1+VALNU)/VALE
            C2 = VALNU/VALE

            DO 540 IORDR=1, NBORDR
               ADRS = (IORDR-1)*TSPAQ + KWORK*SOMNOW*6
               SIXX = ZR(JRWORK + ADRS + 0 )
               SIYY = ZR(JRWORK + ADRS + 1 )
               SIZZ = ZR(JRWORK + ADRS + 2 )
               SIXY = ZR(JRWORK + ADRS + 3 )
               SIXZ = ZR(JRWORK + ADRS + 4 )
               SIYZ = ZR(JRWORK + ADRS + 5 )

C CALCUL DE LA PRESSION HYDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])

               IF ( K .LT. 2 ) THEN

C ON CALCULE PHYDRM UNE FOIS, PARCE QUE LA PRESSION HYDROSTATIQUE
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
     &                   FZM(K)*NZM(K)

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
     &                     EPSZM(K)*NZM(K)

               IF (ABS(EPNORM(K)) .GT. EPNMAX(K)) THEN
                  EPNMAX(K) = EPNORM(K)
               ENDIF

               SEPNMX(K) = SEPNMX(K) + EPNORM(K)
 540        CONTINUE

            NORMOY(K) = SNORM(K)/NBORDR
            EPNMOY(K) = SEPNMX(K)/NBORDR

C ---------------------------------------------------------------------
C       =============================================
C       /      CRITERES AVEC PLANS CRITIQUES        /
C       =============================================
C ---------------------------------------------------------------------


C 1/ CRITERE DE MATAKE
            IF (NOMCRI(1:14) .EQ. 'MATAKE_MODI_AC') THEN
               IF ( (VALA*NORMAX(K)) .GT. 0.0D0 ) THEN
                  SIGEQ(K) = COEPRE*DTAUM(K) + (VALA*NORMAX(K))
                  SIGEQ(K) = SIGEQ(K)*COEFPA
               ELSE
                  SIGEQ(K) = COEPRE*DTAUM(K)
                  SIGEQ(K) = SIGEQ(K)*COEFPA
               ENDIF
            ENDIF

C 2/ CRITERE DE DANG VAN
            IF (NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AC') THEN
               IF ( (VALA*PHYDRM) .GT. 0.0D0 ) THEN
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

            CALL RCCOME ( NOMMAT, 'FATIGUE', PHENOM, CODRET )
            IF ( CODRET .EQ. 'NO' ) CALL U2MESS('F','PREPOST_2')

            CALL RCPARE( NOMMAT, 'FATIGUE', 'WOHLER', CODWO )
            IF ( CODWO .EQ. 'OK' ) THEN
               CALL LIMEND( NOMMAT,SIGEQ(K),'WOHLER',ENDUR)
               IF (ENDUR) THEN
                  NRUPT(K)=R8MAEM()
               ELSE
               CALL RCVALE(NOMMAT,'FATIGUE',1,'SIGM    ',SIGEQ(K),1,
     &                     'WOHLER  ',NRUPT(K),CODRET,'F')
               ENDIF
            ENDIF

            DOM(K) = 1.D0/NRUPT(K)
            NRUPT(K) = NINT(NRUPT(K))

 440     CONTINUE

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
               JAD = 24*(NUNOE-1) + ICMP
               ZL(JCNRL - 1 + JAD) = .TRUE.
               ZR(JCNRV - 1 + JAD) = VRESU(ICMP)

 550     CONTINUE

 400  CONTINUE

C MENAGE

      CALL DETRSD('CHAM_ELEM_S',CESMAT)

      CALL JEDETR('&&DTAUNO.DTAU_MAX')
      CALL JEDETR('&&DTAUNO.RESU_N')
      CALL JEDETR('&&DTAUNO.VECT_NORMA')
      CALL JEDETR('&&DTAUNO.VECT_TANGU')
      CALL JEDETR('&&DTAUNO.VECT_TANGV')
      CALL JEDETR('&&DTAUNO.VECT_NORMA1')
      CALL JEDETR('&&DTAUNO.VECT_TANGU1')
      CALL JEDETR('&&DTAUNO.VECT_TANGV1')
      CALL JEDETR('&&DTAUNO.VECT_NORMA2')
      CALL JEDETR('&&DTAUNO.VECT_TANGU2')
      CALL JEDETR('&&DTAUNO.VECT_TANGV2')
      CALL JEDETR('&&DTAUNO.VECTNO1')
      CALL JEDETR('&&DTAUNO.VECTNO2')
      CALL JEDETR('&&DTAUNO.VECTNO')
      CALL JEDETR('&&DTAUNO.CNCINV')
C
      CALL JEDEMA()
      END
