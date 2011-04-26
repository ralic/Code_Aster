      SUBROUTINE AVPEAK(VALAXE, NBVEC, NBORDR, PSEUIL, IFLAG,
     &                  NPOIN, VALPOI, VALORD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT      NONE
      INTEGER       NBVEC, NBORDR, NPOIN(NBVEC), VALORD(NBVEC*NBORDR)
      INTEGER       IFLAG(NBVEC)
      REAL*8        VALAXE(NBVEC*NBORDR), PSEUIL, VALPOI(NBVEC*NBORDR)
C ----------------------------------------------------------------------
C BUT: EXTRAIRE LES PICS D'UNE FONCTION.
C ----------------------------------------------------------------------
C ARGUMENTS:
C VALAXE    IN   R  : VECTEUR CONTENANT L'HISTORIQUE DES PROJECTIONS
C                     POUR TOUS LES VECTEURS NORMAUX (n) ET TOUS LES
C                     NUMEROS D'ORDRE.
C NBVEC     IN   I  : NOMBRE DE VECTEURS NORMAUX.
C NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE.
C PSEUIL    IN   R  : SEUIL DE DTECTION DES PICS
C IFLAG     IN   I  : VECTEUR DE DRAPEAUX QUI INDIQUE :
C                      - IFLAG(i) = 0 --> CAS GENERAL ;
C                      - IFLAG(i) = 1 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         ALIGNES VERTICALEMENT ;
C                      - IFLAG(i) = 2 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         ALIGNES HORIZONTALEMENT ;
C                      - IFLAG(i) = 3 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         CONTENUS DANS UN CADRE DE
C                                         COTES INFERIEURS A EPSILO.
C NPOIN     OUT  I  : NOMBRE DE PICS DETECTES POUR TOUS LES VECTEURS
C                     NORMAUX.
C VALPOI    OUT  R  : VALEUR DES PICS DETECTES POUR TOUS LES VECTEURS
C                     NORMAUX.
C VALORD    OUT  I  : NUMEROS D'ORDRE ASSOCIES AUX PICS DETECTES POUR
C                     TOUS LES VECTEURS NORMAUX.
C
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
      INTEGER       IVECT, IORDR, PASS, SORTIE, ORDMAX, ORDMIN
C
      REAL*8       VMIN, VMAX, VALEUR, EPSILO
C
C-----------------------------------------------------------------------
C234567                                                              012
C-----------------------------------------------------------------------
      EPSILO = 1.0D-7
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DO 10 IVECT=1, NBVEC
C
         IF (IFLAG(IVECT) .EQ. 3) THEN
            GOTO 10
         ENDIF
C
C ----- LE PREMIER POINT EST UN PIC -----
C
         NPOIN(IVECT) = 1
         VALPOI((IVECT-1)*NBORDR + 1) = VALAXE((IVECT-1)*NBORDR + 1)
         VALORD((IVECT-1)*NBORDR + 1) = 1
         VMAX = VALPOI((IVECT-1)*NBORDR + 1)
         VMIN = VALPOI((IVECT-1)*NBORDR + 1)
         PASS = 0
         SORTIE = 2
C
C ----- RECHERCHE DES PICS INTERMEDIAIRES -----
C
         DO 20 IORDR=2, NBORDR
            VALEUR = VALAXE((IVECT-1)*NBORDR + IORDR)
            IF(VMAX .LT. VALEUR) THEN
               VMAX = VALEUR
               ORDMAX = IORDR
            ENDIF
            IF(VMIN .GT. VALEUR) THEN
               VMIN = VALEUR
               ORDMIN = IORDR
            ENDIF
            IF (PASS .EQ. 0) THEN
               IF((VALEUR-VMIN) .GT. PSEUIL) THEN
                  SORTIE = 1
                  PASS   = 1
               ENDIF
               IF((VMAX-VALEUR) .GT. PSEUIL) THEN
                  SORTIE = 0
                  PASS   = 1
               ENDIF
            ENDIF
            IF ( (SORTIE .EQ. 1) .AND.
     &           ((VMAX-VALEUR)-PSEUIL .GT. EPSILO) ) THEN
               NPOIN(IVECT) = NPOIN(IVECT) + 1
               VALPOI((IVECT-1)*NBORDR + NPOIN(IVECT)) = VMAX
               VALORD((IVECT-1)*NBORDR + NPOIN(IVECT)) = ORDMAX
               VMIN = VALEUR
               ORDMIN = IORDR
               SORTIE = 0
            ENDIF
            IF ( (SORTIE .EQ. 0) .AND.
     &           ((VALEUR-VMIN)-PSEUIL .GT. EPSILO) ) THEN
               NPOIN(IVECT) = NPOIN(IVECT) + 1
               VALPOI((IVECT-1)*NBORDR + NPOIN(IVECT)) = VMIN
               VALORD((IVECT-1)*NBORDR + NPOIN(IVECT)) = ORDMIN
               VMAX = VALEUR
               ORDMAX = IORDR
               SORTIE = 1
            ENDIF
 20      CONTINUE
C
         IF(SORTIE .EQ. 0) THEN
            NPOIN(IVECT) = NPOIN(IVECT) + 1
            VALPOI((IVECT-1)*NBORDR + NPOIN(IVECT)) = VMIN
            VALORD((IVECT-1)*NBORDR + NPOIN(IVECT)) = ORDMIN
         ENDIF
         IF(SORTIE .EQ. 1) THEN
            NPOIN(IVECT) = NPOIN(IVECT) + 1
            VALPOI((IVECT-1)*NBORDR + NPOIN(IVECT)) = VMAX
            VALORD((IVECT-1)*NBORDR + NPOIN(IVECT)) = ORDMAX
         ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
      END
