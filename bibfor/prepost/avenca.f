      SUBROUTINE AVENCA( RVECPG, NBVEC, NBORDR, LSIG0, IFLAG, RMIMA )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 16/11/2009   AUTEUR ANGLES J.ANGLES 
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
      IMPLICIT      NONE
      INTEGER       NBVEC, NBORDR, IFLAG(NBVEC)
      REAL*8        RVECPG(2*NBVEC*NBORDR), RMIMA(4*NBVEC)
      LOGICAL       LSIG0
C ----------------------------------------------------------------------
C BUT: ENCADRER LES POINTS REPRESANTANT LE CISAILLEMENT TAU
C      DANS LE PLAN DE CISAILLEMENT (PLAN u, v).
C ----------------------------------------------------------------------
C ARGUMENTS:
C RVECPG    IN   R  : VECTEUR DE TRAVAIL CONTENANT LES
C                     COMPOSANTES u ET v DU VECTEUR TAU (CISAILLEMENT),
C                     POUR TOUS LES VECTEURS NORMAUX (n) ET TOUS LES
C                     NUMEROS D'ORDRE.
C                     VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX.
C NBVEC     IN   I  : NOMBRE DE VECTEURS NORMAUX.
C NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                     STRUCTURE DE DONNEES RESULTAT.
C LSIG0     OUT  L  : VARIABLE LOGIQUE QUI INDIQUE :
C                      - LSIG0 = FALSE --> CAS GENERAL, LES CONTRAINTES
C                                          SONT DIFFERENTES DE ZERO ;
C                      - LSIG0 =  TRUE --> LES CONTRAINTES SONT NULLES
C                                          A TOUS LES PAS DE TEMPS, QUEL
C                                          QUE SOIT LE VECTEUR NORMAL.
C IFLAG     OUT  I  : VECTEUR DE DRAPEAUX QUI INDIQUE :
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
C RMIMA     OUT  R  : VECTEUR CONTENANT LES COORDONNEES DES POINTS
C                     EXTREMES DU CADRE (CUMIN, CUMAX, CVMIN, CVMAX)
C                     POUR TOUS LES VECTEURS NORMAUX.
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
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      N1, IVECT, IORDR, NSIG0
C
      REAL*8       EPSILO, R8MAEM, CUMIN, CUMAX, CVMIN, CVMAX
      REAL*8       CUI, CVI
C
C-----------------------------------------------------------------------
C234567                                                              012

      CALL JEMARQ()

C-----------------------------------------------------------------------
C     ------------------------------
C    |  TRAITEMENT DU CAS GENERAL  |
C    ------------------------------
C-----------------------------------------------------------------------

      EPSILO = 1.0D-5

C ININTIALISATION

      N1 = 0
      NSIG0 = 0

      DO 30 IVECT=1, NBVEC
         CUMIN = R8MAEM()
         CUMAX = -R8MAEM()
         CVMIN = R8MAEM()
         CVMAX = -R8MAEM()

         DO 40 IORDR=1, NBORDR
            N1 = N1 + 1
            CUI = RVECPG(2*N1 -1)
            CVI = RVECPG(2*N1)

            IF (CUI .LT. CUMIN) THEN
               CUMIN = CUI
            ENDIF
            IF (CUI .GT. CUMAX) THEN
               CUMAX = CUI
            ENDIF
            IF(CVI .LT. CVMIN) THEN
               CVMIN = CVI
            ENDIF
            IF(CVI .GT. CVMAX) THEN
               CVMAX = CVI
            ENDIF
 40      CONTINUE

C-----------------------------------------------------------------------
C   ------------------------------------
C  |  TRAITEMENT DES CAS PARTICULIERS  |
C  ------------------------------------
C-----------------------------------------------------------------------

C 1/ CAS OU TOUS LES POINTS SONT ALIGNES VERTICALEMENT, ON NE FERA PAS
C    DE PROJECTION.

         IF ( ABS(CUMAX-CUMIN)/2.D0 .LT. EPSILO ) THEN
            IFLAG(IVECT) = 1

C 2/ CAS OU TOUS LES POINTS SONT ALIGNES HORIZONTALEMENT, ON NE FERA
C    PAS DE PROJECTION.

         ELSEIF ( ABS(CVMAX-CVMIN)/2.D0 .LT. EPSILO ) THEN
            IFLAG(IVECT) = 2

C 3/ CAS OU TOUS LES POINTS SONT DANS UNE BOITE DONT LES DEUX COTES
C    SONT INFERIEURS A EPSILO, ON NE FERA PAS DE PROJECTION.

         ELSEIF ( (ABS(CVMAX-CVMIN)/2.D0 .LT. EPSILO) .AND.
     &            (ABS(CUMAX-CUMIN)/2.D0 .LT. EPSILO) ) THEN
            IFLAG(IVECT) = 3
            NSIG0 = NSIG0 + 1
         ENDIF

         RMIMA(4*IVECT - 3) = CUMIN
         RMIMA(4*IVECT - 2) = CUMAX
         RMIMA(4*IVECT - 1) = CVMIN
         RMIMA(4*IVECT) = CVMAX

         IF (NSIG0 .EQ. NBVEC) THEN
            LSIG0 = .TRUE.
         ENDIF

 30   CONTINUE

      CALL JEDEMA()
      END
