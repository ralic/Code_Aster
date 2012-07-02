      SUBROUTINE RVFCOM(NMAILA,M1,F1,M2,F2)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
      INCLUDE 'jeveux.h'
      CHARACTER*8 NMAILA
      INTEGER     M1,M2,F1,F2
C
C***********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C
C     ETANT DONNEES DEUX MAILLES M1 ET M2, ET UNE FACE F1 DE M1,TROUVER
C     LA FACE F2 DE M2 QUI COINCIDE AVEC F1
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     NMAILA : NOM DU MAILLAGE
C
C     M1, M2 : NUMERO DES MAILLES (2D)
C
C     F1     : NUMERO LOCALE DE LA FACE (1D) DE M1
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     F2 : NUMERO LOCAL DE LA FACE DE M2 CHERCHEE AVEC LA CONVENTION
C             F2 > 0 : F2 ET F1 DE MEME ORIENTATION
C             F2 < 0 : F2 ET F1 D' ORIENTATION OPPOSEE
C
C***********************************************************************
C
C  FONCTIONS EXTERNES
C  ------------------
C
C
C  -----------------------------------------
C
C
C  ---------------------------------
C
C  VARIABLES LOCALES
C  -----------------
C
      CHARACTER*16 NTYPE
      CHARACTER*15 NCONEC
C
      INTEGER N1G,N1D,N2G,N2D
C
      LOGICAL TROUVE
C
C==================== CORPS DE LA ROUTINE =============================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      NCONEC = NMAILA//'.CONNEX'
      NTYPE  = NMAILA//'.TYPMAIL'
C
      CALL I2EXTF(M1,F1,NCONEC,NTYPE,N1G,N1D)
C
      F2 = 0
C
      TROUVE = .FALSE.
C
10    CONTINUE
      IF ( .NOT. TROUVE ) THEN
C
         F2 = F2 + 1
C
         IF ( F2 .GT. 12 ) CALL U2MESS('F','POSTRELE_19')
C
         CALL I2EXTF(M2,F2,NCONEC,NTYPE,N2G,N2D)
C
         IF ( (N1G .EQ. N2G) .AND. ( N1D .EQ. N2D) ) THEN
C
            TROUVE = .TRUE.
C
         ELSE IF ( (N1D .EQ. N2G) .AND. ( N1G .EQ. N2D) ) THEN
C
            TROUVE = .TRUE.
C
            F2 = -F2
C
         ELSE
C
C
         ENDIF
C
         GOTO 10
C
      ENDIF
C
      END
