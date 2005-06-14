      SUBROUTINE ASCOPC(IATP1,IATP2,NRMAX,IDRESL,NBLC,
     &                  KVALE,COEF,NBMEM,LIADD)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IATP1,IATP2,NRMAX,IDRESL,NBLC
      INTEGER           NBMEM,LIADD(NBMEM)
      CHARACTER*24                                    KVALE
      REAL*8                                                COEF
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ROUTINE QUI ACCUMULE LES TERMES ELEMENTAIRES DANS LES BLOCS DE LA
C     MATRICE ASSEMBLEE POUR UNE MATRICE COMPLEXE
C-----------------------------------------------------------------------
C IN  I   IATP1  : ADRESSE JEVEUX DE L'OBJET ".TEMPOR1".
C IN  I   IATP2  : ADRESSE JEVEUX DE L'OBJET ".TEMPOR2".
C IN  I   NRMAX  : NOMBRE DE REELS A CHARGER.
C IN  I   IDRESL : ADRESSE JEVEUX DE L'OBJET ".VALE(IEL)".
C IN  I   NBLC   : NOMBRE DE BLOCS DE LA MATRICE.
C IN  K24 KVALE  : NOM DE L'OBJET ".VALE".
C IN  R   COEF   : COEFFICIENT REEL MULTIPLICATEUR.!!!! COEF REELS !!!!
C IN  I   NBMEM  : NOMBRE DES 0,1,2 1ER BLOCS DE .VALE DEJA EN MEMOIRE
C IN  I   LIADD  : LISTE DES ADRESSES DES BLOCS 0,1,2 DE .VALE
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
C-----------------------------------------------------------------------
C          DEBUT DES INSTRUCTIONS
C-----------------------------------------------------------------------
C---- BOUCLE SUR LES BLOCS DE LA MATRICE:
      DO 1,I = 1,NBLC
         NBVAL = ZI(IATP1-1+I)
         IF (NBVAL.EQ.0) GO TO 1

         IF (I.GT.NBMEM)  THEN
            CALL JEVEUO(JEXNUM(KVALE,I),'E',IADVAL)
         ELSE
            IADVAL=LIADD(I)
         ENDIF

CCDIR$ IVDEP
         DO 2,J = 1,NRMAX
            IF (ZI(IATP2-1+2* (J-1)+1).EQ.I) THEN
               IADLOC = ZI(IATP2-1+2* (J-1)+2)
               ZC(IADVAL+IADLOC-1) = ZC(IADVAL+IADLOC-1) +
     +                               COEF*ZC(IDRESL-1+J)
            END IF
    2    CONTINUE

         IF (I.GT.NBMEM) CALL JELIBE(JEXNUM(KVALE,I))

C        -- ON REMET NBVAL A ZERO POUR LE PROCHAIN COUP:
         ZI(IATP1-1+I) = 0
    1 CONTINUE
 9999 CONTINUE
      END
