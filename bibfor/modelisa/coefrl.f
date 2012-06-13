      SUBROUTINE COEFRL(NOM1,NOM2,NOM3,NCKMAX,IPAS,IRES,BORNCK,NBORCK,
     &                  COEFCK,IPAS1,IRES1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C  IN    : NOM1      : A RENSEIGNER
C  IN    : NOM2      : A RENSEIGNER
C  IN    : NOM3      : A RENSEIGNER
C  IN    : NCKMAX    : A RENSEIGNER
C  IN    : IPAS      : A RENSEIGNER
C  IN    : IRES      : A RENSEIGNER
C  OUT   : BORNCK    : A RENSEIGNER
C  OUT   : COEFCK    : A RENSEIGNER
C  OUT   : IPAS1     : A RENSEIGNER
C  OUT   : IRES1     : A RENSEIGNER
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER      IPAS, IRES, NCKMAX, NBORCK
      REAL*8       BORNCK(20),COEFCK(20,11)
      CHARACTER*24 NOM1, NOM2, NOM3
C
C     UN COMMON AJOUTE POUR RESORBER UNE GLUTE ANTIQUE (VOIR HISTOR):
      CHARACTER*8  TYPFLU
      COMMON  / KOP144 / TYPFLU

      INTEGER      UNIT, NBOMAX, NBLOC
      INTEGER      JBORNE, JCOEFF, JVIRED, NBVAL1, NBVAL2, NBVAL3
      REAL*8       ZERO,BOCK1(20),COEF1(20,11)
      REAL*8       VRMIN, VRMAX
      CHARACTER*24 NOM4
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- LECTURE DU FICHIER DE DONNEES
C     =============================
C
      NOM4 = TYPFLU//'.UNIT_FAISCEAU'
      CALL JEVEUO(NOM4,'L',IUNIT)
      UNIT = ZI(IUNIT-1+2)
      NBOMAX = 20
      CALL ULOPEN(UNIT,' ',' ','NEW','O')
      READ (UNIT,*) NBLOC
      ZERO = 0.0D0
C
C --- BLOC D'INITIALISATION
      DO 10 I = 1,NBOMAX
        BOCK1 (I) = ZERO
        BORNCK(I) = ZERO
        DO 20 J = 1,NCKMAX
           COEF1 (I,J) = ZERO
           COEFCK(I,J) = ZERO
 20     CONTINUE
 10   CONTINUE
C
      DO 30 KK = 1,NBLOC
         READ (UNIT,*) IPAS1
         READ (UNIT,*) IRES1
         READ (UNIT,*) NB1
         IF(IPAS1 .EQ. IPAS .AND. IRES1 .EQ. IRES) THEN
            NBVAL1 = 3
            NBVAL2 = NB1 + NB1*NCKMAX
            NBVAL3 = 2
            CALL WKVECT (NOM1,'V V I',NBVAL1,JBORNE)
            CALL WKVECT (NOM2,'V V R',NBVAL2,JCOEFF)
            CALL WKVECT (NOM3,'V V R',NBVAL3,JVIRED)
            ZI(JBORNE-1+1) = IPAS1
            ZI(JBORNE-1+2) = IRES1
            ZI(JBORNE-1+3) = NB1
            ZR(JVIRED-1+1) = VRMIN
            ZR(JVIRED-1+2) = VRMAX
            READ (UNIT,*) (BOCK1(I),I = 1,NB1),VRMIN,VRMAX
            DO 40 I = 1,NB1
              ZR( JCOEFF+I-1 ) = BOCK1(I)
  40        CONTINUE
C
            ZR(JVIRED-1+1) = VRMIN
            ZR(JVIRED-1+2) = VRMAX
C
            K = 1
            DO 50 I = 1, NB1
              READ (UNIT,*) (COEF1(I,J),J = 1,NCKMAX)
              DO 60 J = 1,NCKMAX
                ZR(JCOEFF+NB1+K-1) = COEF1(I,J)
                K = K + 1
   60        CONTINUE
  50        CONTINUE
C
            NBORCK = NB1
C
            DO 70 I = 1,NB1
               BORNCK(I) = BOCK1(I)
               DO 80 J = 1,NCKMAX
                  COEFCK(I,J) = COEF1(I,J)
  80          CONTINUE
  70       CONTINUE
            GO TO 120
         ELSE
            READ (UNIT,*) (BOCK1(I),I = 1,NB1),VRMIN,VRMAX
            DO 90 I = 1, NB1
              READ (UNIT,*) (COEF1(I,J),J = 1,NCKMAX)
   90       CONTINUE
            READ (UNIT,*)
         ENDIF
  30  CONTINUE
      IF(IPAS1 .NE. IPAS .OR. IRES1 .NE. IRES) THEN
         CALL U2MESS('F','MODELISA4_33')
      ENDIF
C
 120  CONTINUE
      CALL ULOPEN(-UNIT,' ',' ',' ',' ')
      CALL JEDEMA()
C
      END
