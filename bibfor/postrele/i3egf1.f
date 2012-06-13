      SUBROUTINE I3EGF1 ( DESC, DESCTM, CONEK1, CONEK2,
     +                    IM1, IF1, IAO1, IAE1, IM2, IF2, IAO2, LEGFA )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER          DESC(*), DESCTM(*), CONEK1(*), CONEK2(*),
     +                 IM1, IF1, IAO1, IAE1, IM2, IF2, IAO2
      LOGICAL          LEGFA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
      INTEGER       DECF1, ADESC1, NBNDF1, DECF2, ADESC2, NBNDF2, NBNO2
      INTEGER       I, NUMNO1(15), NUMNO2(15), I1, J1, I2, J2, NBNO1
      INTEGER       L, NBNT
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      LEGFA = .FALSE.
C
      DECF1  = 8 + IF1
      ADESC1 = DESCTM(DESC(IM1))
      NBNDF1 = ZI(ADESC1-1 + 2 + IF1)
      DECF2  = 8 + IF2
      ADESC2 = DESCTM(DESC(IM2))
      NBNDF2 = ZI(ADESC2-1 + 2 + IF2)
C
      IF ( IAE1 .EQ. 0 ) THEN
        DO 10, I = 1, NBNDF1, 1
           NUMNO1(I) = CONEK1( ZI(ADESC1-1 + DECF1 + (I-1)*6) )
 10     CONTINUE
        NBNO1 = NBNDF1
        NBNT  = NBNDF1
      ELSEIF ( IAE1 .EQ. 1 ) THEN
        NUMNO1(1) = CONEK1( ZI(ADESC1-1 + DECF1 + (1-1)*6) )
        NUMNO1(2) = CONEK1( ZI(ADESC1-1 + DECF1 + (2-1)*6) )
        NBNO1 = 2
        NBNT  = 2
        IF ( IAE1 .EQ. IAO1 ) NBNT  = 1
      ELSEIF ( IAE1 .EQ. 2 ) THEN
        NUMNO1(1) = CONEK1( ZI(ADESC1-1 + DECF1 + (2-1)*6) )
        NUMNO1(2) = CONEK1( ZI(ADESC1-1 + DECF1 + (3-1)*6) )
        NBNO1 = 2
        NBNT  = 2
        IF ( IAE1 .EQ. IAO1 ) NBNT  = 1
      ELSEIF ( IAE1 .EQ. 3  .AND.  NBNDF1 .EQ. 3 ) THEN
        NUMNO1(1) = CONEK1( ZI(ADESC1-1 + DECF1 + (3-1)*6) )
        NUMNO1(2) = CONEK1( ZI(ADESC1-1 + DECF1 + (1-1)*6) )
        NBNO1 = 2
        NBNT  = 2
        IF ( IAE1 .EQ. IAO1 ) NBNT  = 1
      ELSEIF ( IAE1 .EQ. 3  .AND.  NBNDF1 .EQ. 4 ) THEN
        NUMNO1(1) = CONEK1( ZI(ADESC1-1 + DECF1 + (3-1)*6) )
        NUMNO1(2) = CONEK1( ZI(ADESC1-1 + DECF1 + (4-1)*6) )
        NBNO1 = 2
        NBNT  = 2
        IF ( IAE1 .EQ. IAO1 ) NBNT  = 1
      ELSEIF ( IAE1 .EQ. 4 ) THEN
        NUMNO1(1) = CONEK1( ZI(ADESC1-1 + DECF1 + (4-1)*6) )
        NUMNO1(2) = CONEK1( ZI(ADESC1-1 + DECF1 + (1-1)*6) )
        NBNO1 = 2
        NBNT  = 2
        IF ( IAE1 .EQ. IAO1 ) NBNT  = 1
      ELSE
        GOTO 9999
      ENDIF
C
      IF ( IAO2 .EQ. 0 ) THEN
        DO 12, I = 1, NBNDF1, 1
           NUMNO2(I) = CONEK2( ZI(ADESC2-1 + DECF2 + (I-1)*6) )
 12     CONTINUE
        NBNO2 = NBNDF1
      ELSEIF ( IAO2 .EQ. 1 ) THEN
        NUMNO2(1) = CONEK2( ZI(ADESC2-1 + DECF2 + (1-1)*6) )
        NUMNO2(2) = CONEK2( ZI(ADESC2-1 + DECF2 + (2-1)*6) )
        NBNO2 = 2
      ELSEIF ( IAO2 .EQ. 2 ) THEN
        NUMNO2(1) = CONEK2( ZI(ADESC2-1 + DECF2 + (2-1)*6) )
        NUMNO2(2) = CONEK2( ZI(ADESC2-1 + DECF2 + (3-1)*6) )
        NBNO2 = 2
      ELSEIF ( IAO2 .EQ. 3  .AND.  NBNDF2 .EQ. 3 ) THEN
        NUMNO2(1) = CONEK2( ZI(ADESC2-1 + DECF2 + (3-1)*6) )
        NUMNO2(2) = CONEK2( ZI(ADESC2-1 + DECF2 + (1-1)*6) )
        NBNO2 = 2
      ELSEIF ( IAO2 .EQ. 3  .AND.  NBNDF2 .EQ. 4 ) THEN
        NUMNO2(1) = CONEK2( ZI(ADESC2-1 + DECF2 + (3-1)*6) )
        NUMNO2(2) = CONEK2( ZI(ADESC2-1 + DECF2 + (4-1)*6) )
        NBNO2 = 2
      ELSEIF ( IAO2 .EQ. 4 ) THEN
        NUMNO2(1) = CONEK2( ZI(ADESC2-1 + DECF2 + (4-1)*6) )
        NUMNO2(2) = CONEK2( ZI(ADESC2-1 + DECF2 + (1-1)*6) )
        NBNO2 = 2
      ELSE
        GOTO 9999
      ENDIF
C
      L = 0
      DO 20, I1 = 1, NBNO1, 1
         J1 = NUMNO1(I1)
         DO 22, I2 = 1, NBNO2, 1
            J2 = NUMNO2(I2)
            IF ( J2 .EQ. J1 ) L = L + 1
 22      CONTINUE
 20   CONTINUE
      IF ( L .EQ. NBNT ) LEGFA = .TRUE.
C
 9999 CONTINUE
      CALL JEDEMA()
      END
