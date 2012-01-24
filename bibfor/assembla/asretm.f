      SUBROUTINE ASRETM(LMASYM,JTMP2,LGTMP2,NBTERM,JSMHC,JSMDI,I1,I2)
      IMPLICIT NONE
      LOGICAL LMASYM
      INTEGER  JTMP2,LGTMP2,NBTERM,JSMHC,JSMDI,I1,I2
      INTEGER  IDEB,IFIN,IMIL
C -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 23/01/2012   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_4
C     ROUTINE SERVANT A RETENIR OU S'ACCUMULENT LES TERMES ELEMENTAIRES:
C     DANS LE CAS D'UN STOCKAGE MORSE SYMETRIQUE
C -----------------------------------------------------------------
C IN/OUT I JTMP2   : ADRESSE JEVEUX DE L'OBJET ".TMP2"
C IN    I4 JSMHC   : ADRESSE DE ".SMHC".
C IN     I JSMDI   : ADRESSE DE ".SMDI".
C IN     I I1,I2   : NUMEROS GLOBAUX (LIGNE ET COLONNE)
C IN/OUT I NBTERM   : INDICE DU TERME (R/C) A RECOPIER
C                     (ISSU DE LA MATRICE ELEMENTAIRE)
C -----------------------------------------------------------------
      INTEGER*4 ZI4
      COMMON  /I4VAJE/ZI4(1)
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      INTEGER ILI,JCO,ICOEFC,ICOEFL,I,NCOEFC,NUBLOC
C -----------------------------------------------------------------
      IF (I1.LE.I2) THEN
        ILI=I1
        JCO=I2
        NUBLOC=1
      ELSE
        ILI=I2
        JCO=I1
        NUBLOC=2
      ENDIF
      IF (LMASYM) NUBLOC=1

      IF (JCO.EQ. 1) THEN
         ICOEFC = 0
      ELSE
         ICOEFC = ZI(JSMDI+JCO-2)
      END IF
      NCOEFC = ZI(JSMDI+JCO-1) - ICOEFC


C     -- CALCUL DE ICOEFL :
C     ------------------------------------------
      ICOEFL = 0
      IF (.FALSE.) THEN
C     -- RECHERCHE BESTIALE :
        DO 10 I = 1,NCOEFC
           IF (ZI4(JSMHC-1+ICOEFC+I).EQ.ILI) THEN
              ICOEFL = I
              GOTO 20
           END IF
10      CONTINUE

      ELSE
C       -- RECHERCHE PAR DICHOTOMIE :
        IDEB=1
        IFIN=NCOEFC
11      CONTINUE
        IF (IFIN-IDEB.LT.5) THEN
          DO 12 I = IDEB,IFIN
             IF (ZI4(JSMHC-1+ICOEFC+I).EQ.ILI) THEN
                ICOEFL = I
                GOTO 20
             END IF
12        CONTINUE
        ENDIF
        IMIL=(IDEB+IFIN)/2
        IF (ZI4(JSMHC-1+ICOEFC+IMIL).GT.ILI) THEN
          IFIN=IMIL
        ELSE
          IDEB=IMIL
        ENDIF
        GOTO 11
      ENDIF
      IF (ICOEFL.EQ.0 )  CALL U2MESS('F','MODELISA_67')


20    CONTINUE

C     -- NBTERM COMPTE LES REELS TRAITES:
      NBTERM = NBTERM + 1
      IF (2*NBTERM.GT.LGTMP2) THEN
          LGTMP2 = 2*LGTMP2
          CALL JUVECA('&&ASSMAM.TMP2',LGTMP2)
C         -- IL NE FAUT PAS QUE .TMP2 SOIT LIBERE :
          CALL JEVEUT('&&ASSMAM.TMP2','E',JTMP2)
      ENDIF
      ZI(JTMP2-1+(NBTERM-1)*2+1) = NUBLOC
      ZI(JTMP2-1+(NBTERM-1)*2+2) = ICOEFC+ICOEFL
      END
