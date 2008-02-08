      SUBROUTINE USOBCE ( DIMOBS, OBSUSE, RCARTE, NOMT19, NBUSUR,
     &                    PARUSU, TYPUSU )
      IMPLICIT   NONE
      INTEGER             DIMOBS, NBUSUR, TYPUSU(*)
      REAL*8              OBSUSE(*), RCARTE, PARUSU(20,*)
      CHARACTER*19        NOMT19
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C-----------------------------------------------------------------------
C     GUIDAGE CERCLE
C ----------------------------------------------------------------------
      INTEGER      I, L, IFM, NIV
      REAL*8       TABR(4), DELTAN
      REAL*8       ALPHAD, ALPHAM, ALPHAF, PROF, AD, AM, AF
      COMPLEX*16   C16B
      CHARACTER*4  T2
      CHARACTER*8  TABK(2)
      CHARACTER*16 NOPARA(7)
C
      DATA NOPARA / 'LIEU'    , 'SECTEUR' , 'TYPE'    , 'ANGL_DEBUT',
     &              'ANGL_FIN', 'ANGL_MAX', 'PROF_MAX' /
C-----------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      TABK(1) = 'OBST'
      TABK(2) = 'TYPE'
      IF ( NIV .GE. 2 ) WRITE(IFM,1000)
      DO 140 I = 1 , NBUSUR
         IF ( TYPUSU(I) .EQ. 0 ) THEN
            GOTO 140
         ELSEIF ( TYPUSU(I) .EQ. 1 ) THEN
            T2 = '   L'
         ELSEIF ( TYPUSU(I) .EQ. 2 ) THEN
            T2 = 'LV_1'
         ELSEIF ( TYPUSU(I) .EQ. 3 ) THEN
            T2 = ' V_1'
         ELSEIF ( TYPUSU(I) .EQ. 4 ) THEN
            T2 = 'LV_2'
         ELSEIF ( TYPUSU(I) .EQ. 5 ) THEN
            T2 = ' V_2'
         ELSE
            T2 = '????'
         ENDIF
         DELTAN  = PARUSU(I,3) - PARUSU(I,1)
         TABK(2) = T2
         TABR(1) = PARUSU(I,1)
         TABR(2) = PARUSU(I,3)
         TABR(3) = PARUSU(I,2)
         TABR(4) = PARUSU(I,4)
         CALL TBAJLI ( NOMT19, 7, NOPARA, I, TABR, C16B, TABK, 0 )
         IF ( NIV .GE. 2 ) WRITE(IFM,1010) I, T2, PARUSU(I,1),
     &                    PARUSU(I,3), PARUSU(I,2), DELTAN, PARUSU(I,4)
 140  CONTINUE
C
C     TRACE DES USURES :
C     ------------------
C
      DIMOBS = 720
      DO 30 L = 1 , DIMOBS
         OBSUSE(2*L-1) = ( L - 1 ) * 0.5D0
         OBSUSE(2*L  ) = RCARTE
 30   CONTINUE
C
      DO 150 I = 1 , NBUSUR
C
         IF ( TYPUSU(I) .EQ. 0 )  GOTO 150
C
         ALPHAD = PARUSU(I,1)
         ALPHAM = PARUSU(I,2)
         ALPHAF = PARUSU(I,3)
         PROF   = PARUSU(I,4)
C
C ------ ON VERIFIE QUE ALPHAD < ALPHAM < ALPHAF
C
         AD = ALPHAD
         IF ( ALPHAD .LT. 0.D0 )  AD = ALPHAD + 360.D0
C
         AM = ALPHAM
         IF ( AD .GT. ALPHAM )  AM = ALPHAM + 360.D0

         AF = ALPHAF
         IF ( AM .GT. ALPHAF )  AF = ALPHAF + 360.D0
C
         CALL ASSERT ( AD.LT.AM .AND. AM.LT.AF )
         CALL USVECT ( 1.D0, AD, AM, AF, PROF, DIMOBS, OBSUSE )
C
 150  CONTINUE
C
 1000 FORMAT('==> IMPRESSION DE PARAMETRES "OBST" PAR SECTEUR USE:',/,
     &       ' SECTEUR   TYPE     ANGL_DEBUT      ANGL_FIN',
     &       '      ANGLE_MAX      DELTA_ANGL     PROFONDEUR')
 1010 FORMAT(1P,4X,I2,5X,A4,5(3X,E12.5))
C
      END
