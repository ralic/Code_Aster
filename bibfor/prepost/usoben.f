      SUBROUTINE USOBEN ( GUIDAG, DIMOBS, OBSUSE, NCO, RAYO, THET,
     &    NBSECT, PARUSU, TYPUSU, NOMT19, ARETE, ARETE2, RCARTE, DENC )
      IMPLICIT   NONE
      INTEGER             DIMOBS, NCO, NBSECT, TYPUSU(*)
      REAL*8              OBSUSE(*), PARUSU(20,*), RAYO(*), THET(*),
     &                    ARETE, ARETE2, RCARTE, DENC
      CHARACTER*8         GUIDAG
      CHARACTER*19        NOMT19
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 18/09/2007   AUTEUR DURAND C.DURAND 
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
C     CALCULE LES TRACES USES D'OBSTACLE (GUIDAGE ENCO_1, ENCO_2)
C
C     PAR SECTEUR I
C        PARUSU(I,1) = ANGLE DEBUT
C        PARUSU(I,2) = ANGLE OU LA PROFONDEUR EST MAXI
C        PARUSU(I,3) = ANGLE FIN
C        PARUSU(I,4) = PROFONDEUR MAX
C
C ----------------------------------------------------------------------
      INTEGER      I, L, IFM, NIV
      INTEGER VALI
      REAL*8       THETA, DELTAN, AI1, BI1, R, Y
      REAL*8       RAD, R8PREM, R8DGRD, TABR(4), ARET1F, ARET2F
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
      RAD  = R8DGRD( )
      ARET1F = 360.D0 - ARETE
      ARET2F = 180.D0 + ARETE2
C
      TABK(1) = 'OBST'
      TABK(2) = 'TYPE'
      IF ( NIV .GE. 2 ) WRITE(IFM,1000)
      DO 10 I = 1 , NBSECT
         IF ( TYPUSU(I) .EQ. 0 ) THEN
            GOTO 10
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
 10   CONTINUE
C
C     TRACE DES USURES :
C     ------------------
C
      DIMOBS = NCO
      DO 20 L = 1 , DIMOBS
         OBSUSE(2*L-1) = THET(L) / RAD
         OBSUSE(2*L  ) = RCARTE
 20   CONTINUE
C
      DO 100 I = 1 , NBSECT
C
         IF ( TYPUSU(I) .EQ. 0 )  GOTO 100
C
         ALPHAD = PARUSU(I,1)
         ALPHAM = PARUSU(I,2)
         ALPHAF = PARUSU(I,3)
         PROF   = PARUSU(I,4)
C
         IF ( ABS(ALPHAM-ALPHAF) .LE. R8PREM() )  THEN
            VALI = I
            CALL U2MESG('A','PREPOST6_4',0,' ',1,VALI,0,0.D0)
            GOTO 100
         ENDIF
C
         IF ( ABS(ALPHAM-ALPHAD) .LE. R8PREM() )  THEN
            VALI = I
            CALL U2MESG('A','PREPOST6_5',0,' ',1,VALI,0,0.D0)
            GOTO 100
         ENDIF
C
C ------ VE + LUNULE SUR LA PREMIERE ENCOCHE
         IF ( TYPUSU(I) .EQ. 2 )  THEN
            IF ( I .EQ. 2 )  THEN
               AI1 = PROF / ( ALPHAM - ALPHAF )
               BI1 = AI1*ALPHAF
               CALL USENCO ( AI1, BI1, 20.D0, ALPHAF, DIMOBS, OBSUSE )
            ELSE
               AI1 = PROF / ( ALPHAM - ALPHAD )
               BI1 = AI1*ALPHAD
               CALL USENCO ( AI1, BI1, ALPHAD, 340.D0, DIMOBS, OBSUSE )
            ENDIF
C
C ------ VE SUR LA PREMIERE ENCOCHE
         ELSEIF ( TYPUSU(I) .EQ. 3 )  THEN
            IF ( I .EQ. 1 )  THEN
               AI1 = PROF / ( ALPHAM - ALPHAF )
               BI1 = AI1*ALPHAF
            ELSE
               AI1 = PROF / ( ALPHAM - ALPHAD )
               BI1 = AI1*ALPHAD
            ENDIF
            CALL USENCO ( AI1, BI1, ALPHAD, ALPHAF, DIMOBS, OBSUSE )
C
C ------ VE + LUNULE SUR LA DEUXIEME ENCOCHE
         ELSEIF ( TYPUSU(I) .EQ. 4 )  THEN
            IF ( NBSECT.EQ.12 .AND. I.EQ.8 )  THEN
               AI1 = PROF / ( ALPHAM - ALPHAF )
               BI1 = AI1*ALPHAF
            ELSEIF ( NBSECT.EQ.10 .AND. I.EQ.7 )  THEN
               AI1 = PROF / ( ALPHAM - ALPHAF )
               BI1 = AI1*ALPHAF
            ELSE
               AI1 = PROF / ( ALPHAM - ALPHAD )
               BI1 = AI1*ALPHAD
            ENDIF
            CALL USENCO ( AI1, BI1, ALPHAD, ALPHAF, DIMOBS, OBSUSE )
C
C ------ VE SUR LA DEUXIEME ENCOCHE
         ELSEIF ( TYPUSU(I) .EQ. 5 )  THEN
            IF ( NBSECT.EQ.12 .AND. I.EQ.7 )  THEN
               AI1 = PROF / ( ALPHAM - ALPHAF )
               BI1 = AI1*ALPHAF
            ELSEIF ( NBSECT.EQ.10 .AND. I.EQ.6 )  THEN
               AI1 = PROF / ( ALPHAM - ALPHAF )
               BI1 = AI1*ALPHAF
            ELSE
               AI1 = PROF / ( ALPHAM - ALPHAD )
               BI1 = AI1*ALPHAD
            ENDIF
            CALL USENCO ( AI1, BI1, ALPHAD, ALPHAF, DIMOBS, OBSUSE )
C
C ------ LUNULE
         ELSE
C
            AD = ALPHAD
            IF ( ALPHAD .LT. 0.D0 )  AD = ALPHAD + 360.D0
            AM = ALPHAM
            IF ( AD .GT. ALPHAM )    AM = ALPHAM + 360.D0
            AF = ALPHAF
            IF ( AM .GT. ALPHAF )    AF = ALPHAF + 360.D0
            IF ( AD.LT.AM .AND. AM.LT.AF ) THEN
               CALL USVECT ( 1.D0, AD, AM, AF, PROF, DIMOBS, OBSUSE )
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
         ENDIF
 9999 CONTINUE
 100  CONTINUE
C
      IF ( GUIDAG .EQ. 'ENCO_1' ) THEN
         DO 200 L = 1 , DIMOBS
            THETA = OBSUSE(2*L-1)
            IF ( THETA .LT. ARETE ) THEN
               R = OBSUSE(2*L)
               Y = R * SIN(THETA*RAD)
               IF ( Y .LT. DENC )  OBSUSE(2*L) = RAYO(L)
            ELSEIF ( THETA .GT. ARET1F ) THEN
               R = OBSUSE(2*L)
               Y = ABS( R * SIN( THETA*RAD ) )
               IF ( Y .LT. DENC )  OBSUSE(2*L) = RAYO(L)
            ENDIF
 200     CONTINUE
      ELSEIF ( GUIDAG .EQ. 'ENCO_2' ) THEN
         DO 210 L = 1 , DIMOBS
            THETA = OBSUSE(2*L-1)
            IF ( THETA .LT. ARETE ) THEN
               R = OBSUSE(2*L)
               Y = R * SIN(THETA*RAD)
               IF ( Y .LT. DENC )  OBSUSE(2*L) = RAYO(L)
            ELSEIF ( THETA.GT.ARETE2 .AND. THETA.LT.180.D0 ) THEN
               R = OBSUSE(2*L)
               Y = R * SIN( THETA*RAD )
               IF ( Y .LT. DENC )  OBSUSE(2*L) = RAYO(L)
            ELSEIF ( THETA.GT.180.D0 .AND. THETA.LT.ARET2F ) THEN
               R = OBSUSE(2*L)
               Y = ABS( R * SIN( THETA*RAD ) )
               IF ( Y .LT. DENC )  OBSUSE(2*L) = RAYO(L)
            ELSEIF ( THETA .GT. ARET1F ) THEN
               R = OBSUSE(2*L)
               Y = ABS( R * SIN( THETA*RAD ) )
               IF ( Y .LT. DENC )  OBSUSE(2*L) = RAYO(L)
            ENDIF
 210     CONTINUE
      ENDIF
C
 1000 FORMAT('==> IMPRESSION DE PARAMETRES "OBST" PAR SECTEUR USE:',/,
     &       ' SECTEUR   TYPE     ANGL_DEBUT      ANGL_FIN',
     &       '      ANGLE_MAX      DELTA_ANGL     PROFONDEUR')
 1010 FORMAT(1P,4X,I2,5X,A4,5(3X,E12.5))
C
      END
