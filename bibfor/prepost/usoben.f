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
C MODIF PREPOST  DATE 15/01/2002   AUTEUR CIBHHLV L.VIVAN 
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
      REAL*8       THETA, DELTAN, AI1, AI2, BI1, BI2, P, R, Y, PM
      REAL*8       RAD, R8PREM, R8DGRD, TABR(4), ARET1F, ARET2F
      COMPLEX*16   C16B
      CHARACTER*4  T2
      CHARACTER*8  TABK(2)
      CHARACTER*16 NOPARA(7)
C
      DATA NOPARA / 'LIEU'    , 'SECTEUR' , 'TYPE'    , 'ANGL_DEBUT', 
     +              'ANGL_FIN', 'ANGL_MAX', 'PROF_MAX' /
C-----------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
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
     +                    PARUSU(I,3), PARUSU(I,2), DELTAN, PARUSU(I,4)
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
         IF ( ABS(PARUSU(I,2)-PARUSU(I,3)) .LE. R8PREM() )  THEN
            CALL UTDEBM('A','USOBEN','VERIFIER LES PARAMETRES ')
            CALL UTIMPI('S','D''USURE POUR LE SECTEUR ',1,I)
            CALL UTFINM()
            GOTO 100
         ENDIF
C
         IF ( ABS(PARUSU(I,2)-PARUSU(I,1)) .LE. R8PREM() )  THEN
            CALL UTDEBM('A','USOBEN','VERIFIER LES PARAMETRES ')
            CALL UTIMPI('S','D''USURE POUR LE SECTEUR ',1,I)
            CALL UTFINM()
            GOTO 100
         ENDIF
C
C ------ VE + LUNULE SUR LA PREMIERE ENCOCHE
         IF ( TYPUSU(I) .EQ. 2 )  THEN
            IF ( I .EQ. 2 )  THEN
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
               BI1 = AI1*PARUSU(I,3)
               L = 0
 102           CONTINUE
               L = L + 1
               IF ( OBSUSE(2*L-1) .LT. 20.D0 ) GOTO 102
CCC               IF ( OBSUSE(2*L-1) .LT. PARUSU(I,1) ) GOTO 102
               THETA = OBSUSE(2*L-1)
 104           CONTINUE
               P = AI1*THETA - BI1
               OBSUSE(2*L) = OBSUSE(2*L) + P
               L = L + 1
               THETA = OBSUSE(2*L-1)
               IF ( THETA .GT. PARUSU(I,3) ) GOTO 100
               GOTO 104
            ELSE
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
               BI1 = AI1*PARUSU(I,1)
               L = 0
 106           CONTINUE
               L = L + 1
               IF ( OBSUSE(2*L-1) .LT. PARUSU(I,1) ) GOTO 106
               THETA = OBSUSE(2*L-1)
 108           CONTINUE
               P = AI1*THETA - BI1
               OBSUSE(2*L) = OBSUSE(2*L) + P
               L = L + 1
               THETA = OBSUSE(2*L-1)
               IF ( THETA .GT. 340.D0 ) GOTO 100
CCC               IF ( THETA .GT. PARUSU(I,3) ) GOTO 100
               GOTO 108
            ENDIF
C
C ------ VE SUR LA PREMIERE ENCOCHE
         ELSEIF ( TYPUSU(I) .EQ. 3 )  THEN
            IF ( I .EQ. 1 )  THEN
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
               BI1 = AI1*PARUSU(I,3)
            ELSE
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
               BI1 = AI1*PARUSU(I,1)
            ENDIF
            L = 0
 110        CONTINUE
            L = L + 1
            IF ( OBSUSE(2*L-1) .LT. PARUSU(I,1) ) GOTO 110
            THETA = OBSUSE(2*L-1)
 112        CONTINUE
            P = AI1*THETA - BI1
            OBSUSE(2*L) = OBSUSE(2*L) + P
            L = L + 1
            THETA = OBSUSE(2*L-1)
            IF ( THETA .GT. PARUSU(I,3) ) GOTO 100
            GOTO 112
C
C ------ VE + LUNULE SUR LA DEUXIEME ENCOCHE
         ELSEIF ( TYPUSU(I) .EQ. 4 )  THEN
            IF ( NBSECT.EQ.12 .AND. I.EQ.8 )  THEN
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
               BI1 = AI1*PARUSU(I,3)
            ELSEIF ( NBSECT.EQ.10 .AND. I.EQ.7 )  THEN
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
               BI1 = AI1*PARUSU(I,3)
            ELSE
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
               BI1 = AI1*PARUSU(I,1)
            ENDIF
            L = 0
 120        CONTINUE
            L = L + 1
            IF ( OBSUSE(2*L-1) .LT. PARUSU(I,1) ) GOTO 120
            THETA = OBSUSE(2*L-1)
 122        CONTINUE
            P = AI1*THETA - BI1
            OBSUSE(2*L) = OBSUSE(2*L) + P
            L = L + 1
            THETA = OBSUSE(2*L-1)
            IF ( THETA .GT. PARUSU(I,3) ) GOTO 100
            GOTO 122
C
C ------ VE SUR LA DEUXIEME ENCOCHE
         ELSEIF ( TYPUSU(I) .EQ. 5 )  THEN
            IF ( NBSECT.EQ.12 .AND. I.EQ.7 )  THEN
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
               BI1 = AI1*PARUSU(I,3)
            ELSEIF ( NBSECT.EQ.10 .AND. I.EQ.6 )  THEN
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
               BI1 = AI1*PARUSU(I,3)
            ELSE
               AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
               BI1 = AI1*PARUSU(I,1)
            ENDIF
            L = 0
 130        CONTINUE
            L = L + 1
            IF ( OBSUSE(2*L-1) .LT. PARUSU(I,1) ) GOTO 130
            THETA = OBSUSE(2*L-1)
 132        CONTINUE
            P = AI1*THETA - BI1
            OBSUSE(2*L) = OBSUSE(2*L) + P
            L = L + 1
            THETA = OBSUSE(2*L-1)
            IF ( THETA .GT. PARUSU(I,3) ) GOTO 100
            GOTO 132
C
C ------ LUNULE
         ELSE
            AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
            BI1 = AI1*PARUSU(I,1)
            AI2 = PARUSU(I,4) / ( PARUSU(I,3) - PARUSU(I,2) )
            BI2 = AI2*PARUSU(I,2)
            PM  = 0.9D0 * ( AI1*PARUSU(I,2) - BI1 )
C
            L = 0
 140        CONTINUE
            L = L + 1
            IF ( OBSUSE(2*L-1) .LT. PARUSU(I,1) ) GOTO 140
            THETA = OBSUSE(2*L-1)
 142        CONTINUE
            IF ( THETA .LT. PARUSU(I,2) ) THEN
               P = AI1*THETA - BI1
            ELSE
               P = PARUSU(I,4) + BI2 - AI2*THETA
            ENDIF
            P = MIN ( P , PM ) 
            OBSUSE(2*L) = OBSUSE(2*L) + P
            L = L + 1
            THETA = OBSUSE(2*L-1)
            IF ( THETA .GT. PARUSU(I,3) ) GOTO 100
            GOTO 142
         ENDIF
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
     +       ' SECTEUR   TYPE     ANGL_DEBUT      ANGL_FIN',
     +       '      ANGLE_MAX      DELTA_ANGL     PROFONDEUR')
 1010 FORMAT(1P,4X,I2,5X,A4,5(3X,E12.5))
C
      END
