      SUBROUTINE USOBCE ( DIMOBS, OBSUSE, RCARTE, NOMT19, NBUSUR,
     &                    PARUSU, TYPUSU )
      IMPLICIT   NONE
      INTEGER             DIMOBS, NBUSUR, TYPUSU(*)
      REAL*8              OBSUSE(*), RCARTE, PARUSU(20,*)
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
C     GUIDAGE CERCLE
C ----------------------------------------------------------------------
      INTEGER      I, L, IFM, NIV,J
      REAL*8       THETA, TABR(4), DELTAN, AI1, AI2, BI1, BI2, P, PM
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
     +                    PARUSU(I,3), PARUSU(I,2), DELTAN, PARUSU(I,4)
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
         AI1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
         BI1 = AI1*PARUSU(I,1)
         AI2 = PARUSU(I,4) / ( PARUSU(I,3) - PARUSU(I,2) )
         BI2 = AI2*PARUSU(I,2)
         PM  = 0.9D0 * ( AI1*PARUSU(I,2) - BI1 )
C
         L = 0
 40      CONTINUE
         L = L + 1
         IF ( OBSUSE(2*L-1) .LT. PARUSU(I,1) ) GOTO 40
         THETA = OBSUSE(2*L-1)
 42      CONTINUE
C ------ CAS OU ANGDEB EST NEGATIF
         IF ( PARUSU(I,1) .LT. 0 ) THEN
C
C --------- ON REDEFINIT LES ANGLES A 360 DEGRES
            PARUSU(I,1) = PARUSU(I,1) + 360.D0
            PARUSU(I,2) = PARUSU(I,2) + 360.D0
            BI1 = AI1 * PARUSU(I,1)
C --------- ON RECHERCHE LE THETA JUSTE AU DESSUS DE ANGDEB
            J = DIMOBS
 44         CONTINUE
            J = J - 1
            IF ( OBSUSE(2*J-1) .GT.PARUSU(I,1) ) GOTO 44
            J = J + 1
            THETA = OBSUSE(2*J-1)
C
 43         CONTINUE
            P = AI1*THETA - BI1    
            P = MIN ( P , PM ) 
            OBSUSE(2*J) = OBSUSE(2*J) + P 
            J = J + 1
            THETA = OBSUSE(2*J-1)
            IF ( J .GT. DIMOBS ) THEN
C ------------ ON  RECUPERE LES ANGLES D ORIGINE
               PARUSU(I,1) = PARUSU(I,1) - 360.D0
               PARUSU(I,2) = PARUSU(I,2) - 360.D0
               BI1 = AI1 * PARUSU(I,1)
C
               J = 0
 45            CONTINUE
               J = J + 1
               IF (OBSUSE(2*J-1).LT.PARUSU(I,2)) THEN
                  THETA =OBSUSE (2*J-1)        
                  P = AI1*THETA - BI1    
                  P = MIN ( P , PM ) 
                  OBSUSE(2*J) = OBSUSE(2*J) + P
                  GOTO 45
               ELSEIF (OBSUSE(2*J-1).LT.PARUSU(I,3)) THEN
                 THETA = OBSUSE(2*J-1)
                 P = PARUSU(I,4) + BI2 - AI2*THETA
                 P = MIN ( P , PM ) 
                 OBSUSE(2*J) = OBSUSE(2*J) + P 
                 GOTO 45
               ELSE
                 GOTO 150
               ENDIF
            ENDIF
            GOTO 43
         ELSE
C --------- SI ANGMAX EST DANS LE DERNIER SECTEUR ON 
C                     VERIFIE QUE ANGFIN NE SOIT PAS REPASSER PAR 0.
            IF ( PARUSU(I,3) .GT. 360.D0 ) THEN 
C ------------ ON RECHERCHE LE THETA JUSTE AU DESSUS DE ANGDEB
               J = DIMOBS
 49            CONTINUE
               J = J - 1
               IF ( OBSUSE(2*J-1) .GT. PARUSU(I,1) ) GOTO 49
               J = J + 1
               THETA =OBSUSE (2*J-1)
C
 50            CONTINUE          
               IF ( THETA .LT. PARUSU(I,2) ) THEN
                  P = AI1*THETA - BI1
               ELSE
                  P = PARUSU(I,4) + BI2 - AI2*THETA
               ENDIF
               P = MIN ( P , PM )   
               OBSUSE(2*J)=  OBSUSE(2*J) + P
               J = J + 1
               THETA = OBSUSE(2*J-1)
C ------------ SI THETA SUPERIEUR A 360 ON REMET LES COMPTEURS 
C                     A ZERO ET THETA VAUT THETA PLUS 360
C                     POUR ETRE DU MEME ORDRE DE GRANDEUR QUE ANGFIN
               IF ( J .GT. DIMOBS ) THEN
                  J = 0
 55               CONTINUE
                  J = J + 1
                   THETA = OBSUSE(2*J-1) + 360.D0
                  P = PARUSU(I,4) + BI2 - AI2*THETA
                  P = MIN ( P , PM ) 
                  OBSUSE(2*J) = OBSUSE(2*J) + P 
                  IF ( THETA .GT. PARUSU(I,3) ) GOTO 150
                  GOTO 55
               ENDIF 
               GOTO 50
            ELSE
C
C ------------ CAS GENERAL OU TOUT CE PASSE BIEN 
               IF ( THETA .LT. PARUSU(I,2) ) THEN
                  P = AI1*THETA - BI1
               ELSE
                  P = PARUSU(I,4) + BI2 - AI2*THETA
               ENDIF
               P = MIN ( P , PM ) 
               OBSUSE(2*L) = OBSUSE(2*L) + P
               L = L + 1
               IF ( L .GT. 720 ) GOTO 150
               THETA = OBSUSE(2*L-1)
               IF ( THETA .GT. PARUSU(I,3) ) GOTO 150
               GOTO 42
            ENDIF
         ENDIF
 150  CONTINUE
C
 1000 FORMAT('==> IMPRESSION DE PARAMETRES "OBST" PAR SECTEUR USE:',/,
     +       ' SECTEUR   TYPE     ANGL_DEBUT      ANGL_FIN',
     +       '      ANGLE_MAX      DELTA_ANGL     PROFONDEUR')
 1010 FORMAT(1P,4X,I2,5X,A4,5(3X,E12.5))
C
      END
