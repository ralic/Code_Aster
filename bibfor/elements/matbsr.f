      SUBROUTINE MATBSR ( NB1    , VECTT  , DUDXRC , INTSR , 
     &                    JDN1RC , JDN2RC , 
     &                    B1SRC  , B2SRC  )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/01/99   AUTEUR D6BHHMA M.ALMIKDAD 
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
C
      IMPLICIT NONE
C
      INTEGER I , J
C
      INTEGER NB1 
C
      INTEGER INTSR
C
      REAL * 8 VECTT  ( 3 , 3 ) 
C
      REAL * 8 DUDXRC ( 9 )
C
      REAL * 8 JDN1RC ( 9 , 51 )
      REAL * 8 JDN2RC ( 9 , 51 )
C
      REAL * 8 B1SRC  ( 2 , 51 , 4 ) 
      REAL * 8 B2SRC  ( 2 , 51 , 4 ) 
C
      REAL * 8 TMP  ( 2 , 51 )
C
      REAL * 8 HSS1 ( 2 , 9 )
C
      REAL * 8 HSS2 ( 2 , 9 )
C
CDEB
C
      CALL HSASH ( VECTT , DUDXRC , HSS1 , HSS2 )
C
C
C --- POUR LA DEFORMATION TOTALE   B1SRC
C
C---- INITIALISATION
C
      CALL R8INIR ( 2 * 51 , 0.D0 , TMP , 1 )
C
      CALL PROMAT ( HSS1   , 2 , 2 , 9           ,
     &              JDN1RC , 9 , 9 , 6 * NB1 + 3 ,
     &              TMP  )
C
C
      DO 100 J = 1 , 6 * NB1 + 3
         DO 110 I = 1 , 2
C
            B1SRC ( I , J , INTSR ) = TMP ( I , J )
C
 110     CONTINUE
 100  CONTINUE
C
C
C---- POUR LA DEFORMATION DIFFERENTIELLE   B2SRC
C
C---- INITIALISATION
C
      CALL R8INIR ( 2 * 51 , 0.D0 , TMP , 1 )
C
C
C
C
      CALL PROMAT ( HSS2   , 2 , 2 , 9           ,
     &              JDN2RC , 9 , 9 , 6 * NB1 + 3 ,
     &              TMP  )
C
C
      DO 200 J = 1 , 6 * NB1 + 3
         DO 210 I = 1 , 2
C
            B2SRC ( I , J , INTSR ) = TMP ( I , J )
C
 210     CONTINUE
 200  CONTINUE
C
C
CFIN
C
      END
