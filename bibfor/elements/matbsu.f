      SUBROUTINE MATBSU ( NB1   , XR         , NPGSR , INTSN ,
     &                    B1MNC , B2MNC      , B1MNI , B2MNI ,
     &                    B1MRI , B2MRI      , B1SRC , B2SRC ,
     &                    B1SU  , B2SU  )
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
      INTEGER NB1 
C
      INTEGER INTSN
      INTEGER NPGSR
C
      INTEGER I   , J , K , L
      INTEGER I1  
C
      REAL * 8 XR ( * )
C
      REAL * 8 B1SU   ( 5 , 51 ) 
      REAL * 8 B2SU   ( 5 , 51 ) 
C
      REAL * 8 B1MNC  ( 3 , 51 ) 
      REAL * 8 B2MNC  ( 3 , 51 ) 
C
      REAL * 8 B1MNI  ( 3 , 51 ) 
      REAL * 8 B2MNI  ( 3 , 51 ) 
C
      REAL * 8 B1MRI  ( 3 , 51 , 4 ) 
      REAL * 8 B2MRI  ( 3 , 51 , 4 ) 
C
C
      REAL * 8 B1SRC  ( 2 , 51 , 4 ) 
      REAL * 8 B2SRC  ( 2 , 51 , 4 ) 
C
C
CDEB
C
C---- INITIALISATION
C
      CALL R8INIR ( 5 * 51 , 0.D0 , B1SU , 1 )
C
      CALL R8INIR ( 5 * 51 , 0.D0 , B2SU , 1 )
C
C---- ADRESSES POUR EXTRAPOLATION DES B
C
      L  = 702
C
      I1 = L + 4 * ( INTSN - 1 )
C
C---- REMPLISSAGE
C
      DO 100 J = 1 , 6 * NB1 + 3
         DO 110 I = 1 , 3
C
C---------- OPERATEURS DE FLEXION
C
            B1SU ( I , J ) = B1MNC ( I , J ) - B1MNI ( I , J )
            B2SU ( I , J ) = B2MNC ( I , J ) - B2MNI ( I , J )
C
C---------- EXTRAPOLATION
C
            DO 120 K = 1 , NPGSR
C
C---------- OPERATEURS DE MEMBRANE
C
               B1SU ( I     , J ) = B1SU ( I     , J )   
     &                         + XR ( I1 + K ) * B1MRI ( I , J , K )
C
               B2SU ( I     , J ) = B2SU ( I     , J )   
     &                         + XR ( I1 + K ) * B2MRI ( I , J , K )
C
                  IF ( I . LE . 2 ) THEN
C
C------------------- OPERATEURS DE SHEAR
C
               B1SU ( I + 3 , J ) = B1SU ( I + 3 , J ) 
     &                         + XR ( I1 + K ) * B1SRC ( I , J , K ) 
C
               B2SU ( I + 3 , J ) = B2SU ( I + 3 , J ) 
     &                         + XR ( I1 + K ) * B2SRC ( I , J , K ) 
C
                  ENDIF 
C
 120        CONTINUE
 110     CONTINUE
 100  CONTINUE
C
C
C
C
CFIN
C
      END
