      SUBROUTINE HSACO ( VECTT , DUDXNC , HSC )
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
      REAL * 8 VECTT ( 3 , 3 )
C
      REAL * 8 DUDXNC ( 9 )
C
      REAL * 8 HSM1 ( 3 , 9 )
C
      REAL * 8 HSM2 ( 3 , 9 )
C
      REAL * 8 HSS1 ( 2 , 9 )
C
      REAL * 8 HSS2 ( 2 , 9 )
C
      REAL * 8 HSC ( 5 , 9 )
C
      INTEGER I , J
C
CDEB
C
C---- MEMBRANE  ( H ) * ( ( S ) + ( A ) )
C
      CALL HSAME ( VECTT , DUDXNC , HSM1 , HSM2 )
C
C---- SHEAR     ( H ) * ( ( S ) + ( A ) )
C
      CALL HSASH ( VECTT , DUDXNC , HSS1 , HSS2 )
C
C                                    (  HSM2 ( 3 , 9 )  )
C---- REMLISSAGE DE  HSC ( 5 , 9 ) = (------------------)
C                                    (  HSS2  ( 2 , 9 )  )
C
C                 HSC ( 5 , 9 ) = H ( 5 , 6 ) * S ( 6, 9 )
C
      DO 100 J = 1 , 9
C
         DO 110 I = 1 , 3
                           HSC ( I     , J ) = HSM2 ( I , J )
         IF ( I . LE . 2 ) HSC ( I + 3 , J ) = HSS2 ( I , J )
C
 110     CONTINUE
C
 100  CONTINUE
C
C
C
CFIN
C
      END
