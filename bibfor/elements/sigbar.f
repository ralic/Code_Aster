      SUBROUTINE SIGBAR ( SIGMA , BARSIG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/11/98   AUTEUR D6BHHMA M.ALMIKDAD 
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
C ......................................................................
C     FONCTION  :  CALCUL DE 
C
C                         (   SIGMA  0       0     )
C      BARS   ( 9 , 9 ) = (   0      SIGMA   0     )
C                         (   0      0       SIGMA )
C
C      AVEC
C
C      SIGMA  ( 3 , 3 ) 
C
C                  POUR LA PARTIE GEOMETRIQUE DE LA MATRICE TANGENTE
C                  COQUE_3D
C
C ......................................................................
C
      INTEGER I  
C
      INTEGER II ,JJ
C
      REAL * 8 SIGMA  ( 3 , 3 )
C
      REAL * 8 BARSIG ( 9 , 9 )
C
C DEB
C
            CALL R8INIR ( 9 * 9 , 0.D0 , BARSIG , 1 )
C
            DO 200 I = 1 , 3
               DO 210 JJ = 1 , 3
                  DO 220 II = 1 , 3
                  BARSIG ( ( I - 1 ) * 3 + II , ( I - 1 ) * 3 + JJ ) =
     &            SIGMA  (                 II ,                 JJ )
 220              CONTINUE
 210           CONTINUE
 200        CONTINUE
C
C FIN
C
      END
