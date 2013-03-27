      SUBROUTINE MASSTG(MATIN,MATOUT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/03/2013   AUTEUR CHEIGNON E.CHEIGNON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE
C -----------------------------------------------------------
C ---  BUT : MISE SOUS LA FORME DES MATRICES DE MASSE DES
C            POU_D_TG(M) DE LA MATRICE ISSUE DE PTMA01
C -----------------------------------------------------------
      REAL*8 MATIN(105),MATOUT(105)

      INTEGER I

      DO 100 I = 1 , 21
            MATOUT(I) = MATIN(I)
100      CONTINUE
         DO 102 I = 22 , 28
            MATOUT(I) = 0.D0
102      CONTINUE
         DO 104 I = 29 , 34
            MATOUT(I) = MATIN(I-7)
104      CONTINUE
         MATOUT(35) = 0.D0
         DO 106 I = 36 , 42
            MATOUT(I) = MATIN(I-8)
106      CONTINUE
         MATOUT(43) = 0.D0
         DO 108 I = 44 , 51
            MATOUT(I) = MATIN(I-9)
108      CONTINUE
         MATOUT(52) = 0.D0
         DO 110 I = 53 , 61
            MATOUT(I) = MATIN(I-10)
110      CONTINUE
         MATOUT(62) = 0.D0
         DO 112 I = 63 , 72
            MATOUT(I) = MATIN(I-11)
112      CONTINUE
         MATOUT(73) = 0.D0
         DO 114 I = 74 , 84
            MATOUT(I) = MATIN(I-12)
114      CONTINUE
         MATOUT(85) = 0.D0
         DO 116 I = 86 , 91
            MATOUT(I) = MATIN(I-13)
116      CONTINUE
         DO 118 I = 92 , 105
            MATOUT(I) = 0.D0
118      CONTINUE

      END
