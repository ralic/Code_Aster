      SUBROUTINE INICES(VALCEN,VALFAC,MAXFA)
      IMPLICIT NONE

C ======================================================================
C CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C ======================================================================
      INTEGER      MAXFA
      REAL*8       VALCEN(14,6)
      REAL*8       VALFAC(MAXFA,14,6)
      INTEGER      I,J,K
      REAL*8       R8MAEM
      DO 10 I=1,14
         DO 11 J = 1 , 6
            VALCEN(I,J)= R8MAEM()
            DO 12 K=1,MAXFA 
               VALFAC(K,I,J)= R8MAEM()
   12       CONTINUE                   
   11    CONTINUE
   10 CONTINUE      
      END
