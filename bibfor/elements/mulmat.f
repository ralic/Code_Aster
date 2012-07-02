      SUBROUTINE MULMAT(K,N,M,A,B,C)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
C-------------------------------------------------------
      IMPLICIT NONE
      REAL*8 A(K,N),B(N,M),C(K,M)
C-----------------------------------------------------------------------
      INTEGER IA ,IB ,K ,K1 ,M ,M1 ,N 
      INTEGER N1 
C-----------------------------------------------------------------------
      DO 20 IB = 1,M
        DO 10 IA = 1,K
          C(IA,IB) = 0.D0
   10   CONTINUE
   20 CONTINUE
      DO 50 K1 = 1,K
        DO 40 N1 = 1,N
          DO 30 M1 = 1,M
            C(K1,M1) = C(K1,M1) + A(K1,N1)*B(N1,M1)
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE
      END
