      SUBROUTINE MULTSY(U22,A3,V22,MSYM)

       IMPLICIT  NONE
       
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/09/2006   AUTEUR MARKOVIC D.MARKOVIC 
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
      REAL*8      MSYM(*)

      REAL*8      U22(2,*),   V22(2,*),
     &            A3(*),      A22(2,2),   MSYM22(2,2)
     
      REAL*8      CP(2,2)
C Construction of the symetric matrix A
      A22(1,1) = A3(1)
      A22(2,2) = A3(2)
      A22(1,2) = A3(3)
      A22(2,1) = A3(3)

C Multiplication: MSYM = U22 * A * V22
       CALL MATMUL(A22,V22,2,2,2,CP)
       CALL MATMUL(U22,CP,2,2,2,MSYM22)
C Construction of the rank-one result matrix
      MSYM(1) = MSYM22(1,1)
      MSYM(2) = MSYM22(2,2)
      MSYM(3) = MSYM22(1,2)

      END 
