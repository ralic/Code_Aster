      SUBROUTINE LCMHDD (NBSYS, NBCOEF, COEFH, HSR)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/07/2010   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE PROIX J-M.PROIX
C     ----------------------------------------------------------------
C     MONOCRISTAL : CALCUL DE LA MATRICE D'INTERACTION HSR POUR DD_CFC
C     ----------------------------------------------------------------
C     IN  NBSYS  :  NOMBRE DE SYSTEMES DE GLISSEMENT
C         NBCOEF  :  NOMBRE DE COEFFICIENTS
C         COEFH  :  COEFFICIENTS H1 A H6
C     OUT HSR    :  MATRICE D'INTERACTION
C     ----------------------------------------------------------------
      INTEGER NBCOEF,NBSYS,IR,IS,I,J,NN(12)
      REAL*8  COEFH(6),HSR(24,24),HGM(12,12), R8VIDE
      REAL*8  A1(3,3),A2(3,3),A3(3,3),A4(3,3),A0(3,3)
      REAL*8  AETOIL,ACOLIN,AGLISS,ALOMER,AHIRTH
      DATA NN/7,9,8,2,1,3,12,11,10,5,4,6/
C     ----------------------------------------------------------------
      CALL ASSERT(NBCOEF.EQ.5)
             
C  MATRICE D INTERACTION (12*12): 5 COEFFICIENTS DD_CFC
C  DEFINITION SELON G.MONET   
      IF (NBSYS.NE.12) CALL U2MESS('F','COMPOR1_24')
      AETOIL=COEFH(1)
      ACOLIN=COEFH(2)
      AGLISS=COEFH(3)
      ALOMER=COEFH(4)
      AHIRTH=COEFH(5)

      CALL R8INIR ( 3*3, AETOIL , A0, 1 )                    
      CALL LCICMA(A0,3,3,3,3,1,1,HGM,12,12,1,1)
      CALL LCICMA(A0,3,3,3,3,1,1,HGM,12,12,4,4)
      CALL LCICMA(A0,3,3,3,3,1,1,HGM,12,12,7,7)
      CALL LCICMA(A0,3,3,3,3,1,1,HGM,12,12,10,10)

      A1(1,1)=ACOLIN
      A1(1,2)=AGLISS
      A1(1,3)=AGLISS
      A1(2,1)=AGLISS
      A1(2,2)=AHIRTH
      A1(2,3)=ALOMER
      A1(3,1)=AGLISS
      A1(3,2)=ALOMER
      A1(3,3)=AHIRTH
      CALL LCICMA(A1,3,3,3,3,1,1,HGM,12,12,4,1)   
      CALL LCICMA(A1,3,3,3,3,1,1,HGM,12,12,1,4)   
      CALL LCICMA(A1,3,3,3,3,1,1,HGM,12,12,10,7)   
      CALL LCICMA(A1,3,3,3,3,1,1,HGM,12,12,7,10)   
              
      A2(1,1)=AHIRTH
      A2(1,2)=AGLISS
      A2(1,3)=ALOMER
      A2(2,1)=AGLISS
      A2(2,2)=ACOLIN
      A2(2,3)=AGLISS
      A2(3,1)=ALOMER
      A2(3,2)=AGLISS
      A2(3,3)=AHIRTH
      CALL LCICMA(A2,3,3,3,3,1,1,HGM,12,12,7,1)   
      CALL LCICMA(A2,3,3,3,3,1,1,HGM,12,12,1,7)   
      CALL LCICMA(A2,3,3,3,3,1,1,HGM,12,12,10,4)   
      CALL LCICMA(A2,3,3,3,3,1,1,HGM,12,12,4,10)   
              
      A3(1,1)=AHIRTH
      A3(1,2)=ALOMER
      A3(1,3)=AGLISS
      A3(2,1)=ALOMER
      A3(2,2)=AHIRTH
      A3(2,3)=AGLISS
      A3(3,1)=AGLISS
      A3(3,2)=AGLISS
      A3(3,3)=ACOLIN
      CALL LCICMA(A3,3,3,3,3,1,1,HGM,12,12,7,4)   
      CALL LCICMA(A3,3,3,3,3,1,1,HGM,12,12,4,7)   
      CALL LCICMA(A3,3,3,3,3,1,1,HGM,12,12,10,1)   
      CALL LCICMA(A3,3,3,3,3,1,1,HGM,12,12,1,10)   
      
      DO 10 I=1,12
      DO 10 J=1,12
         HSR(NN(I),NN(J))=HGM(I,J)
 10   CONTINUE
      
      END
