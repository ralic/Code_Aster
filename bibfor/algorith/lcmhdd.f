      SUBROUTINE LCMHDD (NECOUL,NECRIS,NBSYS, NBCOEF, COEFH,NSG, HSR)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/10/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER NBCOEF,NBSYS,I,J,NN(12),IDBG,NSG
      REAL*8  COEFH(6),HSR(NSG,NSG),HGM(12,12)
      REAL*8  A1(3,3),A2(3,3),A3(3,3),A4(3,3),A0(3,3),A5(3,3),A6(3,3)
      REAL*8  AETOIL,ACOLIN,AGLISS,ALOMER,AHIRTH,C0,C1,C2,C3,C4,C5
      CHARACTER*16  NECRIS,NECOUL
      DATA NN/7,9,8,2,1,3,12,11,10,5,4,6/
C     ----------------------------------------------------------------
      IDBG=0
      IF (NECRIS.EQ.'MONO_DD_CFC') THEN 
             
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
 10     CONTINUE

      ELSEIF (NECRIS.EQ.'MONO_DD_CC') THEN 

             
C  MATRICE D INTERACTION (12*12): 5 COEFFICIENTS DD_CFC
C  DEFINITION SELON G.MONET   
      IF (NBSYS.NE.12) CALL U2MESS('F','COMPOR1_24')
        C0=COEFH(1)
        C1=COEFH(2)
        C2=COEFH(3)
        C3=COEFH(4)
        C4=COEFH(5)
        C5=COEFH(6)
         
        CALL R8INIR ( 3*3, C1 , A0, 1 )
        DO 12 I=1,3
           A0(I,I)=C0
 12     CONTINUE                    
        CALL LCICMA(A0,3,3,3,3,1,1,HGM,12,12,1,1)
        CALL LCICMA(A0,3,3,3,3,1,1,HGM,12,12,4,4)
        CALL LCICMA(A0,3,3,3,3,1,1,HGM,12,12,7,7)
        CALL LCICMA(A0,3,3,3,3,1,1,HGM,12,12,10,10)
        
        A1(1,1)=C4
        A1(1,2)=C3
        A1(1,3)=C2
        A1(2,1)=C3
        A1(2,2)=C5
        A1(2,3)=C3
        A1(3,1)=C2
        A1(3,2)=C3
        A1(3,3)=C4
        CALL LCICMA(A1,3,3,3,3,1,1,HGM,12,12,10,7)   
        CALL LCICMA(A1,3,3,3,3,1,1,HGM,12,12,7,10)   
                
        A2(1,1)=C4
        A2(1,2)=C2
        A2(1,3)=C3
        A2(2,1)=C2
        A2(2,2)=C4
        A2(2,3)=C3
        A2(3,1)=C3
        A2(3,2)=C3
        A2(3,3)=C5
        CALL LCICMA(A2,3,3,3,3,1,1,HGM,12,12,10,1)   
        CALL LCICMA(A2,3,3,3,3,1,1,HGM,12,12,1,10)   
                
        A3(1,1)=C5
        A3(1,2)=C3
        A3(1,3)=C3
        A3(2,1)=C3
        A3(2,2)=C4
        A3(2,3)=C2
        A3(3,1)=C3
        A3(3,2)=C2
        A3(3,3)=C4
        CALL LCICMA(A3,3,3,3,3,1,1,HGM,12,12,7,1)   
        CALL LCICMA(A3,3,3,3,3,1,1,HGM,12,12,1,7)   
        
        
        A4(1,1)=C2
        A4(1,2)=C3
        A4(1,3)=C4
        A4(2,1)=C3
        A4(2,2)=C5
        A4(2,3)=C3
        A4(3,1)=C4
        A4(3,2)=C3
        A4(3,3)=C2
        CALL LCICMA(A4,3,3,3,3,1,1,HGM,12,12,1,4)   
        CALL LCICMA(A4,3,3,3,3,1,1,HGM,12,12,4,1)   
                
        A5(1,1)=C3
        A5(1,2)=C3
        A5(1,3)=C5
        A5(2,1)=C2
        A5(2,2)=C4
        A5(2,3)=C3
        A5(3,1)=C4
        A5(3,2)=C2
        A5(3,3)=C3
        CALL LCICMA(A5,3,3,3,3,1,1,HGM,12,12,10,4)   
        CALL LCICMA(A5,3,3,3,3,1,1,HGM,12,12,4,10)   
                
        A6(1,1)=C3
        A6(1,2)=C2
        A6(1,3)=C4
        A6(2,1)=C3
        A6(2,2)=C4
        A6(2,3)=C2
        A6(3,1)=C5
        A6(3,2)=C3
        A6(3,3)=C3
        CALL LCICMA(A6,3,3,3,3,1,1,HGM,12,12,7,4)   
        CALL LCICMA(A6,3,3,3,3,1,1,HGM,12,12,4,7)   
        
        DO 11 I=1,12
        DO 11 J=1,12
           HSR(I,J)=HGM(I,J)
 11     CONTINUE
        IF (IDBG.EQ.1) THEN
           WRITE(6,*) 'MATRICE D INTERACTION POUR',NECRIS
           DO 13 I=1,12
             WRITE(6,'(12(1X,E11.4))') (HGM(I,J),J=1,12)
 13        CONTINUE
        ENDIF

      ENDIF

      
      END
