      SUBROUTINE MMMFFM(ALIAS ,KSI1  ,KSI2   ,TYPBAR,FF    ,
     &                  DFF   )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 01/04/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8 ALIAS 
      REAL*8      KSI1
      REAL*8      KSI2   
      INTEGER     TYPBAR
      REAL*8      FF(9)
      REAL*8      DFF(2,9)                  
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C MODIFICATION DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT 
C DE L'ELEMENT DE REFERENCE
C
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  KSI1   : POINT DE CONTACT SUIVANT KSI1
C IN  KSI2   : POINT DE CONTACT SUIVANT KSI2
C IN  TYPBAR : INDICATEUR D'ELEMENT DE BARSOUM
C I/O FF     : FONCTIONS DE FORMES EN KSI1,KSI2
C I/O DFF    : DERIVEES DES FONCTIONS DE FORMES EN KSI1,KSI2
C                DFF(I,J): DERIVEE PAR RAPPORT A KSI_I DU NOEUD_J
C
C ----------------------------------------------------------------------
C
C
C --- MODIFICATIONS DES DERIVEES PREMIERES ET SECONDES
C
      IF (ALIAS(1:3).EQ.'SG3') THEN
        IF (TYPBAR .EQ. 0) THEN
          FF(1) = FF(1) - ((0.01D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1))
          FF(2) = FF(2) - ((0.01D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1))
          FF(3) = FF(3) + ((0.02D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1))
        END IF  
        IF (TYPBAR .EQ. 0) THEN
          DFF(1,1) = DFF(1,1)-((0.01D-5)*(1.D0-3*(KSI1**2)))
          DFF(1,2) = DFF(1,2)-((0.01D-5)*(1.D0-3*(KSI1**2)))
          DFF(1,3) = DFF(1,3)+((0.02D-5)*(1.D0-3*(KSI1**2)))
        END IF        
      ELSE IF (ALIAS(1:3).EQ.'QU8') THEN
        IF (TYPBAR .EQ. 1 .OR. TYPBAR .EQ. 2) THEN
          FF(1) = FF(1) - ((0.01D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1))
          FF(4) = FF(4) - ((0.01D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1))
          FF(8) = FF(8) + ((0.02D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1))
          FF(2) = FF(2) - ((0.01D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1))   
          FF(3) = FF(3) - ((0.01D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1)) 
          FF(6) = FF(6) + ((0.02D-5)*KSI1*(1.D0+KSI1)*(1.D0-KSI1))  
        ELSEIF (TYPBAR .EQ. 3) THEN
          FF(1) = FF(1) - ((0.01D-5)*KSI2*(1.D0+KSI2)*(1.D0-KSI2))  
          FF(2) = FF(2) - ((0.01D-5)*KSI2*(1.D0+KSI2)*(1.D0-KSI2))  
          FF(3) = FF(3) - ((0.01D-5)*KSI2*(1.D0+KSI2)*(1.D0-KSI2))   
          FF(4) = FF(4) - ((0.01D-5)*KSI2*(1.D0+KSI2)*(1.D0-KSI2))   
          FF(5) = FF(5) + ((0.02D-5)*KSI2*(1.D0+KSI2)*(1.D0-KSI2)) 
          FF(7) = FF(7) + ((0.02D-5)*KSI2*(1.D0+KSI2)*(1.D0-KSI2))
        ENDIF   
        IF (TYPBAR .EQ. 1 .OR. TYPBAR .EQ. 2) THEN
          DFF(1,1) = DFF(1,1) - ((0.01D-5)*(1.D0-3*(KSI1**2)))    
          DFF(1,4) = DFF(1,4) - ((0.01D-5)*(1.D0-3*(KSI1**2))) 
          DFF(1,8) = DFF(1,8) + ((0.02D-5)*(1.D0-3*(KSI1**2))) 
          DFF(1,2) = DFF(1,2) - ((0.01D-5)*(1.D0-3*(KSI1**2)))     
          DFF(1,3) = DFF(1,3) - ((0.01D-5)*(1.D0-3*(KSI1**2)))    
          DFF(1,6) = DFF(1,6) + ((0.02D-5)*(1.D0-3*(KSI1**2))) 
        END IF        
        IF (TYPBAR .EQ. 3) THEN
          DFF(2,1) = DFF(2,1) - ((0.01D-5)*(1.D0-3*(KSI2**2)))    
          DFF(2,2) = DFF(2,2) - ((0.01D-5)*(1.D0-3*(KSI2**2)))     
          DFF(2,3) = DFF(2,3) - ((0.01D-5)*(1.D0-3*(KSI2**2)))    
          DFF(2,4) = DFF(2,4) - ((0.01D-5)*(1.D0-3*(KSI2**2)))    
          DFF(2,5) = DFF(2,5) + ((0.02D-5)*(1.D0-3*(KSI2**2))) 
          DFF(2,7) = DFF(2,7) + ((0.02D-5)*(1.D0-3*(KSI2**2)))
        END IF        
      ELSE 
        CALL ASSERT(.FALSE.)
      END IF
      END
