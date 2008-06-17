      SUBROUTINE CAL2FX(ALIAS ,KSI1  ,KSI2  ,DDFF  )
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
      REAL*8      KSI1,KSI2,DDFF(3,9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DES DERIVEEES SECONDES DES FONCTIONS DE FORME EN UN POINT DE 
C L'ELEMENT DE REFERENCE
C
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  KSI1   : POINT DE CONTACT SUIVANT KSI1 DES FONCTIONS DE FORME 
C IN  KSI2   : POINT DE CONTACT SUIVANT KSI2 DES FONCTIONS DE FORME 
C OUT DDFF   : DERIVEES SECONDES DES FONCTIONS DE FORMES EN KSI1,KSI2
C                DDFF(1,J): DERIVEE PAR RAPPORT A KSI1.KSI1 DU NOEUD_J
C                DDFF(2,J): DERIVEE PAR RAPPORT A KSI2.KSI2 DU NOEUD_J
C                DDFF(3,J): DERIVEE PAR RAPPORT A KSI1.KSI2 DU NOEUD_J
C
C ----------------------------------------------------------------------
C
      REAL*8  AL31,AL32,AL33
      REAL*8  DAL31,DAL32,DAL33      
      REAL*8  UNS4
      REAL*8  X,U
      INTEGER I
C
C ----------------------------------------------------------------------
C
      AL31(X) = 0.5D0*X* (X-1.D0)
      AL32(X) = - (X+1.D0)* (X-1.D0)
      AL33(X) = 0.5D0*X* (X+1.D0)
      DAL31(U) = 0.5D0* (2.D0*U-1.D0)
      DAL32(U) = -2.D0*U
      DAL33(U) = 0.5D0* (2.D0*U+1.D0)      
      UNS4    = 0.25D0
C
C ----------------------------------------------------------------------
C
      DO 10 I=1,9
        DDFF(1,I) = 0.D0  
        DDFF(2,I) = 0.D0  
        DDFF(3,I) = 0.D0  
 10   CONTINUE
C
      IF (ALIAS(1:3).EQ.'SG2') THEN
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 0.D0
      ELSE IF (ALIAS(1:3).EQ.'SG3') THEN    
        DDFF(1,1) = 1.D0
        DDFF(1,2) = 1.D0
        DDFF(1,3) = -2.D0
      ELSE IF (ALIAS(1:3).EQ.'TR3') THEN
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 0.D0
        DDFF(1,3) = 0.D0
        DDFF(2,1) = 0.D0
        DDFF(2,2) = 0.D0
        DDFF(2,3) = 0.D0
        DDFF(3,1) = 0.D0
        DDFF(3,2) = 0.D0
        DDFF(3,3) = 0.D0
      ELSE IF (ALIAS(1:3).EQ.'TR6') THEN
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 1.D0
        DDFF(1,3) = 1.D0
        DDFF(1,4) = 0.D0
        DDFF(1,5) = -2.D0
        DDFF(1,6) = 0.D0
        DDFF(2,1) = 1.D0
        DDFF(2,2) = 1.D0
        DDFF(2,3) = 0.D0
        DDFF(2,4) = -2.D0
        DDFF(2,5) = 0.D0
        DDFF(2,6) = 0.D0
        DDFF(3,1) = 0.D0
        DDFF(3,2) = 1.D0
        DDFF(3,3) = 0.D0
        DDFF(3,4) = -1.D0
        DDFF(3,5) = -1.D0
        DDFF(3,6) = 1.D0
      ELSE IF (ALIAS(1:3).EQ.'QU4') THEN
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 0.D0
        DDFF(1,3) = 0.D0
        DDFF(1,4) = 0.D0
        DDFF(2,1) = 0.D0
        DDFF(2,2) = 0.D0
        DDFF(2,3) = 0.D0
        DDFF(2,4) = 0.D0
        DDFF(3,1) = -UNS4
        DDFF(3,2) = UNS4
        DDFF(3,3) = -UNS4
        DDFF(3,4) = UNS4
       ELSE IF (ALIAS(1:3).EQ.'QU8') THEN
        DDFF(1,1) = 0.5D0* (1.D0+KSI2)
        DDFF(1,2) = 0.5D0* (1.D0-KSI2)
        DDFF(1,3) = 0.5D0* (1.D0-KSI2)
        DDFF(1,4) = 0.5D0* (1.D0+KSI2)
        DDFF(1,5) = 0.D0
        DDFF(1,6) = - (1.D0-KSI2)
        DDFF(1,7) = 0.D0
        DDFF(1,8) = - (1.D0+KSI2)
        DDFF(2,1) = 0.5D0* (1.D0-KSI1)
        DDFF(2,2) = 0.5D0* (1.D0-KSI1)
        DDFF(2,3) = 0.5D0* (1.D0+KSI1)
        DDFF(2,4) = 0.5D0* (1.D0+KSI1)
        DDFF(2,5) = - (1.D0-KSI1)
        DDFF(2,6) = 0.D0
        DDFF(2,7) = - (1.D0+KSI1)
        DDFF(2,8) = 0.D0
        DDFF(3,1) = 0.25D0* (-1.D0-2.D0*KSI2+2.D0*KSI1)
        DDFF(3,2) = 0.25D0* (1.D0-2.D0*KSI2-2.D0*KSI1)
        DDFF(3,3) = 0.25D0* (-1.D0+2.D0*KSI2-2.D0*KSI1)
        DDFF(3,4) = 0.25D0* (1.D0+2.D0*KSI2+2.D0*KSI1)
        DDFF(3,5) = KSI2
        DDFF(3,6) = KSI1
        DDFF(3,7) = -KSI2
        DDFF(3,8) = -KSI1
      ELSE IF (ALIAS(1:3).EQ.'QU9') THEN
        DDFF(1,1) = 1.D0*AL33(KSI2)
        DDFF(1,2) = 1.D0*AL31(KSI2)
        DDFF(1,3) = 1.D0*AL31(KSI2)
        DDFF(1,4) = 1.D0*AL33(KSI2)
        DDFF(1,5) = 1.D0*AL32(KSI2)
        DDFF(1,6) = -2.D0*AL31(KSI2)
        DDFF(1,7) = 1.D0*AL32(KSI2)
        DDFF(1,8) = -2.D0*AL33(KSI2)
        DDFF(1,9) = -2.D0*AL32(KSI2)
        DDFF(2,1) = AL31(KSI1)*1.D0
        DDFF(2,2) = AL31(KSI1)*1.D0
        DDFF(2,3) = AL33(KSI1)*1.D0
        DDFF(2,4) = AL33(KSI1)*1.D0
        DDFF(2,5) = AL31(KSI1)* (-2.D0)
        DDFF(2,6) = AL32(KSI1)*1.D0
        DDFF(2,7) = AL33(KSI1)* (-2.D0)
        DDFF(2,8) = AL32(KSI1)*1.D0
        DDFF(2,9) = AL32(KSI1)* (-2.D0)
        DDFF(3,1) = DAL31(KSI1)*DAL33(KSI2)
        DDFF(3,2) = DAL31(KSI1)*DAL31(KSI2)
        DDFF(3,3) = DAL33(KSI1)*DAL31(KSI2)
        DDFF(3,4) = DAL33(KSI1)*DAL33(KSI2)
        DDFF(3,5) = DAL31(KSI1)*DAL32(KSI2)
        DDFF(3,6) = DAL32(KSI1)*DAL31(KSI2)
        DDFF(3,7) = DAL33(KSI1)*DAL32(KSI2)
        DDFF(3,8) = DAL32(KSI1)*DAL33(KSI2)
        DDFF(3,9) = DAL32(KSI1)*DAL32(KSI2)
      ELSE 
        CALL ASSERT(.FALSE.)
      END IF


      END
