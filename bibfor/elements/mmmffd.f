      SUBROUTINE MMMFFD(ALIAS ,XI    ,YI     ,TYPBAR,
     &                  FF    ,DFF   ,DDFF   ,IRET)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/09/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8      XI
      REAL*8      YI   
      INTEGER     TYPBAR
      REAL*8      FF(9)
      REAL*8      DFF(2,9)
      REAL*8      DDFF(3,9)
      INTEGER     IRET                  
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT - UTILITAIRE)
C
C CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT 
C DE L'ELEMENT DE REFERENCE
C
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  XI     : POINT DE CONTACT SUIVANT KSI1 DES
C               FONCTIONS DE FORME ET LEURS DERIVEES
C IN  YI     : POINT DE CONTACT SUIVANT KSI2 DES
C               FONCTIONS DE FORME ET LEURS DERIVEES
C IN  TYPBAR : INDICATEUR D'ELEMENT DE BARSOUM
C OUT FF     : FONCTIONS DE FORMES EN XI,YI
C OUT DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME EN XI YI
C OUT DDFF   : DERIVEES SECONDES DES FONCTIONS DE FORME EN XI YI
C OUT IRET   : RETOURNE UN CODE ERREUR
C                0  TOUT VA BIEN
C                1  ELEMENT INCONNU
C
C ----------------------------------------------------------------------
C
      REAL*8  X,U
      REAL*8  AL31,AL32,AL33
      REAL*8  DAL31,DAL32,DAL33
      REAL*8  UNS4
      REAL*8  A,B,C,D
      INTEGER I
C
C ----------------------------------------------------------------------
C
      AL31(X)  = 0.5D0*X* (X-1.D0)
      AL32(X)  = - (X+1.D0)* (X-1.D0)
      AL33(X)  = 0.5D0*X* (X+1.D0)
      DAL31(U) = 0.5D0* (2.D0*U-1.D0)
      DAL32(U) = -2.D0*U
      DAL33(U) = 0.5D0* (2.D0*U+1.D0)
      UNS4     = 0.25D0
C
C ----------------------------------------------------------------------
C
      IRET   = 0
      DO 10 I=1,9
        FF(I)     = 0.D0
        DFF(1,I)  = 0.D0  
        DFF(2,I)  = 0.D0                    
        DDFF(1,I) = 0.D0  
        DDFF(2,I) = 0.D0  
        DDFF(3,I) = 0.D0  
 10   CONTINUE
C      
      IF (ALIAS(1:3).EQ.'SG2') THEN
        FF(1) = 0.5D0* (1-XI)
        FF(2) = 0.5D0* (1+XI)
        DFF(1,1) = -0.5D0
        DFF(1,2) = 0.5D0
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 0.D0
      ELSE IF (ALIAS(1:3).EQ.'SG3') THEN
        FF(1) = -0.5D0* (1-XI)*XI
        FF(2) = 0.5D0* (1+XI)*XI
        FF(3) = 1.D0* (1+XI)* (1-XI)
        IF (TYPBAR .EQ. 0) THEN
          FF(1) = FF(1) - ((0.01D-5)*XI*(1.D0+XI)*(1.D0-XI))
          FF(2) = FF(2) - ((0.01D-5)*XI*(1.D0+XI)*(1.D0-XI))
          FF(3) = FF(3) + ((0.02D-5)*XI*(1.D0+XI)*(1.D0-XI))
        END IF
        DFF(1,1) = -0.5D0* (1-2*XI)
        DFF(1,2) = 0.5D0* (1+2*XI)
        DFF(1,3) = -2.D0*XI
        IF (TYPBAR .EQ. 0) THEN
          DFF(1,1) = DFF(1,1)-((0.01D-5)*(1.D0-3*(XI**2)))
          DFF(1,2) = DFF(1,2)-((0.01D-5)*(1.D0-3*(XI**2)))
          DFF(1,3) = DFF(1,3)+((0.02D-5)*(1.D0-3*(XI**2)))
        END IF        
        DDFF(1,1) = 1.D0
        DDFF(1,2) = 1.D0
        DDFF(1,3) = -2.D0
      ELSE IF (ALIAS(1:3).EQ.'TR3') THEN
        FF(1) = 0.5D0* (1+YI)
        FF(2) = -0.5D0* (XI+YI)
        FF(3) = 0.5D0* (1+XI)
        DFF(1,1) = 0.D0
        DFF(1,2) = -0.5D0
        DFF(1,3) = 0.5D0
        DFF(2,1) = 0.5D0
        DFF(2,2) = -0.5D0
        DFF(2,3) = 0.D+00
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
        FF(1) = 0.5D0* (1.D+00+YI)*YI
        FF(2) = 0.5D0* (XI+YI)* (XI+YI+1)
        FF(3) = 0.5D0* (1.D+00+XI)*XI
        FF(4) = - (1.D0+YI)* (XI+YI)
        FF(5) = - (1.D0+XI)* (XI+YI)
        FF(6) = (1.D0+XI)* (1.D0+YI)

C LES DERIVEES 1ERES / XI
        DFF(1,1) = 0.D0
        DFF(1,2) = 0.5D0* (2.D0*XI+2.D0*YI+1.D0)
        DFF(1,3) = 0.5D0* (2.D0*XI+1.D0)
        DFF(1,4) = - (1.D0+YI)
        DFF(1,5) = - (1.D0+YI+2.D0*XI)
        DFF(1,6) = (1.D0+YI)
C ----------        / YI
        DFF(2,1) = 0.5D0* (2.D0*YI+1.D0)
        DFF(2,2) = 0.5D0* (2.D0*XI+2.D0*YI+1.D0)
        DFF(2,3) = 0.D+00
        DFF(2,4) = - (1.D0+XI+2.D0*YI)
        DFF(2,5) = - (1.D0+XI)
        DFF(2,6) = (1.D0+XI)

C ----------      /XIXI
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 1.D0
        DDFF(1,3) = 1.D0
        DDFF(1,4) = 0.D0
        DDFF(1,5) = -2.D0
        DDFF(1,6) = 0.D0
C ---------      /YIYI
        DDFF(2,1) = 1.D0
        DDFF(2,2) = 1.D0
        DDFF(2,3) = 0.D0
        DDFF(2,4) = -2.D0
        DDFF(2,5) = 0.D0
        DDFF(2,6) = 0.D0
C ---------      /XIYI
        DDFF(3,1) = 0.D0
        DDFF(3,2) = 1.D0
        DDFF(3,3) = 0.D0
        DDFF(3,4) = -1.D0
        DDFF(3,5) = -1.D0
        DDFF(3,6) = 1.D0

      ELSE IF (ALIAS(1:3).EQ.'QU4') THEN
        UNS4 = 0.25D0
        A = 1.D0 + XI
        B = 1.D0 + YI
        C = 1.D0 - XI
        D = 1.D0 - YI
C     LES FFS
        FF(1) = C*B*UNS4
        FF(2) = C*D*UNS4
        FF(3) = A*D*UNS4
        FF(4) = A*B*UNS4
C     LES DD 1ER / XI
        DFF(1,1) = -B*UNS4
        DFF(1,2) = -D*UNS4
        DFF(1,3) = D*UNS4
        DFF(1,4) = B*UNS4
C     LES DD 1ER / YI
        DFF(2,1) = C*UNS4
        DFF(2,2) = -C*UNS4
        DFF(2,3) = -A*UNS4
        DFF(2,4) = A*UNS4
C     LES DD 2ER / XIXI
        DDFF(1,1) = 0.D0
        DDFF(1,2) = 0.D0
        DDFF(1,3) = 0.D0
        DDFF(1,4) = 0.D0
C     LES DD 2ER / YIYI
        DDFF(2,1) = 0.D0
        DDFF(2,2) = 0.D0
        DDFF(2,3) = 0.D0
        DDFF(2,4) = 0.D0
C                  / XIYI
        DDFF(3,1) = -UNS4
        DDFF(3,2) = UNS4
        DDFF(3,3) = -UNS4
        DDFF(3,4) = UNS4

       ELSE IF (ALIAS(1:3).EQ.'QU8') THEN

        FF(1) = (1.D0+YI)* (1.D0-XI)* (-1.D0-XI+YI)*0.25D0
        FF(2) = (1.D0-YI)* (1.D0-XI)* (-1.D0-XI-YI)*0.25D0
        FF(3) = (1.D0-YI)* (1.D0+XI)* (-1.D0+XI-YI)*0.25D0
        FF(4) = (1.D0+YI)* (1.D0+XI)* (-1.D0+XI+YI)*0.25D0
        FF(5) = (1.D0-YI)* (1.D0-XI)* (1.D0+YI)*0.5D0
        FF(6) = (1.D0-YI)* (1.D0-XI)* (1.D0+XI)*0.5D0
        FF(7) = (1.D0-YI)* (1.D0+XI)* (1.D0+YI)*0.5D0
        FF(8) = (1.D0+YI)* (1.D0-XI)* (1.D0+XI)*0.5D0
        IF (TYPBAR .EQ. 1 .OR. TYPBAR .EQ. 2) THEN
          FF(1) = FF(1) - ((0.01D-5)*XI*(1.D0+XI)*(1.D0-XI))
          FF(4) = FF(4) - ((0.01D-5)*XI*(1.D0+XI)*(1.D0-XI))
          FF(8) = FF(8) + ((0.02D-5)*XI*(1.D0+XI)*(1.D0-XI))
          FF(2) = FF(2) - ((0.01D-5)*XI*(1.D0+XI)*(1.D0-XI))   
          FF(3) = FF(3) - ((0.01D-5)*XI*(1.D0+XI)*(1.D0-XI)) 
          FF(6) = FF(6) + ((0.02D-5)*XI*(1.D0+XI)*(1.D0-XI))  
        ELSEIF (TYPBAR .EQ. 3) THEN
          FF(1) = FF(1) - ((0.01D-5)*YI*(1.D0+YI)*(1.D0-YI))  
          FF(2) = FF(2) - ((0.01D-5)*YI*(1.D0+YI)*(1.D0-YI))  
          FF(3) = FF(3) - ((0.01D-5)*YI*(1.D0+YI)*(1.D0-YI))   
          FF(4) = FF(4) - ((0.01D-5)*YI*(1.D0+YI)*(1.D0-YI))   
          FF(5) = FF(5) + ((0.02D-5)*YI*(1.D0+YI)*(1.D0-YI)) 
          FF(7) = FF(7) + ((0.02D-5)*YI*(1.D0+YI)*(1.D0-YI)) 
        END IF
C     LES DD 1ER / XI

        DFF(1,1) = 0.25D0* (1.D0+YI)* (2.D0*XI-YI)
        DFF(1,2) = 0.25D0* (1.D0-YI)* (2.D0*XI+YI)
        DFF(1,3) = 0.25D0* (1.D0-YI)* (2.D0*XI-YI)
        DFF(1,4) = 0.25D0* (1.D0+YI)* (2.D0*XI+YI)
        DFF(1,5) = -0.5D0* (1.D0-YI)* (1.D0+YI)
        DFF(1,6) = - (1.D0-YI)*XI
        DFF(1,7) = 0.5D0* (1.D0-YI)* (1.D0+YI)
        DFF(1,8) = - (1.D0+YI)* (XI)
        IF (TYPBAR .EQ. 1 .OR. TYPBAR .EQ. 2) THEN
          DFF(1,1) = DFF(1,1) - ((0.01D-5)*(1.D0-3*(XI**2)))    
          DFF(1,4) = DFF(1,4) - ((0.01D-5)*(1.D0-3*(XI**2))) 
          DFF(1,8) = DFF(1,8) + ((0.02D-5)*(1.D0-3*(XI**2))) 
          DFF(1,2) = DFF(1,2) - ((0.01D-5)*(1.D0-3*(XI**2)))     
          DFF(1,3) = DFF(1,3) - ((0.01D-5)*(1.D0-3*(XI**2)))    
          DFF(1,6) = DFF(1,6) + ((0.02D-5)*(1.D0-3*(XI**2))) 
        END IF        
C     LES DD 1ER / YI
        DFF(2,1) = 0.25D0* (1.D0-XI)* (2.D0*YI-XI)
        DFF(2,2) = 0.25D0* (1.D0-XI)* (2.D0*YI+XI)
        DFF(2,3) = 0.25D0* (1.D0+XI)* (2.D0*YI-XI)
        DFF(2,4) = 0.25D0* (1.D0+XI)* (2.D0*YI+XI)
        DFF(2,5) = - (1.D0-XI)*YI
        DFF(2,6) = -0.5D0* (1.D0-XI)* (1.D0+XI)
        DFF(2,7) = - (1.D0+XI)*YI
        DFF(2,8) = (1.D0+XI)* (1.D0-XI)*0.5D0
        IF (TYPBAR .EQ. 3) THEN
          DFF(2,1) = DFF(2,1) - ((0.01D-5)*(1.D0-3*(YI**2)))    
          DFF(2,2) = DFF(2,2) - ((0.01D-5)*(1.D0-3*(YI**2)))     
          DFF(2,3) = DFF(2,3) - ((0.01D-5)*(1.D0-3*(YI**2)))    
          DFF(2,4) = DFF(2,4) - ((0.01D-5)*(1.D0-3*(YI**2)))    
          DFF(2,5) = DFF(2,5) + ((0.02D-5)*(1.D0-3*(YI**2))) 
          DFF(2,7) = DFF(2,7) + ((0.02D-5)*(1.D0-3*(YI**2)))
        END IF        
C     LES DD 2ER / XIXI
        DDFF(1,1) = 0.5D0* (1.D0+YI)
        DDFF(1,2) = 0.5D0* (1.D0-YI)
        DDFF(1,3) = 0.5D0* (1.D0-YI)
        DDFF(1,4) = 0.5D0* (1.D0+YI)
        DDFF(1,5) = 0.D0
        DDFF(1,6) = - (1.D0-YI)
        DDFF(1,7) = 0.D0
        DDFF(1,8) = - (1.D0+YI)
C     LES DD 2ER / YIYI
        DDFF(2,1) = 0.5D0* (1.D0-XI)
        DDFF(2,2) = 0.5D0* (1.D0-XI)
        DDFF(2,3) = 0.5D0* (1.D0+XI)
        DDFF(2,4) = 0.5D0* (1.D0+XI)
        DDFF(2,5) = - (1.D0-XI)
        DDFF(2,6) = 0.D0
        DDFF(2,7) = - (1.D0+XI)
        DDFF(2,8) = 0.D0
C     LES DD 2ER / XIYI
        DDFF(3,1) = 0.25D0* (-1.D0-2.D0*YI+2.D0*XI)
        DDFF(3,2) = 0.25D0* (1.D0-2.D0*YI-2.D0*XI)
        DDFF(3,3) = 0.25D0* (-1.D0+2.D0*YI-2.D0*XI)
        DDFF(3,4) = 0.25D0* (1.D0+2.D0*YI+2.D0*XI)
        DDFF(3,5) = YI
        DDFF(3,6) = XI
        DDFF(3,7) = -YI
        DDFF(3,8) = -XI
      ELSE IF (ALIAS(1:3).EQ.'QU9') THEN

        FF(1) = AL31(XI)*AL33(YI)
        FF(2) = AL31(XI)*AL31(YI)
        FF(3) = AL33(XI)*AL31(YI)
        FF(4) = AL33(XI)*AL33(YI)
        FF(5) = AL31(XI)*AL32(YI)
        FF(6) = AL32(XI)*AL31(YI)
        FF(7) = AL33(XI)*AL32(YI)
        FF(8) = AL32(XI)*AL33(YI)
        FF(9) = AL32(XI)*AL32(YI)

        DFF(1,1) = DAL31(XI)*AL33(YI)
        DFF(2,1) = AL31(XI)*DAL33(YI)
        DFF(1,2) = DAL31(XI)*AL31(YI)
        DFF(2,2) = AL31(XI)*DAL31(YI)
        DFF(1,3) = DAL33(XI)*AL31(YI)
        DFF(2,3) = AL33(XI)*DAL31(YI)
        DFF(1,4) = DAL33(XI)*AL33(YI)
        DFF(2,4) = AL33(XI)*DAL33(YI)
        DFF(1,5) = DAL31(XI)*AL32(YI)
        DFF(2,5) = AL31(XI)*DAL32(YI)
        DFF(1,6) = DAL32(XI)*AL31(YI)
        DFF(2,6) = AL32(XI)*DAL31(YI)
        DFF(1,7) = DAL33(XI)*AL32(YI)
        DFF(2,7) = AL33(XI)*DAL32(YI)
        DFF(1,8) = DAL32(XI)*AL33(YI)
        DFF(2,8) = AL32(XI)*DAL33(YI)
        DFF(1,9) = DAL32(XI)*AL32(YI)
        DFF(2,9) = AL32(XI)*DAL32(YI)
C
        DDFF(1,1) = 1.D0*AL33(YI)
        DDFF(1,2) = 1.D0*AL31(YI)
        DDFF(1,3) = 1.D0*AL31(YI)
        DDFF(1,4) = 1.D0*AL33(YI)
        DDFF(1,5) = 1.D0*AL32(YI)
        DDFF(1,6) = -2.D0*AL31(YI)
        DDFF(1,7) = 1.D0*AL32(YI)
        DDFF(1,8) = -2.D0*AL33(YI)
        DDFF(1,9) = -2.D0*AL32(YI)
C
        DDFF(2,1) = AL31(XI)*1.D0
        DDFF(2,2) = AL31(XI)*1.D0
        DDFF(2,3) = AL33(XI)*1.D0
        DDFF(2,4) = AL33(XI)*1.D0
        DDFF(2,5) = AL31(XI)* (-2.D0)
        DDFF(2,6) = AL32(XI)*1.D0
        DDFF(2,7) = AL33(XI)* (-2.D0)
        DDFF(2,8) = AL32(XI)*1.D0
        DDFF(2,9) = AL32(XI)* (-2.D0)
C
        DDFF(3,1) = DAL31(XI)*DAL33(YI)
        DDFF(3,2) = DAL31(XI)*DAL31(YI)
        DDFF(3,3) = DAL33(XI)*DAL31(YI)
        DDFF(3,4) = DAL33(XI)*DAL33(YI)
        DDFF(3,5) = DAL31(XI)*DAL32(YI)
        DDFF(3,6) = DAL32(XI)*DAL31(YI)
        DDFF(3,7) = DAL33(XI)*DAL32(YI)
        DDFF(3,8) = DAL32(XI)*DAL33(YI)
        DDFF(3,9) = DAL32(XI)*DAL32(YI)
       ELSE 
         IRET = 1
       END IF
       END
