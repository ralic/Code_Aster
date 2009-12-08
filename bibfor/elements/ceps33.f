      SUBROUTINE CEPS33 (LAMBDA,DEUXMU,TR2D,D1,D2,GMT,GMC
     &                   ,EPS33,DE33D1,DE33D2,KSI2D,DKSI1,DKSI2)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/11/2006   AUTEUR MARKOVIC D.MARKOVIC 
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

C BUT : CALCUL DE LA COMPOSANTE DE LA DEFORMATION EZZ ET DE SES 
C       DERIVEES POUR LE MODELE GLRC_DM
C
C IN:
C       LAMBDA  : PARAMETRE D ELASTICITE
C       DEUXMU  : PARAMETRE D ELASTICITE
C       TR2D    : TRACE 2D = EXX + EYY
C       D1      : ENDOMMAGEMENT DE LA PLAQUE D'UN COTE
C       D2      : ET DE L AUTRE 
C       GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION 
C       GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
C OUT:
C       EPS33  : COMPOSANTE DE LA DEFORMATION EZZ
C       DE33D1 : DERIVEE DE EZZ PAR RAPPORT A D1 
C       DE33D2 : DERIVEE DE EZZ PAR RAPPORT A D2 
C       KSI2D : FONCTION CARACTERISTIQUE D ENDOMMAGEMENT,
C                KSI = KSI(TR2D,D1,D2) 
C       DKSI1  : DERIVEE DE KSI PAR RAPPORT A D1
C       DKSI2  : DERIVEE DE KSI PAR RAPPORT A D2
C ----------------------------------------------------------------------
      IMPLICIT NONE


      REAL*8   TR2D,D1,D2,GMT,GMC,EPS33,DE33D1,DE33D2
      INTEGER  K
      REAL*8   KSI2D,DKSI1,DKSI2,LAMBDA,DEUXMU    
      
      
      IF(TR2D .GT. 0.0D0) THEN
        KSI2D = (1.0D0 + GMT*D1) / (1.0D0 + D1) *0.5D0          
        KSI2D = KSI2D + (1.0D0 + GMT*D2) / (1.0D0 + D2) *0.5D0
     
        DKSI1 = -0.5D0 * (1.0D0 - GMT)/(1.0D0 + D1)**2
        DKSI2 = -0.5D0 * (1.0D0 - GMT)/(1.0D0 + D2)**2         
      ELSE
        KSI2D = (1.0D0 + GMC*D1) / (1.0D0 + D1) *0.5D0          
        KSI2D = KSI2D + (1.0D0 + GMC*D2) / (1.0D0 + D2) *0.5D0
     
        DKSI1 = -0.5D0 * (1.0D0 - GMC)/(1.0D0 + D1)**2
        DKSI2 = -0.5D0 * (1.0D0 - GMC)/(1.0D0 + D2)**2         
      ENDIF
      
      EPS33  = -LAMBDA*TR2D*KSI2D/(DEUXMU + LAMBDA*KSI2D)
          
      DE33D1 = -DEUXMU*LAMBDA*TR2D/(DEUXMU + LAMBDA*KSI2D)**2 * DKSI1
      DE33D2 = -DEUXMU*LAMBDA*TR2D/(DEUXMU + LAMBDA*KSI2D)**2 * DKSI2
      
      END
