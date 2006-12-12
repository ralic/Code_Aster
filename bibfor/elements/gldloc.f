      SUBROUTINE GLDLOC (LAMBDA,DEUXMU,SEUIL,ALF,GMT,GMC,COF1,COF2,VIM,
     &                   Q2D,QFF,TR2D,EPS33,DE33D1,DE33D2,KSI2D,
     &                   DA1,DA2,KDMAX,TOLD)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
      REAL*8             VIM(*),GF,GMT,GMC,QFF(2),TR2D,EPS33,COF1
      REAL*8             LAMBDA,DEUXMU,SEUIL,ALF,Q2D,TOLD
      REAL*8             DE33D1,DE33D2,KSI2D,DA1,DA2,COF2
      INTEGER            KDMAX

C ----------------------------------------------------------------------
C
C      CALCUL D'ENDOMMAGEMENT (DA1 ET DA2) ET DE LA COMPOSANTE DE 
C       DEFORMATION EPS33, APPELE PAR "LCGLDM" (LOI GLRC_DM)
C
C IN:
C       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
C       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
C       GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION 
C       GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
C       SEUIL   : INITIAL MEMBRANE
C       ALF     : PARAMETRE DE SEUIL FLEXION
C       VIM     : VARIABLES INTERNES EN T-
C       Q2D     : PARTIE CONSTANTE DU RESIDU MEMBRANE
C       QFF(2)  : PARTIE CONSTANTE DU RESIDU FLEXION
C       TR2D    : EPS11 + EPS22
C       COF1,COF2    : PARAMETRES
C OUT:
C       DA1     : ENDOMMAGEMENT SUR LA PARTIE 1 DE LA PLAQUE
C       DA2     : ENDOMMAGEMENT SUR LA PARTIE 2 DE LA PLAQUE
C       KSI2D   : VALEUR DE LA FONCTION CARACTERISTIQUE DE L'ENDOM.
C       EPS33   : COMPOSANTE 33 DE LA DEFORMATION
C       DE33D1  : DERIVEE DE EPS33 PAR RAPPORT A DA1
C       DE33D2  : DERIVEE DE EPS33 PAR RAPPORT A DA2
C       ELAS    : .TRUE. SI ELASTIQUE
C       ELAS1   : .TRUE. SI DA1 == VIM(1)
C       ELAS2   : .TRUE. SI DA2 == VIM(2)
C ----------------------------------------------------------------------

      REAL*8      DM1,DM2,DF1,DF2,DKSI1,DKSI2,QM1,QM2
      REAL*8      RD1,RD2,DR1D,DR2D,DD1,DD2
      INTEGER     I

C ----------------------------------------------------------------------
      QM1 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D
      QM2 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D

C----SOLUTION EXACTE SI QFF = 0        
      IF(QM1.GT.0.0D0) THEN
        DM1 = SQRT(QM1/SEUIL) - 1.0D0
      ELSE
        DM1 = 0.0D0
      ENDIF    

      IF(QM2.GT.0.0D0) THEN
        DM2 = SQRT(QM2/SEUIL) - 1.0D0
      ELSE
        DM2 = 0.0D0
      ENDIF    

      IF(DM1.LT.VIM(1)) DM1 = VIM(1)
      IF(DM2.LT.VIM(2)) DM2 = VIM(2)

C----SOLUTION EXACTE SI QM = 0        
      IF(QFF(1).GT.0.0D0) THEN
        DF1 = SQRT(QFF(1)/SEUIL) - ALF
      ELSE
        DF1 = 0.0D0
      ENDIF    

      IF(QFF(2).GT.0.0D0) THEN
        DF2 = SQRT(QFF(2)/SEUIL) - ALF
      ELSE
        DF2 = 0.0D0
      ENDIF    

      IF(DF1.LT.VIM(1)) DF1 = VIM(1)
      IF(DF2.LT.VIM(2)) DF2 = VIM(2)
        
C----VAL. INITIALES POUR D1,D2,CONSTRUITES A PARTIR DE DM1,DM2,DF1,DF2
      IF(DF1.GT.DM1) THEN
        DA1 = DF1
      ELSE
        DA1 = DM1        
      ENDIF         

      IF(DF2.GT.DM2) THEN
        DA2 = DF2
      ELSE
        DA2 = DM2        
      ENDIF         


      CALL CEPS33 (LAMBDA,DEUXMU,TR2D,DA1,DA2,GMT,GMC
     &                   ,EPS33,DE33D1,DE33D2,KSI2D,DKSI1,DKSI2)

      QM1 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D
      QM2 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D

      RD1 = QM1/(1.0D0 + DA1)**2  - SEUIL
      RD2 = QM2/(1.0D0 + DA2)**2  - SEUIL

C----CONTRIBUTION DES COURBURES---------
      RD1 = RD1 + QFF(1)/(ALF + DA1)**2 
      RD2 = RD2 + QFF(2)/(ALF + DA2)**2 

C-----VERIFIER SI SEUIL EST ATTEINT
      IF(((RD1 .GT. 0.0D0).AND.(DA1.GE.VIM(1))) 
     &     .OR. ((RD2 .GT. 0.0D0).AND.(DA2.GE.VIM(2)))) THEN
        
          DO 60, I = 1,KDMAX

             DR1D  = (COF1*EPS33 + COF2)*DE33D1/(1.0D0 + DA1)**2 
     &             - 2.0D0*QM1   /(1.0D0 + DA1)**3 
     &             - 2.0D0*QFF(1)/(ALF   + DA1)**3  
             DR2D  = (COF1*EPS33 + COF2)*DE33D2/(1.0D0 + DA2)**2 
     &             - 2.0D0*QM2   /(1.0D0 + DA2)**3
     &             - 2.0D0*QFF(2)/(ALF   + DA2)**3  

             DD1 = - RD1/DR1D
             DD2 = - RD2/DR2D
      
             IF(( (ABS(DD1*RD1) .LT. TOLD) .OR. 
     &                  ((RD1 .LT. 0.0D0) .AND. (DA1 .LE. VIM(1))) ) 
     &        .AND. 
     &            (ABS(DD2*RD2) .LT. TOLD) .OR. 
     &                  ((RD2 .LT. 0.0D0) .AND. (DA2 .LE. VIM(2))) ) 
     &          GOTO 61   
      
             DA1 = DA1 + DD1
             DA2 = DA2 + DD2
             
             IF(DA1.LT.0.0D0 .AND. RD1.LT.0.0D0) DA1 = VIM(1)
             IF(DA2.LT.0.0D0 .AND. RD2.LT.0.0D0) DA2 = VIM(2)

             CALL CEPS33 (LAMBDA,DEUXMU,TR2D,DA1,DA2,GMT,GMC
     &                   ,EPS33,DE33D1,DE33D2,KSI2D,DKSI1,DKSI2)

             QM1 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D
             QM2 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D

             RD1 = QM1/(1.0D0 + DA1)**2 - SEUIL
             RD2 = QM2/(1.0D0 + DA2)**2 - SEUIL

C----CONTRIBUTION DES COURBURES---------
             RD1 = RD1 + QFF(1)/(ALF + DA1)**2 
             RD2 = RD2 + QFF(2)/(ALF + DA2)**2 

60         CONTINUE       
61         CONTINUE       
C----FIN BOUCLE NEWTON SUR D1,D2,EPS33
           IF(I .GT. KDMAX) THEN 
              CALL U2MESS('F','ELEMENTS4_68')
           ENDIF

C  R1&2 < 0.0
         ENDIF    
      END
