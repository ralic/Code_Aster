      SUBROUTINE CFADJU(ALIAS,KSI1  ,KSI2  ,TOLEOU,IPROJ)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/06/2009   AUTEUR DESOZA T.DESOZA 
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
      REAL*8      KSI1,KSI2,TOLEOU
      INTEGER     IPROJ
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE- APPARIEMENT)
C
C AJUSTE LES COORDONNES PARAMETRIQUES POUR RESTER DANS LA MAILLE
C      
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : TYPE DE L'ELEMENT
C               'SE2','SE3'  
C               'TR3','TR6'  
C               'QU4','QU8','QU9' 
C I/O KSI1   : POINT DE CALCUL SUIVANT KSI1 DES
C               FONCTIONS DE FORME ET LEURS DERIVEES
C I/O KSI2   : POINT DE CALCUL SUIVANT KSI2 DES
C               FONCTIONS DE FORME ET LEURS DERIVEES
C IN  TOLEOU : TOLERANCE POUR PROJECTION HORS SEGMENT 
C OUT IPROJ  : VAUT 0 SI POINT PROJETE DANS L'ELEMENT
C                   1 SI POINT PROJETE DANS LA ZONE DEFINIE PAR TOLEOU
C                   2 SI POINT PROJETE EN DEHORS (EXCLUS)
C
C ----------------------------------------------------------------------
C      
      REAL*8   ECART,KSI1E,KSI2E
      INTEGER  IZONE
C
C ----------------------------------------------------------------------
C
      IPROJ  = 0
      ECART  = -1.D0
C
      IF (ALIAS(1:2).EQ.'SE') THEN
C
        IF ((KSI1.GE.-1.D0).AND.(KSI1.LE.1.D0)) THEN
          GOTO 999
        ENDIF    
C
C --- CALCUL DE L'ECART
C
        ECART  = ABS(KSI1)-1.D0
C
C --- RABATTEMENT
C   
        IPROJ = 1
C
        IF (KSI1.LT.-1.D0) THEN
          KSI1 = -1.D0
        ELSEIF (KSI1.GT.1.D0) THEN 
          KSI1 = 1.D0 
        ENDIF  
C
        IF (ECART.GT.TOLEOU) THEN
          IPROJ = 2
        ENDIF  
C
      ELSE IF (ALIAS(1:2).EQ.'TR') THEN
C
        IF ((KSI1.GE.0.D0).AND.
     &      (KSI2.GE.0.D0).AND.((KSI1+KSI2).LE.1.D0)) THEN
          GOTO 999
        ENDIF
C
C --- SECTEUR CONCERNE
C    
        IZONE = 0    
        IF (KSI1.LT.0.D0) THEN
          IF (KSI2.LT.0.D0) THEN
            IZONE = 1         
          ELSEIF ((KSI2.GE.0.D0).AND.(KSI2.LE.1.D0)) THEN  
            IZONE = 2
          ELSEIF (KSI2.GT.1.D0) THEN
            IZONE = 3
          ELSE 
            CALL ASSERT(.FALSE.)            
          ENDIF     
        ENDIF
        IF (KSI2.LT.0.D0) THEN
          IF (KSI1.LT.0.D0) THEN
            IZONE = 1
          ELSEIF ((KSI1.GE.0.D0).AND.(KSI1.LE.1.D0)) THEN  
            IZONE = 8
          ELSEIF (KSI1.GT.1.D0) THEN
            IZONE = 7
          ELSE 
            CALL ASSERT(.FALSE.)  
          ENDIF  
        ENDIF
        IF (KSI1.GE.0.D0) THEN
          IF (KSI2.GT.(KSI1+1.D0)) THEN
            IZONE = 4
          ELSEIF ((KSI2.GT.(-KSI1+1.D0)).AND.
     &            (KSI2.GE.(KSI1-1.D0)).AND.
     &            (KSI2.LE.(KSI1+1.D0))) THEN
            IZONE = 5
            KSI1E = 5.D-1*(1.D0+KSI1-KSI2)
            KSI2E = 5.D-1*(1.D0-KSI1+KSI2)
          ELSEIF ((KSI2.GE.0.D0).AND.
     &            (KSI2.LT.(KSI1-1.D0))) THEN 
            IZONE = 6 
          ENDIF
        ENDIF 
C
C --- CALCUL DE L'ECART
C
        IF (IZONE.EQ.1) THEN
          ECART = SQRT(ABS(KSI1)*ABS(KSI1)+
     &                 ABS(KSI2)*ABS(KSI2))
        ELSEIF (IZONE.EQ.2) THEN
          ECART = SQRT(ABS(KSI1)*ABS(KSI1))
        ELSEIF (IZONE.EQ.3.OR.IZONE.EQ.4) THEN
          ECART = SQRT(ABS(KSI1)*ABS(KSI1)+
     &                (KSI2-1.D0)*(KSI2-1.D0))
        ELSEIF (IZONE.EQ.5) THEN
          ECART = SQRT((KSI1-KSI1E)*(KSI1-KSI1E)+
     &                   (KSI2-KSI2E)*(KSI2-KSI2E))
        ELSEIF (IZONE.EQ.6.OR.IZONE.EQ.7) THEN
          ECART = SQRT(ABS(KSI2)*ABS(KSI2)+
     &                (KSI1-1.D0)*(KSI1-1.D0))
        ELSEIF (IZONE.EQ.8) THEN
          ECART = SQRT(ABS(KSI2)*ABS(KSI2))
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF  
C
C --- RABATTEMENT
C        
        IPROJ = 1
C
        IF (IZONE.EQ.1) THEN
          KSI1 = 0.D0
          KSI2 = 0.D0
        ELSEIF (IZONE.EQ.2) THEN
          KSI1 = 0.D0
        ELSEIF (IZONE.EQ.3.OR.IZONE.EQ.4) THEN
          KSI1 = 0.D0
          KSI2 = 1.D0
        ELSEIF (IZONE.EQ.5) THEN
          KSI1 = KSI1E
          KSI2 = KSI2E     
        ELSEIF (IZONE.EQ.6.OR.IZONE.EQ.7) THEN
          KSI1 = 1.D0
          KSI2 = 0.D0    
        ELSEIF (IZONE.EQ.8) THEN
          KSI2 = 0.D0 
        ENDIF       
C              
        IF (ECART.GT.TOLEOU) THEN
          IPROJ = 2
        ENDIF
C
      ELSE IF (ALIAS(1:2).EQ.'QU') THEN
C
        IF ((ABS(KSI1).LE.1.D0).AND.(ABS(KSI2).LE.1.D0)) THEN
          GOTO 999
        ENDIF
C
C --- SECTEUR CONCERNE
C        
        IZONE = 0
        IF (KSI1.LT.-1.D0) THEN
          IF (KSI2.LT.-1.D0) THEN
            IZONE = 1
            
          ELSEIF ((KSI2.GE.-1.D0).AND.(KSI2.LE.1.D0)) THEN  
            IZONE = 2            
          ELSEIF (KSI2.GT.1.D0) THEN
            IZONE = 3           
          ELSE 
            CALL ASSERT(.FALSE.)            
          ENDIF     
        ENDIF
        IF (KSI1.GT.1.D0) THEN
          IF (KSI2.LT.-1.D0) THEN
            IZONE = 7 
          ELSEIF ((KSI2.GE.-1.D0).AND.(KSI2.LE.1.D0)) THEN  
            IZONE = 6            
          ELSEIF (KSI2.GT.1.D0) THEN
            IZONE = 5           
          ELSE 
            CALL ASSERT(.FALSE.)            
          ENDIF     
        ENDIF        
         IF ((KSI1.GE.-1.D0).AND.(KSI1.LE.1.D0)) THEN
          IF (KSI2.LT.-1.D0) THEN
            IZONE = 8          
          ELSEIF (KSI2.GT.1.D0) THEN
            IZONE = 4                       
          ENDIF     
        ENDIF       
C
C --- CALCUL DE L'ECART
C
        IF (IZONE.EQ.1) THEN
          ECART = SQRT((ABS(KSI1)-1.D0)*(ABS(KSI1)-1.D0)+
     &                 (ABS(KSI2)-1.D0)*(ABS(KSI2)-1.D0))
        ELSEIF (IZONE.EQ.2) THEN
          ECART = SQRT((ABS(KSI1)-1.D0)*(ABS(KSI1)-1.D0))
        ELSEIF (IZONE.EQ.3) THEN
          ECART = SQRT((ABS(KSI1)-1.D0)*(ABS(KSI1)-1.D0)+
     &                 (ABS(KSI2)-1.D0)*(ABS(KSI2)-1.D0))
        ELSEIF (IZONE.EQ.4) THEN
          ECART = SQRT((ABS(KSI2)-1.D0)*(ABS(KSI2)-1.D0))  
        ELSEIF (IZONE.EQ.5) THEN
          ECART = SQRT((ABS(KSI1)-1.D0)*(ABS(KSI1)-1.D0)+
     &                 (ABS(KSI2)-1.D0)*(ABS(KSI2)-1.D0))
        ELSEIF (IZONE.EQ.6) THEN
          ECART = SQRT((ABS(KSI1)-1.D0)*(ABS(KSI1)-1.D0)) 
        ELSEIF (IZONE.EQ.7) THEN
          ECART = SQRT((ABS(KSI1)-1.D0)*(ABS(KSI1)-1.D0)+
     &                 (ABS(KSI2)-1.D0)*(ABS(KSI2)-1.D0))
        ELSEIF (IZONE.EQ.8) THEN
          ECART = SQRT((ABS(KSI2)-1.D0)*(ABS(KSI2)-1.D0)) 
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF        
C
C --- RABATTEMENT
C     
        IPROJ = 1
C   
        IF (IZONE.EQ.1) THEN
          KSI1 = -1.D0
          KSI2 = -1.D0
        ELSEIF (IZONE.EQ.2) THEN
          KSI1 = -1.D0
        ELSEIF (IZONE.EQ.3) THEN
          KSI1 = -1.D0
          KSI2 = 1.D0
        ELSEIF (IZONE.EQ.4) THEN
          KSI2 = 1.D0        
        ELSEIF (IZONE.EQ.5) THEN
          KSI1 = 1.D0
          KSI2 = 1.D0        
        ELSEIF (IZONE.EQ.6) THEN
          KSI1 = 1.D0        
        ELSEIF (IZONE.EQ.7) THEN
          KSI1 = 1.D0
          KSI2 = -1.D0        
        ELSEIF (IZONE.EQ.8) THEN
          KSI2 = -1.D0 
        ENDIF 
C
        IF (ECART.GT.TOLEOU) THEN
          IPROJ = 2
        ENDIF
C            
      ELSE
        CALL ASSERT(.FALSE.)  
      END IF

 999  CONTINUE

      END
