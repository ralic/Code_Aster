      SUBROUTINE MEFASR(NDIM,NBCYL,NBGRP,NBTRON,NUMGRP,IDIR,IGRP,       
     &                  XINT,YINT,RINT,SGN,ORIG,BETA,A,B)               
      IMPLICIT REAL*8 (A-H,O-Z)                                         
C                                                                       
      INTEGER       NDIM(14),NBCYL,NBGRP,NBTRON,NUMGRP(*),IDIR,IGRP     
      INTEGER       SGN(*),ORIG(*)                                      
      REAL*8        XINT(*),YINT(*),RINT(*),BETA(*)                     
      REAL*8        A(2*NBTRON*NBCYL,*),B(*)                            
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 05/10/1999   AUTEUR KXBADNG A.ADOBES 
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
C ----------------------------------------------------------------------
C     ASSEMBLAGE POUR L ENCEINTE RECTANGULAIRE                          
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST, MEFREC              
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE    
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"                                   
C ----------------------------------------------------------------------
C IN  : NDIM   : TABLEAU DES DIMENSIONS                                 
C IN  : NBCYL  : NOMBRE DE CYLINDRES                                    
C IN  : NBGRP  : NOMBRE DE GROUPES D EQUIVALENCE                        
C IN  : NBTRON : ORDRE DE TRONCATURE DES SERIES DE LAURENT DANS LA BASE 
C                MODALE                                                 
C IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE                      
C IN  : IDIR   : INDICES DE CYLINDRE                                    
C IN  : IGRP   : INDICES DE GROUPE DE CYLINDRE                          
C IN  : XINT   : COORDONNEES 'X' DES CENTRES DES CYLINDRES DANS         
C                LE REPERE AXIAL                                        
C IN  : YINT   : COORDONNEES 'Y' DES CENTRES DES CYLINDRES DANS         
C                LE REPERE AXIAL                                        
C IN  : RINT   : RAYONS DES CYLINDRES                                   
C IN  : SGN    : -1 OU +1, COEFFICIENT INTERVENANT DANS LA DECOMPOSITION
C                EN SERIE DE LAURENT, SELON LE NIVEAU D IMAGE           
C IN  : ORIG   : NUMERO DU CYLINDRE D ORIGINE DES CYLINDRES REELS OU    
C                IMAGES                                                 
C IN  : BETA   : ANGLE CUMULE INTERVENANT DANS LA DECOMPOSITION EN      
C                SERIE DE LAURENT, POUR LES CYLINDRES IMAGES            
C IN  : A      : TABLEAU DE TRAVAIL: SOUS MATRICE DU SYSTEME A.X = B    
C IN  : B      : TABLEAU DE TRAVAIL: SECOND MEMBRE DU SYSTEME A.X = B   
C ----------------------------------------------------------------------
      INTEGER      I,J,K,L,NJ,NL,M,NM                                   
      REAL*8       MEFAC1,MEFAC2                                        
      REAL*8       FIC,DC,DC1                                           
      REAL*8       COEF,COEF1,COEF2                                     
      REAL*8       RAYK,RAYI,ARG                                        
      REAL*8       PI                                                   
C ----------------------------------------------------------------------
C                                                                       
C --- LECTURE DES DIMENSIONS                                            
      NBCYL  = NDIM(3)                                                  
      NBGRP  = NDIM(4)                                                  
      NBTRON = NDIM(5)                                                  
      NIMA   = NDIM(7)                                                  
      NIMA2  = NDIM(8)                                                  
      NBTOT  = NBCYL*(2*NIMA+1)*(2*NIMA+1)                              
      NBFIN  = NBTOT + 4*(NIMA2)*(NIMA2+2*NIMA+1)                       
C                                                                       
C                                                                       
      PI = R8PI()                                                       
      EPSIT = 1.D-5                                                     
C                                                                       
C                                                                       
      DO 1 I = 1,NBCYL                                                  
         RAYI = 1.D0/RINT(I)                                            
                                                                        
         DO 11 J = 1,NBTRON                                             
            NJ = 2*J+2*NBTRON*(I-1)                                     
            RAYI = RAYI*RINT(I)                                         
                                                                        
            DO 111 K = 1,NBTOT                                          
               DC = SQRT((XINT(K)-XINT(I))*(XINT(K)-XINT(I))+           
     &                  (YINT(K)-YINT(I))*(YINT(K)-YINT(I)))            
                                                                        
               IF (DC.NE.0.D0) THEN                                     
                  IF (ABS(ABS(DC)-ABS(XINT(K)-XINT(I))).GT.EPSIT)THEN   
                     FIC = ACOS((XINT(K)-XINT(I))/DC)                   
                     IF ((YINT(K)-YINT(I)).LT.0.D0) THEN                
                        FIC = 2.D0*PI-FIC                               
                     ENDIF                                              
                  ELSE                                                  
                     FIC = PI / 2.D0 * (1.D0 - ((XINT(K)-XINT(I))/DC)   
     &                                 / ABS(((XINT(K)-XINT(I))/DC)))   
                     IF ((YINT(K)-YINT(I)).LT.0.D0) THEN                
                        FIC = 2.D0*PI-FIC                               
                     ENDIF                                              
                  ENDIF                                                 
               ELSE                                                     
                  FIC = 0.D0                                            
               ENDIF                                                    
C                                                                       
               IF (K.NE.I) THEN                                         
                  RAYK = RINT(K)                                        
                  DC1 = DC**J                                           
C                                                                       
                  DO 1111 L = 1,NBTRON                                  
                     RAYK = RINT(K)*RAYK                                
                     DC1 = DC*DC1                                       
                     NL = 2*L+2*NBTRON*(ORIG(K)-1)                      
                     COEF = MEFAC2(L,J)*RAYI*RAYK/DC1                   
                     COEF = COEF*((-1)**L)                              
                     ARG = (J+L)*FIC-L*BETA(K)                          
                     A(NJ-1,NL-1) = COEF*COS(ARG)+A(NJ-1,NL-1)          
                     A(NJ,NL-1) = COEF*SIN(ARG)+A(NJ,NL-1)              
                     A(NJ-1,NL) = SGN(K)*COEF*SIN(ARG)+A(NJ-1,NL)       
                     A(NJ,NL) = -SGN(K)*COEF*COS(ARG)+A(NJ,NL)          
 1111             CONTINUE                                              
C                                                                       
               ELSE                                                     
                  NL = 2*J+2*NBTRON*(ORIG(K)-1)                         
                  A(NJ-1,NL-1) = A(NJ-1,NL-1)-J                         
                  A(NJ,NL) = A(NJ,NL)-J                                 
               ENDIF                                                    
 111        CONTINUE                                                    
C                                                                       
            DO 112 K = NBTOT+1,NBFIN                                    
               DC = SQRT((XINT(K)-XINT(I))*(XINT(K)-XINT(I))+           
     &                  (YINT(K)-YINT(I))*(YINT(K)-YINT(I)))            
C                                                                       
               IF (DC.NE.0.D0) THEN                                     
                  IF (ABS(ABS(DC)-ABS(XINT(K)-XINT(I))).GT.EPSIT)THEN   
                     FIC = ACOS((XINT(K)-XINT(I))/DC)                   
                     IF ((YINT(K)-YINT(I)).LT.0.D0) THEN                
                        FIC = 2.D0*PI-FIC                               
                     ENDIF                                              
                  ELSE                                                  
                     FIC = PI / 2.D0 * (1.D0 - ((XINT(K)-XINT(I))/DC)   
     &                                 / ABS(((XINT(K)-XINT(I))/DC)))   
                     IF ((YINT(K)-YINT(I)).LT.0.D0) THEN                
                        FIC = 2.D0*PI-FIC                               
                     ENDIF                                              
                  ENDIF                                                 
               ELSE                                                     
                  FIC = 0.D0                                            
               ENDIF                                                    
               DC1 = DC**J                                              
C                                                                       
               DO 1121 L = 1,NBTRON                                     
                  DC1 = DC1*DC                                          
                  COEF = MEFAC2(L,J)*(RAYI)/DC1                         
                  COEF = COEF*((-1)**L)                                 
                  ARG = (L+J)*FIC-L*BETA(K)                             
                  COEF1 = COEF*COS(ARG)                                 
                  COEF2 = COEF*SIN(ARG)                                 
C                                                                       
                  DO 11211 M = 1,NBCYL                                  
                     NM = 2*NBTRON*(M-1)+2*L                            
                     ARG = RINT(M)**(L+1)                               
                     A(NJ-1,NM-1) = COEF1*ARG+A(NJ-1,NM-1)              
                     A(NJ,NM-1) = COEF2*ARG+A(NJ,NM-1)                  
                     A(NJ-1,NM) = SGN(K)*COEF2*ARG+A(NJ-1,NM)           
                     A(NJ,NM) = -SGN(K)*COEF1*ARG+A(NJ,NM)              
11211            CONTINUE                                               
 1121          CONTINUE                                                 
 112        CONTINUE                                                    
  11     CONTINUE                                                       
C                                                                       
         IF (NUMGRP(I).EQ.IGRP) THEN                                    
            B(2*NBTRON*(I-1)+IDIR) = 1.D0                               
         ENDIF                                                          
   1  CONTINUE                                                          
C                                                                       
      END 
