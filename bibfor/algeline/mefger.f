      SUBROUTINE MEFGER(NDIM,SOM,XINT,YINT,RINT,SGN,ORIG,BETA)          
      IMPLICIT REAL*8 (A-H,O-Z)                                         
C                                                                       
      INTEGER       NDIM(14),SGN(*),ORIG(*)                             
      REAL*8        SOM(9),XINT(*),YINT(*),RINT(*),BETA(*)              
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/11/2006   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     MISE EN FORME DES DONNEES POUR LA PRISE EN COMPTE DES CONDITIONS  
C     AUX LIMITES PAR UNE METHODE DERIVEE DE LA METHODE DES IMAGES      
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST                      
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE    
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"                                   
C ----------------------------------------------------------------------
C IN  : NDIM   : TABLEAU DES DIMENSIONS                                 
C IN  : SOM    : COORDONNEES DES SOMMETS DE L'ENCEINTE RECTANGULAIRE    
C                OU XEXT,YEXT,REXT                                      
C IN  : XINT   : COORDONNEES 'X' DES CENTRES DES CYLINDRES DANS         
C                LE REPERE AXIAL                                        
C IN  : YINT   : COORDONNEES 'Y' DES CENTRES DES CYLINDRES DANS         
C                LE REPERE AXIAL                                        
C IN  : RINT   : RAYONS DES CYLINDRES                                   
C OUT : SGN    : -1 OU +1, COEFFICIENT INTERVENANT DANS LA DECOMPOSITION
C                EN SERIE DE LAURENT, SELON LE NIVEAU D IMAGE           
C OUT : ORIG   : NUMERO DU CYLINDRE D ORIGINE DES CYLINDRES REELS OU    
C                IMAGES                                                 
C OUT : BETA   : ANGLE CUMULE INTERVENANT DANS LA DECOMPOSITION EN      
C                SERIE DE LAURENT, POUR LES CYLINDRES IMAGES            
C ----------------------------------------------------------------------
      REAL*8       XSOM(4),YSOM(4)                                      
      REAL*8       XCENT,YCENT                                          
      REAL*8       X12,Y12,X23,Y23                                      
      REAL*8       ALPH12,ALPH23,LONG12,LONG23                          
      REAL*8       X0,Y0
      REAL*8       PI                                                   
      REAL*8       TRIGOM
C ----------------------------------------------------------------------
C                                                                       
C --- LECTURE DES DIMENSIONS                                            
      NBCYL  = NDIM(3)                                                  
      IENCEI = NDIM(6)                                                  
      NIMA   = NDIM(7)                                                  
      NIMA2  = NDIM(8)                                                  
      NBTOT  = NBCYL*(2*NIMA+1)*(2*NIMA+1)                              
C                                                                       
C                                                                       
      PI = R8PI()                                                       
C                                                                       
C --- INITIALISATIONS                                                   
C                                                                       
      DO 1 I = 1,NBTOT                                                  
         BETA(I) = 0.D0                                                 
         SGN(I) = 0                                                     
         ORIG(I) = 0                                                    
   1  CONTINUE                                                          
C                                                                       
C --- CONSTRUCTION DES IMAGES                                           
C                                                                       
      DO 2 I = 1,NBCYL                                                  
         ORIG(I) = I                                                    
         SGN(I) = 1                                                     
         BETA(I) = 0.0D0                                                
   2  CONTINUE                                                          
C                                                                       
      IF(IENCEI.EQ.2) THEN                                              
C                                                                       
         DO 3 I = 1,4                                                   
             XSOM(I) = SOM(2*I-1)                                       
             YSOM(I) = SOM(2*I)                                         
   3     CONTINUE                                                       
C                                                                       
C ---    DEFINITION DES DROITES DE SYMETRIES                            
C                                                                       
         X12 = XSOM(2)-XSOM(1)                                          
         Y12 = YSOM(2)-YSOM(1)                                          
         LONG12 = X12*X12+Y12*Y12                                       
C                                                                       
         X23 = XSOM(3)-XSOM(2)                                          
         Y23 = YSOM(3)-YSOM(2)                                          
         LONG23 = X23*X23+Y23*Y23                                       
C                                                                       
         IF (X12/SQRT(LONG12).GT.1.D0) THEN
            ALPH12=0.D0
         ELSE IF (X12/SQRT(LONG12).LT.-1.D0) THEN
            ALPH12=PI
         ELSE
            ALPH12 = TRIGOM('ACOS',X12/SQRT(LONG12))
         ENDIF
         IF (Y12.LT.0.D0) THEN                                          
            ALPH12 = PI-ALPH12                                          
         ENDIF                                                          
C                                                                       
         IF (X23/SQRT(LONG23).GT.1.D0) THEN
            ALPH23=0.D0
         ELSE IF (X23/SQRT(LONG23).LT.-1.D0) THEN
            ALPH23=PI
         ELSE
            ALPH23 = TRIGOM('ACOS',X23/SQRT(LONG23))
         ENDIF
         IF (Y23.LT.0.D0) THEN                                          
            ALPH23 = PI-ALPH23                                          
         ENDIF                                                          
C                                                                       
         XCENT = (XSOM(3)+XSOM(1))/2.D0                                 
         YCENT = (YSOM(3)+YSOM(1))/2.D0                                 
C                                                                       
      ENDIF                                                             
C                                                                       
      NJ = NBCYL                                                        
      X0 = XSOM(1)
      Y0 = YSOM(1)
C                                                                       
      DO 9 I = 1,NIMA                                                   
         DO 91 J = 1,NBCYL                                              
            NJ = NJ+1                                                   
            IF (I.EQ.1) THEN                                            
               NP = NJ-NBCYL
            ELSE                                                        
               NP = NJ-8*(I-1)*NBCYL
            ENDIF
            XINT(NJ) = XINT(NP)-2.D0*X23/LONG23*
     &                 (X23*(XINT(NP)-X0)+Y23*(YINT(NP)-Y0))
            YINT(NJ) = YINT(NP)-2.D0*Y23/LONG23*
     &                 (X23*(XINT(NP)-X0)+Y23*(YINT(NP)-Y0))
            BETA(NJ) = -BETA(NP)+2.D0*ALPH12                   
            BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI           
            SGN(NJ) = (-1)**I                                           
            RINT(NJ) = RINT(NP)                                         
            ORIG(NJ) = ORIG(NP)                                         
  91     CONTINUE                                                       
C                                                                       
         X0 = X0-X23
         Y0 = Y0-Y23
C
         DO 92 J = 1,I                                                  
            X0 = X0+X12
            Y0 = Y0+Y12
            DO 921 K = 1,NBCYL                                          
               NJ = NJ+1                                                
               NP = NJ-NBCYL
               XINT(NJ) = XINT(NP)-2.D0*X12/LONG12*
     &                    (X12*(XINT(NP)-X0)+Y12*(YINT(NP)-Y0))
               YINT(NJ) = YINT(NP)-2.D0*Y12/LONG12*
     &                    (X12*(XINT(NP)-X0)+Y12*(YINT(NP)-Y0))
               BETA(NJ) = -BETA(NP)+2.D0*ALPH23                   
               BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI        
               SGN(NJ) = -SGN(NP)                                 
               RINT(NJ) = RINT(NP)                                
               ORIG(NJ) = ORIG(NP)                                
 921        CONTINUE                                                    
  92     CONTINUE                                                       
C                                                                       
         X0 = X0+X12
         Y0 = Y0+Y12
C
         DO 93 J = 1,2*I                                                
            X0 = X0+X23
            Y0 = Y0+Y23
            DO 931 K = 1,NBCYL                                          
               NJ = NJ+1                                                
               NP = NJ-NBCYL
               XINT(NJ) = XINT(NP)-2.D0*X23/LONG23*
     &                    (X23*(XINT(NP)-X0)+Y23*(YINT(NP)-Y0))
               YINT(NJ) = YINT(NP)-2.D0*Y23/LONG23*
     &                    (X23*(XINT(NP)-X0)+Y23*(YINT(NP)-Y0))
               BETA(NJ) = -BETA(NP)+2.D0*ALPH12                   
               BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI        
               SGN(NJ) = -SGN(NP)                                 
               RINT(NJ) = RINT(NP)                                
               ORIG(NJ) = ORIG(NP)                                
 931        CONTINUE                                                    
  93     CONTINUE                                                       
C                                                                       
         X0 = X0+X23
         Y0 = Y0+Y23
C
         DO 94 J = 1,2*I                                                
            X0 = X0-X12
            Y0 = Y0-Y12
            DO 941 K = 1,NBCYL                                          
               NJ = NJ+1                                                
               NP = NJ-NBCYL
               XINT(NJ) = XINT(NP)-2.D0*X12/LONG12*
     &                    (X12*(XINT(NP)-X0)+Y12*(YINT(NP)-Y0))
               YINT(NJ) = YINT(NP)-2.D0*Y12/LONG12*
     &                    (X12*(XINT(NP)-X0)+Y12*(YINT(NP)-Y0))
               BETA(NJ) = -BETA(NP)+2.D0*ALPH23                   
               BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI        
               SGN(NJ) = -SGN(NP)                                 
               RINT(NJ) = RINT(NP)                                
               ORIG(NJ) = ORIG(NP)                                
 941        CONTINUE                                                    
  94     CONTINUE                                                       
C                                                                       
         X0 = X0-X12
         Y0 = Y0-Y12
C
         DO 95 J = 1,2*I                                                
            X0 = X0-X23
            Y0 = Y0-Y23
            DO 951 K = 1,NBCYL                                          
               NJ = NJ+1                                                
               NP = NJ-NBCYL
               XINT(NJ) = XINT(NP)-2.D0*X23/LONG23*
     &                    (X23*(XINT(NP)-X0)+Y23*(YINT(NP)-Y0))
               YINT(NJ) = YINT(NP)-2.D0*Y23/LONG23*
     &                    (X23*(XINT(NP)-X0)+Y23*(YINT(NP)-Y0))
               BETA(NJ) = -BETA(NP)+2.D0*ALPH12                   
               BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI        
               SGN(NJ) = -SGN(NP)                                 
               RINT(NJ) = RINT(NP)                                
               ORIG(NJ) = ORIG(NP)                                
 951        CONTINUE                                                    
  95     CONTINUE                                                       
C                                                                       
         X0 = X0-X23
         Y0 = Y0-Y23
C
         DO 96 J = 1,I-1                                                
            X0 = X0+X12
            Y0 = Y0+Y12
            DO 961 K = 1,NBCYL                                          
               NJ = NJ+1                                                
               NP = NJ-NBCYL
               XINT(NJ) = XINT(NP)-2.D0*X12/LONG12*
     &                    (X12*(XINT(NP)-X0)+Y12*(YINT(NP)-Y0))
               YINT(NJ) = YINT(NP)-2.D0*Y12/LONG12*
     &                    (X12*(XINT(NP)-X0)+Y12*(YINT(NP)-Y0))
               BETA(NJ) = -BETA(NP)+2.D0*ALPH23                   
               BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI        
               SGN(NJ) = -SGN(NP)                                 
               RINT(NJ) = RINT(NP)                                
               ORIG(NJ) = ORIG(NP)                                
 961        CONTINUE                                                    
  96     CONTINUE                                                       
C                                                                       
         X0 = X0+X12
         Y0 = Y0+Y12
C
   9  CONTINUE                                                          
C                                                                       
C                                                                       
      NJ = NBTOT                                                        
C                                                                       
      DO 10 I = 1,NIMA2                                                 
         NJ = NJ+1                                                      
         XINT(NJ) = XCENT-(NIMA+I)*X23                                  
         YINT(NJ) = YCENT-(NIMA+I)*Y23                                  
         IF (I.EQ.1) THEN                                               
            BETA(NJ) = -BETA(NJ-8*NIMA*NBCYL)+2.D0*ALPH12               
         ELSE                                                           
            BETA(NJ) = -BETA(NJ-8*(NIMA+I-1))+2.D0*ALPH12               
         ENDIF                                                          
         BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI              
         SGN(NJ) = (-1)**(NIMA+I)                                       
C                                                                       
         DO 101 J = 1,NIMA+I                                            
C                                                                       
            NJ = NJ+1                                                   
            XINT(NJ) = XINT(NJ-1)+X12                                   
            YINT(NJ) = YINT(NJ-1)+Y12                                   
            BETA(NJ) = -BETA(NJ-1)+2.D0*ALPH23                          
            BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI           
            SGN(NJ) = -SGN(NJ-1)                                        
 101     CONTINUE                                                       
C                                                                       
         DO 102 J = 1,2*(NIMA+I)                                        
            NJ = NJ+1                                                   
            XINT(NJ) = XINT(NJ-1)+X23                                   
            YINT(NJ) = YINT(NJ-1)+Y23                                   
            BETA(NJ) = -BETA(NJ-1)+2.D0*ALPH12                          
            BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI           
            SGN(NJ) = -SGN(NJ-1)                                        
 102     CONTINUE                                                       
C                                                                       
         DO 103 J = 1,2*(NIMA+I)                                        
            NJ = NJ+1                                                   
            XINT(NJ) = XINT(NJ-1)-X12                                   
            YINT(NJ) = YINT(NJ-1)-Y12                                   
            BETA(NJ) = -BETA(NJ-1)+2.D0*ALPH23                          
            BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI           
            SGN(NJ) = -SGN(NJ-1)                                        
 103     CONTINUE                                                       
C                                                                       
         DO 104 J = 1,2*(NIMA+I)                                        
            NJ = NJ+1                                                   
            XINT(NJ) = XINT(NJ-1)-X23                                   
            YINT(NJ) = YINT(NJ-1)-Y23                                   
            BETA(NJ) = -BETA(NJ-1)+2.D0*ALPH12                          
            BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI           
            SGN(NJ) = -SGN(NJ-1)                                        
 104     CONTINUE                                                       
C                                                                       
         DO 105 J = 1,NIMA+I-1                                          
            NJ = NJ+1                                                   
            XINT(NJ) = XINT(NJ-1)+X12                                   
            YINT(NJ) = YINT(NJ-1)+Y12                                   
            BETA(NJ) = -BETA(NJ-1)+2.D0*ALPH23                          
            BETA(NJ) = BETA(NJ)-INT(BETA(NJ)/2.D0/PI)*2.D0*PI           
            SGN(NJ) = -SGN(NJ-1)                                        
 105     CONTINUE                                                       
                                                                        
  10  CONTINUE                                                          
C                                                                       
C                                                                       
      END 
