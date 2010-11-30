      SUBROUTINE MEFINT(NBZ,NBGRP,NBMOD,NBNOE,NBDDL,IROT,NUMNOG,  
     &                  NBNOG,ZINT,DEFM,PHIX,PHIY,Z,NUM)                
      IMPLICIT REAL*8 (A-H,O-Z)                                         
C                                                                       
      INTEGER     NBZ,NBGRP,NBMOD,NBNOE,NBDDL                     
      INTEGER     IROT(3),NUMNOG(NBGRP),NBNOG(NBGRP),NUM(NBZ)           
      REAL*8      ZINT(NBZ,NBGRP),DEFM(6*NBNOE,NBMOD),Z(*)              
      REAL*8      PHIX(NBZ,NBGRP,NBMOD),PHIY(NBZ,NBGRP,NBMOD)           
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C     INTERPOLATION DES DEFORMEES MODALES POUR TOUS LES CYLINDRES       
C     EQUIVALENTS (OU REELS, SI IL N'Y A PAS DE GROUPE D EQUIVALENCE)   
C     AUX POINTS COMMUNS DE DISCRETISATION, DONT ON CREE LE TABLEAU DES 
C     COTES: Z(I=1,NBZ).                                                
C     OPERATEUR APPELANT : OP0144 , FLUST3                              
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE    
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"                                   
C ----------------------------------------------------------------------
C IN  : NBZ    : NOMBRE DE NOEUDS DE LA DISCRETISATION AXIALE
C IN  : NBGRP  : NOMBRE DE GROUPES D EQUIVALENCE                        
C IN  : NBMOD  : NOMBRE DE MODES                                  
C IN  : NBNOE  : NOMBRE DE NOEUDS DU MAILLAGE                           
C IN  : NBDDL  : NOMBRE DE DEGRES DE LIBERTE                            
C IN  : IROT   : INDICE DE PERMUTATION CIRCULAIRE DU CHANGEMENT DE      
C                REPERE                                                 
C IN  : NUMNOG : TABLEAU DES ADRESSES DES NUMEROS DES NOEUDS DES        
C                CYLINDRES                                              
C IN  : NBNOG  : TABLEAU DED NOMBRED DE NOEUDS DE CHAQUE CYLINDRE       
C IN  : ZINT   : COORDONNEES 'Z' DANS LE REPERE AXIAL DES               
C                NOEUDS DES CYLINDRES                                   
C IN  : DEFM   : DEFORMEES MODALES DES NOEUDS DES CYLINDRES EQUIVALENTS 
C OUT : PHIX   : TABLEAU DES DEFORMEES MODALES INTERPOLEES DANS LA      
C                DIRCTION 'X' DU REPERE AXIAL                           
C OUT : PHIY   : TABLEAU DES DEFORMEES MODALES INTERPOLEES DANS LA      
C                DIRCTION 'Y' DU REPERE AXIAL                           
C OUT : Z      : COORDONNEES 'Z' DANS LE REPERE AXIAL DES               
C                POINTS DISCRETISES, (IDENTIQUES POUR TOUS LES CYLINDRES
C --  : NUM    : TABLEAU DE TRAVAIL, INDICES POUR LE CLASSEMENT PAR     
C                ORDRE CROISSANT SUIVANT Z, DES NOEUDS DES CYLINDRES    
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI                                               
      COMMON  /IVARJE/ ZI(1)                                            
      REAL*8           ZR                                               
      COMMON  /RVARJE/ ZR(1)                                            
      COMPLEX*16       ZC                                               
      COMMON  /CVARJE/ ZC(1)                                            
      LOGICAL          ZL                                               
      COMMON  /LVARJE/ ZL(1)                                            
      CHARACTER*8      ZK8                                              
      CHARACTER*16            ZK16                                      
      CHARACTER*24                    ZK24                              
      CHARACTER*32                            ZK32                      
      CHARACTER*80                                    ZK80              
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)           
      CHARACTER*32     JEXNUM, JEXNOM                                   
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C ----------------------------------------------------------------------
      INTEGER      I,J                                                  
C ----------------------------------------------------------------------
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C --- DETERMINATION DES COTES MAXIMALE ET MINIMALE DE L ENSEMBLE DES    
C --- CYLINDRES                                                         
      ZMAX = ZINT(1,1)                                                  
      ZMIN = ZINT(1,1)                                                  
      DO 10 J = 2,NBNOG(1)                                              
         IF(ZINT(J,1).GT.ZMAX) ZMAX = ZINT(J,1)                         
         IF(ZINT(J,1).LT.ZMIN) ZMIN = ZINT(J,1)                         
  10  CONTINUE                                                          
      DO 30 I = 2,NBGRP                                                 
         WMAX = ZINT(1,I)                                               
         WMIN = ZINT(1,I)                                               
         DO 20 J = 2,NBNOG(I)                                           
            IF(ZINT(J,I).GT.WMAX) WMAX = ZINT(J,I)                      
            IF(ZINT(J,I).LT.WMIN) WMIN = ZINT(J,I)                      
  20     CONTINUE                                                       
         IF(WMIN.GT.ZMIN) ZMIN = WMIN                                   
         IF(WMAX.LT.ZMAX) ZMAX = WMAX                                   
  30  CONTINUE                                                          
      DO 40 I = 1,NBZ                                                   
          Z(I) = ZMIN + (ZMAX-ZMIN)*(I-1)/(NBZ-1)                       
  40  CONTINUE                                                          
C                                                                       
C                                                                       
C --- INTERPOLATION DES DEFORMEES MODALES                               
C                                                                       
C --- DEBUT BES BOUCLES SUR LES CYLINDRES OU GROUPES DE CYLINDRES       
      DO 200 I = 1,NBGRP                                                
C                                                                       
C ---    CLASSEMENT POUR LE CYLINDRE I DES NOEUDS PAR ORDRE DE COTE     
         ICOMP = 0                                                      
         DO 115 J = 1,NBNOG(I)                                          
            NUM(J) = J                                                  
 115     CONTINUE                                                       
         DO 120 J = 1,NBNOG(I)                                          
            Z0 = ZINT(NUM(J),I)                                         
            IND = J                                                     
            ICOMP = ICOMP + 1                                           
            DO 100 K = ICOMP+1,NBNOG(I)                                 
               IF(Z0.GT.ZINT(NUM(K),I)) THEN                            
                  Z0 = ZINT(NUM(K),I)                                   
                  IND = K                                               
               ENDIF                                                    
 100        CONTINUE                                                    
            IF(IND.NE.J) THEN                                           
               NN = NUM(IND)                                            
               DO 110 K = 1,(IND-ICOMP)                                 
                  NUM(IND-K+1) = NUM(IND-K)                             
 110           CONTINUE                                                 
               NUM(ICOMP) = NN                                          
            ENDIF                                                       
 120     CONTINUE                                                       
C                                                                       
C ---    BOUCLE SUR LES POINTS DE DISCRETISATION DU CYLINDRE I          
         DO 190 J = 1,NBZ                                               
C ---       RECHERCHE DU NOEUDS REEL LE PLUS PROCHE DU POINT DE         
C ---       DISCRETISATION DE COTE J                                    
            DO 130 K = 1,NBNOG(I)                                       
               IF(ZINT(NUM(K),I).GT.Z(J)) THEN                          
                  IF(K.GT.1) THEN                                       
                      IND1 = NUM(K-1)                                   
                      IND2 = NUM(K)                                     
                  ELSE                                                  
                      IND1 = NUM(K)                                     
                      IND2 = NUM(K+1)                                   
                  ENDIF                                                 
                  GOTO 140                                              
               ENDIF                                                    
 130        CONTINUE                                                    
            IND1 = NUM(NBNOG(I)-1)                                      
            IND2 = NUM(NBNOG(I))                                        
 140        CONTINUE                                                    
C                                                                       
            NNO1 = ZI(NUMNOG(I)+IND1-1)                                 
            NNO2 = ZI(NUMNOG(I)+IND2-1)                                 
C                                                                       
C ---       INTERPOLATION DES DEFORMEES MODALES                         
C ---       DEBUT BES BOUCLES SUR LES MODES                             
            DO 150 NM = 1,NBMOD                                         
               PHIX(J,I,NM) = DEFM(NBDDL*(NNO1-1)+IROT(1),NM)           
     &            +( DEFM(NBDDL*(NNO2-1)+IROT(1),NM)                    
     &             - DEFM(NBDDL*(NNO1-1)+IROT(1),NM) )                  
     &            *( Z(J)-ZINT(IND1,I) ) / (ZINT(IND2,I)-ZINT(IND1,I) ) 
               PHIY(J,I,NM) = DEFM(NBDDL*(NNO1-1)+IROT(2),NM)           
     &            +( DEFM(NBDDL*(NNO2-1)+IROT(2),NM)                    
     &             - DEFM(NBDDL*(NNO1-1)+IROT(2),NM) )                  
     &            *( Z(J)-ZINT(IND1,I) ) / (ZINT(IND2,I)-ZINT(IND1,I) ) 
C                                                                       
C                                                                       
C ---       FIN BES BOUCLES SUR LES MODES                               
 150        CONTINUE                                                    
C                                                                       
C ---    FIN BES BOUCLES SUR LES POINTS DE DISCRETISATION               
 190     CONTINUE                                                       
C                                                                       
C --- FIN BES BOUCLES SUR LES CYLINDRES                                 
 200  CONTINUE                                                          
C                                                                       
C                                                                       
      END 
