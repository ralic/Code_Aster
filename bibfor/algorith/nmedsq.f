      SUBROUTINE NMEDSQ(SG,QG,DSDUG,D,NPG,TYPMOD,IMATE,BUM,
     &                     BDU,SIGN,VIM,OPTION,GEOM,NNO,LGPG,KPG,DEF)
     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2005   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
     
      INTEGER       NNO, NPG, KPG, IMATE, LGPG      
      REAL*8        SG(2),QG(2,*),DSDUG(2,8),D(4,*),SIGN(*)
      REAL*8        GEOM(2,NNO)
      REAL*8        VIM(LGPG,NPG)
      REAL*8        BUM(6),BDU(6),DEF(4,NNO,2)
      CHARACTER*8   TYPMOD(*)
      CHARACTER*16  OPTION
         
C------------------------------------------------------------------
C  CALCUL DU VECTEUR S, DE SA DERIVEE PAR RAPPORT AU DEPL (DSDUG) 
C  ET DE LA MATRICE Q AU POINT DE GAUSS COURANT POUR L'ELEMENT A 
C  DISCONTINUITE INTERNE AFIN DE RESOUDRE LE SYSTEME NON LINEAIRE 
C  SUIVANT   :   S+Q.ALPHA = VECT_SIG(ALPHA)
C------------------------------------------------------------------
C
C  IN : D,NPG,TYPMOD,IMATE,BUM,BDU,SIGN,VIM,OPTION,GEOM,NNO,LGPG
C       KPG,DEF
C OUT : SG , QG , DSDUG 
C     
C------------------------------------------------------------------

      INTEGER I,J,K,KL,N
      REAL*8  EPSM(6),DEPS(6),SIG(6)
      REAL*8  LONG,DA,DDA,ALPHAP(2),ALPHAM(2)
      REAL*8  MTEMP(4,8),DSIDEP(6,6)
      REAL*8  XA,XB,YA,YB
      LOGICAL AXI
 
      AXI  = TYPMOD(1) .EQ. 'AXIS'
            
      CALL R8INIR(2 , 0.D0, SG,1)
      CALL R8INIR(4 , 0.D0, QG,1)
      CALL R8INIR(6 , 0.D0, EPSM,1)
      CALL R8INIR(16, 0.D0, DSDUG,1)
      CALL R8INIR(32, 0.D0, MTEMP,1)
      CALL R8INIR(36, 0.D0, DSIDEP,1)
      
      
      ALPHAM(1) = VIM(1,KPG)
      ALPHAM(2) = VIM(2,KPG) 
      
C SOIT A ET B LES MILIEUX DES COTES [14] ET [23]  
C t TANGENT AU COTE [AB]
    
      XA = ( GEOM(1,1) + GEOM(1,4) ) / 2
      YA = ( GEOM(2,1) + GEOM(2,4) ) / 2
      
      XB = ( GEOM(1,2) + GEOM(1,3) ) / 2
      YB = ( GEOM(2,2) + GEOM(2,3) ) / 2

C     LONGUEUR DE L'ELEMENT : NORME DU COTE [AB]   
      LONG = SQRT( (XA-XB)*(XA-XB) + (YA-YB)*(YA-YB) )      
      IF (AXI) LONG = LONG * (XA + XB)/2.D0     
                                 
      
C CALCUL DE SG :
C -------------      
          
      ALPHAP(1) = 0.D0
      ALPHAP(2) = 0.D0            
      DO 80 I=1,4
        DA = 0.D0
        DDA = 0.D0
        DO 70 J=1,2 
          DA  = DA  + D(I,J)*ALPHAM(J) 
          DDA = DDA + D(I,J)*(ALPHAP(J)-ALPHAM(J))
 70     CONTINUE
        EPSM(I) =  BUM(I) +  DA
        DEPS(I) =  BDU(I) +  DDA
 80   CONTINUE

      CALL R8INIR(6 , 0.D0, SIG,1)
      CALL NMEDEL(2,TYPMOD,IMATE,DEPS,SIGN,OPTION,SIG,DSIDEP)

      DO 40 I=1,2
        DO 50 KL=1,4       
          SG(I) = SG(I) - D(KL,I)*SIG(KL)/LONG  
 50     CONTINUE
 40   CONTINUE  


C CALCUL DE DSDUG :
C ---------------
C DERIVEE DE SG PAR RAPPORT AU DEPL, NECESSAIRE 
C POUR LE CALCUL DE LA MATRICE TANGENTE  (DSDUG = Dt DSIDEP DEF / LONG)

      
      DO 41 K=1,4
      
        DO 51 N=1,NNO       
        DO 52 I=1,2
        
          KL=2*(N-1)+I 
          DO 53 J=1,4
            MTEMP(K,KL) = MTEMP(K,KL) + DSIDEP(K,J)*DEF(J,N,I)        
 53       CONTINUE 
                
 52     CONTINUE
 51     CONTINUE
 
 41   CONTINUE        


      DO 42 I=1,2
        DO 43 J=1,8
          DO 44 KL=1,4       
             DSDUG(I,J) = DSDUG(I,J) - D(KL,I)*MTEMP(KL,J)/LONG
 44       CONTINUE     
 43     CONTINUE
 42   CONTINUE  

 
C CALCUL DE QG :
C -------------       

C     PREMIERE COLONNE :
                                    
      ALPHAP(1) = 1.D0
      ALPHAP(2) = 0.D0
      DO 81 I=1,4
        DA = 0.D0
        DDA = 0.D0
        DO 71 J=1,2 
          DA  = DA  + D(I,J)*ALPHAM(J) 
          DDA = DDA + D(I,J)*(ALPHAP(J)-ALPHAM(J))
 71     CONTINUE
        EPSM(I) =  BUM(I) +  DA
        DEPS(I) =  BDU(I) +  DDA
 81   CONTINUE

      CALL R8INIR(6 , 0.D0, SIG,1)
      CALL NMEDEL(2,TYPMOD,IMATE,DEPS,SIGN,OPTION,SIG,DSIDEP)      

      DO 100 I=1,2
        DO 110 KL=1,4      
          QG(I,1) = QG(I,1) - D(KL,I)*SIG(KL)/LONG
 110    CONTINUE
        QG(I,1) = QG(I,1) - SG(I)            
 100  CONTINUE 


C     DEUXIEME COLONNE :
                         
      ALPHAP(1) = 0.D0
      ALPHAP(2) = 1.D0
      DO 82 I=1,4
        DA = 0.D0
        DDA = 0.D0
        DO 72 J=1,2 
          DA  = DA  + D(I,J)*ALPHAM(J) 
          DDA = DDA + D(I,J)*(ALPHAP(J)-ALPHAM(J))          
 72     CONTINUE
        EPSM(I) =  BUM(I) +  DA
        DEPS(I) =  BDU(I) +  DDA
 82   CONTINUE
 
      CALL R8INIR(6 , 0.D0, SIG,1)
      CALL NMEDEL(2,TYPMOD,IMATE,DEPS,SIGN,OPTION,SIG,DSIDEP)

      DO 160 I=1,2
        DO 170 KL=1,4    
          QG(I,2) = QG(I,2) - D(KL,I)*SIG(KL)/LONG
 170    CONTINUE
        QG(I,2) = QG(I,2) - SG(I)            
 160  CONTINUE 


C ON IMPOSE SG=0 SI RIGI_MECA_TANG, QG LUI N'EST PAS 
C NUL POUR RIGI_MECA_TANG CAR INDEP DE SIG    
 
      IF ( OPTION .EQ. 'RIGI_MECA_TANG' ) THEN 
        CALL R8INIR(2 , 0.D0, SG,1)     
      ENDIF

      END
