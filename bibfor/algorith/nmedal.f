      SUBROUTINE NMEDAL(ALPHAP,SIGMC,GC,S,Q,SEUIL)
      
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
      
      REAL*8 ALPHAP(2),SEUIL
      REAL*8 S(2),Q(2,2)
      REAL*8 SIGMC,GC 
           
C----------------------------------------------------------------
C   CALCUL DU SAUT ALPHA DANS l'ELEMENT A DISCONTINUITE INTERNE :
C   RESOLUTION DU SYSTEME NON LINEAIRE S+Q.ALPHA=VECT_SIG(ALPHA)
C  
C IN  : S,Q,SEUIL,SIGMC ET GC
C OUT : ALPHAP (SAUT A L'INSTANT +)
C  
C----------------------------------------------------------------- 
           
      LOGICAL CRITG,CRITD 
      INTEGER I,J,K    
      REAL*8  X,XP,FX,DFX,GX,DGX,NORMS,NORMA,ETA,DET
      REAL*8  P(2,2),VALP(2),SP(2),PA(2),QVECT(3)
      REAL*8  TEMP1,TEMP2,TEMP3,TEMP4,TEMPAL(2),MAT(3),SIGN
      
C INITIALISATIONS :
            
      CALL R8INIR(2, 0.D0, ALPHAP,1)   
      ETA   = 1.D-12            
      NORMS = SQRT( S(1)*S(1) + S(2)*S(2) )
      CRITG = (S(1).LE.0.D0) .AND. (ABS(S(2)).LE.SIGMC)      
      CRITD = (S(1).GT.0.D0) .AND. (NORMS .LE. SIGMC)

C        *****************************                  
C     I. CAS DANS CRITERE ET SEUIL NUL
C        *****************************    

      IF ((SEUIL.EQ.0.D0).AND.(CRITG .OR. CRITD))THEN
        ALPHAP(1) = 0.D0
        ALPHAP(2) = 0.D0         
        GOTO 999
      ENDIF
      
C         **********************    
C     II. CAS DECHARGE ELASTIQUE
C         **********************   
      
      IF (SEUIL.NE.0.D0) THEN
                
C       ON SUPPOSE QUE ALPHAP1=0, ON CALCUL ALPHAP2 (TEMPAL(2)) :

        CALL R8INIR(2, 0.D0,TEMPAL ,1)      
        TEMPAL(2) = S(2)/(-Q(2,2)+
     &         SIGMC*EXP(-SIGMC*SEUIL/GC)/SEUIL) 
              
        NORMA = ABS(TEMPAL(2))
        SIGN  = S(1) + Q(1,2)*TEMPAL(2)
        
C     SI SIGMA NORMALE EST <=0 ET QU'ON EST SOUS LE SEUIL      
        IF ( ((SIGN).LE.1.D-10) .AND. (NORMA.LT.SEUIL) )THEN 
          ALPHAP(1) = 0.D0
          ALPHAP(2) = TEMPAL(2)       
          GOTO 999 
        ENDIF    

C       ON SUPPOSE QUE ALPHA1.NE.0, ON CALCUL ALPHAP1 et ALPHAP2 :
C       RESOLTION DE (V.ID-Q)ALPHAP=S AVEC 
C       V = EXP(-SIGMC*NORMA/GC)/NORMA . ON MET (vID-Q) DANS MAT, 

        CALL R8INIR(3, 0.D0,MAT ,1)
        MAT(1) = SIGMC*EXP(-SIGMC*SEUIL/GC)/SEUIL - Q(1,1)
        MAT(2) = SIGMC*EXP(-SIGMC*SEUIL/GC)/SEUIL - Q(2,2)
        MAT(3) = -Q(1,2)
  
        DET = MAT(1)*MAT(2)-MAT(3)*MAT(3)

        IF (ABS(DET).LE.1.D-12) 
     &  CALL UTMESS('F','NMEDAL','MATRICE NON INVERSIBLE')
       
        CALL R8INIR(2, 0.D0,TEMPAL ,1)
        TEMPAL(1) =   ( MAT(2)*S(1) - MAT(3)*S(2)) / DET
        TEMPAL(2) =   (-MAT(3)*S(1) + MAT(1)*S(2)) / DET  

        NORMA = SQRT( TEMPAL(1)**2 + TEMPAL(2)**2 )

        IF (NORMA.LT.SEUIL) THEN       
          ALPHAP(1) = TEMPAL(1)
          ALPHAP(2) = TEMPAL(2)
          GOTO 999
        ENDIF   
                                         
      ENDIF
      
C        **********************    
C    III. CAS SORTIE DU CRITERE
C        **********************  

C       1) ON SUPPOSE ALPHAP1.EQ.0, CALCUL DE ALPHAP2
C          --------------------------------------------

C     TESTS D'EXISTANCE ET D'UNICITE D'UNE SOLUTION
      IF ( Q(2,2).GT.0.D0 )
     &  CALL UTMESS('F','NMEDAL','PAS EXISTANCE DE SOLUTION 
     &                                        POUR LE SAUT')

      IF ( ABS(Q(2,2)).LT.(SIGMC*SIGMC/GC) )
     &  CALL UTMESS('F','NMEDAL','EXISTANCE D''UN ELEMENT A 
     &                            DISCONTINUITE TROP GRAND 
     &                            NON UNICITE DU SAUT')
      
C     CALCUL DE ALPHAP(2) PAR NEWTON      
      IF ( S(2).GT.SIGMC ) THEN 
               
        K=0           
        XP=0.D0      
 2000   CONTINUE     
                            
        X = XP         
        FX  = S(2) + Q(2,2)*X - SIGMC*EXP(-SIGMC*ABS(X)/GC)
        DFX = Q(2,2) + SIGMC*SIGMC*EXP(-SIGMC*ABS(X)/GC)/GC
        XP = X - FX/DFX
        K=K+1
        
        IF ((ABS(XP-X).GE.1.D10).OR.(K.GT.3000))
     &  CALL UTMESS('F','NMEDAL','NON CONVERGENCE DU NEWTON
     &                            POUR LE CALCUL DU SAUT NO1')
     
        IF (ABS(XP-X).LE.ETA) GOTO 1000
        GOTO 2000      
 1000   CONTINUE       
            
      ELSEIF ( S(2).LT.-SIGMC ) THEN    
      
        K=0
        XP=0.D0      
 3000   CONTINUE  
    
        X = XP        
        FX  = S(2) + Q(2,2)*X + SIGMC*EXP(-SIGMC*ABS(X)/GC)
        DFX = Q(2,2) - SIGMC*SIGMC*EXP(-SIGMC*ABS(X)/GC)/GC     
        XP = X - FX/DFX
        K=K+1
        
        IF ((ABS(XP-X).GE.1.D10).OR.(K.GT.3000))
     &  CALL UTMESS('F','NMEDAL','NON CONVERGENCE DU NEWTON
     &                            POUR LE CALCUL DU SAUT NO2')
                       
        IF ( ABS(XP-X).LE.ETA ) GOTO 4000
        GOTO 3000      
 4000   CONTINUE
          
      ENDIF 
       
C     ON VERIFIE SI XP A ETE CALCULE DANS UN DES DEUX CAS PRECEDENTS
      IF ( (S(2).GT.SIGMC) .OR. (S(2).LT.-SIGMC) ) THEN
                                  
        NORMA = ABS(XP)  
        SIGN  = S(1) + Q(1,2)*XP      

C       SI SIGMA NORMALE EST <=0 ET QU'ON EST AU DELA DU SEUIL ALORS
        IF ( ((SIGN).LE.1.D-10) .AND. (NORMA.GE.SEUIL) ) THEN
          ALPHAP(1) = 0.D0
          ALPHAP(2) = XP       
          GOTO 999 
        ENDIF
        
      ENDIF

C       2) ON SUPPOSE ALPHAP(1).NE.0, CALCUL DE ALPHAP(1) ET ALPHAP(2)
C          -----------------------------------------------------------

C ECRITURE DE Q EN VECTEUR : QVECT(3)= (Q(1,1)  ,  Q(2,2)  ,  Q(1,2))
      QVECT(1) = Q(1,1)
      QVECT(2) = Q(2,2)
      QVECT(3) = Q(1,2)
      
C     DIAGONALISATION DE Q : 
      CALL R8INIR(4, 0.D0, P,1)
      CALL R8INIR(2, 0.D0, VALP,1)      
      CALL DIAGO2(QVECT,P,VALP)

C     S EXPRIME DANS LA BASE PROPRE P DE Q :
      CALL R8INIR(2, 0.D0, SP,1) 
      DO 10 I=1,2              
        DO 20 J=1,2
          SP(I) = SP(I) + P(J,I)*S(J)
  20    CONTINUE  
  10  CONTINUE  
    
C     CALCUL DE ALPHAP(1) ET ALPHAP(2) PAR NEWTON :

      K=0
      XP=0.D0    
 5000 CONTINUE  
    
      X = XP
               
      GX  = SIGMC*EXP(-SIGMC*X/GC) 
      DGX =  - (SIGMC*SIGMC/GC)*EXP(-SIGMC*X/GC)  
              
      TEMP1 = SP(1)/(GX-VALP(1)*X)
      TEMP2 = SP(2)/(GX-VALP(2)*X)
      TEMP3 = (DGX-VALP(1))/(GX-X*VALP(1))
      TEMP4 = (DGX-VALP(2))/(GX-X*VALP(2)) 
        
      FX  = 1.D0 - TEMP1*TEMP1 - TEMP2*TEMP2 
      DFX = 2*(TEMP1*TEMP1*TEMP3 + TEMP2*TEMP2*TEMP4)       
      XP = X - FX/DFX
      K=K+1
      
      IF ((ABS(XP-X).GE.1.D10).OR.(K.GT.3000))
     &  CALL UTMESS('F','NMEDAL','NON CONVERGENCE DU NEWTON
     &                            POUR LE CALCUL DU SAUT NO3')
                    
      IF ( ABS(XP-X).LE.ETA ) GOTO 6000
      GOTO 5000      
 6000 CONTINUE 

      PA(1) = SP(1)/( (SIGMC/XP)*EXP(-SIGMC*XP/GC) - VALP(1) )
      PA(2) = SP(2)/( (SIGMC/XP)*EXP(-SIGMC*XP/GC) - VALP(2) )
      
C    ON REMET LA SOLUTION DS LA BONNE BASE ET ON LA STOCKE DANS TEMPAL

      CALL R8INIR(2, 0.D0,TEMPAL ,1)
      DO 30 I=1,2
        DO 40 J=1,2
          TEMPAL(I) = TEMPAL(I) + P(I,J)*PA(J)
  40    CONTINUE
  30  CONTINUE

      NORMA = SQRT( TEMPAL(1)**2 + TEMPAL(2)**2 )
      
C     SI ON EST AU DELA DU SEUIL :   
      IF (NORMA.GT.SEUIL) THEN
                       
        ALPHAP(1) = TEMPAL(1)
        ALPHAP(2) = TEMPAL(2)
        GOTO 999        
        
      ENDIF 

C     ON EST PASSE DANS AUCUN TEST :
      CALL UTMESS('F','NMEDAL','ERREUR DANS LE CALCUL DU SAUT')
           
 999  CONTINUE   


      END           
