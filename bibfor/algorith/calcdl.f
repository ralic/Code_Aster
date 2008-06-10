      SUBROUTINE CALCDL(VP,I1E,SIGEQE,NBMAT,MATERF,PARAME,DERIVE,SIG3,
     &                 VECP,ETA,DG,SE,DETADG,DGDL,DDLDE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2005   AUTEUR JMBHH01 J.M.PROIX 
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
C =====================================================================
      IMPLICIT NONE
      INTEGER   NBMAT
      REAL*8   DDLDE(6),VECP(3,3),MATERF(NBMAT,2),PARAME(4),DERIVE(5)
      REAL*8   VP(3),I1E,SIGEQE,ETA,SE(6),DG,DETADG,SIG3,DGDL
C ======================================================================
C --- LOI DE HOEK BROWN : CALCUL DE DDLAMBDA/DEPS ---------------------
C ======================================================================
C IN  SE      DEVIATEUR ELASTIQUE --------------------------------------
C IN  VP      VALEURS PROPRES DU DEVIATEUR ELASTIQUE SE ----------------
C IN  I1E     TRACE DE SE ----------------------------------------------
C IN  NBMAT   NOMBRE DE DONNEES MATERIAU -------------------------------
C IN  MATERF  DONNEES MATERIAU -----------------------------------------
C IN  PARAME  VALEUR DES PARAMETRES DE LA LOI S*SIG, M*SIG, B ----------
C IN  DERIVE  VALEUR DES DERIVEES DES PARAMETRES PAR RAPPORT A GAMMA ---
C IN  SIG3    CONTRAINTE PRINCIPALE SIG3 -------------------------------
C IN  VECP    VECTEURS PROPRES DE SE -----------------------------------
C IN  DG      INCREMENT DU PARAMETRE D ECROUISSAGE GAMMA ---------------
C IN  DETADG  DERIVEE DE ETA PAR RAPPORT A GAMMA -----------------------
C IN  DGDL    DERIVEE  DE GAMMA PAR RAPPORT A LAMBDA -------------------
C OUT DDLDE   DDLAMDA/DEPS ---------------------------------------------
C ======================================================================
      REAL*8   DSDDE(6,6),UN,DEUX,TROIS,K,DL,MU
      REAL*8   DA1DE(6),DA2DE(6),DA3DE(6),DA6DE(6)
      REAL*8   A2,A3,A4,C5,A6,AUX1,AUX2,AUX3,DENOM,AUX4
      INTEGER  II,NDT,NDI,I,J,JJ
C =================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      CALL LCINMA(0.0D0,DSDDE)  
      MU = MATERF(4,1)
      K  = MATERF(5,1)
C ======================================================================
      A2 = VP(3)-VP(1)    
      A3 = TROIS*MU/SIGEQE           
      A4 = TROIS*K*ETA 
      C5 = UN/MATERF(14,2)     
      A6 = A3*VP(3)   
      DL = DG/(ETA+UN)      
C ======================================================================
C --- CALCUL DE DSD/DE ------------------------------------------------
C ======================================================================
      DO 40 II = 1, NDI
        DO 50 JJ = 1, NDI
            DSDDE(II,JJ) = DEUX*MU*VECP(II,JJ)*VECP(II,JJ)
     &      -DEUX*MU*(VECP(1,II)**2+VECP(2,II)**2+VECP(3,II)**2)/TROIS
 50     CONTINUE
         DSDDE(II,4) = DEUX*MU*VECP(II,1)*VECP(II,2)
     &             -DEUX*MU*(VECP(1,1)*VECP(1,2)+VECP(2,1)*VECP(2,2)
     &                +VECP(3,1)*VECP(3,2))/TROIS
        IF (NDT.EQ.6) THEN        
           DSDDE(II,5) = DEUX*MU*VECP(II,1)*VECP(II,3)
     &             -DEUX*MU*(VECP(1,1)*VECP(1,3)+VECP(2,1)*VECP(2,3)
     &                +VECP(3,1)*VECP(3,3))/TROIS           
           DSDDE(II,6) = DEUX*MU*VECP(II,2)*VECP(II,3)
     &             -DEUX*MU*(VECP(1,3)*VECP(1,2)+VECP(2,3)*VECP(2,2)
     &                +VECP(3,3)*VECP(3,2))/TROIS           
       ENDIF
 40   CONTINUE
      DO 55 JJ = 1, NDI
           DSDDE(4,JJ) = DEUX*MU*VECP(1,JJ)*VECP(2,JJ)
 55   CONTINUE
      DSDDE(4,4) = DEUX*MU*VECP(1,1)*VECP(2,2)
      IF (NDT.EQ.6) THEN
           DO 60 JJ = 1, NDI
             DSDDE(5,JJ) = DEUX*MU*VECP(1,JJ)*VECP(3,JJ)
             DSDDE(6,JJ) = DEUX*MU*VECP(2,JJ)*VECP(3,JJ)
 60        CONTINUE        
           DSDDE(4,5) = DEUX*MU*VECP(1,1)*VECP(2,3)
           DSDDE(4,6) = DEUX*MU*VECP(1,2)*VECP(2,3)        
           DSDDE(5,4) = DEUX*MU*VECP(1,1)*VECP(3,2)
           DSDDE(5,5) = DEUX*MU*VECP(1,1)*VECP(3,3)              
           DSDDE(5,6) = DEUX*MU*VECP(1,2)*VECP(3,3)
           DSDDE(6,4) = DEUX*MU*VECP(2,1)*VECP(3,2)           
           DSDDE(6,5) = DEUX*MU*VECP(2,1)*VECP(3,3)           
           DSDDE(6,6) = DEUX*MU*VECP(2,2)*VECP(3,3)           
      ENDIF
C ====================================================================
C --- ON TRAITE LE CAS DE DEUX VALEURS PROPRES EGALES ----------------
C ====================================================================
      IF ((ABS(VP(3)-VP(2)).LT.1.D-8).OR.
     +   (ABS(VP(3)-VP(2)).LT.(MAX(VP(3),VP(2))*1.D-8)))  THEN
          DO 153 II=1,3
           AUX1 = DSDDE(II,2)+DSDDE(II,3)
           DSDDE(II,2) = 0.5D0*AUX1
           DSDDE(II,3) = 0.5D0*AUX1        
 153      CONTINUE
      ENDIF 
      IF ((ABS(VP(1)-VP(2)).LT.1.D-8).OR.
     +   (ABS(VP(1)-VP(2)).LT.(MAX(VP(1),VP(2))*1.D-8)))  THEN
          DO 154 II=1,3
           AUX1 = DSDDE(II,2)+DSDDE(II,1)
           DSDDE(II,2) = 0.5D0*AUX1
           DSDDE(II,1) = 0.5D0*AUX1        
 154      CONTINUE
      ENDIF                     
C =====================================================================
      DO 70 II=1,NDT
           DA1DE(II) = DSDDE(II,3)   
 70   CONTINUE        
      DO 80 II=1,NDI
           DA1DE(II) = DA1DE(II) + K   
 80   CONTINUE                                 
      DO 75 II=1,NDT
           DA2DE(II) = DSDDE(II,3)-DSDDE(II,1)   
 75   CONTINUE                    
      DO 85 II=1,NDT
           DA3DE(II) = -9.0D0*MU*MU*SE(II)/(SIGEQE**3) 
 85   CONTINUE        
      DO 95 II=1,NDT
           DA6DE(II) = VP(3)*DA3DE(II)+A3*DSDDE(II,3)
 95   CONTINUE                           
C ======================================================================
C --- CALCUL DU DENOMINATEUR -------------------------------------------
C ======================================================================
      AUX1 = PARAME(1)-PARAME(2)*SIG3
      AUX2 = DL*TROIS*K*DETADG*DGDL+A6+A4
      AUX3 = DGDL*(DERIVE(1)
     &         -SIG3*DERIVE(2))
     &         + PARAME(2)*AUX2      
      DENOM = -A2*A3
     &       -DERIVE(3)*DGDL*(UN+C5*SIG3)+PARAME(3)*C5*AUX2
     &   - AUX3/(SQRT(AUX1)*DEUX) 
C ======================================================================
C --- CALCUL DE DDL/DEPS -----------------------------------------------
C ======================================================================
      DO 100 II=1,NDI
        AUX4 = DA1DE(II)-DA6DE(II)*DL                             
        DDLDE(II) = (-(UN-A3*DL)*DA2DE(II)+A2*DA3DE(II)*DL
     &        +PARAME(3)*C5*AUX4
     &        -PARAME(2)*AUX4/(DEUX*SQRT(AUX1)))/DENOM         
 100  CONTINUE  
      DO 102 II=NDI+1,NDT
        AUX4 = DA1DE(II)-DA6DE(II)*DL
        DDLDE(II) = (-(UN-A3*DL)*DA2DE(II)
     &   +A2*DA3DE(II)*DL+PARAME(3)*C5*AUX4
     &      -PARAME(2)*AUX4/(DEUX*SQRT(AUX1)))/DENOM  
 102  CONTINUE   
      DO 110 II=NDT+1,6
          DDLDE(II) = 0.0D0        
 110  CONTINUE    
C ======================================================================
      END
