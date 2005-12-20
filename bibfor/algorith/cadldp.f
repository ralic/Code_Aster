      SUBROUTINE CADLDP(VP,SIGEQE,NBMAT,MATERF,PARAME,DERIVE,SIG3,
     &            ETA,DG,DETADG,DGDL,DDLDSP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2005   AUTEUR JOUMANA J.EL-GHARIB 
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
      REAL*8   DDLDSP,MATERF(NBMAT,2),PARAME(4),DERIVE(5)
      REAL*8   VP(3),SIGEQE,ETA,DG,DETADG,SIG3,DGDL
C ======================================================================
C --- LOI DE HOEK BROWN : CALCUL DE DDLAMBDA/DSIP (CONT. TOTALES) -----
C ======================================================================
C IN  VP      VALEURS PROPRES DU DEVIATEUR ELASTIQUE SE ----------------
C IN  NBMAT   NOMBRE DE DONNEES MATERIAU -------------------------------
C IN  MATERF  DONNEES MATERIAU -----------------------------------------
C IN  PARAME  VALEUR DES PARAMETRES DE LA LOI S*SIG, M*SIG, B ----------
C IN  DERIVE  VALEUR DES DERIVEES DES PARAMETRES PAR RAPPORT A GAMMA ---
C IN  SIG3    CONTRAINTE PRINCIPALE SIG3 -------------------------------
C IN  DG      INCREMENT DU PARAMETRE D ECROUISSAGE GAMMA ---------------
C IN  DETADG  DERIVEE DE ETA PAR RAPPORT A GAMMA -----------------------
C IN  DGDL    DERIVEE  DE GAMMA PAR RAPPORT A LAMBDA -------------------
C OUT DDLDSP  DDLAMDA/DSIP ---------------------------------------------
C ======================================================================
      REAL*8   UN,DEUX,TROIS,K,DL,MU
      REAL*8   A2,A3,A4,C5,A6,AUX1,AUX2,AUX3,DENOM
      INTEGER  NDT,NDI
C ======================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
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
C --- CALCUL DE DDL/DSIGP ----------------------------------------------
C ======================================================================
      DDLDSP = (PARAME(3)*C5-PARAME(2)*0.5D0/SQRT(AUX1))/DENOM          
C ======================================================================
      END
