      SUBROUTINE LCEJEX(MATE,OPTION,AM,DA,TM,TP,SIGMA,DSIDEP,VIM,VIP)

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
      INTEGER MATE
      REAL*8  AM(2),DA(2),SIGMA(2),DSIDEP(2,2),VIM(*),VIP(*),TM,TP
      CHARACTER*16 OPTION

C-----------------------------------------------------------------------
C                  LOI DE COMPORTEMENT D'INTERFACE
C                  POUR LES ELEMENTS DE JOINT : CZM_EXP_REG
C
C IN : AM SAUT INSTANT - : AM(1) = SAUT NORMAL, AM(2) = SAUT TANGENTIEL
C IN : DA    INCREMENT DE SAUT
C IN : MATE, OPTION, VIM, TM, TP
C OUT : SIGMA , DSIDEP , VIP
C-----------------------------------------------------------------------

      LOGICAL RESI, RIGI, ELAS
      INTEGER DISS
      REAL*8  SC,GC,LC,K0,VAL(4),VALPA
      REAL*8  A(2),NA,KA,KAP,R0,RC,BETA,RK,RA,COEF,COEF2
      CHARACTER*2 COD(4)
      CHARACTER*8 NOM(4)
      

C OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE

      RESI = OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA'
      RIGI = OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RIGI_MECA'
      ELAS = OPTION.EQ.'FULL_MECA_ELAS' .OR. OPTION.EQ.'RIGI_MECA_ELAS'


C CALCUL DU SAUT EN T+

      CALL DCOPY(2,AM,1,A,1)
      IF (RESI) CALL DAXPY(2,1.D0,DA,1,A,1)
      
       
C RECUPERATION DES PARAMETRES PHYSIQUES

      NOM(1) = 'GC'
      NOM(2) = 'SIGM_C'
      NOM(3) = 'PENA_ADHERENCE'
      NOM(4) = 'PENA_CONTACT'
      
      IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
        VALPA=TM
      ELSE
        VALPA=TP
      ENDIF
      
      CALL RCVALA(MATE,' ','RUPT_FRAG',1,'TEMP',VALPA,4,
     &            NOM,VAL,COD,'F ')

      GC   = VAL(1)      
      SC   = VAL(2)  
      LC   = GC/SC
      K0   = LC*VAL(3)
      R0   = SC*EXP(-K0/LC)/K0 
      BETA = VAL(4)

C INITIALISATION

      KA = MAX(K0,VIM(1)) 
      NA = SQRT( MAX(0.D0,A(1))**2 + A(2)**2 )
      RK = SC * EXP(-KA/LC) / KA
      RC = RK + BETA*(R0-RK)
 
 
C INITIALISATION COMPLEMENTAIRE POUR RIGI_MECA_TANG (SECANTE PENALISEE)

      IF (.NOT. RESI) THEN
        IF (ELAS) THEN
          DISS = 0
        ELSE
          DISS = NINT(VIM(2))
        END IF
        GOTO 5000
      END IF        


C CALCUL DE LA CONTRAINTE          
      
C    CONTRAINTE DE CONTACT PENALISE
      SIGMA(1) = RC * MIN(0.D0,A(1))
      SIGMA(2) = 0.D0
      
C    CONTRAINTE DE FISSURATION
      IF (NA .LE. KA) THEN
        DISS     = 0
        SIGMA(1) = SIGMA(1) + RK * MAX(0.D0,A(1))
        SIGMA(2) = SIGMA(2) + RK * A(2)
      ELSE
        DISS     = 1
        RA       = SC * EXP(-NA/LC) / NA
        SIGMA(1) = SIGMA(1) + RA * MAX(0.D0,A(1))
        SIGMA(2) = SIGMA(2) + RA * A(2)
      END IF
        
      
C ACTUALISATION DES VARIABLES INTERNES

      KAP    = MAX(KA,NA)
      VIP(1) = KAP
      VIP(2) = DISS
      VIP(3) = 1 - EXP(-KAP/LC)
      IF (A(1).GT.0.D0) THEN
        VIP(4) = 1
      ELSE
        VIP(4) = 2
      END IF


C -- MATRICE TANGENTE

 5000 CONTINUE
      IF (.NOT. RIGI) GOTO 9999
      
      CALL R8INIR(4, 0.D0, DSIDEP,1)

C    MATRICE TANGENTE DE CONTACT PENALISE
      IF (A(1).LE.0.D0) DSIDEP(1,1) = DSIDEP(1,1) + RC

C    MATRICE TANGENTE DE FISSURATION
      IF ((DISS.EQ.0) .OR. ELAS) THEN
      
        IF (A(1).GT.0.D0) DSIDEP(1,1) = DSIDEP(1,1) + RK
        DSIDEP(2,2) = DSIDEP(2,2) + RK
      
      ELSE
      
        IF (A(1).LE.0.D0) THEN
          DSIDEP(2,2) = DSIDEP(2,2) - SC/LC * EXP(-NA/LC)
        ELSE
          COEF        = (SC/LC + SC/NA) / NA**2
          COEF2       = EXP(-NA/LC)
          DSIDEP(1,1) = DSIDEP(1,1) + SC*COEF2/NA  
          DSIDEP(2,2) = DSIDEP(2,2) + SC*COEF2/NA  
          DSIDEP(1,1) = DSIDEP(1,1) - COEF*COEF2*A(1)*A(1)    
          DSIDEP(1,2) = DSIDEP(1,2) - COEF*COEF2*A(1)*A(2)    
          DSIDEP(2,1) = DSIDEP(2,1) - COEF*COEF2*A(2)*A(1)    
          DSIDEP(2,2) = DSIDEP(2,2) - COEF*COEF2*A(2)*A(2)    
        END IF
        
      END IF
      
 9999 CONTINUE  
      END
