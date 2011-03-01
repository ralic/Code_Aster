      SUBROUTINE LCIMPL(FAMI,KPG,KSP,IMATE,EM,EP,SIGM,TMOINS,TPLUS,
     &                  DEPS,VIM,OPTION,COMPOR,SIGP,VIP,DSDE)

    
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/02/2011   AUTEUR BARGELLI R.BARGELLINI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL*8 TEMPP,TEMPM,EM,EP,ET,SIGY,TREF,TMOINS,TPLUS
      REAL*8 SIGM,DEPS,PM,VIM(*),VIP(*),RESU,DT,P
      REAL*8 SIGP,DSDE,RBID
      CHARACTER*16 OPTION,COMPOR(*)
      CHARACTER*(*) FAMI
      INTEGER KPG,KSP,IMATE
C     ------------------------------------------------------------------
C     VARIABLES LOCALES
C     ------------------------------------------------------------------
      REAL*8 RPRIM,RM,SIGE,VALPAR,VALRES(2),DEPSTH,AIRERP,DUM
      REAL*8 SIELEQ,RP,DP,NU,ASIGE
      INTEGER JPROLM,JVALEM,NBVALM,NBVALP,JPROLP,JVALEP,IRET
      CHARACTER*2 BL2,FB2,CODRES(2)
      CHARACTER*8 NOMECL(2),TYPE
      DATA NOMECL/'D_SIGM_EPSI','SY'/
      
      
C      PRINT *,'LAAAA'

      BL2 = '  '
      FB2 = 'FM'

      PM = VIM(1)
      DT = TPLUS-TMOINS
        
      
        CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ECRO_LINE',
     &              0,' ',0.D0,1,NOMECL,
     &              VALRES,CODRES,FB2)
        CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ECRO_LINE',
     &              0,' ',0.D0,1,
     &             NOMECL(2), VALRES(2),CODRES(2),BL2)

     
        IF (CODRES(2).NE.'OK') VALRES(2) = 0.D0
        ET = VALRES(1)
        SIGY = VALRES(2)        
        RPRIM = EP*ET/ (EP-ET)
        RM = RPRIM*VIM(1) + SIGY
      
C     ------------------------------------------------------------------
C     ESTIMATION ELASTIQUE
C     ------------------------------------------------------------------
      CALL VERIFT(FAMI,KPG,KSP,'T',IMATE,'ELAS',1,DEPSTH,IRET)
      SIGE = EP* (SIGM/EM+DEPS-DEPSTH)
      SIELEQ = ABS(SIGE)
C     ------------------------------------------------------------------
C     CALCUL EPSP, P , SIG
C     ------------------------------------------------------------------
      IF (OPTION.EQ.'RAPH_MECA') THEN
        IF (SIELEQ.LE.RM) THEN        
          DP=0.D0
          SIGP = SIGE
          DSDE = EP
          VIP(1) = VIM(1)
        ELSE
          DP = ABS(SIGE) - RM
          DP = DP/ (RPRIM+EP)
          RP = SIGY + RPRIM* (PM+DP)
          VIP(1) = VIM(1) + DP
          SIGP = SIGE/ (1.D0+EP*DP/RP)
        END IF
        VIP(2) = DP/DT
      END IF
      IF (OPTION(1:16) .EQ. 'RIGI_MECA_IMPLEX') THEN
C    EXTRAPOLATION
        DP=MAX(VIM(2)*DT,0.D0)
        P= VIM(1) + DP
C    MISE A JOUR DE LA VARIABLE INTERNE
        RP=SIGY+RPRIM*(P)
C    CONTRAINTES
        SIGP=SIGE/(1.D0+(EP*DP/RP))
C    MATRICE
        DSDE = EP
      END IF

      END
