        SUBROUTINE HUJDP (MOD, DEPS, SIGD, SIGF, MATER,
     &                    VIN, NDEC, IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/04/2008   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ---------------------------------------------------------------
C       TESTS SUR LES CRITERES D'EVOLUTION RELATIFS AUX CONTRAINTES
C       IN  DEPS   :  INCREMENT DE DEFORMATION
C           SIGD   :  CONTRAINTE  A T
C           SIGF   :  CONTRAINTE ELASTIQUE A T+DT
C           MATER  :  PROPRIETES MATERIAU
C       OUT NDEC   :  NOMBRE DE REDECOUPAGE DE DEPS
C           IRET   :  CODE RETOUR
C       ---------------------------------------------------------------
        INTEGER       NDT, NDI, I, J, NDEC, IRET
        REAL*8        I1D, N, PREF, K0, NI
        REAL*8        DEPS(6), SIGD(6), SIGF(6)
        REAL*8        MATER(22,2), TOLE, TOL, RI, VIN(*)
        REAL*8        ZERO, UN, D13, DEUX, DEPSV, COEF 
        REAL*8        EPSVP, BETA, D, FR, I1E 
        REAL*8        DFDS(6), E, NU, AL, LA, DEMU 
        REAL*8        HOOKNL(6,6), DSIG(6), FIDSIG   
        REAL*8        PCO, RATIO, PF, PD, QF, QD, TAUF(3)
        REAL*8        TAUD(3), RELA1, RELA2
        CHARACTER*8   MOD

        COMMON /TDIM/ NDT, NDI

        DATA ZERO, D13, UN, DEUX, TOL
     &  / 0.D0, 0.33333333333334D0, 1.D0, 2.D0, 1.D-6 /

        IF (NDEC.GT.1) THEN
          IRET =1
          GOTO 500
        ENDIF

        PREF  = MATER(8,2)
        N     = MATER(1,2)
        BETA  = MATER(2,2)
        D     = MATER(3,2)
        PCO   = MATER(7,2)
        EPSVP = VIN(23)
        
        DEPSV = DEPS(1)+DEPS(2)+DEPS(3)
        I1D   = D13*(SIGD(1)+SIGD(2)+SIGD(3))
         
        TOLE = 0.1D0


C ----------------------------------------------------
C 1 --- CRITERE LIMITANT L EVOLUTION DE P: DP/P < TOLE
C ----------------------------------------------------
        K0   = D13*MATER(1,1) /(UN-DEUX*MATER(2,1))
        COEF = (I1D/PREF)**N
        IF ((I1D/PREF) .GT. TOL) THEN
          RI = K0*COEF*DEPSV /I1D
        ELSE
          RI = ZERO
          WRITE(6,'(A)')'HUJDP :: DP/P : CAS NON PREVU'
        ENDIF
        
        IF (RI.GT.UN) THEN
          RI = UN
        ELSEIF (RI.LT.TOLE) THEN
          RI = TOLE
        ENDIF
        NDEC = NINT(RI/TOLE)   


C ----------------------------------------------------------------      
C 2 --- CRITERE A RESPECTER POUR L EVOLUTION DU MECANISME ISOTROPE
C   ---         FR/(DFDS*C*DEPS)*TOLE > TETA = 1/RI             
C ====================================================================
C -------------------- 2.1 CONSTRUCTION DE C -------------------------
C ====================================================================
        CALL LCINMA (ZERO, HOOKNL)
        
        I1E  = D13*(SIGF(1)+SIGF(2)+SIGF(3))
        E    = MATER(1,1) * (I1E/PREF)**N
        NU   = MATER(2,1)
        AL   = E *(UN-NU) /(UN+NU) /(UN-DEUX*NU)
        DEMU = E        /(UN+NU)
        LA   = E*NU/(UN+NU)/(UN-DEUX*NU)

        IF (MOD(1:2) .EQ. '3D'     .OR.
     &      MOD(1:6) .EQ. 'D_PLAN' .OR.
     &      MOD(1:4) .EQ. 'AXIS') THEN
     
          DO 30 I = 1, NDI
          DO 30 J = 1, NDI
            IF (I.EQ.J) HOOKNL(I,J) = AL
            IF (I.NE.J) HOOKNL(I,J) = LA
 30       CONTINUE
          DO 35 I = NDI+1, NDT
             HOOKNL(I,I) = DEMU
 35       CONTINUE

        ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &          MOD(1:2) .EQ. '1D')   THEN
     
          CALL U2MESS('F','COMPOR1_4')
          
        ENDIF
        
        CALL LCPRMV (HOOKNL, DEPS, DSIG)


C ====================================================================
C -------------- 2.2 CALCUL DE FIDSIG = DFDS*DSIG --------------------
C ====================================================================
        FIDSIG = ZERO
        DO 40 I = 1, NDI
          FIDSIG = FIDSIG - D13*DSIG(I)
 40       CONTINUE
        FR = D*PCO*EXP(-BETA*EPSVP)
        RI = ABS(FIDSIG/FR)

        IF (RI.GT.UN) THEN
          RI = UN
        ELSEIF (RI.LT.TOLE) THEN
          RI = TOLE
        ENDIF
        NI = NINT(RI/TOLE)   
        IF (NDEC.LT.NI) NDEC = NI


C -------------------------------------------------------
C 3 --- CRITERE LIMITANT L EVOLUTION DE Q: DQ/PREF < TOLE
C -------------------------------------------------------    
        DO 45 I = 1, 3
        
          CALL HUJPRJ(I, SIGD, TAUD, PD, QD)
          CALL HUJPRJ(I, SIGF, TAUF, PF, QF)
          IF (ABS(QF-QD).LT.TOLE) THEN
            RI =ZERO
          ELSE  
            RI =ABS((QF-QD)/PREF)
          ENDIF
          
          RELA1 = ZERO
          RELA2 = ZERO
          IF (ABS(TAUD(1)).GT.TOLE)
     &    RELA1 = ABS((TAUF(1)-TAUD(1))/PREF)
          IF (ABS(TAUD(3)).GT.TOLE)
     &    RELA2 = ABS((TAUF(3)-TAUD(3))/PREF)
          IF (RELA1.GT.RI) RI = RELA1
          IF (RELA2.GT.RI) RI = RELA2
          
          IF (RI.GT.UN) THEN
            RI = UN
          ELSEIF (RI.LT.TOLE) THEN
            RI = TOLE
          ENDIF
          NI = NINT(RI/TOLE)
          IF (NDEC.LT.NI) NDEC = NI
          
 45     CONTINUE 
        
C        write(6,*) 'incmax=',ndec
 500   CONTINUE
       END
