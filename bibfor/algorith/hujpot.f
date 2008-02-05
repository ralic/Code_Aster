        SUBROUTINE HUJPOT (MOD,MATER, VIND, DEPSH, SIGD, SIGE, 
     &                     ETATF, RDCTPS, IRET, AREDEC)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/02/2008   AUTEUR KHAM M.KHAM 
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
C   ------------------------------------------------------------------
C   DEFINITION DU DOMAINE POTENTIEL DES MECANISMES ACTIFS
C   IN  MOD    :  MODELISATION
C       MATER  :  COEFFICIENTS MATERIAU A T+DT
C       VIND   :  VARIABLES INTERNES  A T
C       DEPS   :  INCREMENT DE DEFORMATION
C       SIGD   :  CONTRAINTE A T
C       SIGE   :  CONTRAINTE A T+DT  (ELAS)
C
C   OUT VIND   :  VARIABLES INTERNES MODIFIEES PAR LES NOUVEAUX 
C                 MECANISMES
C       ETATF  :  ETAT PLASTIQUE OU ELASTIQUE DU POINT CONSIDERE
C       RDCTPS :  REDECOUPAGE DU PAS DE TEMPS SI NECESSAIRE 
C       AREDEC :  DECOUPAGE LOCAL ACTIF = .TRUE.
C       IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI DE HUJEUX
C                    IRET=0 => PAS DE PROBLEME
C                    IRET=1 => ECHEC 
C   ------------------------------------------------------------------
        INTEGER       NDT, NDI, NVI, ELAS, JJ, IRET 
        INTEGER       K, I, INDI(4), J, MONO(4),HIST(4,2) 
        REAL*8        TOLE, SIGD(6),SIGE(6)
        REAL*8        VIND(*), CHARGE
        REAL*8        MATER(22,2), UN, ZERO, PREF 
        REAL*8        AL, LA, DEMU, E, NU, I1E, YE(15)
        REAL*8        HOOKNL(6,6), DFDS(6), DEPSH(6), DSIG(6)
        REAL*8        ACTIF, DPSIDS(6,6), N, DEUX, SEUIL 
        REAL*8        EPSVP, PCO, D, BETA, RATIO, DEPS(6)
        REAL*8        R8PREM
        LOGICAL       DEBUG, PROX, RDCTPS, AREDEC 
        CHARACTER*7   ETATF
        CHARACTER*8   MOD
C ----------------------------------------------------------------------
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG
C ----------------------------------------------------------------------
        PARAMETER     (TOLE = 1.D-7)
        PARAMETER     (UN   = 1.D0)
        PARAMETER     (ZERO = 0.D0)
        PARAMETER     (DEUX = 2.D0)
        
C ======================================================================
C -------------------- DETERMINATION DES CRITERES ACTIFS A T ----------
C ======================================================================
C       WRITE(6,'(A,6(1X,E16.9))')'SIGD =',(SIGD(I),I=1,6)
C       WRITE(6,'(A,6(1X,E16.9))')'SIGE =',(SIGE(I),I=1,6)      
C       WRITE(6,'(A,E16.9)')'DEPS =',(DEPSH(1)+DEPSH(2)+DEPSH(3))
C       WRITE(6,'(A,32(1X,E16.9))')'---> VIND =',(VIND(I),I=1,32)

C --- MISE A ZERO POUR CORRIGER ZERO NUMERIQUE
        DO 10 I = 1, NDT
          DEPS(I) = DEPSH(I)
          IF(ABS(DEPS(I)).LT.R8PREM())DEPS(I)=ZERO
  10    CONTINUE        
        ELAS = 0
        RDCTPS = .FALSE.
        
C ====================================================================
C --- PROPRIETES MATERIAU HUJEUX -------------------------------------
C ====================================================================

        N       = MATER(1,2)
        PREF    = MATER(8,2)
        
C ====================================================================
C ------------------ INITIALISATION VARIABLES ------------------------
C ====================================================================

        DO 15 I = 1, 4
          HIST(I,1) = 0
          HIST(I,2) = 0
          INDI(I)   = 0
          MONO(I)   = I
  15    CONTINUE
        DO 25 I = 1, NDT
          YE(I) = SIGE(I)
  25    CONTINUE
        YE(NDT+1) = VIND(23)
        
C ====================================================================
C --------------------- I) CONSTRUCTION DE C -------------------------
C ====================================================================

        CALL LCINMA (ZERO, HOOKNL)
        
        I1E  = (SIGE(1)+SIGE(2)+SIGE(3))/3.D0
        E    = MATER(1,1) * (I1E/PREF)**N
        NU   = MATER(2,1)
        AL   = E *(UN-NU) /(UN+NU) /(UN-DEUX*NU)
        DEMU = E        /(UN+NU)
        LA   = E*NU/(UN+NU)/(UN-DEUX*NU)
        
C ====================================================================
C --- 3D/DP/AX -------------------------------------------------------
C ====================================================================

         IF (MOD(1:2) .EQ. '3D'     .OR.
     &       MOD(1:6) .EQ. 'D_PLAN' .OR.
     &       MOD(1:4) .EQ. 'AXIS') THEN
           DO 30 I = 1, NDI
           DO 30 J = 1, NDI
             IF (I.EQ.J) HOOKNL(I,J) = AL
             IF (I.NE.J) HOOKNL(I,J) = LA
 30        CONTINUE
           DO 35 I = NDI+1, NDT
             HOOKNL(I,I) = DEMU
 35        CONTINUE
 
C ====================================================================
C --- CP/1D ----------------------------------------------------------
C ====================================================================

         ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D')   THEN
           CALL U2MESS('F','COMPOR1_4')
         ENDIF

C ====================================================================
C -------------- CALCUL DE DSIGMA = C*DEPSILON -----------------------
C ====================================================================

         CALL LCPRMV (HOOKNL, DEPS, DSIG)

C ====================================================================
C ----------- DETERMINATION DES CRITERES ACTIFS PRECEDEMMENT ---------
C ====================================================================

         J = 0   
         DO 40 I = 1, 4
           IF((VIND(23+I).EQ.UN).OR.(VIND(23+I).EQ.ZERO))THEN
             YE(NDT+1+I) = VIND(I)
             HIST(I,1)   = I
             INDI(I)     = I
             IF(VIND(23+I).EQ.UN)THEN
               HIST(I,2)   = 1
             ELSE
               HIST(I,2)   = 0
             ENDIF
           ELSE
             YE(NDT+1+I) = VIND(I+4)
             HIST(I,1)   = I + 4
             INDI(I)     = I + 4 
             IF(VIND(27+I).EQ.UN)THEN
               HIST(I,2)   = 1
             ELSE
               HIST(I,2)   = 0
             ENDIF
           ENDIF
           
C ====================================================================
C --------------------- CALCUL DE DFDS*C(SIGE)*DEPS ------------------
C ====================================================================
           CALL HUJDDD ('DFDS  ', INDI(I), MATER, INDI, YE, VIND,
     &                    DFDS, DPSIDS,IRET)
           IF(IRET.EQ.1)THEN
             IF(.NOT.AREDEC)THEN
               RDCTPS = .TRUE.
               IRET = 0
             ENDIF
             GOTO 999
           ENDIF
           ACTIF = ZERO
           DO 45 JJ = 1, NDT
             ACTIF = ACTIF + DSIG(JJ)*DFDS(JJ) 
 45        CONTINUE  
           
           ACTIF = ACTIF/MATER(1,1)
C          WRITE(6,'(A,I3)')'INDI(I) =',INDI(I)
C          WRITE(6,'(A,E16.9)')'ACTIF =',ACTIF
C           WRITE(6,'(A,6(1X,E12.5))')'DFDS =',(DFDS(JJ),JJ=1,6)
           CHARGE = - UN
           IF(INDI(I).GT.4)THEN
             YE(NDT+1+I) = VIND(I) 
             CALL HUJDDD ('DFDS  ', MONO(I), MATER, MONO, YE, VIND,
     &                     DFDS, DPSIDS,IRET)

             IF(IRET.EQ.1)THEN
               IF(.NOT.AREDEC)THEN
                 RDCTPS = .TRUE.
                 IRET = 0
               ENDIF
               GOTO 999
             ENDIF

             CHARGE = ZERO
             DO 46 JJ = 1, NDT
               CHARGE = CHARGE + DSIG(JJ)*DFDS(JJ)             
 46          CONTINUE  
             CHARGE = CHARGE/MATER(1,1)
C            WRITE(6,'(A,E16.9)')'CHARGE =',CHARGE
           ENDIF
C ====================================================================
C --------------------- CRITERE MONOTONE ACTIF ? ---------------------
C ====================================================================
           IF(CHARGE.GE.(-R8PREM()))THEN
             IF(INDI(I).NE.8)THEN
C               CALL HUJCRD(INDI(I)-4, MATER, SIGE, VIND, SEUIL)
C              IF(INDI(I).EQ.7)WRITE(6,*)'SEUIL =',SEUIL
C               IF(SEUIL.GT.TOLE)THEN
                 CALL HUJPXD(INDI(I), MATER, SIGD ,VIND, PROX)
                 IF (PROX) THEN        
                   VIND(19+INDI(I))   = UN
                   VIND(23+INDI(I))   = ZERO
                   VIND(INDI(I)*4-11) = ZERO
                   VIND(INDI(I)*4-10) = ZERO
                   VIND(INDI(I)*4-9)  = ZERO
                   VIND(INDI(I)*4-8)  = ZERO
                   VIND(INDI(I))      = MATER(18,2)
                   GOTO 40               
                 ELSEIF(.NOT.AREDEC)THEN               
C --> SINON ==> REDECOUPAGE DU PAS DE TEMPS 
                   RDCTPS = .TRUE.
                   GOTO 999             
C                ELSE
C                  VIND(19+INDI(I)) = UN
C                  VIND(23+INDI(I)) = ZERO
C                  GOTO 40              
                 ENDIF  
C               ENDIF                     
             ELSE
               CALL HUJPXS(MATER, SIGD, VIND, PROX)              
C              CALL HUJCRI(MATER, SIGE, VIND, SEUIL)
C              WRITE(6,*)'SEUIL =',SEUIL
C              IF(SEUIL.GT.TOLE)THEN
                 IF (PROX) THEN 
                   VIND(21) = ZERO
                   VIND(22) = ZERO
                   VIND(27) = UN
                   VIND(31) = ZERO      
                   GOTO 40       
                 ELSEIF(.NOT.AREDEC)THEN               
C --> SINON ==> REDECOUPAGE DU PAS DE TEMPS
                   RDCTPS = .TRUE.
                   GOTO 999               
C                ELSE
C                  VIND(21) = ZERO
C                  VIND(22) = ZERO
C                  VIND(27) = UN
C                  VIND(31) = ZERO
C                  GOTO 40      
                 ENDIF
C              ENDIF     
             ENDIF
           ENDIF  
           
C =====================================================================
C -------------------------- CRITERE ACTIF ----------------------------
C =====================================================================
           IF(INDI(I).LT.5)THEN
C **************************
C --- CRITERES MONOTONES ---
C **************************
             IF(HIST(I,2).EQ.1)THEN
               IF(ACTIF.GE.(-R8PREM()))THEN
                 VIND(23+INDI(I)) = UN
               ELSE
                 VIND(23+INDI(I)) = - UN
                 IF(INDI(I).LT.4)THEN
                   VIND(I+4) = MATER(18,2)
                   CALL HUJMED(INDI(I), MATER, VIND, SIGD)
                   CALL HUJCDC(INDI(I), MATER, SIGE, VIND, SEUIL)
                 ELSEIF(INDI(I).EQ.4)THEN
                   CALL HUJMEI(VIND)
                   VIND(8) = MATER(19,2)
                   CALL HUJCIC(MATER, SIGE, VIND, SEUIL)
                 ENDIF
                 IF(SEUIL.GT.TOLE)THEN
                   VIND(27+INDI(I)) = UN
                 ELSE
                   VIND(27+INDI(I)) = ZERO
                   ELAS = ELAS + 1
                 ENDIF  
               ENDIF
             ELSE
               IF(ACTIF.GE.(-R8PREM()))THEN
                 IF(INDI(I).LT.4)THEN
                   CALL HUJCRD(I, MATER, SIGE, VIND, SEUIL)
                 ELSE
                   CALL HUJCRI(MATER, SIGE, VIND, SEUIL)
                 ENDIF
C                 WRITE(6,'(A,E16.9)')'SEUILM =',SEUIL
                 IF(SEUIL.GT.TOLE)THEN
                   VIND(23+INDI(I)) = UN 
                 ELSE
                   VIND(23+INDI(I)) = ZERO
                   ELAS = ELAS + 1
                 ENDIF
               ELSE
                 VIND(23+INDI(I)) = ZERO
                 ELAS = ELAS + 1
               ENDIF
             ENDIF
           ELSE
C **************************
C --- CRITERES CYCLIQUES ---
C **************************
             IF(HIST(I,2).EQ.1)THEN
               IF(ACTIF.GE.(-R8PREM()))THEN
                 VIND(23+INDI(I)) = UN
               ELSE
                 IF(INDI(I).LT.8)THEN
                   VIND(I+4) = MATER(18,2)
                   CALL HUJMED(INDI(I), MATER, VIND, SIGD)
                   CALL HUJCDC(INDI(I)-4, MATER, SIGE, VIND, SEUIL)
                 ELSE
                   CALL HUJMEI(VIND)
                   VIND(8) = MATER(19,2)
                   CALL HUJCIC(MATER, SIGE, VIND, SEUIL)
                 ENDIF
                 IF(SEUIL.GT.TOLE)THEN
                   VIND(23+INDI(I)) = UN
                 ELSE
                   VIND(23+INDI(I)) = ZERO
                   ELAS = ELAS + 1
                 ENDIF  
               ENDIF
             ELSE
               IF(ACTIF.GE.(-R8PREM()))THEN
                 IF((INDI(I).GT.4).AND.(INDI(I).LT.8))THEN
                   CALL HUJCDC(INDI(I)-4, MATER, SIGE, VIND, SEUIL)
                   IF(SEUIL.GT.TOLE)THEN
                     VIND(23+INDI(I)) = UN
                   ELSE
                     VIND(23+INDI(I)) = ZERO
                     ELAS = ELAS + 1
                   ENDIF
                 ELSE
                   CALL HUJCIC(MATER, SIGE, VIND, SEUIL)
                   IF(SEUIL.GT.TOLE)THEN
                     VIND(23+INDI(I)) = UN
                   ELSE
                     VIND(23+INDI(I)) = ZERO
                     ELAS = ELAS + 1
                   ENDIF                   
                 ENDIF     
               ELSE
                 SEUIL = ZERO
                 IF(INDI(I).LT.8)THEN
                   IF(VIND(I+4).NE.MATER(18,2))THEN 
                     VIND(I+4) = MATER(18,2)
                     CALL HUJMED(INDI(I), MATER, VIND, SIGD)
                     CALL HUJCDC(INDI(I)-4, MATER, SIGE, VIND, SEUIL)
                   ENDIF
                 ELSE
                   CALL HUJMEI(VIND)
                   VIND(8) = MATER(19,2)
                   CALL HUJCIC(MATER, SIGE, VIND, SEUIL)
                 ENDIF
                 IF(SEUIL.GT.TOLE)THEN
                   VIND(23+INDI(I)) = UN
                 ELSE
                   VIND(23+INDI(I)) = ZERO
                   ELAS = ELAS + 1
                 ENDIF  
               ENDIF
             ENDIF
           ENDIF
 40      CONTINUE
         

C ======================================================================
C ---------------- DETERMINATION ETAT ELASTIQUE OU PLASTIQUE -----------
C ======================================================================
        
        IF (ELAS .EQ. 4) THEN
          ETATF = 'ELASTIC'
        ELSE
          ETATF = 'PLASTIC'
        ENDIF
C       WRITE(6,'(A,A)')'ETATF =',ETATF

 999    CONTINUE
1001    FORMAT(A,I3)    
2000    FORMAT(A,28(1X,E12.5)) 
2010    FORMAT(A,4(1X,I2))
        END
