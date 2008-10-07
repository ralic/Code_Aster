      SUBROUTINE HUJACT (MATER, VIND, VINF, VINS, SIGD, SIGF,
     &                     NEGMUL, CHGMEC, IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/10/2008   AUTEUR DEVESA G.DEVESA 
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
C   IN  MATER    :  COEFFICIENTS MATERIAU A T+DT
C       VIND     :  VARIABLES INTERNES  A T
C       VINF     :  VARIABLES INTERNES A T+DT
C       VINS     :  VARIABLES INTERNES A T AVANT CREATION DU DOMAINE 
C                   POTENTIEL DE MECANISMES
C       SIGD     :  CHAMPS DE CONTRAINTES A T
C       SIGF     :  CHAMPS DE CONTRAINTES A T+DT
C       NEGMUL() = .TRUE. ---> MULTIPLICATEUR PLASTIQUE NEGATIF 
C
C   OUT VIND   :  VARIABLES INTERNES MODIFIEES SI CHGMEC = .TRUE.
C       VINF   :  VARIABLES INTERNES MODIFIEES SI NECESSAIRE
C       CHGMEC   = .TRUE. SI MODIFICATION DU DOMAINE POTENTIEL 
C                            DES MECANISMES ACTIFS
C   ------------------------------------------------------------------
        INTEGER       NDT, NDI, I, J, MONO, IRET 
        REAL*8        TOLE, SIGD(6),SIGF(6),TOLE2
        REAL*8        VIND(*), VINF(*), VINS(50), VINT(50)
        REAL*8        MATER(22,2), UN, ZERO 
        REAL*8        I1F, I1D, PCREF, PSF, PSM
        REAL*8        SEUIL, RD, RF
        LOGICAL       DEBUG, CHGMEC, NEGMUL(8), PROX 
C --------------------------------------------------------------------
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG
C --------------------------------------------------------------------
        PARAMETER     (TOLE = 1.D-7)
        PARAMETER     (UN   = 1.D0)
        PARAMETER     (ZERO = 0.D0)
        PARAMETER     (TOLE2 = 1.D-12)
        
C ===================================================================
C -------------- DETERMINATION DES CRITERES ACTIFS A T+DT -----------
C ===================================================================

        DO 30 I = 1, 50
          VINT(I) = VIND(I)
  30    CONTINUE

        DO 40 I = 1, 4
C ====================================================================
C ---------------- MECANISME MONOTONE SUPPOSÉ ACTIF ------------------
C ====================================================================
          IF(VIND(23+I).EQ.UN)THEN
            IF(NEGMUL(I))THEN
              CHGMEC     = .TRUE.
              NEGMUL(I)  = .FALSE.
              IF(I.LT.4)THEN
                IF(VIND(I).EQ.MATER(13,2))THEN
                  VIND(23+I) = ZERO
                ELSE              
                  IF((VINS(4*I+7).NE.ZERO).OR.(VINS(4*I+8).NE.ZERO))
     &            THEN
                    VIND(4+I)   = VINS(4+I)
                    VIND(4*I+5) = VINS(4*I+5)
                    VIND(4*I+6) = VINS(4*I+6)
                    VIND(4*I+7) = VINS(4*I+7)
                    VIND(4*I+8) = VINS(4*I+8)
                    VIND(23+I)  = - UN
                  ELSE
                    VIND(23+I) = - UN
                    CALL HUJMED(I, MATER, VIND, SIGD)   
                    VIND(I+4) = MATER(18,2)
                  ENDIF
                ENDIF
              ELSE
                IF(VIND(I).EQ.MATER(14,2))THEN
                  VIND(23+I) = ZERO
                ELSE
                  IF(VINS(22).NE.ZERO)THEN
                    VIND(8)  = VINS(8)
                    VIND(21) = VINS(21)
                    VIND(22) = VINS(22)
                    VIND(23+I) = - UN
                  ELSE
                    CALL HUJRMO(MATER, SIGD , VIND, RD)
                    CALL HUJRMO(MATER, SIGF , VINF, RF)
                    IF(RD.GT.RF)THEN
                      VIND(23+I) = - UN
                      CALL HUJMEI(VIND)
                      VIND(8) = MATER(19,2)
                    ELSE
                      VIND(23+I) = ZERO
                    ENDIF
                  ENDIF
                ENDIF  
              ENDIF
            ENDIF
            GOTO 40
            
C ==================================================================
C ---------- MECANISME MONOTONE SUPPOSÉ ELASTIQUE ------------------
C ==================================================================
          ELSEIF(VIND(23+I).EQ.ZERO)THEN

C ************************
C --- MECANISME DEVIATOIRE
C ************************
            IF(I.LT.4)THEN
              CALL HUJCRD(I, MATER, SIGF, VINF, SEUIL)
              IF(SEUIL.GT.TOLE)THEN
                CHGMEC      = .TRUE.
                VIND(23+I)  = UN
                VIND(4*I+5) = ZERO
                VIND(4*I+6) = ZERO
                VIND(4*I+7) = ZERO
                VIND(4*I+8) = ZERO
                VIND(4+I)   = MATER(18,2)
              ENDIF
              GOTO 40         

C ******************************
C --- MECANISME DE CONSOLIDATION
C ******************************
            ELSE
              CALL HUJCRI (MATER, SIGF, VINF, SEUIL)
              MONO = 0
              IF(SEUIL.GT.TOLE)THEN
                CHGMEC    = .TRUE.
                VIND(27)  = UN
                MONO      = 1
              ENDIF
              IF(MONO.NE.1)THEN
                CALL HUJRMO(MATER, SIGD , VIND, RD)
                CALL HUJRMO(MATER, SIGF , VINF, RF)
                IF(RD.GT.RF)THEN
                  VIND(23+I) = - UN
                  CALL HUJMEI(VIND)
                  VIND(8)   = MATER(19,2)
                  CHGMEC    = .TRUE.
                ENDIF
              ENDIF
              GOTO 40
            ENDIF
            
C ====================================================================
C ---------- MECANISME MONOTONE SUPPOSÉ EN DECHARGE ------------------
C ====================================================================
          ELSEIF(VIND(23+I).EQ.-UN)THEN

C ***********************************************
C --- VERIFICATION DES MULTIPLICATEURS PLASTIQUES
C ***********************************************
            IF(NEGMUL(I+4))THEN
              CHGMEC      = .TRUE.
              NEGMUL(I+4) = .FALSE.
              VIND(27+I)  = ZERO
              GOTO 40   
            ENDIF
                 

C *************************************
C --- VERIFICATION DES SEUILS MONOTONES
C *************************************
            IF (I.LT.4)THEN
              CALL HUJCRD(I, MATER, SIGF, VINF, SEUIL)
            ELSE
              CALL HUJCRI (MATER, SIGF, VINF, SEUIL)
            ENDIF
            IF (SEUIL.GT.TOLE)THEN
              CHGMEC = .TRUE.
              VIND(27+I) = ZERO
              VIND(23+I) = UN   
              IF(I.LT.4)THEN
                VIND(4*I+5) = ZERO
                VIND(4*I+6) = ZERO
                VIND(4*I+7) = ZERO
                VIND(4*I+8) = ZERO
                VIND(I+4)   = MATER(18,2)
              ELSE
                VIND(21) = ZERO
                VIND(22) = ZERO
              ENDIF
              GOTO 40   
            ENDIF

C ************************************
C --- VERIFICATION DES POINTS TANGENTS
C ************************************
            IF ((I.LT.4).AND.(VINF(27+I).EQ.UN))THEN
              CALL HUJDRC(I,MATER,SIGF,VINF,PSM,PSF,
     &                    SEUIL,IRET)

              IF(IRET.EQ.1)GOTO 999

              IF((SEUIL .LE. TOLE2).AND.(PSM.GT.TOLE))THEN

              CHGMEC = .TRUE.
             IF((VIND(5*I+31).NE.ZERO).OR.
     &           (VIND(5*I+32).NE.ZERO).OR. 
     &           (VIND(5*I+33).NE.ZERO).OR. 
     &           (VIND(5*I+34).NE.ZERO))THEN 

                VIND(4*I+5)  = VIND(5*I+31)
                VIND(4*I+6)  = VIND(5*I+32)
                VIND(4*I+7)  = VIND(5*I+33)
                VIND(4*I+8)  = VIND(5*I+34)
                VIND(4+I)    = VIND(5*I+35)

              ELSE
    
                VIND(4*I+5)  = ZERO
                VIND(4*I+6)  = ZERO
                VIND(4*I+7)  = ZERO
                VIND(4*I+8)  = ZERO
                VIND(27+I)   = ZERO
                VIND(23+I)   = UN                  

              ENDIF

                VIND(5*I+31) = ZERO
                VIND(5*I+32) = ZERO
                VIND(5*I+33) = ZERO
                VIND(5*I+34) = ZERO
                VIND(5*I+35) = MATER(18,2)

                GOTO 40

              ELSEIF((PSM.GT.TOLE).AND.(SEUIL.GE.TOLE2))THEN
                CHGMEC = .TRUE.     
C --- ENREGISTREMENT SURFACE ACTUELLE
                VIND(5*I+31) = VIND(4*I+5)
                VIND(5*I+32) = VIND(4*I+6)
                VIND(5*I+33) = VIND(4*I+7)
                VIND(5*I+34) = VIND(4*I+8)
                VIND(5*I+35) = VIND(4+I)
C --- MODIFICATION SURFACE
                VIND(4*I+5) = VIND(4*I+5)-2*VIND(I+4)*VIND(4*I+7)
                VIND(4*I+6) = VIND(4*I+6)-2*VIND(I+4)*VIND(4*I+8)
                VIND(4*I+7) =-VIND(4*I+7)
                VIND(4*I+8) =-VIND(4*I+8)
                GOTO 40
              ENDIF
            ENDIF

C ***********************************************************
C --- EMPECHE L INTERSECTION DES CERCLES CYCLIQUE ET MONOTONE
C ***********************************************************
C          IF ((I.LT.4).AND.(VINF(27+I).EQ.UN))THEN
C            IF((VINF(I+4).GT.VINF(5*I+35)).AND.
C     &         (VINF(5*I+35).NE.MATER(18,2))) THEN
C              CHGMEC = .TRUE.   
C              VIND(4*I+5)  = VIND(5*I+31)
C              VIND(4*I+6)  = VIND(5*I+32)
C              VIND(4*I+7)  = VIND(5*I+33)
C              VIND(4*I+8)  = VIND(5*I+34)
C              VIND(4+I)    = VIND(5*I+35)                
C              VIND(5*I+31) = ZERO
C              VIND(5*I+32) = ZERO
C              VIND(5*I+33) = ZERO
C              VIND(5*I+34) = ZERO
C              VIND(5*I+35) = MATER(18,2)
C            GOTO 40
C            ENDIF
C          ENDIF

C ****************************************
C --- MECANISME CYCLIQUE SUPPOSE ELASTIQUE
C ****************************************
            IF(VIND(27+I).EQ.ZERO)THEN

C ------------------------
C --- MECANISME DEVIATOIRE
C ------------------------
              IF(I.LT.4)THEN
                CALL HUJCDC(I, MATER, SIGF, VINF, SEUIL)
                IF (SEUIL.GT.TOLE)THEN
                  CHGMEC      = .TRUE. 
                  VIND(27+I)  = UN
                ENDIF
                IF(((VINS(4*I+5).NE.VIND(4*I+5)).AND.
     &             (VINS(4*I+5).NE.ZERO)).OR.
     &             ((VINS(4*I+6).NE.VIND(4*I+6)).AND.
     &             (VINS(4*I+6).NE.ZERO)))THEN
                  CALL HUJDRC(I,MATER,SIGF,VINF,PSM,PSF,
     &                        SEUIL,IRET)
                  IF(IRET.EQ.1)GOTO 999

                  IF(PSF.GT.ZERO)THEN
                    VINF(4*I+5) = VINS(4*I+5)
                    VINF(4*I+6) = VINS(4*I+6)
                    VINF(4*I+7) = VINS(4*I+7)
                    VINF(4*I+8) = VINS(4*I+8)
                    VINF(4+I)   = VINS(4+I)
                    CALL HUJCDC(I, MATER, SIGF, VINF, SEUIL)
                    IF(SEUIL.GT.TOLE)THEN
                      CHGMEC      = .TRUE.
                      VIND(4*I+5) = VINS(4*I+5)
                      VIND(4*I+6) = VINS(4*I+6)
                      VIND(4*I+7) = VINS(4*I+7)
                      VIND(4*I+8) = VINS(4*I+8)
                      VIND(4+I)   = VINS(4+I)
                      VIND(27+I)  = UN
                    ENDIF
                  ENDIF
                ELSEIF(((VINS(4*I+5).NE.VIND(4*I+5)).AND.
     &                 (VINS(4*I+5).EQ.ZERO)).OR.
     &                 ((VINS(4*I+6).NE.VIND(4*I+6)).AND.
     &                 (VINS(4*I+6).EQ.ZERO)))THEN

                  CALL HUJDRC(I,MATER,SIGF,VINF,PSM,PSF,
     &                        SEUIL,IRET)
                  IF(IRET.EQ.1)GOTO 999

                  IF(PSF.GT.TOLE)THEN
                    VINF(4*I+5) = ZERO
                    VINF(4*I+6) = ZERO
                    VINF(4*I+7) = ZERO
                    VINF(4*I+8) = ZERO
                    VINF(4+I)   = MATER(18,2)
                    VINF(23+I)  = UN
                    VINF(27+I)  = ZERO
                    CHGMEC      = .FALSE.
                  ENDIF
                ELSE
                  IF(VIND(4+I).NE.MATER(18,2))THEN
                    CALL HUJMED(I, MATER, VINF, SIGF)
                    VINF(4+I) = MATER(18,2)
                  ENDIF
                ENDIF
                GOTO 40
C ------------------------------
C --- MECANISME DE CONSOLIDATION
C ------------------------------
              ELSE
                CALL HUJCIC(MATER, SIGF, VINF, SEUIL)
                CALL HUJRMO(MATER, SIGD , VIND, RD)
                CALL HUJRMO(MATER, SIGF , VINF, RF)

                IF((VIND(22).EQ.UN).AND.(RD.GE.RF))THEN
                  IF (SEUIL.GT.TOLE)THEN
                    CHGMEC      = .TRUE. 
                    VIND(31)    = UN
                  ELSE
                    VINF(21) = VINS(21)
                    VINF(22) = VINS(22)
                    VINF(8)  = VINS(8)

                    IF(VINS(22).EQ.ZERO)VINF(27)=ZERO
                  ENDIF                  
                ELSEIF((VIND(22).EQ.-UN).AND.(RD.LT.RF))THEN
                  IF (SEUIL.GT.TOLE)THEN
                    CHGMEC      = .TRUE. 
                    VIND(31)    = UN
                  ELSE
                    VINF(21) = VINS(21)
                    VINF(22) = VINS(22)
                    VINF(8)  = VINS(8)
                  ENDIF   
                ELSEIF((VIND(22).EQ.UN).AND.(RD.LT.RF))THEN

                  IF(VINS(22).NE.VINF(22))THEN
                    VINF(21) = VINS(21)
                    VINF(22) = VINS(22)
                    VINF(8)  = VINS(8)
                    SEUIL = ZERO
                    IF(VINF(22).NE.ZERO) THEN
                      CALL HUJCIC(MATER, SIGF, VINF, SEUIL)
                    ELSE
                      VINF(27) = ZERO
                    ENDIF
                    IF(SEUIL.GT.TOLE)THEN
                      CHGMEC      = .TRUE.
                      VIND(21)    = VINS(21)
                      VIND(22)    = VINS(22)
                      VIND(8)     = VINS(8)
                      VIND(31)    = UN
                    ENDIF
                  ELSE
                    CALL HUJMEI(VINT)
                    VINT(23) = VINF(23)
                    VINT(8)  = MATER(19,2)
                    CALL HUJCIC(MATER, SIGF, VINT, SEUIL)
                    IF(SEUIL.GT.TOLE)THEN
                      CHGMEC      = .TRUE.
                      VIND(21)    = VINT(21)
                      VIND(22)    = VINT(22)
                      VIND(8)     = VINT(8)
                      VIND(31)    = UN
                    ENDIF
                  ENDIF
                ELSEIF((VIND(22).EQ.-UN).AND.(RD.GT.RF))THEN
                  IF(VINS(22).NE.VINF(22))THEN
                    VINF(21) = VINS(21)
                    VINF(22) = VINS(22)
                    VINF(8)  = VINS(8)
                    CALL HUJCIC(MATER, SIGF, VINF, SEUIL)
                    IF(SEUIL.GT.TOLE)THEN
                      CHGMEC      = .TRUE.
                      VIND(21)    = VINS(21)
                      VIND(22)    = VINS(22)
                      VIND(8)     = VINS(8)
                      VIND(31)    = UN
                    ENDIF
                  ELSE
                    CALL HUJMEI(VINT)
                    VINT(23) = VINF(23)
                    VINT(8)  = MATER(19,2)
                    CALL HUJCIC(MATER, SIGF, VINT, SEUIL)
                    IF(SEUIL.GT.TOLE)THEN
                      CHGMEC      = .TRUE.
                      VIND(21)    = VINT(21)
                      VIND(22)    = VINT(22)
                      VIND(8)     = VINT(8)
                      VIND(31)    = UN
                    ENDIF
                  ENDIF
                ENDIF
                GOTO 40
              ENDIF
            ENDIF
          ENDIF

  40    CONTINUE
 999    CONTINUE
1001    FORMAT(A,I3)    
2000    FORMAT(A,28(1X,E12.5)) 
2010    FORMAT(A,4(1X,I2))
        END
