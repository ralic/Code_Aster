      SUBROUTINE ASCORM ( MONOAP, TYPCMO, NBSUP, NSUPP, NEQ, NBMODE,
     +                    REPMO1, REPMO2, AMORT, MODAL, ID, TEMPS, 
     +                    RECMOR, RECMOP, TABS, NOMSY, VECMOD, REASUP,
     +                    SPECTR, CORFRE, MUAPDE, TCOSUP, NINTRA, NBDIS,
     +                    F1GUP, F2GUP)
      IMPLICIT  NONE
      INTEGER           NBSUP, NSUPP(*), NEQ, NBMODE, ID, NINTRA,
     +                  TCOSUP(NBSUP,*),NBDIS(NBSUP)
      REAL*8            VECMOD(NEQ,*), SPECTR(*)
      REAL*8            REPMO1(NBSUP,NEQ,*), AMORT(*)
      REAL*8            REPMO2(NBSUP,NEQ,*)
      REAL*8            REASUP(NBSUP,NBMODE,*)
      REAL*8            MODAL(NBMODE,*), TABS(NBSUP,*)
      REAL*8            RECMOR(NBSUP,NEQ,*), RECMOP(NBSUP,NEQ,*)
      REAL*8            TEMPS, F1GUP, F2GUP
      CHARACTER*(*)     TYPCMO
      CHARACTER*16      NOMSY
      LOGICAL           MONOAP, CORFRE, MUAPDE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 06/02/2012   AUTEUR CHANSARD F.CHANSARD 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21 CRS_1404
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL
C        RECOMBINAISON DES REPONSES MODALES
C        POUR LE MULTI_APPUI, CAS DES EXCITATIONS DECORRELEES
C     ------------------------------------------------------------------
C IN  : TYPCMO : TYPE DE COMBINAISON
C IN  : MONOAP : =.TRUE. , CAS DU MONO-APPUI
C IN  : NBSUP  : NOMBRE DE SUPPORT
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBMODE : NOMBRE DE MODES
C IN  : REPMOD : VECTEUR DES REPONSES MODALES
C IN  : AMORT  : VECTEUR DES AMORTISSEMENTS MODAUX
C IN  : MODAL  : VECTEUR DES PARAMETRES MODAUX
C IN  : ID     : DIRECTION
C IN  : TEMPS  : DUREE DE LA PARTIE FORTE SU SEISME (TYPCMO='DSC')
C OUT : RECMOR : VECTEUR DES COMBINAISONS DES REPONSES RIGIDES DES MODES
C OUT : RECMOP : VECTEUR DES COMBINAISONS DES REPONSES PERIO DES MODES
C IN  : NINTRA : NOMBRE d'INTRA-GROUPE
C IN  : NBDIS  : APPARTENANCE DES SUPPORTS AUX INTRAGROUPES
C IN  : F1GUP  : FREQUENCE F1 POUR LA METHODE DE GUPTA 
C IN  : F2GUP  : FREQUENCE F2 POUR LA METHODE DE GUPTA
C     ------------------------------------------------------------------
      INTEGER  NSUP, II, IM, IM1, IM2, IN, IS, IOC
      REAL*8   B1, B12, B2, B22, W0, W1, W12, W2, W22
      REAL*8   B1W1, B2W2, BP1, BP2, WP1, WP2, BP1W1, BP2W2
      REAL*8   XNU, XDE, XXX, XX1, XX2, TEST
      REAL*8   ZERO, DEMI, UN, DEUX, QUATRE, HUIT
      REAL*8   FPROP, ALPHA, ALPHA1, ALPHA2, R8PI, PI
      REAL*8   REPMOR(NBSUP,NEQ,ID), REPMOP(NBSUP,NEQ,ID)
C      
      PI = R8PI()
      ZERO   = 0.D0
      DEMI   = 0.5D0
      UN     = 1.D0
      DEUX   = 2.D0
      QUATRE = 4.D0
      HUIT   = 8.D0
C
      IF (MONOAP.OR.(.NOT.MUAPDE)) THEN 
CCC         NSUP=NBSUP
         NSUP=1
      ELSE
         NSUP=NSUPP(ID)
      ENDIF
C
      DO 2 IS = 1,NSUP      
         DO 3 IN = 1,NEQ
            RECMOR(IS,IN,ID) = ZERO
            RECMOP(IS,IN,ID) = ZERO
 3       CONTINUE
 2    CONTINUE
C
C     --- COMBINAISON EN VALEURS ABSOLUES ---
      IF (TYPCMO(1:3).EQ.'ABS') THEN
C
         DO 5 IM = 1,NBMODE
            CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                    NBMODE, VECMOD, MODAL, ID, REASUP,
     +                    SPECTR, REPMO1, CORFRE, AMORT, MUAPDE,
     +                    TCOSUP, IM, NBDIS)
            DO 4 IS = 1,NSUP 
               DO 6 IN = 1,NEQ
                  XXX = REPMO1(IS,IN,ID)
                  IOC = NBDIS(IS)
                  RECMOP(IOC,IN,ID) = RECMOP(IOC,IN,ID) + 
     +                                      ABS(XXX)
 6             CONTINUE
 4          CONTINUE
 5       CONTINUE
         DO 7 IS = 1,NSUP      
            DO 8 IN = 1,NEQ
               IOC = NBDIS(IS)
               RECMOP(IOC,IN,ID) = RECMOP(IOC,IN,ID) * 
     +                             RECMOP(IOC,IN,ID)
 8          CONTINUE
 7       CONTINUE
C
C     --- AVEC REGLE DES "DIX POUR CENT" ---
      ELSEIF (TYPCMO(1:3).EQ.'DPC') THEN
         IM = 0
 40      CONTINUE
         IM = IM + 1
         IF (IM.LE.NBMODE) THEN
            IM1 = IM
            CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                    NBMODE, VECMOD, MODAL, ID, REASUP,
     +                    SPECTR, REPMO1, CORFRE, AMORT, MUAPDE,
     +                    TCOSUP, IM1, NBDIS)
            DO 41 IS = 1,NSUP
               DO 42 IN = 1,NEQ
                  TABS(IS,IN) = ABS(REPMO1(IS,IN,ID))  
 42            CONTINUE                                 
 41         CONTINUE
 52         CONTINUE
            IF (IM1.LE.NBMODE) THEN
               W1 = SQRT(MODAL(IM1,1))
               II = 0
               DO 44 IM2 = IM1+1,NBMODE
                  W2 = SQRT(MODAL(IM2,1))
                  W0 = DEMI * ( W1 + W2 )
                  TEST = ( W1 - W2 ) / W0
                  IF (ABS(TEST).LE.0.10D0) THEN
                     II = 1
                     IM = IM + 1
              CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                      NBMODE, VECMOD, MODAL, ID, REASUP,
     +                      SPECTR, REPMO2, CORFRE, AMORT, MUAPDE,
     +                      TCOSUP, IM2, NBDIS)
                     DO 45 IS = 1,NSUP      
                        DO 46 IN = 1,NEQ
                           XXX = ABS(REPMO2(IS,IN,ID))
                           TABS(IS,IN)= TABS(IS,IN)  + XXX
 46                     CONTINUE
 45                  CONTINUE
                  ELSE
                     IF (II.EQ.0) GOTO 48
                     IM1 = IM2 - 1
                     GOTO 52
                  ENDIF
 44            CONTINUE
            ENDIF
 48         CONTINUE
            DO 49 IS = 1,NSUP      
               DO 50 IN = 1,NEQ
                   XXX =  TABS(IS,IN)
                   IOC = NBDIS(IS)
                   RECMOP(IOC,IN,ID) = RECMOP(IOC,IN,ID) + 
     +                                       XXX*XXX
 50            CONTINUE
 49         CONTINUE
            GOTO 40
         ENDIF
C
C     --- GUPTA ---
      ELSEIF (TYPCMO(1:3).EQ.'GUP') THEN
      DO 88 IM = 1,NBMODE
C
C       CALCUL DU FACTEUR DE REPONSE RIGIDE DU MODE IM
        FPROP = SQRT(MODAL(IM,1))/(DEUX*PI)
        IF (FPROP.LE.F1GUP) THEN
          ALPHA = ZERO
        ELSEIF (FPROP.GE.F2GUP) THEN
          ALPHA = UN
        ELSE
          ALPHA = LOG(FPROP/F1GUP)/LOG(F2GUP/F1GUP)
        ENDIF
        CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                NBMODE, VECMOD, MODAL, ID, REASUP,
     +                SPECTR, REPMO1, CORFRE, AMORT, MUAPDE,
     +                TCOSUP, IM, NBDIS)
        DO 89 IS = 1,NSUP
          DO 90 IN = 1,NEQ
C
C           CALCUL DES PARTIES RIGIDE ET PERIODIQUE DE LA REPONDE DU 
C           MODE IM
            REPMOR(IS,IN,ID) = ALPHA*REPMO1(IS,IN,ID) 
            REPMOP(IS,IN,ID) = SQRT(UN-ALPHA*ALPHA)*REPMO1(IS,IN,ID)
            IOC = NBDIS(IS)   
C
C           SOMME DES VALEURS ABS DES REPONSES RIGIDES MODALES
            RECMOR(IOC,IN,ID) = RECMOR(IOC,IN,ID)+ ABS(REPMOR(IS,IN,ID))
C
C           SOMME DES CARRES DES REPONSES MODALES DYNAMIQUES
C           METHODE SIMPLE            
            RECMOP(IOC,IN,ID) = RECMOP(IOC,IN,ID)+ 
     +      (REPMOP(IS,IN,ID)*REPMOP(IS,IN,ID))
 90       CONTINUE
 89     CONTINUE   
 88   CONTINUE
C
C     SOMME DES CARRES DES REPONSES MODALES DYNAMIQUES
C     METHODE CQC   
      DO 220 IM1 = 1,NBMODE-1
        FPROP = SQRT(MODAL(IM1,1))/(DEUX*PI)
        IF (FPROP.LE.F1GUP) THEN
          ALPHA1 = ZERO
        ELSEIF (FPROP.GE.F2GUP) THEN
          ALPHA1 = UN
        ELSE
          ALPHA1 = LOG(FPROP/F1GUP)/LOG(F2GUP/F1GUP)
        ENDIF
        B1 = AMORT(IM1)
        W1 = SQRT(MODAL(IM1,1))
        B1W1 = B1 * W1
        B12 = B1 * B1
        W12 = W1 * W1
        DO 222 IM2 = IM1+1,NBMODE
          FPROP = SQRT(MODAL(IM2,1))/(DEUX*PI)
          IF (FPROP.LE.F1GUP) THEN
            ALPHA2 = ZERO
          ELSEIF (FPROP.GE.F2GUP) THEN
            ALPHA2 = UN
          ELSE
            ALPHA2 = LOG(FPROP/F1GUP)/LOG(F2GUP/F1GUP)
          ENDIF
          B2 = AMORT(IM2)
          W2 = SQRT(MODAL(IM2,1))
          B2W2 = B2 * W2
          B22 = B2 * B2
          W22 = W2 * W2
          XNU = (SQRT(B1W1*B2W2)) * (B1W1+B2W2) * W1 * W2 * HUIT
          XDE = (W12-W22)*(W12-W22) + B1W1*B2W2*(W12+W22)*QUATRE
     +                              + (B12+B22)*W12*W22*QUATRE
          XXX = XNU / XDE
          CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                  NBMODE, VECMOD, MODAL, ID, REASUP,
     +                  SPECTR, REPMO1, CORFRE, AMORT, MUAPDE ,
     +                  TCOSUP, IM1, NBDIS)
          CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                  NBMODE, VECMOD, MODAL, ID, REASUP,
     +                  SPECTR, REPMO2, CORFRE, AMORT, MUAPDE ,
     +                  TCOSUP, IM2, NBDIS)
          DO 221 IS = 1,NSUP      
            DO 224 IN = 1,NEQ
              XX1 = SQRT(UN-ALPHA1*ALPHA1)*REPMO1(IS,IN,ID)
              XX2 = SQRT(UN-ALPHA2*ALPHA2)*REPMO2(IS,IN,ID)
              IOC = NBDIS(IS)
              RECMOP(IOC,IN,ID) = RECMOP(IOC,IN,ID) 
     +                            + (DEUX*XX1*XX2*XXX)
 224        CONTINUE
 221      CONTINUE
 222    CONTINUE
 220  CONTINUE      
      ELSE
C
C     --- COMBINAISON QUADRATIQUE SIMPLE ---
      DO 11 IM = 1,NBMODE
         CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                 NBMODE, VECMOD, MODAL, ID, REASUP,
     +                 SPECTR, REPMO1, CORFRE, AMORT, MUAPDE,
     +                 TCOSUP, IM, NBDIS)
         DO 10 IS = 1,NSUP      
            DO 12 IN = 1,NEQ
               XXX = REPMO1(IS,IN,ID)   
               IOC = NBDIS(IS)         
               RECMOP(IOC,IN,ID) = RECMOP(IOC,IN,ID) + 
     +                                   ( XXX * XXX )
 12         CONTINUE
 10      CONTINUE
 11   CONTINUE
C
C     --- CQC AVEC FORMULE DE DER-KIUREGHIAN ---
      IF (TYPCMO(1:3).EQ.'CQC') THEN
         DO 20 IM1 = 1,NBMODE-1
            B1 = AMORT(IM1)
            W1 = SQRT(MODAL(IM1,1))
            B1W1 = B1 * W1
            B12 = B1 * B1
            W12 = W1 * W1
            DO 22 IM2 = IM1+1,NBMODE
               B2 = AMORT(IM2)
               W2 = SQRT(MODAL(IM2,1))
               B2W2 = B2 * W2
               B22 = B2 * B2
               W22 = W2 * W2
               XNU = (SQRT(B1W1*B2W2)) * (B1W1+B2W2) * W1 * W2 * HUIT
               XDE = (W12-W22)*(W12-W22) + B1W1*B2W2*(W12+W22)*QUATRE
     +                                   + (B12+B22)*W12*W22*QUATRE
               XXX = XNU / XDE
               CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                       NBMODE, VECMOD, MODAL, ID, REASUP,
     +                       SPECTR, REPMO1, CORFRE, AMORT, MUAPDE ,
     +                       TCOSUP, IM1, NBDIS)
               CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                       NBMODE, VECMOD, MODAL, ID, REASUP,
     +                       SPECTR, REPMO2, CORFRE, AMORT, MUAPDE ,
     +                       TCOSUP, IM2, NBDIS)
               DO 21 IS = 1,NSUP      
                  DO 24 IN = 1,NEQ
                     XX1 = REPMO1(IS,IN,ID)
                     XX2 = REPMO2(IS,IN,ID)
                     IOC = NBDIS(IS)
                     RECMOP(IOC,IN,ID) = RECMOP(IOC,IN,ID) 
     +                                + (DEUX*XX1*XX2*XXX)
 24               CONTINUE
 21            CONTINUE
 22         CONTINUE
 20      CONTINUE
C
C     --- DSC AVEC FORMULE DE ROSENBLUETH ---
      ELSEIF (TYPCMO(1:3).EQ.'DSC') THEN
         DO 30 IM1 = 1,NBMODE-1
            B1 = AMORT(IM1)
            W1 = SQRT(MODAL(IM1,1))
            BP1 = B1 + ( DEUX / ( TEMPS * W1 ) )
            WP1 = W1 * (SQRT( UN - (BP1*BP1) ) ) 
            BP1W1 = BP1 * W1    
            DO 31 IM2 = IM1+1,NBMODE
               B2 = AMORT(IM2)
               W2 = SQRT(MODAL(IM2,1))
               BP2 = B2 + ( DEUX / ( TEMPS * W2 ) )
               WP2 = W2 * (SQRT( UN - (BP2*BP2) ) )
               BP2W2 = BP2 * W2      
               XDE = ( WP1-WP2 ) / ( BP1W1 + BP2W2 )
               XXX = UN / ( UN + (XDE*XDE) )
               CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                       NBMODE, VECMOD, MODAL, ID, REASUP,
     +                       SPECTR, REPMO1, CORFRE, AMORT, MUAPDE,
     +                       TCOSUP, IM1, NBDIS)
               CALL ASCARM ( NOMSY, MONOAP, NBSUP, NSUPP, NEQ,
     +                       NBMODE, VECMOD, MODAL, ID, REASUP,
     +                       SPECTR, REPMO2, CORFRE, AMORT, MUAPDE,
     +                       TCOSUP, IM2, NBDIS)
               DO 32 IS = 1,NSUP      
                  DO 34 IN = 1,NEQ
                     XX1 = REPMO1(IS,IN,ID)
                     XX2 = REPMO2(IS,IN,ID)
                     IOC = NBDIS(IS)
                     RECMOP(IOC,IN,ID) = RECMOP(IOC,IN,ID) 
     +                                + (DEUX*XX1*XX2*XXX)
 34               CONTINUE
 32            CONTINUE
 31         CONTINUE
 30      CONTINUE
      ENDIF
      ENDIF
C
      END
