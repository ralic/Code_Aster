      SUBROUTINE HUJLNF(TOLER,NMAT,MATER,NVI,VIND,VINF,VINS,
     &                  NR,YD,YF,SIGD,SIGF,INDI,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/04/2013   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C TOLE CRS_1404
      IMPLICIT NONE
C     ------------------------------------------------------------
C     CONTROLE DES MECANISMES ACTIVES - POST-TRAITEMENT SPECIFIQUE
C     ------------------------------------------------------------
C     IN :TOLER : TOLERANCE ISSUE DE RESI_INTE_RELA
C         NMAT  : DIMENSION TABLEAU PARAMETRES MATERIAU
C         MATER : PARAMETRES MATERIAU
C         NVI   : NOMBRE DE VARIABLES INTERNES 
C         VIND  : VARIABLES INTERNES A T 
C         VINS  : VARIABLES INTERNES A T (VIND0 - LCPLNL)
C         NR    : DIMENSION SYSTEME NL A RESOUDRE
C         YF    : VECTEUR SOLUTION
C         SIGD  : ETAT DE CONTRAINTES A T
C         INDI  : INDICATEUR DES MECANISMES POT. ACTIFS
C     OUT :
C         SIGF  : ETAT DE CONTRAINTES A T+DT
C         VINF  : VARIABLES INTERNES A T+DT
C         IRET  : CODE RETOUR
C                 0 - OK / 1 - ECHEC
C                 2 - RE-INTEGRATION / 3 - REDECOUPAGE
C ----------------------------------------------------------------
      INTEGER     NVI,NR,IRET,INDI(7),NMAT
      REAL*8      TOLER,VIND(NVI),MATER(NMAT,2),SIGF(6),SIGD(6)
      REAL*8      VINS(NVI),VINF(NVI),YD(NR),YF(NR)
C
      INTEGER     K,NBMECA,KK,NDT,I
      REAL*8      MAXI,UN,E0,PREF,YDT(NR),YFT(NR),RATIO,CUMULI
      REAL*8      MATERT(22,2)
      LOGICAL     NEGMUL(8),CHGMEC
C
      PARAMETER  (UN   = 1.D0)      
      PARAMETER  (NDT  = 6   )
C ----------------------------------------------------------------
C --- PARAMETRES MATERIAU
      E0    = MATER(1,1)
      PREF  = MATER(8,2)

C --- CONSTRUCTION DE NBMECA + NEGMUL
      NBMECA = 0
      DO 10 K = 1, 8
        IF (VIND(23+K) .EQ. UN) NBMECA = NBMECA + 1
        NEGMUL(K) = .FALSE.
 10   CONTINUE

C ---------------------------------------------
C --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
C --- REDIMENSIONNEMENT DE YD ET YF POUR HUJACT
C ---------------------------------------------
      CALL LCEQVN(NR, YD, YDT)
      CALL LCEQVN(NR, YF, YFT)

      DO 20 I = 1, NDT
        YDT(I) = YD(I)*E0
        YFT(I) = YF(I)*E0
  20  CONTINUE
      DO 30 I = 1, NBMECA
        YDT(NDT+1+I) = YD(NDT+1+I)*E0/ABS(PREF)
        YFT(NDT+1+I) = YF(NDT+1+I)*E0/ABS(PREF)
  30  CONTINUE    

C ---------------------------------------
C --- REMPLISSAGE DE VINF A PARTIR DE YFT
C ---------------------------------------

C --- DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE
      VINF(23) = YFT(NDT+1)
      
C ----------------------------------------------
C ---> AFFECTATION DES RAYONS DE YF VERS VINF
C --- ON S'ASSURE QUE (R+>=R-) ET (R+CYC<=RMON)
C ----------------------------------------------
      DO 40 K = 1, NBMECA
        KK = INDI(K)
        IF (YFT(NDT+1+K) .GT. VIND(KK)) THEN
          IF ((KK.GT.4).AND.(KK.LT.8)) THEN
            IF (YFT(NDT+1+K).LE.VIND(KK-4)) THEN
              VINF(KK) = YFT(NDT+1+K)
            ELSE
              VINF(KK) = VIND(KK-4)
            ENDIF
          ELSE
            VINF(KK) = YFT(NDT+1+K)
          ENDIF
        ELSE 
          VINF(KK) = VIND(KK)
        ENDIF   
  40  CONTINUE
      
C ----------------------------------------------
C --- CONSTRUCTION DE NEGMUL POUR HUJACT
C ----------------------------------------------
      MAXI = TOLER
C      DO 50 K = 1, NBMECA
C        IF (YF(NDT+1+NBMECA+K).GT.MAXI) MAXI = YF(NDT+1+NBMECA+K)
C 50   CONTINUE

      DO 60 K = 1, NBMECA
        RATIO = YF(NDT+1+NBMECA+K)/MAXI
        IF (RATIO .LT. -UN) THEN 
          NEGMUL(INDI(K)) = .TRUE.
        ENDIF
 60   CONTINUE    

C ----------------------------------------------
C --- CONSTRUCTION DE SIGF POUR HUJACT
C ----------------------------------------------
      DO 70 I = 1, NDT
        SIGF(I) = YFT(I)
  70  CONTINUE    

C --- APPEL A LA ROUTINE HUJACT
      CHGMEC = .FALSE.

      DO 80 I = 1, 22
        MATERT(I,1) = MATER(I,1)
        MATERT(I,2) = MATER(I,2)
  80  CONTINUE

      CALL HUJACT(MATERT,VIND,VINF,VINS,SIGD,SIGF,NEGMUL,
     &            CHGMEC,INDI)
      
      CUMULI = VINF(35)

      IF(CHGMEC)THEN
        IRET = 2
        CALL LCEQVN (NVI, VIND, VINF)
        VINF(35) = CUMULI
        GOTO 999
      ENDIF
      
 999  CONTINUE
 
      END
