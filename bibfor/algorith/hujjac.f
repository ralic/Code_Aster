      SUBROUTINE HUJJAC(MOD,NMAT,MATER,INDI,DEPS,NR,YD,YF,YE,NVI,VIND,
     &                  VINS,VINF,DRDY,BNEWS,MTRAC,IRET)
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
C     ----------------------------------------------------------------
C     CALCUL DU JACOBIEN DU SYSTEME NL POUR MODELE DE HUJEUX
C     ----------------------------------------------------------------
C     IN   MOD    :  TYPE DE MODELISATION
C          MATER  :  DONNEES MATERIAU
C          NMAT   :  DIMENSION TABLEAU DONNEES MATERIAU
C          INDI   :  MECANISMES POTENTIELLEMENT ACTIFS 
C          DEPS   :  INCREMENT DEFORMATION
C          NR     :  DIMENSION DU VECTEUR INCONNU
C          YD     :  VECTEUR SOLUTION A T
C          YF     :  VECTEUR SOLUTION A T+DT?
C          YE     :  VECTEUR SOLUTION APRES LCINIT
C          NVI    :  NOMBRE DE VARIABLES INTERNES
C          VIND   :  VARIABLES INTERNES A T
C          VINS   :  VARIABLES INTERNES D'ORIGINE
C          BNEWS  :  INDICATEUR LIES A LA TRACTION
C     OUT  DRDY   :  JACOBIEN DU SYSTEME NL A RESOUDRE
C          BNEWS  :  INDICATEUR LIES A LA TRACTION
C          MTRAC  :  INDICATEUR LIE A LA TRACTION
C          IRET   :  CODE RETOUR (>0 -> PB)
C     ----------------------------------------------------------------
      CHARACTER*8   MOD
      REAL*8        MATER(NMAT,2),DEPS(6),YD(NR),YF(NR),VIND(NVI)
      REAL*8        DRDY(NR,NR),VINS(NR),YE(NR),VINF(NVI)
      INTEGER       INDI(7),NR,NVI,IRET,NMAT
      LOGICAL       BNEWS(3),MTRAC
C
      LOGICAL       PROX(4),PROXC(4),TRACTI,PROBT,MODIF,NEGLAM(3)
      REAL*8        R(NR),YDT(NR),YFT(NR),DEV(3),PF,QF,R8PREM
      REAL*8        PREF,E0,PTRAC,RTRAC,UN,DEUX,ZERO,YET(NR),PROB(3)
      REAL*8        MATERT(22,2)
      INTEGER       NBMECA,NBMECT,I,J,NDT,MSUP(2),K
C
      PARAMETER     (NDT  = 6   )
      PARAMETER     (ZERO = 0.D0)
      PARAMETER     (UN   = 1.D0)
      PARAMETER     (DEUX = 2.D0)
C     ----------------------------------------------------------------
C --- INITIALISATION DE LA JACOBIENNE A ZERO
      DO 10 I = 1, NR
        DO 20 J = 1, NR
          DRDY(I,J) = ZERO
  20    CONTINUE
  10  CONTINUE

C --- PROPRIETES MATERIAU
      PREF  = MATER(8,2)
      E0    = MATER(1,1)
      RTRAC = ABS(PREF*1.D-6)
      PTRAC = MATER(21,2)

C --- REDIMENSIONNEMENT DE YD ET YF POUR S'ADAPTER A HUJJID
C --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
      CALL LCEQVN(NR, YD, YDT)
      CALL LCEQVN(NR, YF, YFT)
      CALL LCEQVN(NR, YE, YET)

      DO 30 I = 1, 6
        YDT(I) = YD(I)*E0
        YFT(I) = YF(I)*E0
        YET(I) = YE(I)*E0
  30  CONTINUE      

      NBMECA = 0
      DO 40 K = 1, 7
        IF (INDI(K) .GT. 0) THEN
          IF (INDI(K).LE.8) NBMECA = NBMECA + 1
        ENDIF
  40  CONTINUE

      NBMECT = NBMECA
      DO 50 I = 1, 7
        IF (INDI(I).GT.8) THEN
          NBMECT = NBMECT + 1
        ENDIF
  50  CONTINUE

      DO 60 I = 1, NBMECA
        YDT(NDT+1+I) = YD(NDT+1+I)*E0/ABS(PREF)
        YFT(NDT+1+I) = YF(NDT+1+I)*E0/ABS(PREF)
        YET(NDT+1+I) = YE(NDT+1+I)*E0/ABS(PREF)
  60  CONTINUE

      DO 70 I = 1, 22
        MATERT(I,1) = MATER(I,1)
        MATERT(I,2) = MATER(I,2)
  70  CONTINUE

      CALL HUJJID(MOD,MATERT,INDI,DEPS,PROX,PROXC,YDT,YFT,VIND,
     &            R,DRDY,IRET)
      
C ------------------------------------------------------------
C ---> SI ECHEC DANS LE CALCUL DE LA JACOBIENNE DR/DY
C ---  ON VERIFIE LES ETATS DE CONTRAINTES DE YF A L'ITERATION
C ---  DE CORRECTION PRECEDENTE. SI TRACTION IL Y A, ON TRAITE 
C ---  LE PB 
C ------------------------------------------------------------
      TRACTI = .FALSE.
      PROBT = .FALSE.
      IF (IRET.EQ.1) THEN
        IRET = 3
        DO 80 I = 1, 3
          IF(PROX(I))THEN
            PROB(I) = UN
            PROBT   = .TRUE.
          ELSEIF(PROXC(I))THEN
            PROB(I) = DEUX
            PROBT   = .TRUE.
          ENDIF
  80    CONTINUE
        DO 90 I = 1, 3
          CALL HUJPRJ(I,YFT,DEV,PF,QF)
          IF (((RTRAC+PF-PTRAC)/ABS(PREF)).GE.-R8PREM())THEN
            TRACTI = .TRUE.
          ENDIF
  90    CONTINUE
      ENDIF

      IF(PROBT)THEN
        CALL LCEQVN(NVI,VINS,VIND)
        DO 100 I = 1, 3
          IF (PROB(I).EQ.UN) THEN
            VIND(I+4)    = MATER(18,2)
            VIND(23+I)   = UN
            VIND(27+I)   = ZERO
            VIND(4*I+5)  = ZERO
            VIND(4*I+6)  = ZERO
            VIND(4*I+7)  = ZERO
            VIND(4*I+8)  = ZERO
            VIND(5*I+31) = ZERO
            VIND(5*I+32) = ZERO
            VIND(5*I+33) = ZERO
            VIND(5*I+34) = ZERO
            VIND(5*I+35) = MATER(18,2)
          ELSEIF (PROB(I).EQ.DEUX) THEN
            VIND(27+I)   = ZERO
          ENDIF
 100    CONTINUE
        IRET = 2
        PROBT = .FALSE.

C --- Y AVAIT IL UN MECANISME CYCLIQUE DEJA DESACTIVE
C     DURANT CETTE TENTATIVE?
      MSUP(1) = 0
      MSUP(2) = 0
      J = 0
      DO 110 I=5,8
        IF((VIND(23+I).NE.VINS(23+I)).AND.
     &     (VIND(23+I).EQ.ZERO))THEN
            J = J+1
            MSUP(J) = I
        ENDIF
 110  CONTINUE
C --- MECANISME CYCLIQUE A DESACTIVE 
C --- ET DEJA DESACTIVE ANTERIEUREMENT
        IF(J.NE.0)THEN
          DO 120 I=1,J
            VIND(23+MSUP(I)) = ZERO
 120      CONTINUE
        ENDIF

        CALL LCEQVN(NVI,VIND,VINF)
      
      ENDIF

      IF (TRACTI) THEN
        CALL LCEQVN(NVI,VINS,VIND)
        MODIF = .FALSE.
        DO 130 I = 1, NBMECT
          IF (YET(NDT+1+NBMECA+I).EQ.ZERO) THEN
            MODIF = .TRUE.
            IF (INDI(I).LE.8) THEN
              IF (INDI(I).LT.5) THEN
                IF ((ABS(VIND(4*INDI(I)+5)).GT.R8PREM()).OR.
     &              (ABS(VIND(4*INDI(I)+6)).GT.R8PREM())) THEN
                  VIND(23+INDI(I)) = -UN
                ELSE
                  VIND(23+INDI(I)) = ZERO
                ENDIF
              ELSE
                VIND(23+INDI(I)) = ZERO
              ENDIF
            ELSE
              BNEWS(INDI(I)-8) = .TRUE.
              NEGLAM(INDI(I)-8) = .TRUE.
            ENDIF
            TRACTI = .FALSE.
          ENDIF
 130    CONTINUE 
       
        DO 140 I = 1, NBMECT
          IF(INDI(I).EQ.8)THEN
            VIND(23+INDI(I)) = ZERO
            MODIF = .TRUE.
          ENDIF
 140    CONTINUE

        MTRAC = .FALSE.
        DO 150 I = 1, 3
C --- ON NE DOIT PAS REACTIVE UN MECANISME DE TRACTION QUI DONNE 
C     COMME PREDICTEUR UN MULTIPLICATEUR PLASTIQUE NEGATIF
          IF(.NOT.NEGLAM(I))THEN
            CALL HUJPRJ(I,YFT,DEV,PF,QF)
C ----------------------------------------------------
C ---> ACTIVATION MECANISMES DE TRACTION NECESSAIRES
C ----------------------------------------------------
            IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
              BNEWS(I) = .FALSE.
              IF(.NOT.MODIF)MTRAC = .TRUE.
            ENDIF
          ENDIF
 150    CONTINUE
        CALL LCEQVN(NVI,VIND,VINF)
        IRET   = 2
      ENDIF


      END
