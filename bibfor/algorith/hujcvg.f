      SUBROUTINE HUJCVG(MATER,NVI,VIND,VINF,VINS,NR,YD,DY,R,
     &                  INDI,ITER,ITMAX,INTG,TOLER,BNEWS,MTRAC,
     &                  YE,LRELI,DEBUG,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_20 CRS_1404
      IMPLICIT NONE
C     ----------------------------------------------------------------
C     CRITERE DE CONVERGENCE SUR SOLUTION PROPOSEE PAR NEWTON
C     ----------------------------------------------------------------
C     IN:  MATER  : DONNEES MATERIAU
C          NVI    : NOMBRE DE VARIABLES INTERNES
C          VIND   : VARIABLES INTERNES A T
C          VINF   : VARIABLES INTERNES A T+DT
C          VINS   : VARIABLES INTERNES A T (NON MODIFIEES)
C          NR     : DIMENSION DU SYSTEME NL
C          YD     : VECTEUR SOLUTION A T
C          DY     : INCREMENT DES GRANDEURS INCONNUES A T+DT
C          R      : RESIDU DU SYSTEME NL
C          INDI   : INDICE DES MECANISMES POTENTIEL ACTIFS
C          ITER   : NUMERO ITERATION NEWTON LOCAL
C          ITMAX  : NB MAXI TERATIONS LOCALES
C          INTG   : NUMERO D'INTEGRATION COURANTE
C          TOLER  : TOLERANCE A CONVERGENCE
C          BNEWS  : INDICATEUR LIE A LA TRACTION
C          MTRAC  : INDEICATEUR LIE A LA TRACTION (BIS)
C          YE     : VALEURS DES INCONNUES APRES LCINIT
C          LRELI  : TYPE DE SCHEMA D'INTEGRATION
C     OUT: IRET   : = 0 CONVERGENCE
C                   = 1 ITERATION SUIVANTE
C                   = 2 RE-INTEGRATION
C                   = 3 REDECOUPAGE DU PAS DE TEMPS
C          BNEWS  : INDICATEUR LIE A LA TRACTION MAJ
C     ----------------------------------------------------------------
      REAL*8     MATER(22,2), VIND(NVI), VINF(NVI), VINS(NVI)
      REAL*8     YD(NR),DY(NR),R(NR),TOLER,YE(NR)
      INTEGER    NVI,NR,INDI(7),ITER,IRET,ITMAX,INTG
      LOGICAL    BNEWS(3),MTRAC,LRELI,DEBUG     
C
      INTEGER    NBMECA,NBMECT,NDT,K,I,J,MSUP(2),RESI,IMIN,NDI
      INTEGER    KK,JJ
      REAL*8     ERR,YF(NR),YDT(NR),YFT(NR),DEV(3),PF,QF
      REAL*8     E0,PREF,RTRAC,PTRAC,R8PREM,ZERO,MAXI,RATIO
      REAL*8     UN,DEUX,YET(NR),LAMIN,CINQ,PROB(3)
      LOGICAL    TRACTI,NOCONV,NEGTRA,PROX(4),PROXC(4),MODIF
      LOGICAL    NEGLAM(3),CYCL,EULER,LTRY,IMPOSE,PROBT 
C
      PARAMETER (NDI  = 3   )
      PARAMETER (NDT  = 6   )
      PARAMETER (ZERO = 0.D0)
      PARAMETER (UN   = 1.D0)
      PARAMETER (DEUX = 2.D0)
      PARAMETER (CINQ = 5.D0)
C     ----------------------------------------------------------------
C --- PARAMETRES MATERIAU
      E0    = MATER(1,1)
      PREF  = MATER(8,2)
      PTRAC = MATER(21,2)
      RTRAC = ABS(PREF*1.D-6)
C
      TRACTI = .FALSE.
  
C --- DETERMINATION DU NOMBRE DE MECANISMES POTENTIELLEMENT ACTIFS
      NBMECA = 0
      NBMECT = 0
      DO 10 K = 1, 7
        IF (INDI(K) .GT. 0) THEN
          NBMECT = NBMECT + 1
          IF (INDI(K).LE.8) NBMECA = NBMECA + 1
        ENDIF
 10   CONTINUE

C --- MISE A L'ECHELLE DES CONTRAINTES ET VARIABLES CONTENUES DANS YF
C --- SIGMA * E0, R * E0/PREF
      CALL LCSOVN ( NR , YD , DY , YF )

C --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
      CALL LCEQVN(NR, YD, YDT)
      CALL LCEQVN(NR, YF, YFT)
      CALL LCEQVN(NR, YE, YET)

      DO 20 I = 1, NDT
        YDT(I) = YD(I)*E0
        YFT(I) = YF(I)*E0
        YET(I) = YE(I)*E0
  20  CONTINUE
      DO 30 I = 1, NBMECA
        YDT(NDT+1+I) = YD(NDT+1+I)*E0/ABS(PREF)
        YFT(NDT+1+I) = YF(NDT+1+I)*E0/ABS(PREF)
        YET(NDT+1+I) = YE(NDT+1+I)*E0/ABS(PREF)
  30  CONTINUE    
C --- CONTROLE DU NIVEAU DE CONVERGENCE ATTEINT
      CALL LCNRVN (NR, R, ERR)

C --- INCREMENT DU NOMBRE D'INTEGRATION COURANTE TENTEE
      IF(ITER.EQ.1)INTG = INTG + 1

C --- INCREMENT DU NOMBRE D'ITERATIONS LOCALES
      VINF(35) = VINF(35)+1

C --- SI ITERATION MAXIMALE ATTEINTE
C --- POSSIBILITE DE RELANCER SUIVANT ALGO DE HUJMID

      IF(ITER.LE.ITMAX)THEN
C ----------------------------------
C --- CONTROLE DU NOMBRE DE BOUCLES
C ----------------------------------
        IF(INTG.GT.5)THEN
          IRET = 3
          GOTO 999
        ENDIF
C -------------------------
C ----   CONVERVENCE   ----
C -------------------------
        IF (ERR .LT. TOLER) THEN
          GOTO 60
        ELSE
C --------------------------------------------
C --- NON CONVERGENCE : ITERATION SUIVANTE ---
C --------------------------------------------
          IRET = 1
          IF ((NBMECA.NE.NBMECT).AND.(NBMECA.EQ.0)) THEN
            IF(ERR.GT.1D5)THEN
              IRET = 3
              GOTO 999
            ENDIF
          ELSE
            DO 40 I = 1, 3
              CALL HUJPRJ(I,YFT,DEV,PF,QF)
              IF (((PF+RTRAC-PTRAC)/ABS(PREF)).GE.-R8PREM()) THEN
                DO 50 J = 1, NBMECA
                  IF((INDI(J).EQ.I).OR.(INDI(J).EQ.(I+4)))THEN
                    TRACTI = .TRUE.
                    GOTO 999
                  ENDIF
  50            CONTINUE
              ENDIF
              IF(ABS(PF).GT.E0*1.D1)THEN
                IRET = 3
                GOTO 999                
              ENDIF
  40        CONTINUE
          ENDIF
        GOTO 1000
        ENDIF
      ELSE
C --- SI NEWTON_RELI ACTIF - ON LE RETIRE
        IF(LRELI)THEN
          IRET = 2
          LRELI = .FALSE. 
          GOTO 1000
        ELSE
C --- ITERATIONS MAXI ATTEINT
          IRET = 3
          GOTO 999
        ENDIF
      ENDIF

  60  CONTINUE
C ---------------------------------------
C --- CONTROLE DE LA SOLUTION OBTENUE ---
C --------------------------------------- 
C -------------------------------------------------
C ---- VERIFICATION DES MULTIPLICATEURS PLASTIQUES
C -------------------------------------------------
      MAXI = TOLER
      DO 70 K = 1, NBMECT
        IF (YFT(NDT+1+NBMECA+K).GT.MAXI)MAXI = YFT(NDT+1+NBMECA+K)
  70  CONTINUE

      NEGTRA = .FALSE.

      DO 80 K = 1, NBMECT
        RATIO = YFT(NDT+1+NBMECA+K)/MAXI
        IF (RATIO .LT. (-TOLER)) THEN 
          IF (INDI(K).GT.8) THEN
C ----------------------------------------------
C ---> MECANISME DE TRACTION
C LAMBDA < 0 --> DESACTIVATION DU MECANISME POUR 
C LA PROCHAINE TENTATIVE D'INTEGRATION
C ----------------------------------------------
            BNEWS(INDI(K)-8) = .TRUE.
            NEGTRA = .TRUE.
          ENDIF
        ENDIF
  80  CONTINUE    
      
C -------------------------------------------------------
C ---> MECANISME DE TRACTION
C LAMBDA < 0 --> TENTATIVE SUPPL D'INTEGRATION SI COMPT<5
C -------------------------------------------------------
      IF (NEGTRA) THEN
        IF (INTG.GT.5) THEN
          IRET = 3
          GOTO 1000
        ELSEIF(NBMECA.NE.0)THEN
          CALL LCEQVN(NVI,VIND, VINF)
        ENDIF
        IRET = 2
      ENDIF

C -------------------------------------------------------
C --- ON CONTROLE QUE LES PRESSIONS ISOTROPES PAR PLAN
C     NE PRESENTENT PAS DE TRACTION
C -------------------------------------------------------
      NOCONV = .FALSE.
      DO 90 I = 1, NDI
        CALL HUJPRJ(I, YFT, DEV, PF, QF)
        IF (((PF+RTRAC-PTRAC)/ABS(PREF)).GT.ZERO) THEN
          NOCONV=.TRUE.
          BNEWS(I) = .FALSE.
          IRET = 2
        ENDIF
  90  CONTINUE

C -------------------------------------------------------
C --- SI TRACTION DETECTEE ET NON CONVERGENCE, ON IMPOSE
C --- ETAT DE CONTRAINTES ISOTROPE
C -------------------------------------------------------
      IF((NOCONV).AND.(NBMECT.NE.NBMECA))THEN
        NOCONV=.FALSE.
        DO 100 I = 1, 3
          VIND(23+I)  = ZERO
          VIND(27+I)  = ZERO
          VIND(5+4*I) = ZERO
          VIND(6+4*I) = ZERO
          VIND(7+4*I) = ZERO
          VIND(8+4*I) = ZERO
 100    CONTINUE
        DO 110 I = 1, NDI
          DY(I)     = -YD(I)-DEUX*RTRAC/E0    
          DY(NDI+I) = -YD(I)
 110    CONTINUE
        DO 120 I = 1, NBMECA+NBMECT+1
          DY(NDT+I) = ZERO
 120    CONTINUE
        CALL LCEQVN(NVI,VIND,VINF)
        IRET = 0
      ENDIF

      GOTO 1000
      
 999  CONTINUE 
C -----------------------------------------------
C --- NOMBRE DE TENTATIVES DE RELANCE DEPASSE ---
C -----------------------------------------------
      IF (INTG.GT.5) THEN
C --- ON REGARDE SI L'ETAT INITIAL MATERIAU AVAIT SOLLICITE
C --- UN MECANISME DE TRACTION : ETAT INIT = SIGD, VINS
        IRET = 3
        NOCONV = .TRUE.
        DO 130 I = 1, NDI
          CALL HUJPRJ(I, YDT, DEV, PF, QF)
          IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM()) THEN
            NOCONV=.FALSE.
            IRET = 0
          ENDIF
 130    CONTINUE
        IF(.NOT.NOCONV)THEN
C --- EN POSANT NOCONV = .TRUE., ON CONDUIT L'ALGORITHME PRESENT
C --- A IMPOSER UN ETAT DE CONTRAINTES ISOTROPE COMMUN 
C --- AUX 3 SEUILS PLASTIQUES DE TRACTION 
          DO 140 I = 1, NR
            DY(I) = ZERO
 140      CONTINUE  
          CALL LCEQVN(NVI,VINS,VIND)
          CALL LCEQVN(NVI,VINS,VINF)
          IRET = 0
        ELSE

C -------------------------------------------------------
C --- SI PROCHE TRACTION ET NON CONVERGENCE, ON IMPOSE
C --- ETAT DE CONTRAINTES ISOTROPE
C -------------------------------------------------------
          IMPOSE = .FALSE.
          DO 150 I = 1, NDI
            CALL HUJPRJ(I, YFT, DEV, PF, QF)      
            IF ((ABS(PF-PTRAC)/ABS(PREF)).LT.
     &           CINQ*RTRAC/ABS(PREF))THEN
              IMPOSE = .TRUE.
            ENDIF
 150      CONTINUE
          IF(IMPOSE)THEN
            NOCONV = .FALSE.
            DO 160 I = 1, 3
              VIND(23+I)  = ZERO
              VIND(27+I)  = ZERO
              VIND(5+4*I) = ZERO
              VIND(6+4*I) = ZERO
              VIND(7+4*I) = ZERO
              VIND(8+4*I) = ZERO
 160        CONTINUE
            DO 170 I = 1, NDI
              DY(I)     = -YD(I)-DEUX*RTRAC/E0    
              DY(NDI+I) = -YD(NDI+I)
 170        CONTINUE
            DO 180 I = 1, NBMECA+NBMECT+1
              DY(NDT+I) = ZERO
 180        CONTINUE
            CALL LCEQVN(NVI,VIND,VINF)
            IRET = 0
          ENDIF
        ENDIF

        GOTO 1000
      ENDIF

C -------------------------------------------
C --- GESTION DES NON-CONVERGENCE LOCALES ---
C -------------------------------------------
C --- Y AVAIT IL UN MECANISME CYCLIQUE DEJA DESACTIVE
C     DURANT CETTE TENTATIVE?
      MSUP(1) = 0
      MSUP(2) = 0
      JJ = 0
      DO 200 I=5,8
        IF((VIND(23+I).NE.VINS(23+I)).AND.
     &     (VIND(23+I).EQ.ZERO))THEN
            JJ = JJ+1
            MSUP(JJ) = I
        ENDIF
 200  CONTINUE
      
C --- EXISTE-T-IL UN PB DE TANGENCE ENTRE MECANISMES 
      DO 210 K=1,4
        PROX(K)  = .FALSE.
        PROXC(K) = .FALSE.
 210  CONTINUE    

      DO 220 K = 1, NBMECA
        IF((INDI(K).GT.4).AND.(INDI(K).LT.8))THEN
          KK = INDI(K)-4
          CALL HUJPXD(INDI(K),MATER,YFT,VIND,PROX(KK),PROXC(KK))
        ENDIF
 220  CONTINUE

      PROBT = .FALSE.
      DO 230 I = 1, 3
        PROB(I) = ZERO
        IF(PROX(I))THEN
          PROB(I) = UN
          PROBT   = .TRUE.
        ELSEIF(PROXC(I))THEN
          PROB(I) = DEUX
          PROBT   = .TRUE.
        ENDIF
 230    CONTINUE
 
      IF (PROBT) THEN
        CALL LCEQVN(NVI,VINS,VIND)
        DO 240 I = 1, 3
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
 240    CONTINUE
        IRET = 0
        PROBT = .FALSE.

C --- MECANISME CYCLIQUE A DESACTIVE 
C --- ET DEJA DESACTIVE ANTERIEUREMENT
        IF(JJ.NE.0)THEN
         DO 250 I=1,JJ
            VIND(23+MSUP(I)) = ZERO
 250     CONTINUE
        ENDIF

        CALL LCEQVN(NVI,VIND,VINF)
        IRET = 2
        GOTO 1000
      ENDIF

      IF (TRACTI) THEN
        CALL LCEQVN(NVI,VINS,VIND)
        MODIF = .FALSE.
        DO 260 I = 1, NBMECT
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
          ENDIF
 260    CONTINUE 
       
        DO 270 I = 1, NBMECT
          IF(INDI(I).EQ.8)THEN
            VIND(23+INDI(I)) = ZERO
            MODIF = .TRUE.
          ENDIF
 270    CONTINUE

        MTRAC = .FALSE.
        DO 280 I = 1, 3
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
 280    CONTINUE
        CALL LCEQVN(NVI,VIND,VINF)
        IRET   = 2
        GOTO 1000
      ENDIF

C-----------------------------------------------------------
C --- ESSAIS HEURISTIQUES POUR RELANCER LA RESOLUTION LOCALE
C-----------------------------------------------------------
      MAXI = ZERO
      RESI = 0
      DO 290 I = 1, NR
        IF (ABS(R(I)).GT.MAXI) THEN
          MAXI = ABS(R(I))
          RESI = I
        ENDIF
 290  CONTINUE
      CYCL = .FALSE.
      DO 300 I = 1, NBMECA
        IF ((INDI(I).GT.4) .AND. (INDI(I).LT.8)
     &       .AND. (VIND(INDI(I)).EQ.MATER(18,2))) THEN
          CYCL = .TRUE.
        ENDIF
 300  CONTINUE

C ---------------------------------------------------------------
C --- SI RESIDU LOCAL MAXI PORTE PAR RDEV_CYC => MECANISME RETIRE
C ---------------------------------------------------------------

      IF ((RESI.GT.7).AND.(RESI.LE.7+NBMECA)) THEN
        RESI = RESI - 7
        IF ((INDI(RESI).GT.4).AND.(INDI(RESI).LT.8)) THEN

          CALL LCEQVN(NVI,VINS,VIND)
          VIND(23+INDI(RESI)) = ZERO
          IF(JJ.NE.0)THEN
            DO 310 I=1,JJ
              VIND(23+MSUP(I)) = ZERO
 310        CONTINUE
          ENDIF

C --- EXISTE-T-IL UN MECANISME DEVIATOIRE AYANT LE MEME COMPORTEMENT 
C     QUE CELUI IDENTIFIE PRECEDEMMENT COMME POSANT PROBLEME ? 
          DO 320 I = 1, NBMECA
            IF ((INDI(I).GT.4).AND.(INDI(I).LT.8).AND.
     &         (((MAXI-ABS(R(7+I)))/TOLER).LT.TOLER).AND.
     &         (I.NE.RESI)) THEN
              VIND(23+INDI(I)) = ZERO
            ENDIF
 320      CONTINUE

          IRET = 2
          CALL LCEQVN(NVI,VIND,VINF)
          GOTO 1000
        ELSE
          IRET = 3
        ENDIF
      ENDIF

C ---------------------------------------------------------------
C --- SI MECA CYCLIQUE ALORS ILS SONT RETIRES
C ---------------------------------------------------------------

      IF (CYCL) THEN
        CALL LCEQVN(NVI,VINS,VIND)
        DO 330 I = 1, NBMECA
          IF ((INDI(I).GT.4) .AND. (INDI(I).LT.8)
     &       .AND. (VIND(INDI(I)).EQ.MATER(18,2))) THEN
            VIND(23+INDI(I)) = ZERO
          ENDIF
 330    CONTINUE
        IRET = 2
        CALL LCEQVN(NVI,VIND,VINF)
        GOTO 1000
      ENDIF
      
C ---------------------------------------------------------------
C --- SI MECANISME TRACTION ACTIF => RETIRE DE MPOT
C ---------------------------------------------------------------

      IF (NBMECT.NE.NBMECA) THEN
        CALL LCEQVN(NVI,VINS,VIND)
        IRET   = 2
        DO 340 I = NBMECA+1, NBMECT
          IF (YET(NDT+1+NBMECA+I).EQ.ZERO) THEN
            BNEWS(INDI(I)-8) = .TRUE.
          ENDIF
 340    CONTINUE 
        CALL LCEQVN(NVI,VIND,VINF)
        GOTO 1000
      ENDIF

C ---------------------------------------------------------------
C --- CONTROLE DU PREDICTEUR ELASTIQUE: YE(LAMBDA)
C ---------------------------------------------------------------

      CALL LCEQVN(NVI,VINS,VIND)
      EULER = .TRUE.
      LAMIN = 1.D2
      IMIN  = 0
      DO 350 I = 1, NBMECA
        IF (YET(NDT+1+NBMECA+I).EQ.ZERO) THEN
          IF ((INDI(I).GT.4).AND.(INDI(I).LT.9)) THEN
            VIND(INDI(I)+23) = 0
            EULER = .FALSE.
          ELSEIF (INDI(I).LT.5) THEN
            IF ((ABS(VIND(4*INDI(I)+5)).GT.R8PREM()).OR.
     &       (ABS(VIND(4*INDI(I)+6)).GT.R8PREM())) THEN
              VIND(23+INDI(I)) = -UN
            ELSE
              VIND(23+INDI(I)) = ZERO
            ENDIF
            EULER = .FALSE.
          ENDIF
        ELSEIF(YET(NDT+1+NBMECA+I).LT.LAMIN)THEN
          LAMIN = YET(NDT+1+NBMECA+I)
          IMIN  = I 
        ENDIF
 350  CONTINUE

      IF (.NOT.EULER) THEN
C --- MECANISME CYCLIQUE A DESACTIVE
C --- ET DEJA DESACTIVE ANTERIEUREMENT
        IF(JJ.NE.0)THEN
          DO 360 I=1,JJ
            VIND(23+MSUP(I)) = ZERO
 360      CONTINUE
        ENDIF

        CALL LCEQVN(NVI,VIND,VINF)              
        IRET = 2
        GOTO 1000
      ELSEIF(IMIN.GT.0)THEN
        IF (INDI(IMIN).LT.5) THEN
          VIND(23+INDI(IMIN)) = -UN
        ELSE
          VIND(23+INDI(IMIN)) = ZERO
        ENDIF
        CALL LCEQVN(NVI,VIND,VINF)              
        IRET = 2
        GOTO 1000
      ENDIF

C ---------------------------------------------------------------
C --- DERNIER ESSAI: VALEUR DES CONTRAINTES PRE, DURANT ET POST
C ---------------------------------------------------------------
      LTRY = .FALSE.
      DO 370 I = 1, NDI
        CALL HUJPRJ(I, YD, DEV, PF, QF)
        IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
          BNEWS(I) = .FALSE.       
          LTRY = .TRUE.
        ENDIF
        CALL HUJPRJ(I, YET, DEV, PF, QF)
        IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
          BNEWS(I) = .FALSE.       
          LTRY = .TRUE.
        ENDIF
        CALL HUJPRJ(I, YFT, DEV, PF, QF)
        IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
          BNEWS(I) = .FALSE.
          LTRY = .TRUE.
        ENDIF
 370  CONTINUE

      IF(LTRY)THEN
        CALL LCEQVN(NVI,VIND,VINF)              
        IRET = 2
        GOTO 1000              
      ELSE
        IRET = 3
      ENDIF

1000  CONTINUE 
      END
