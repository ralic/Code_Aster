      SUBROUTINE GERPAS( LOI,     MOD,    IMAT, MATCST,
     &                   N,     NMAT,  Y,
     &                   PAS,     EPS,    TOLY,  COTHE, COEFF,
     &                   DCOTHE,  DCOEFF, E,     NU,    ALPHA,
     &                   SIGI,    EPSD,   DETOT, TPERD, DTPER,
     &                   TPEREF, BZ, X )
      IMPLICIT REAL*8 (A-H,O-Z)
C     ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/01/98   AUTEUR CIBHHLB L.BOURHRARA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C     ----------------------------------------------------------------
C     INTEGRATION DE LOIS DE COMPORTEMENT ELASTO-VISCOPLASTIQUE
C     PAR UNE METHODE DE RUNGE KUTTA
C
C     GESTION AUTOMATIQUE DES PAS DE TEMPS
C     ----------------------------------------------------------------
C     IN  LOI     :  NOM DU MODELE DE COMPORTEMENT
C         MOD     :  TYPE DE MODELISATION
C         IMAT    :  CODE DU MATERIAU CODE
C         MATCST  : 'OUI' SI MATERIAU CST ENTRE T ET T+DT
C                   'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
C                   'NON' SINON
C         N       :  NOMBRE DE VARIABLES INTERNES
C         NMAT    :  NOMBRE DE PARAMETRES MATERIAU
C         Y       :  VARIABLES INTERNES
C         PAS     :  INTERVALLE DE TEMPS TF-TD
C         EPS     :  PARAMETRE DE CONVERGENCE CRIT(3) POUR RK21CO
C         TOLY    :  CRITERE DE CONVERGENCE POUR GERPAS
C         COTHE   :  COEFFICIENTS MATERIAU ELAS A T
C         COEFF   :  COEFFICIENTS MATERIAU INELAS A T
C         DCOTHE  :  COEFFICIENTS MATERIAU ELAS A T+DT
C         DCOEFF  :  COEFFICIENTS MATERIAU INELAS A T+DT
C         E       :  COEFFICIENT MODULE D'YOUNG
C         NU      :  COEFFICIENT DE POISSON
C         ALPHA   :  COEFFICIENT DE DILATATION THERMIQUE
C         SIGI    :  CONTRAINTES A L'INSTANT COURANT
C         EPSD    :  DEFORMATION TOTALE A T
C         DETOT   :  INCREMENT DE DEFORMATION TOTALE
C         TPERD   :  TEMPERATURE A T
C         DTPER   :  INTERVALE DE TEMPERATURE ENTRE T+DT ET T
C         TPEREF  :  TEMPERATURE DE REFERENCE
C         BZ      :  VARIABLE LOGIQUE :
C                   'VRAI' ON UTILISE LE MODELE POLY PILVIN
C                   'FAUX' ON UTILISE LE MODELE POLY B.Z.
C     OUT X       :  INSTANT COURANT
C     ----------------------------------------------------------------
      CHARACTER*16 LOI
      CHARACTER*8  MOD
      PARAMETER (NF=1688,N3F=3*NF)
      INTEGER      IMAT
      CHARACTER*3  MATCST
      LOGICAL BZ
      REAL*8 E, NU, ALPHA
      REAL*8 X, PAS
      REAL*8 TPERD, DTPER, TPEREF
      REAL*8 Y(NF)
      REAL*8 COTHE(3),DCOTHE(3)
      REAL*8 COEFF(NMAT),DCOEFF(NMAT)
      REAL*8 SIGI(6),EPSD(6),DETOT(6)
      REAL*8 WK(N3F),YMFS(NF)
      REAL*8 MAXOUT
      REAL*8 MAXDOM
      PARAMETER     ( MAXDOM = 9.90D-01  )
C
C
      DMG1=0.0D0
C
      MAXOUT=MAXDOM-(EPS)
      NE=0
      NY=N
      NA=NY+N
      KPOK=1
      X=0.0D0
      H=PAS
      IP=0
      DO 10 I=1,N
        YMFS(I)=MAX(TOLY,ABS(Y(I)))
   10 CONTINUE
      XOUT=PAS
   40 CONTINUE
      IF ((X+H).GE.XOUT) THEN
        H=XOUT-X
        IP=1
      ENDIF
      DO 50 I=1,N
        WK(NY+I)=Y(I)
   50 CONTINUE
      XR=X
   60 CONTINUE
      CALL RK21CO(LOI,MOD,IMAT,MATCST,
     &            N,NMAT,Y,KPOK,WK(NE+1),WK(NA+1),H,
     &            COTHE,COEFF,DCOTHE,DCOEFF,E,NU,ALPHA,X,PAS,
     &            SIGI,EPSD,DETOT,TPERD,DTPER,TPEREF,BZ)
      W=ABS(WK(1))/YMFS(1)
      DO 70 I=2,N
        WZ=ABS(WK(I))/YMFS(I)
        IF (WZ.GT.W) W=WZ
   70 CONTINUE
      IF (W.LE.EPS) THEN
        KPOK=1
        IF (IP.EQ.1) THEN
          GOTO 9999
        ELSE
C     ---------------------TEST SUR LA LOI---------------------------
          IF (LOI(1:9).EQ.'VENDOCHAB') THEN
C     ----------------TEST SUR LE NIVEU DE DOMMAGE-------------------
            IF (Y(9).GE.MAXDOM) THEN
              DMG0=(Y(9)-WK(NE+9))-(WK(NA+9)*H)
              IF (DMG0.GE.MAXOUT) THEN
                DO 99 II=1,N
                  Y(II)=(Y(II)-WK(NE+II))-(WK(NA+II)*H)
   99           CONTINUE
                GOTO 9999
              ELSE
                H=(MAXOUT-DMG0)/((WK(NE+9)/H)+WK(NA+9))
                IF (H.GT.PAS) H=PAS
                GOTO 40
              ENDIF
            ELSE
C     --------------FIN TEST SUR LE NIVEU DE DOMMAGE-----------------
              W=W/ABS(EPS)
              W=MAX(W,1.0D-05)
              H=H*W**(-2.0D-01)*9.0D-01
              IF (H.GT.PAS) H=PAS
              GOTO 40
            ENDIF
          ELSE
            W=W/ABS(EPS)
            W=MAX(W,1.0D-05)
            H=H*W**(-2.0D-01)*9.0D-01
            IF (H.GT.PAS) H=PAS
            GOTO 40
          ENDIF
C     ---------------------FIN TEST SUR LA LOI-----------------------
        ENDIF
      ELSE
        KPOK=0
        DMG1=Y(9)
        DO 80 I=1,N
          Y(I)=WK(NY+I)
   80   CONTINUE
        X=XR
        IP=0
C     ---------------------TEST SUR LA LOI---------------------------
        IF (LOI(1:9).EQ.'VENDOCHAB') THEN
C     ----------------TEST SUR LE NIVEU DE DOMMAGE-------------------
          IF (DMG1.GE.MAXDOM) THEN
            DMG0=(DMG1-WK(NE+9))-(WK(NA+9)*H)
            H=(MAXOUT-DMG0)/((WK(NE+9)/H)+WK(NA+9))
            IF (H.LT.1.0D-20) THEN
              CALL UTMESS('S','GERPAS','LE PAS TEND VERS 0 ...')
            ENDIF
            GOTO 60
          ELSE
            W=W/ABS(EPS)
            W=MIN(W,1.0D08)
            H=H*W**(-2.0D-01)*9.0D-01
            IF (H.LT.1.0D-20) THEN
              CALL UTMESS('S','GERPAS','LE PAS TEND VERS 0 ...')
            ENDIF
            GOTO 60
          ENDIF
C     --------------FIN TEST SUR LE NIVEU DE DOMMAGE-----------------
        ELSE
          W=W/ABS(EPS)
          W=MIN(W,1.0D08)
          H=H*W**(-2.0D-01)*9.0D-01
          IF (H.LT.1.0D-20) THEN
            CALL UTMESS('S','GERPAS','LE PAS TEND VERS 0 ...')
          ENDIF
          GOTO 60
        ENDIF
C     ---------------------FIN TEST SUR LA LOI-----------------------
      ENDIF
 9999 CONTINUE
      END
