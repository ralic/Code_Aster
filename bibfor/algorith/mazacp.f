      SUBROUTINE MAZACP(OPTION,NDIMSI,EPSM,DEPS,EPSANE,
     &                  EE,MAZARS,VARM,VARP,SIGP,
     &                  DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2013   AUTEUR PROIX J-M.PROIX 
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
      IMPLICIT NONE
      CHARACTER*16   OPTION
      INTEGER        NDIMSI
      REAL*8         EPSM(*),DEPS(*),VARM(*),VARP(*),SIGP(*),DSIDEP(6,6)
      REAL*8         EPSANE,MAZARS(*)

C --- ------------------------------------------------------------------
C
C  IN :
C     OPTION   : FULL_MECA RAPH_MECA RIGI_MECA_TANG
C     NDIMSI   : DIMENSION DES TENSEURS
C     EPSM     : DEFORMATION TOTALE INSTANT MOINS
C     DEPS     : INCREMENT DE DEFORMATION TOTALE
C     EPSANE   : DEFORMATION ANELASTIQUE : THER, SECH, HYDR
C     EE       : MODULE D'YOUNG INITIAL
C     MAZARS   : LES COEFFICIENTS DE LA LOI, DANS CET ORDRE
C                    EPSD0,K,AC,BC,AT,BT,SIGM_LIM,EPSI_LIM,NU
C     VARM      : VARIABLES INTERNES A L'INSTANT MOINS
C
C  OUT :
C     SIGP     : CONTRAINTE A L'INSTANT PLUS
C     VARP     : VARIABLES INTERNES A L'INSTANT PLUS
C     DSIDEP   : MATRICE TANGENTE
C
C --- ------------------------------------------------------------------
C     VARIABLES INTERNES
C        1  -> ICELS  : CRITERE SIGMA
C        2  -> ICELU  : CRITERE EPSI
C        3  -> IDOMM  : ENDOMMAGEMENT
C        4  -> IEPSQT : VALEUR DE EPSEQT DE TRACTION
C        5  -> IEPSQC : VALEUR DE EPSEQT DE COMPRESSION
C        6  -> IRSIGM : FACTEUR DE TRIAXIALITE EN CONTRAINTE
C        7  -> ITEMP  : TEMPERATURE MAXIMALE ATTEINTE PAR LE MATERIAU
C        8  -> IDISSD : DISSIPATION D'ENDOMMAGEMENT
C --- ------------------------------------------------------------------
C     INDEX DES VARIABLES INTERNES
      INTEGER    ICELS,  ICELU
      PARAMETER (ICELS=1,ICELU=2)
      INTEGER    IDOMM,  IEPSQT,  IEPSQC,  IRSIGM,  IDISSD
      PARAMETER (IDOMM=3,IEPSQT=4,IEPSQC=5,IRSIGM=6,IDISSD=8)
C --- ------------------------------------------------------------------
      LOGICAL    RIGI,RESI
      LOGICAL    ELAS,PROG
      INTEGER    II,JJ,LL

      REAL*8     RAC2,GRDEXP,R8PREM,SGMXVE
      REAL*8     EE,EPSD0,KK,AC,BC,AT,BT,NU,SGELS,EPELU
      REAL*8     AA,BB,COEFF,RR,GAMMA

      REAL*8     TR(6),VECPE(3,3),EPSEQT,EPSEQC,SIGEQC,SIGEQT,SIGEQ
      REAL*8     EPSPLU(6),EPSPRI(3),SIGPRI(6),EPSEQ
      REAL*8     TRSIGA,TRSIGT,TRSIGC,LAMBDA,DEUXMU,DOMMAG,YYP,YY
      REAL*8     RTEMP,EPSELA(6),SIGELA(6)

      REAL*8     KRON(6)
      DATA       KRON/1.0D0,1.0D0,1.0D0,0.0D0,0.0D0,0.0D0/
      DATA       GRDEXP,RAC2/200.0D0,1.4142135623731D0/

C --- ------------------------------------------------------------------
C
C     RIGI_MECA_TANG ->        DSIDEP        -->  RIGI
C     FULL_MECA      ->  SIGP  DSIDEP  VARP  -->  RIGI  RESI
C     RAPH_MECA      ->  SIGP          VARP  -->        RESI
      RIGI   = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI   = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
C
C --- CARACTERISTIQUES MATERIAUX
      EPSD0 = MAZARS(1)
      KK    = MAZARS(2)
      AC    = MAZARS(3)
      BC    = MAZARS(4)
      AT    = MAZARS(5)
      BT    = MAZARS(6)
      SGELS = MAZARS(7)
      EPELU = MAZARS(8)
      NU    = MAZARS(9)
C
      LAMBDA = EE*NU/(1.0D0+NU)/(1.0D0 - 2.0D0*NU)
      DEUXMU = EE/(1.0D0+NU)
C
C --- ------------------------------------------------------------------
C     CALCUL DE LA DEFORMATION ELASTIQUE
C     C'EST LA SEULE QUI CONTRIBUE A FAIRE EVOLUER L'ENDOMMAGEMENT
      CALL R8INIR(6, 0.0D0, EPSELA,1)
      IF ( RESI ) THEN
         DO 100 II = 1 , NDIMSI
            EPSELA(II) = EPSM(II) + DEPS(II) - EPSANE*KRON(II)
100      CONTINUE
      ELSE
         DO 110 II = 1 , NDIMSI
            EPSELA(II) = EPSM(II) - EPSANE*KRON(II)
110      CONTINUE
      ENDIF
C
C --- ------------------------------------------------------------------
C     ON EST OBLIGATOIREMENT EN CONTRAINTES PLANES
      EPSELA(3) = -NU*(EPSELA(1)+EPSELA(2))/(1.0D0-NU)
      DO 130 II = 4 , NDIMSI
         EPSELA(II) = EPSELA(II)/RAC2
130   CONTINUE
C
C --- ------------------------------------------------------------------
C     ON PASSE DANS LE REPERE PROPRE DE EPS
C        VECPE  : VECTEURS PROPRES, MATRICE DE PASSAGE BP VERS BASE INIT
C        EPSPRI : VALEURS PROPRES
      CALL DIAGO3(EPSELA, VECPE , EPSPRI )
C
C --- ------------------------------------------------------------------
C     CALCUL DE EPSPLU = <EPSE>+ DANS LE REPERE INITIAL
C     CALCUL DE EPSEQT = SQRT( TR(<EPSE>+ * <EPSE>+)  )
C               EPSEQC = SQRT( TR(<EPSE>- * <EPSE>-)  )
C               EPSEQ  = SQRT( TR(<EPSE>  * <EPSE> )  )
      EPSEQT = 0.0D0
      EPSEQC = 0.0D0
      EPSEQ  = 0.0D0
      CALL R8INIR(6, 0.D0, TR,1)
      CALL R8INIR(6, 0.D0, EPSPLU,1)
      DO 140 II = 1 , 3
         EPSEQ = EPSEQ + (EPSPRI(II)**2)
         IF (EPSPRI(II) .GT. 0.0D0) THEN
            EPSEQT  = EPSEQT + (EPSPRI(II)**2)
            TR(II)  = EPSPRI(II)
         ELSE
            EPSEQC  = EPSEQC + (EPSPRI(II)**2)
         ENDIF
140   CONTINUE
      EPSEQT = SQRT(EPSEQT)
      EPSEQC = SQRT(EPSEQC)
      EPSEQ  = SQRT(EPSEQ)
C     PASSAGE BASE PROPRE VERS BASE INITIALE
      CALL BPTOBG(TR,EPSPLU,VECPE)
      DO  150 II = 4 , NDIMSI
         EPSPLU(II) = EPSPLU(II)*RAC2
150   CONTINUE
C
C --- ------------------------------------------------------------------
C     CALCUL DES CONTRAINTES ELASTIQUES
C     DANS LE REPERE PRINCIPAL DE DEFORMATION ==> ELLES SONT PLANES
      DO 170 II = 1 , 3
         SIGPRI(II) = LAMBDA*(EPSPRI(1)+EPSPRI(2)+EPSPRI(3))
     &              + DEUXMU*EPSPRI(II)
170   CONTINUE
C     CALCUL DE : |SIGMA| , SIGMA+ , SIGMA- , SIGEQT , SIGEQC , SIGEQ
      TRSIGA = 0.0D0
      TRSIGT = 0.0D0
      TRSIGC = 0.0D0
      SIGEQT = 0.0D0
      SIGEQC = 0.0D0
      SIGEQ  = 0.0D0
      DO 180 II = 1 , 3
         TRSIGA = TRSIGA + ABS(SIGPRI(II))
         SIGEQ  = SIGEQ  + (SIGPRI(II)**2)
         IF (SIGPRI(II) .LT. 0.0D0) THEN
            TRSIGC = TRSIGC +  SIGPRI(II)
            SIGEQC = SIGEQC + (SIGPRI(II)**2)
         ELSE
            TRSIGT = TRSIGT +  SIGPRI(II)
            SIGEQT = SIGEQT + (SIGPRI(II)**2)
         ENDIF
180   CONTINUE
      SIGEQT = SQRT(SIGEQT)
      SIGEQC = SQRT(SIGEQC)
      SIGEQ  = SQRT(SIGEQ)
C --- ------------------------------------------------------------------
C     CALCUL DE GAMMA 3D:[ 0.577... ; 1 ] CP:[ 0.707... ; 1 ]
      GAMMA = 1.0D0
      IF ( 0.57D0*EPSEQT .GT. EPSD0 ) THEN
         IF ( SIGEQC .GT. 0.01D0*EE*EPSD0 ) THEN
            GAMMA = SIGEQC / ABS(TRSIGC)
         ENDIF
      ENDIF
C --- ------------------------------------------------------------------
C     CALCUL DE R : 1 EN TRACTION PURE, 0 EN COMPRESSION
      IF ( TRSIGA .GT. TRSIGT+R8PREM() ) THEN
         RR = TRSIGT / TRSIGA
      ELSE
         RR = 1.0D0
      ENDIF
      IF (RR .LT. 0.00001D0) RR = 0.0D0
      IF (RR .GT. 0.99999D0) RR = 1.0D0
C --- ------------------------------------------------------------------
C     VARIABLES INTERNES PRECEDENTES
      DOMMAG = VARM(IDOMM)
      YY     = VARM(IEPSQT)
C --- ------------------------------------------------------------------
C     CALCUL DES CONTRAINTES ET VARIABLES INTERNES
C     RESI = OPTIONS FULL_MECA ET RAPH_MECA
C --- ------------------------------------------------------------------
      PROG = .FALSE.
      ELAS = .TRUE.
      IF ( RESI ) THEN
         YYP = GAMMA*EPSEQT
         IF ( (YYP.GT.EPSD0).AND.(YYP.GT.YY) ) THEN
            YY = YYP
C           CALCUL DE L'ENDOMMAGEMENT
            AA = 2.0D0*(RR*RR)*(AT-2.0D0*KK*AT+AC) -
     &           RR*(AT-4.0D0*KK*AT+3.0D0*AC) + AC
            BB = (RR*RR)*BT + (1.0D0-RR*RR)*BC
C           IL FAUT EVITER QUE LE CALCUL PLANTE DANS L'EVALUATION
C           DE EXP(RTEMP) SI RTEMP TROP GRAND
            RTEMP = BB*(YY-EPSD0)
            DOMMAG = 1.0D0 - EPSD0*(1.0D0-AA)/YY
            IF (RTEMP .LE. GRDEXP) DOMMAG = DOMMAG - (AA/EXP(RTEMP))
            DOMMAG = MIN( MAX( VARM(IDOMM), DOMMAG ) , 0.99999D0 )
C
            PROG = ( DOMMAG .GT. VARM(IDOMM) )
            ELAS = ( DOMMAG .LE. 0.0D0 )
         ENDIF
C
C        CALCUL DES CONTRAINTES DANS LE REPERE INITIAL
         CALL R8INIR(6, 0.0D0, SIGP,1)
         CALL R8INIR(6, 0.0D0, TR ,1)
         TR(1) = SIGPRI(1)*(1.0D0-DOMMAG)
         TR(2) = SIGPRI(2)*(1.0D0-DOMMAG)
         TR(3) = SIGPRI(3)*(1.0D0-DOMMAG)
C        PASSAGE BASE PROPRE VERS BASE INITIALE
         CALL BPTOBG(TR,SIGP,VECPE)
         DO 200 II = 4, NDIMSI
            SIGP(II) = RAC2*SIGP(II)
200      CONTINUE
C        CORRESPOND AUX CRITERES ELS, ELU DANS LE CAS NON-LINEAIRE 1D
         VARP(ICELS) = SIGEQ*SGMXVE(3,SIGPRI)*(1.0D0-DOMMAG)/SGELS
         VARP(ICELU) = EPSEQ*SGMXVE(3,EPSPRI)/EPELU
C        MISE A JOUR DES VARIABLES INTERNES
         VARP(IDOMM)  = DOMMAG
         VARP(IEPSQT) = YY
         VARP(IEPSQC) = EPSEQC
         VARP(IRSIGM) = RR
         VARP(IDISSD) = 0.0D0
      ENDIF
C
C --- ------------------------------------------------------------------
C     CALCUL DE LA MATRICE TANGENTE DSIDEP
C     RIGI = OPTIONS RIGI_MECA_TANG ET FULL_MECA
C --- ------------------------------------------------------------------
      IF ( RIGI ) THEN
C        MATRICE ELASTIQUE ENDOMMAGEE
         CALL R8INIR(36, 0.0D0, DSIDEP,1)
         LAMBDA = LAMBDA*(1.0D0-DOMMAG)
         DEUXMU = DEUXMU*(1.0D0-DOMMAG)
         DSIDEP(1,1) = LAMBDA+DEUXMU
         DSIDEP(2,2) = LAMBDA+DEUXMU
         DSIDEP(3,3) = LAMBDA+DEUXMU
         DSIDEP(1,2) = LAMBDA
         DSIDEP(2,1) = LAMBDA
         DSIDEP(1,3) = LAMBDA
         DSIDEP(3,1) = LAMBDA
         DSIDEP(2,3) = LAMBDA
         DSIDEP(3,2) = LAMBDA
         DSIDEP(4,4) = DEUXMU
         DSIDEP(5,5) = DEUXMU
         DSIDEP(6,6) = DEUXMU
C ---    CONTRIBUTION DE L'ENDOMMAGEMENT
         PROG = .FALSE.
         IF ( (.NOT.ELAS).AND.PROG ) THEN
            RTEMP = BB*(YY-EPSD0)
            COEFF = EPSD0*(1.0D0-AA)/YY**2
            IF (RTEMP .LE. GRDEXP) COEFF = COEFF + AA*BB/EXP(RTEMP)
            COEFF = COEFF*GAMMA*GAMMA/YY
C
            CALL R8INIR(6, 0.0D0, SIGELA,1)
            CALL R8INIR(6, 0.0D0, TR ,1)
            TR(1) = SIGPRI(1)
            TR(2) = SIGPRI(2)
            TR(3) = SIGPRI(3)
C           PASSAGE BASE PROPRE VERS BASE INITIALE
            CALL BPTOBG(TR,SIGELA,VECPE)
            DO 220 II = 4, NDIMSI
               SIGELA(II) = RAC2*SIGELA(II)
220         CONTINUE
            DO 230 II=1,6
               DO 240 JJ=1,6
                  DSIDEP(II,JJ) = DSIDEP (II,JJ) -
     &                            COEFF*SIGELA(II)*EPSPLU(JJ)
240            CONTINUE
230         CONTINUE
         ENDIF
C ---    CORRECTION CONTRAINTES PLANES
         DO 410 II = 1, NDIMSI
            IF (II.GE.3) GO TO 410
            DO 420 LL = 1 , NDIMSI
               IF (LL.GE.3) GO TO 420
               DSIDEP(II,LL) = DSIDEP(II,LL)
     &                - DSIDEP(II,3)*DSIDEP(3,LL)/DSIDEP(3,3)
420         CONTINUE
410      CONTINUE
      ENDIF
      END
