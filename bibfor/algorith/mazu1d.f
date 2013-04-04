      SUBROUTINE MAZU1D(EE,MAZARS,SIGM,VARM,EPSM,
     &                  DEPS,ESOUT,SIGP,VARP,OPTION)
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
C RESPONSABLE FLEJOU J-L.FLEJOU
C ----------------------------------------------------------------------
C
C              LOI DE MAZARS UNILATERALE EN 1D
C
C ----------------------------------------------------------------------
      IMPLICIT       NONE
      CHARACTER*(*)  OPTION
      REAL*8         EE,SIGM,EPSM,DEPS,ESOUT,SIGP
      REAL*8         MAZARS(*),VARM(*),VARP(*)
C
C ----------------------------------------------------------------------
C  IN :
C     EE       : MODULE D'YOUNG INITIAL
C     MAZARS   : LES COEFFICIENTS DE LA LOI, DANS CET ORDRE
C                    EPSD0,BETA,AC,BC,AT,BT,SIGM_ELS,EPSI_ELU,NU
C     SIGM     : CONTRAINTE A L'INSTANT MOINS
C     VARM     : VARIABLES INTERNES A L'INSTANT MOINS
C     EPSM     : DEFORMATION TOTALE A L'INSTANT MOINS
C     DEPS     : INCREMENT DE DEFORMATION TOTALE
C     OPTION   : FULL_MECA,     :  MAT  VI  SIG  :  RIGI  RESI
C                RAPH_MECA      :       VI  SIG  :        RESI
C                RIGI_MECA_TANG :  MAT           :  RIGI
C
C  OUT :
C     ESOUT    : MODULE SECANT OU TANGENT
C     SIGP     : CONTRAINTE A L'INSTANT PLUS
C     VARP     : VARIABLES INTERNES A L'INSTANT PLUS
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
      INTEGER    IDOMM,  IEPSQT,  IEPSQC,  IRSIGM,  ITEMP,  IDISSD
      PARAMETER (IDOMM=3,IEPSQT=4,IEPSQC=5,IRSIGM=6,ITEMP=7,IDISSD=8)
C --- ------------------------------------------------------------------
      LOGICAL    RIGI,RESI
C
      INTEGER    INDXVP
      REAL*8     GRDEXP,        RAC2
      PARAMETER (GRDEXP=200.0D0,RAC2=1.4142135623731D+00)
C
      REAL*8     EPSELA,DOMMAG,DDMDEQ,DOMMT,DOMMC,DOMMTC
      REAL*8     EPSD0,AC,BC,AT,BT,NU,SGELS,EPELU,XX1
      REAL*8     AA,BB,RR,DEQTEP,EPSEQ,RTEMP,EPSQT,EPSQC,EPSQTC
C --- ------------------------------------------------------------------
C
C     RIGI_MECA_TANG ->       DSIDEP       -->  RIGI
C     FULL_MECA      ->  SIG  DSIDEP  VIP  -->  RIGI  RESI
C     RAPH_MECA      ->  SIG          VIP  -->        RESI
      RIGI = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      RIGI = .TRUE.
C
C --- ------------------------------------------------------------------
C --- CARACTERISTIQUES MATERIAUX
      EPSD0 = MAZARS(1)
C       KK    = MAZARS(2)
      AC    = MAZARS(3)
      BC    = MAZARS(4)
      AT    = MAZARS(5)
      BT    = MAZARS(6)
      SGELS = MAZARS(7)
      EPELU = MAZARS(8)
      NU    = MAZARS(9)
C --- ------------------------------------------------------------------
C     CALCUL DES ENDOMMAGEMENTS PRECEDENTS : TRACTION, COMPRESSION
      EPSQT = VARM(IEPSQT)
      IF (EPSQT.GT.EPSD0) THEN
C        CALCUL DE L'ENDOMMAGEMENT
         DOMMAG = 1.0D0 - (EPSD0*(1.0D0-AT)/EPSQT)
C        IL FAUT EVITER QUE LE CALCUL PLANTE DANS EXP(RTEMP)
         RTEMP = BT*(EPSQT-EPSD0)
         IF (RTEMP.LE.GRDEXP) DOMMAG = DOMMAG - AT*EXP(-RTEMP)
         DOMMT = MIN(MAX(DOMMAG,0.0D0),0.99999D0 )
      ELSE
         DOMMT = 0.0D0
      ENDIF
      EPSQC = VARM(IEPSQC)
      IF (EPSQC.GT.EPSD0) THEN
C        CALCUL DE L'ENDOMMAGEMENT
         DOMMAG = 1.0D0 - (EPSD0*(1.0D0-AC)/EPSQC)
C        IL FAUT EVITER QUE LE CALCUL PLANTE DANS EXP(RTEMP)
         RTEMP = BC*(EPSQC-EPSD0)
         IF (RTEMP.LE.GRDEXP) DOMMAG = DOMMAG - AC*EXP(-RTEMP)
         DOMMC = MIN(MAX(DOMMAG,0.0D0),0.99999D0 )
      ELSE
         DOMMC = 0.0D0
      ENDIF
C --- ------------------------------------------------------------------
C     CALCUL DE LA DEFORMATION ELASTIQUE
C     C'EST LA SEULE QUI CONTRIBUE A FAIRE EVOLUER L'ENDOMMAGEMENT
      EPSELA = EPSM + DEPS
C     DEFORMATION EQUIVALENTE
C     ENDOMMAGEMENT ET DEFORMATION EQUIVALENTE PRECEDENTS
      IF (EPSELA.GE.0.0D0) THEN
         EPSEQ  = ABS(EPSELA)
         DOMMTC = DOMMT
         EPSQTC = EPSQT
         RR     = 1.0D0
         DEQTEP = 1.0D0
         INDXVP = IEPSQT
         AA     = AT
         BB     = BT
      ELSE
         EPSEQ  = ABS(EPSELA*RAC2*NU)
         DOMMTC = DOMMC
         EPSQTC = EPSQC
         RR     = 0.0D0
         DEQTEP = -RAC2*NU
         INDXVP = IEPSQC
         AA     = AC
         BB     = BC
      ENDIF
C --- ------------------------------------------------------------------
C     ENDOMMAGEMENT PRECEDENT
      DOMMAG = DOMMTC
C     DERIVE DE L'ENDOMMAGEMENT PAR RAPPORT A EPSI
      DDMDEQ = 0.0D0
C --- ------------------------------------------------------------------
C     CALCUL DES CONTRAINTES ET VARIABLES INTERNES
C     RESI = OPTIONS FULL_MECA ET RAPH_MECA
C --- ------------------------------------------------------------------
      IF ( RESI ) THEN
C        MISE A JOUR DES VARIP : PAR DEFAUT ELLES NE VARIENT PAS
         VARP(IEPSQT) = EPSQT
         VARP(IEPSQC) = EPSQC
C        PROGRESSION DE L'ENDOMMAGEMENT
         IF ( (EPSEQ.GT.EPSD0).AND.(EPSEQ.GT.EPSQTC)) THEN
C           CALCUL DE L'ENDOMMAGEMENT
            DOMMAG = 1.0D0 - (EPSD0*(1.0D0-AA)/EPSEQ)
            DDMDEQ = EPSD0*(1.0D0-AA)/EPSEQ**2
C           IL FAUT EVITER QUE LE CALCUL PLANTE DANS EXP(RTEMP)
            RTEMP = BB*(EPSEQ-EPSD0)
            IF (RTEMP.LE.GRDEXP) THEN
               DOMMAG = DOMMAG - AA*EXP(-RTEMP)
               DDMDEQ = DDMDEQ + AA*BB*EXP(-RTEMP)
            ENDIF
            DDMDEQ = DDMDEQ*DEQTEP
            IF ( DOMMAG .LE. DOMMTC ) THEN
               DOMMAG = DOMMTC
               DDMDEQ = 0.0D0
            ENDIF
            IF ( DOMMAG .GT. 0.99999D0 ) THEN
               DOMMAG = 0.99999D0
               DDMDEQ = 0.0D0
            ENDIF
         ENDIF
C        CALCUL DES CONTRAINTES
         SIGP = EE*EPSELA*(1.0D0-DOMMAG)
C        CORRESPOND AUX CRITERES ELS, ELU DANS LE CAS NON-LINEAIRE
         VARP(ICELS) = SIGP/SGELS
         VARP(ICELU) = EPSELA*SQRT(1.0D0 + 2.0D0*NU*NU)/EPELU
C        MISE A JOUR DES VARIABLES INTERNES
         VARP(IDOMM)  = DOMMAG
         VARP(INDXVP) = MAX( EPSEQ, EPSQTC )
         VARP(IRSIGM) = RR
         VARP(ITEMP)  = 0.0D0
C        DISSIPATION IRREVERSIBLE
         XX1 = EE*(1.0D0-DOMMAG)*DEPS
         VARP(IDISSD) = VARM(IDISSD) + (XX1-(SIGP-SIGM))*DEPS/2.0D0
      ENDIF
C
C --- ------------------------------------------------------------------
C     MATRICE TANGENTE
      IF ( RIGI ) THEN
         ESOUT = EE*(1.0D0-DOMMAG) - EE*EPSELA*DDMDEQ
      ENDIF
      END
