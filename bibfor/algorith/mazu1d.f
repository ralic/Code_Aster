      SUBROUTINE MAZU1D(EE,MAZARS,SIGM,VARM,EPSM,
     &                  DEPS,ESOUT,SIGP,VARP,OPTION)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/07/2012   AUTEUR FLEJOU J-L.FLEJOU 
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
C        1  -> ICELS : CRITERE ELS
C        2  -> ICELU : CRITERE ELU
C        3  -> IDTRA : ENDOMMAGEMENT EN TRACTION
C        4  -> IDCOM : ENDOMMAGEMENT EN COMPRESSION
C        5  -> IDISS : DISSIPATION D'ENDOMMAGEMENT
C --- ------------------------------------------------------------------
      LOGICAL        RIGI,RESI
C     INDEX DES VARIABLES INTERNES
      INTEGER        ICELS,  ICELU,  IDTRA,  IDCOM,  IDISS
      PARAMETER     (ICELS=1,ICELU=2,IDTRA=3,IDCOM=4,IDISS=5)
C
      REAL*8         GRDEXP,        RAC2
      PARAMETER     (GRDEXP=200.0D0,RAC2=1.4142135623731D+0)
C
      REAL*8         EPSELA,EPSEQ,DOMMAG,RTEMPT,RTEMPC,DDMDEP
      REAL*8         EPSD0,AC,BC,AT,BT,NU,SGELS,EPELU,XX1
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
C     CALCUL DE LA DEFORMATION ELASTIQUE
C     C'EST LA SEULE QUI CONTRIBUE A FAIRE EVOLUER L'ENDOMMAGEMENT
      EPSELA = EPSM + DEPS
C --- CARACTERISTIQUES MATERIAUX
      EPSD0 = MAZARS(1)
C      BETA  = MAZARS(2)
      AC    = MAZARS(3)
      BC    = MAZARS(4)
      AT    = MAZARS(5)
      BT    = MAZARS(6)
      SGELS = MAZARS(7)
      EPELU = MAZARS(8)
      NU    = MAZARS(9)
C --- ------------------------------------------------------------------
C     DEFORMATION EQUIVALENTE
C     ENDOMMAGEMENT PRECEDENT : TRACTION, COMPRESSION
      IF (EPSELA .GE. 0.0D0) THEN
         EPSEQ = ABS(EPSELA)
         DOMMAG = VARM(IDTRA)
      ELSE
         EPSEQ = ABS(EPSELA*RAC2*NU)
         DOMMAG = VARM(IDCOM)
      ENDIF
C     DERIVE DE L'ENDOMMAGEMENT PAR RAPPORT A EPSI
      DDMDEP = 0.0D0
C --- ------------------------------------------------------------------
C     CALCUL DES CONTRAINTES ET VARIABLES INTERNES
C     RESI = OPTIONS FULL_MECA ET RAPH_MECA
C --- ------------------------------------------------------------------
      IF ( RESI ) THEN
C        INITIALISATION DES VARIABLES PLUS
         VARP(IDTRA)=VARM(IDTRA)
         VARP(IDCOM)=VARM(IDCOM)
C        PROGRESSION DE L'ENDOMMAGEMENT
         IF (EPSEQ .GT. EPSD0) THEN
C           CALCUL DE L'ENDOMMAGEMENT
C           IL FAUT EVITER QUE LE CALCUL PLANTE DANS EXP(RTEMP)
C           SI RTEMP TROP GRAND
            IF (EPSELA .GE. 0.0D0) THEN
               RTEMPT = BT*(EPSEQ-EPSD0)
               DOMMAG = 1.0D0 - (EPSD0*(1.0D0-AT)/EPSEQ)
               DDMDEP = EPSD0*(1.0D0-AT)/EPSEQ**2
               IF (RTEMPT.LE.GRDEXP) THEN
                  DOMMAG = DOMMAG - AT*EXP(-RTEMPT)
                  DDMDEP = DDMDEP + AT*BT*EXP(-RTEMPT)
               ENDIF
               IF ( DOMMAG .LE. VARM(IDTRA) ) THEN
                  DOMMAG = VARM(IDTRA)
                  DDMDEP = 0.0D0
               ENDIF
            ELSE
               RTEMPC = BC*(EPSEQ-EPSD0)
               DOMMAG = 1.0D0 - (EPSD0*(1.0D0-AC)/EPSEQ)
               DDMDEP = EPSD0*(1.0D0-AC)/EPSEQ**2
               IF (RTEMPC.LE.GRDEXP) THEN
                  DOMMAG = DOMMAG - AC*EXP(-RTEMPC)
                  DDMDEP = DDMDEP + AC*BC*EXP(-RTEMPC)
               ENDIF
               DDMDEP = -DDMDEP*RAC2*NU
               IF ( DOMMAG .LE. VARM(IDCOM) ) THEN
                  DOMMAG = VARM(IDCOM)
                  DDMDEP = 0.0D0
               ENDIF
            ENDIF
            IF ( DOMMAG .GT. 0.99999D0 ) THEN
               DOMMAG = 0.99999D0
               DDMDEP = 0.0D0
            ENDIF
         ENDIF
C        MISE A JOUR DES VARIABLES INTERNES
         IF (EPSELA .GE. 0.0D0) THEN
            VARP(IDTRA) = DOMMAG
         ELSE
            VARP(IDCOM) = DOMMAG
         ENDIF
C        CALCUL DES CONTRAINTES
         SIGP = EE*EPSELA*(1.0D0-DOMMAG)
C        DISSIPATION IRREVERSIBLE
         XX1 = EE*(1.0D0-DOMMAG)*DEPS
         VARP(IDISS) = VARM(IDISS) + (XX1-(SIGP-SIGM))*DEPS/2.0D0
C        CRITERE ELS, ELU
         IF ( SIGP .LE. 0.0D0 ) THEN
            VARP(ICELS) = ABS(SIGP/SGELS)
            VARP(ICELU) = ABS(EPSELA/EPELU)
         ELSE
            VARP(ICELS) = 0.0D0
            VARP(ICELU) = 0.0D0
         ENDIF
      ENDIF
C
C --- ------------------------------------------------------------------
C     MATRICE TANGENTE
      IF ( RIGI ) THEN
         ESOUT = EE*(1.0D0-DOMMAG) - EE*EPSELA*DDMDEP
      ENDIF
      END
