        SUBROUTINE HUJAYD (MATER,NVI,VIND,VINF,NR,YD,BNEWS,MTRAC)
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
        IMPLICIT NONE
C     ----------------------------------------------------------------
C     CHOIX DES VALEURS DE VIND A AFFECTER A YD
C     ----------------------------------------------------------------
C     IN   MATER  :  PROPRIETES MATERIAU
C          NVI    :  NOMBRE DE VARIABLES INTERNES
C          VIND   :  VARIABLES INTERNES A T
C          VINF   :  VARIABLES INTERNES A T+DT (BASE SUR PRED_ELAS)
C          NR     :  DIMENSION MAXIMALE DE YD 
C     OUT  YD     :  VECTEUR INITIAL
C          VIND   :  IMAGE DE VINF (COHERENCE AVEC ROUTINE HUJMID.F)
C          NR     :  DIMENSION DU SYSTEME NL A RESOUDRE
C     ----------------------------------------------------------------
      INTEGER  NVI, NR
      REAL*8   VIND(NVI), VINF(NVI), YD(NR),MATER(22,2)
      LOGICAL  BNEWS(3),MTRAC
C
      INTEGER  I,II,NBMECA,NDT
      REAL*8   ZERO,UN
C
      PARAMETER (ZERO = 0.D0)      
      PARAMETER (UN   = 1.D0)      
      PARAMETER (NDT  = 6)
C     ----------------------------------------------------------------
C ---  DEFINITION DU NOMBRE DE MECANISMES POTENTIELS ACTIFS
      NBMECA = 0
      DO 10 I = 1, 8
        IF (VINF(23+I) .EQ. UN) NBMECA = NBMECA + 1
 10   CONTINUE
C ---  DIMENSION DU SYSTEME NL A RESOUDRE FONCTION DE NBMECA
      NR = NDT + 1 + 2*NBMECA

C --- AFFECTATION DE VIND A VINF 
C    (COHERENCE AVEC SCHEMA D'INTEGRATION SPECIFIQUE)
C
      CALL LCEQVN(NVI, VINF, VIND)

C ---  YD(NDT+1) = EPS_V^P = VIND(23) A T       
      YD(NDT+1) = VIND(23)        

      II = 1
      DO 30 I = 1, 8
        IF (VIND(23+I) .EQ. UN) THEN
        
          IF (I .NE. 4) THEN
            YD(NDT+1+II)        = VIND(I)
            YD(NDT+1+NBMECA+II) = ZERO
            II                  = II + 1
          ELSE
            YD(NDT+1+NBMECA)    = VIND(I)
            YD(NDT+1+2*NBMECA)  = ZERO
          ENDIF  
          
        ENDIF
 30   CONTINUE

C --- REDIMENSIONNEMENT DE YD ET YF POUR S'ADAPTER A HUJJID
C --- SIGMA/E0, R * PREF/ E0
      DO 40 I = 1, 6
        YD(I) = YD(I)/MATER(1,1)
  40  CONTINUE
C
      DO 50 I = 1, NBMECA
        YD(NDT+1+I) = YD(NDT+1+I)/MATER(1,1)*ABS(MATER(8,2))
  50  CONTINUE

C --- VARIABLE DE GESTION DES MECANISMES DE TRACTION
      DO 60 I = 1, 3
        BNEWS(I) = .TRUE.
  60  CONTINUE
      MTRAC = .FALSE.

C --- MISE A ZERO DU COMPTEUR D'ITERATIONS LOCALES
      VINF(35) = ZERO


      END
