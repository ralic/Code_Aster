      SUBROUTINE LCPOPL(LOI,ANGMAS,NMAT,MATERD,MATERF,MOD,DEPS,SIGD,
     &                  SIGF,VIND,VINF)
      IMPLICIT NONE
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
C ======================================================================
C     ----------------------------------------------------------------
C     ROUTINE DE POST-TRAITEMENT POUR CERTAINES LOIS
C     ----------------------------------------------------------------
C     ----------------------------------------------------------------
C     IN  NMAT    :  NOMBRE DE PARAMETRES MATERIAU INELASTIQUE
C         MATERD :  COEFFICIENTS MATERIAU A T
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C         MOD    :  TYPE DE MODELISATION
C         ANGMAS :  ANGLES NAUTIQUES (AFFE_CARA_ELEM)
C     OUT SIGF   :  CONTRAINTE A T+DT
C         VINF   :  VARIABLES INTERNES A T+DT
C     ----------------------------------------------------------------

      INTEGER  NMAT
      REAL*8   MATERD(NMAT,2),MATERF(NMAT,2),SIGF(*),VIND(*),VINF(*)
      REAL*8   ANGMAS(3),SIGD(6),DEPS(6)
      CHARACTER*8 MOD
      CHARACTER*16 LOI
C
      REAL*8   R8VIDE,BID66(6,6),HILL,DSIG(6),NSIG,NEPS,R8PREM
      REAL*8   ZERO,UN,DEUX,DIX
      LOGICAL  REORIE
      INTEGER  I,NDT
C
      PARAMETER (NDT  = 6   )      
      PARAMETER (ZERO = 0.D0)
      PARAMETER (UN   = 1.D0)
      PARAMETER (DEUX = 2.D0)
      PARAMETER (DIX  = 1.D1)
C 
      IF ( LOI(1:6) .EQ. 'LAIGLE') THEN
         CALL LGLDCM( NMAT, MATERF, SIGF, VINF )
      ENDIF
C
C --  CONTRAINTES PLANES
      IF ( MOD(1:6) .EQ. 'C_PLAN' ) SIGF(3) = 0.D0
C
      IF (LOI.EQ.'HAYHURST') THEN
         MATERD(1,1)=MATERD(1,1)*(1.0D0-VIND(11))
         MATERF(1,1)=MATERF(1,1)*(1.0D0-VINF(11))
      ENDIF
      IF (LOI.EQ.'VENDOCHAB') THEN
         MATERD(1,1)=MATERD(1,1)*(1.0D0-VIND(9))
         MATERF(1,1)=MATERF(1,1)*(1.0D0-VINF(9))
      ENDIF

      IF(LOI(1:6).EQ.'HUJEUX')THEN
C --- 1 ORIENTATION DES CONTRAINTES SELON ANGMAS VERS REPERE GLOBAL
         IF (ANGMAS(1).EQ.R8VIDE()) CALL U2MESS('F','ALGORITH8_20')
         REORIE =(ANGMAS(1).NE.ZERO) .OR. (ANGMAS(2).NE.ZERO)
     &         .OR. (ANGMAS(3).NE.ZERO)
         CALL HUJORI ('GLOBA', 1, REORIE, ANGMAS, SIGD, BID66)
         CALL HUJORI ('GLOBA', 1, REORIE, ANGMAS, DEPS, BID66)
         CALL HUJORI ('GLOBA', 1, REORIE, ANGMAS, SIGF, BID66)
          
C --- TRAVAIL DU 2ND ORDRE
         HILL = ZERO
         NSIG = ZERO
         NEPS = ZERO
         DO 10 I = 1, NDT
           DSIG(I) = SIGF(I) - SIGD(I)
           HILL    = HILL + DSIG(I)*DEPS(I)
           NSIG    = NSIG + DSIG(I)**2.D0
           NEPS    = NEPS + DEPS(I)**2.D0
 10      CONTINUE

C --- NORMALISATION DU CRITERE : VARIE ENTRE -1 ET 1
         IF ((NEPS.GT.R8PREM()).AND.(NSIG.GT.R8PREM())) THEN
           VINF(32) = HILL/SQRT(NEPS*NSIG)
         ELSE
           VINF(32) = ZERO
         ENDIF
      
         VINF(34) = ZERO
         DO 20 I=1,8
           IF (ABS(VINF(23+I)-UN).LT.R8PREM()) THEN
             IF (I.EQ.1) VINF(34)=VINF(34)+DIX**ZERO
             IF (I.EQ.2) VINF(34)=VINF(34)+DIX**UN
             IF (I.EQ.3) VINF(34)=VINF(34)+DIX**DEUX
             IF (I.EQ.4) VINF(34)=VINF(34)+DIX**3.D0
             IF (I.EQ.5) VINF(34)=VINF(34)+DIX**4.D0
             IF (I.EQ.6) VINF(34)=VINF(34)+DIX**5.D0
             IF (I.EQ.7) VINF(34)=VINF(34)+DIX**6.D0
             IF (I.EQ.8) VINF(34)=VINF(34)+DIX**7.D0
           ENDIF
 20      CONTINUE

      ENDIF

      END
