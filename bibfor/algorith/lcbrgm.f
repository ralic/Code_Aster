      SUBROUTINE LCBRGM (NDIM, TYPMOD, IMATE, EPSM, DEPS,
     &                   VIM, OPTION, SIG, VIP,DSIDPT,
     &                   PROJ,CDRETT)

C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2010   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C LOI DE COMPORTEMENT ELASTIQUE ENDO HETEROGENE
C (AVEC REGULARIS. DES CONTRAINTES)
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_TANG ->     DSIDEP
C                 FULL_MECA      -> SIG DSIDEP VIP
C                 RAPH_MECA      -> SIG        VIP
C OUT SIG     : CONTRAINTE
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C                 2   -> ELASTIQUE (0) OU POINTE (1) RUPT AMORCAGE (2)
C                     -> RUPT PROPAGATION (3)
C                 3   -> CONTRAINTE RUPT AMORCAGE
C                 4   -> CONTRAINTE RUPT PROPAGATION
C                 5   -> NUMERO ELEMENT POINTE 1
C                 6   -> NUMERO ELEMENT POINTE 2 (SI RUPT AMORCAGE)
C                 7   -> IT DE NEWTON DE RUPTURE
C                 8   -> IT DE NEWTON COURANTE
C                 9   -> COORX POINTE DE FISSURE (APRES RUPT PROPA)
C                 10  -> COORY POINTE DE FISSURE (APRES RUPT PROPA)
C OUT DSIDPT  : MATRICE TANGENTE
C OUT PROJ    : NE SERT PLUS A RIEN
C ----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       OPTION
      INTEGER            NDIM, IMATE, CDRETT
      REAL*8             EPSM(12),DEPS(12),VIM(*)
      REAL*8             SIG(6),VIP(*),DSIDPT(6,6,2),PROJ(6,6)
C ----------------------------------------------------------------------




      LOGICAL     CPLAN,RESI,RIGI,NONLIN
      INTEGER     NDIMSI,K,L,ETAT,REUSS

      REAL*8      EPS(6),EPSR(6),TREPS,COPLAN,SIGEL(6)
      REAL*8      KRON(6),SIGELL(6),W(3),WORK(9),Z(3,3)
      REAL*8      RIGMIN,FD,D,DM,E,NU,LAMBDA,DEUXMU

      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)

      REAL*8      DMAX
      PARAMETER  (DMAX = 0.999999D0)
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------



C ======================================================================
C                            INITIALISATION
C ======================================================================

C -- OPTION ET MODELISATION

      RESI = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'

      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      NDIMSI = 2*NDIM

C -- COUPURE ISOTROPE DE LA REGULARISATION SI ENDOMMAGEMENT SATURE
C
      IF (NINT(PROJ(1,1)) .EQ. 1) THEN
        CDRETT=NINT(PROJ(1,1))
      ENDIF
      
C -- LECTURE DES CARACTERISTIQUES ELASTIQUES

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')

      E     = VALRES(1)
      NU    = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)

C -- DEFORMATIONS

      CALL DCOPY(NDIMSI, EPSM,1, EPS,1)
      CALL DCOPY(NDIMSI, EPSM(7),1, EPSR,1)
      IF (RESI) THEN
        CALL DAXPY(NDIMSI, 1.D0, DEPS,1, EPS,1)
        CALL DAXPY(NDIMSI, 1.D0, DEPS(7),1, EPSR,1)
      END IF



C ======================================================================
C                         CONTRAINTES ELASTIQUES
C ======================================================================

C -- CALCUL DES CONTRAINTES ELASTIQUES

      TREPS = EPS(1)+EPS(2)+EPS(3)
      DO 60 K=1,NDIMSI
        SIGEL(K)  = LAMBDA*TREPS*KRON(K) + DEUXMU*EPS(K)
 60   CONTINUE

C ======================================================================
C                 INTEGRATION DE LA LOI DE COMPORTEMENT
C ======================================================================
      
      IF (RESI) THEN
        DM   = VIM(1)
        ETAT = NINT(VIP(2))
        VIP(3)= VIM(3)
        VIP(4)= VIM(4)
        
        IF (ETAT.EQ.3) THEN
           D = DMAX
           ETAT = 3
        ELSEIF (ETAT.EQ.2) THEN
           D = DMAX
           ETAT = 2
        ELSEIF (ETAT.EQ.1) THEN
            D = DM
            ETAT = 1
        ELSEIF (ETAT.EQ.0) THEN
            D = DM
            ETAT = 0
        ENDIF
        
        DO 30 K=1,NDIMSI
         SIG(K) = (1-D) * SIGEL(K)
 30     CONTINUE
 
         VIP(1) = D
         VIP(2) = ETAT
         
      ELSE
        D   = VIM(1)
        ETAT=NINT(VIM(2))
      END IF


C ======================================================================
C                            MATRICE TANGENTE
C ======================================================================

      IF (RIGI) THEN
        CALL R8INIR(72, 0.D0, DSIDPT, 1)
        FD = 1-D
        DO 100 K = 1,3
          DO 110 L = 1,3
            DSIDPT(K,L,1) = FD*LAMBDA
 110      CONTINUE
 100    CONTINUE
        DO 120 K = 1,NDIMSI
          DSIDPT(K,K,1) = DSIDPT(K,K,1) + FD*DEUXMU
 120    CONTINUE

        IF (CPLAN) THEN
          DO 300 K=1,NDIMSI
            IF (K.EQ.3) GO TO 300
            DO 310 L=1,NDIMSI
              IF (L.EQ.3) GO TO 310
              DSIDPT(K,L,1)=DSIDPT(K,L,1)
     &        - 1.D0/DSIDPT(3,3,1)*DSIDPT(K,3,1)*DSIDPT(3,L,1)
 310        CONTINUE
 300      CONTINUE
        ENDIF


      END IF
      
      END
