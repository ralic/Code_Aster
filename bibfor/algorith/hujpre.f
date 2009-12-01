      SUBROUTINE HUJPRE (ETAT, MOD, CRIT, IMAT, MATER, DEPS, SIGD,
     &                    SIGF, EPSD, VIND, IRET)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/02/2009   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ================================================================
C                   CALCUL DE LA PREDICTION EN CONTRAINTE
C       ================================================================
C       IN      ETAT    COMPORTEMENT DU POINT DU POINT DE CALCUL
C                               'ELASTIC'     > ELASTIQUE
C                               'PLASTIC'     > PLASTIQUE
C               MOD     TYPE DE MODELISATION
C               CRIT    CRITERES  LOCAUX
C                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                                 (ITER_INTE_MAXI == ITECREL)
C                       CRIT(2) = TYPE DE JACOBIEN A T+DT
C                                 (TYPE_MATR_COMP == MACOMP)
C                                 0 = EN VITESSE     > SYMETRIQUE
C                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
C                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
C                                 (RESI_INTE_RELA == RESCREL)
C                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
C                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
C                                 (RESI_INTE_PAS == ITEDEC )
C                                 0 = PAS DE REDECOUPAGE
C                                 N = NOMBRE DE PALIERS
C               DEPS    INCREMENT DE DEFORMATION TOTALE
C               SIGD    CONTRAINTE A T
C               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C               OPT     OPTION DE CALCUL A FAIRE
C                               'RIGI_MECA_TANG'> DSDE(T)
C                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
C                               'RAPH_MECA'     > SIG(T+DT)
C       OUT     SIGF    CONTRAINTE A T+DT
C               IRET    CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
C                              IRET=0 => PAS DE PROBLEME
C                              IRET=1 => ECHEC
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      
      INTEGER     NDT, NDI, IMAT, IRET, IADZI, IAZK24, INDM, I
      REAL*8      CRIT(*), VIND(*)
      REAL*8      EPSD(6), DEPS(6)
      REAL*8      SIGD(6), SIGF(6), DSIG(6), DSDE(6,6)
      REAL*8      MATER(22,2), I1, D13, TOLE1, TRACE, UN, ZERO
      REAL*8      PTRAC, PREF, PISO, MAXI, COHES, FACTOR
      CHARACTER*7 ETAT
      CHARACTER*8 MOD, NOMAIL
      LOGICAL     DEBUG

      COMMON /TDIM/   NDT, NDI
      COMMON /MESHUJ/ DEBUG
      
      DATA   UN, ZERO / 1.D0, 0.D0/
      DATA   D13, TOLE1 /0.33333333334D0, 1.0D-6/ 

      PREF  = MATER(8,2)
      PTRAC = MATER(21,2)
      PISO  = 1.5D0*PTRAC

      IF (ETAT .EQ. 'ELASTIC') THEN
      
        CALL HUJELA (MOD, CRIT, MATER, DEPS, SIGD, SIGF, 
     &               EPSD, IRET)
     
      ELSEIF (ETAT .EQ. 'PLASTIC') THEN
      
        CALL HUJTID (MOD, IMAT, SIGD, VIND, DSDE, IRET)
        IF (IRET.EQ.0) THEN
          CALL LCPRMV (DSDE, DEPS, DSIG)
          CALL LCSOVN (NDT, SIGD, DSIG, SIGF)
          I1   =D13*TRACE(NDI,SIGF)
        ELSE
          IRET =0
          I1   =PISO
          IF (DEBUG) THEN
            CALL TECAEL(IADZI,IAZK24)
            NOMAIL = ZK24(IAZK24-1+3) (1:8)
            WRITE(6,'(10(A))')
     &     'HUJPRE :: ECHEC DANS LA PSEUDO-PREDICTION ELASTIQUE DANS ',
     &     'LA MAILLE ',NOMAIL
          ENDIF
        ENDIF
        
        IF ((I1 -PISO)/PREF .LE. TOLE1) THEN
          IF (DEBUG) THEN
            CALL TECAEL(IADZI,IAZK24)
            NOMAIL = ZK24(IAZK24-1+3) (1:8)
            WRITE(6,'(10(A))')
     &      'HUJPRE :: TRACTION DANS LA PSEUDO-PREDICTION ELASTIQUE ',
     &      'DANS LA MAILLE ',NOMAIL
          ENDIF
          CALL HUJELA (MOD, CRIT, MATER, DEPS, SIGD, SIGF, 
     &                 EPSD, IRET)
        ENDIF
        
      ENDIF


C ---> CONTROLE QU'AUCUNE COMPOSANTE DU VECTEUR SIGF NE SOIT POSITIVE
      DO 10 I = 1, NDT
        DSIG(I)= SIGF(I) - SIGD(I)
  10  CONTINUE

      MAXI  = UN
      INDM = 0
      COHES = -1.D2+PTRAC
      FACTOR = UN

      DO 20 I = 1, NDI
        IF(SIGF(I).GT.PTRAC)THEN
          FACTOR = (-SIGD(I)+COHES)/DSIG(I)
          IF((FACTOR.GT.ZERO).AND.(FACTOR.LT.MAXI)) THEN
            MAXI = FACTOR
            INDM = I
          ENDIF
        ENDIF
  20  CONTINUE


C ---> SI IL EXISTE SIG(I)>0, ALORS MODIFICATION DE LA PREDICTION      
      IF(INDM.GT.0)THEN
        IF(DSIG(INDM).GT.TOLE1)THEN
          DO 30 I = 1, NDT
            DSIG(I) = MAXI * DSIG(I)
  30      CONTINUE
          CALL LCSOVN (NDT, SIGD, DSIG, SIGF)        
          IF (DEBUG) THEN
            WRITE (6,'(A,A,E12.5)')
     &      'HUJPRE :: APPLICATION DE FACTOR POUR MODIFIER ',
     &      'LA PREDICTION -> FACTOR =',FACTOR
            WRITE(6,'(A,6(1X,E12.5))')'SIGF =',(SIGF(I),I=1,NDT)
          ENDIF
        ELSE
          IRET = 1
          CALL TECAEL(IADZI,IAZK24)
          NOMAIL = ZK24(IAZK24-1+3) (1:8)
          IF(DEBUG)WRITE(6,'(10(A))')
     &    'HUJPRE :: TRACTION DANS LA PSEUDO-PREDICTION ELASTIQUE ',
     &    'DANS LA MAILLE ',NOMAIL
        ENDIF
      ENDIF
      
      END
