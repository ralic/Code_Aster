      SUBROUTINE ELAGON (NDIM,IMATE,CRIT,SAT,BIOT,
     &                   TM,TP,ALPHA,DEPS,E,NU,SNETM,
     &                   OPTION,SNETP,DSIDEP,P1,P2,DP1,
     &                   DSIDP1,DSIDP2)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ROUTINE ELAGON
C MODELE POUR L ARGILE GONFLANTE (HOXHNA COLLIN) EN CONTRAINTES NET
C
C ======================================================================
C SNET : CONTRAINTES NET : SIGTOT=SIGNET-biot*Pgaz
C                          SIGNET=SIGTOT+biot*Pgaz
C       DANS CETTE VERSION SIP=-biot*Pgaz
C ======================================================================
      IMPLICIT NONE
      INTEGER            NDIM,IMATE
      CHARACTER*16       OPTION
      REAL*8             CRIT(*),TM,TP,ALPHA
      REAL*8             DEPS(6),BIOT,SAT,P1,P2,DP1
      REAL*8             SNETM(6),SNETP(6),DSIDEP(6,6)
      REAL*8             DSIDP1(6),DSIDP2(6)
C
      REAL*8      VALRES(2)
      REAL*8      BETAM,PREF
      REAL*8      DEPSMO,SIGMMO,E,NU,K0,DEUXMU
      REAL*8      KRON(6),DEPSDV(6),SIGMDV(6),SIGPDV(6)
      REAL*8      SIGPMO
      REAL*8      P1M
      REAL*8      VALPAM(1)
      INTEGER     NDIMSI
      INTEGER     K,L
      CHARACTER*2 FB2, CODRET(2)
      CHARACTER*8 NOMRES(2)
      CHARACTER*8 NPREFR(1)

      REAL*8  PRGONF,DPGFP1
C ======================================================================
      INTEGER NDT,NDI
      COMMON /TDIM/   NDT  , NDI
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ======================================================================

C
C     --  INITIALISATIONS :
C     ----------------------
      NDIMSI = 2*NDIM
C
      FB2 = 'F '
      PREF= 1.D6
      P1M=P1-DP1
C
C
C     --  RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
      NOMRES(1)='BETAM'
      NOMRES(2)='PREF'
C
      NPREFR(1) = 'TEMP'
      VALPAM(1) = TM
C
      CALL RCVALA(IMATE,' ','ELAS_GONF ',1,NPREFR,VALPAM,1,
     +                 NOMRES(1),VALRES(1),CODRET(1), FB2 )
      BETAM=VALRES(1)
C
      CALL RCVALA(IMATE,' ','ELAS_GONF ',1,NPREFR,VALPAM,1,
     +                 NOMRES(2),VALRES(2),CODRET(2), FB2 )
      PREF = VALRES(2)


      DEUXMU = E/(1.D0+NU)
      K0 = E/(3.D0*(1.D0-2.D0*NU))


C ======================================================================
C --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ----------
C ======================================================================
      DO 110 K=1,NDI
        DEPS(K)   = DEPS(K) - ALPHA*(TP-TM)
 110  CONTINUE
C
C     --  CALCUL DE DEPSMO ET DEPSDV :
C     --------------------------------
      DEPSMO = 0.D0
      DO 111 K=1,3
        DEPSMO = DEPSMO + DEPS(K)
 111  CONTINUE

      DO 112 K=1,NDIMSI
        DEPSDV(K)   = DEPS(K) - DEPSMO/3.D0 * KRON(K)
 112  CONTINUE

C     --  CALCUL DES CONTRAINTES
C     ----------------------------
C Contraintes moyenne (1/3 trace(sig) )
      SIGMMO = 0.D0
      DO 116 K =1,3
        SIGMMO = SIGMMO + SNETM(K)/3.D0
 116  CONTINUE
C
      DO 117 K = 1,NDIMSI
        SIGMDV(K) = SNETM(K) - SIGMMO * KRON(K)
 117  CONTINUE
      SIGPMO = 0.D0
      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN

C     --------------------------------
C MODELE DE GONFLEMENT APPLIQUE A LA CONTRAINTE MOYENNE
C
C ATTENTION ICI KO INDEP DE LA SUCCION
C ON N APPLIQUE PAS LA NON LINEARITE DEMANDANT LES PARAMETRES R ET BETA
C CE QUI POURRAIT ETRE ENVISAGE DANS UN SECOND TEMPS
C
        SIGPMO = SIGMMO+K0*DEPSMO
     >      +PRGONF(BIOT,BETAM,PREF,P1)-PRGONF(BIOT,BETAM,PREF,P1M)
      ENDIF

      DO 118 K=1,NDIMSI
        SIGPDV(K)  = SIGMDV(K) + DEUXMU * DEPSDV(K)
        SNETP(K)   = SIGPDV(K) + SIGPMO*KRON(K)
 118  CONTINUE

C
C     --  CALCUL DE L'OPERATEUR TANGENT :
C     --------------------------------
      IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG'.OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA'         ) THEN

C     --9.0 INITIALISATION DE L'OPERATEUR TANGENT
C     ---------------------------------------
        DO 125 K=1,6
          DSIDP1(K) = 0.D0
          DSIDP2(K) = 0.D0
          DO 126 L=1,6
            DSIDEP(K,L) = 0.D0
 126      CONTINUE
 125    CONTINUE

        DO 127 K=1,3
          DO 128 L=1,3
            DSIDEP(K,L) = K0-DEUXMU/3.D0
 128      CONTINUE
 127    CONTINUE
        DO 129 K=1,NDIMSI
           DSIDEP(K,K) = DSIDEP(K,K)+DEUXMU
 129    CONTINUE
C
      ENDIF

      IF (OPTION(1:9).EQ.'FULL_MECA'.OR.
     &  OPTION(1:9).EQ.'RAPH_MECA') THEN

        DO 139 K=1,NDIMSI
            DSIDP1(K) = DPGFP1(BIOT,BETAM,PREF,P1)
 139    CONTINUE

      ENDIF
      END
