      SUBROUTINE LCEIGV (FAMI,KPG,KSP,NDIM, TYPMOD,IMATE,COMPOR,EPSM,
     &                   DEPS,VIM,NONLOC,OPTION,SIG,VIP,DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/11/2011   AUTEUR BOTTONI M.BOTTONI 
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


      IMPLICIT NONE
      CHARACTER*8        TYPMOD(2)
      CHARACTER*16       COMPOR(*),OPTION
      CHARACTER*(*)      FAMI
      INTEGER            NDIM, IMATE, KSP, KPG
      REAL*8             NONLOC(2),EPSM(6), DEPS(6), VIM(2)
      REAL*8             SIG(6), VIP(2), DSIDEP(6,6,4)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDO_ISOT_BETON (NON LOCAL GRAD_VARI)
C
C IN  FAMI    : FAMILLE DE POINT DE GAUSS
C IN  KPG     : POINT DE GAUSS CONSIDERE
C IN  KSP     :
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
C                               (2) = NB VARIABLES INTERNES / PG
C                               (3) = HYPOTHESE SUR LES DEFORMATIONS
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  NONLOC  : VARIABLES NON LOCALES
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_TANG ->     DSIDEP
C                 FULL_MECA      -> SIG DSIDEP VIP
C                 RAPH_MECA      -> SIG        VIP
C OUT SIG     : CONTRAINTE
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C                 2   -> ETAT DE L'ENDOMMAGEMENT
C                        0: non endommage
C                        1: endommage mais < 1
C                        2: ruine endommagement = 1
C OUT DSIDEP  : MATRICE TANGENTE
C ----------------------------------------------------------------------
C LOC EDFRC1  COMMON CARACTERISTIQUES DU MATERIAU (AFFECTE DANS EDFRMA)
      LOGICAL     RIGI, RESI,ELAS, COUP,SECANT
      INTEGER     NDIMSI, K, L, I, J, M, N, T(3,3),IRET,NRAC
      REAL*8      EPS(6),  TREPS, SIGEL(6), KRON(6)
      REAL*8      RAC2
      REAL*8      RIGMIN,TOLD, FD, D, ENER
      REAL*8      TR(6), RTEMP2
      REAL*8      EPSP(3), VECP(3,3), DSPDEP(6,6)
      REAL*8      DEUMUD(3), LAMBDD, SIGP(3),RTEMP,RTEMP3,RTEMP4
      REAL*8        KDESS, BENDO, LAMBDA, DEUXMU, GAMMA
      REAL*8      SEUIL,EPSTH(2)
      REAL*8      PHI,Q2,Q1,Q0,ETAT,FEL,FSAT,RAC(3)
      REAL*8      COEF1,COEF2,COEF3
      REAL*8      DDOT
      REAL*8      HYDRM,HYDRP,SECHM,SECHP,SREF
      REAL*8      R
      PARAMETER  (RIGMIN = 1.D-5)
      PARAMETER  (TOLD = 1.D-6)
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C ----------------------------------------------------------------------



C -- OPTION ET MODELISATION

      RIGI  = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI  = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      COUP  = (OPTION(6:9).EQ.'COUP')
      IF (COUP) RIGI=.TRUE.
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)
      SECANT=.FALSE.

      CALL RCVARC(' ','HYDR','-',FAMI,KPG,KSP,HYDRM,IRET)
      IF (IRET.NE.0) HYDRM=0.D0
      CALL RCVARC(' ','HYDR','+',FAMI,KPG,KSP,HYDRP,IRET)
      IF (IRET.NE.0) HYDRP=0.D0
      CALL RCVARC(' ','SECH','-',FAMI,KPG,KSP,SECHM,IRET)
      IF (IRET.NE.0) SECHM=0.D0
      CALL RCVARC(' ','SECH','+',FAMI,KPG,KSP,SECHP,IRET)
      IF (IRET.NE.0) SECHP=0.D0
      CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,SREF,IRET)
      IF (IRET.NE.0) SREF=0.D0

C -- INITIALISATION

      CALL LCEIB1 (FAMI,KPG,KSP,IMATE,COMPOR,NDIM,EPSM,
     &             SREF,SECHM,HYDRM,T, LAMBDA,DEUXMU,EPSTH,
     &             KDESS,BENDO,GAMMA,SEUIL)


C -- MAJ DES DEFORMATIONS ET PASSAGE AUX DEFORMATIONS REELLES 3D

      IF (RESI) THEN
        DO 10 K = 1, NDIMSI
          EPS(K) = EPSM(K) + DEPS(K)
     &                   - KRON(K) *  (  EPSTH(2)
     &                                 - KDESS * (SREF-SECHP)
     &                                 - BENDO *  HYDRP     )
 10     CONTINUE
      ELSE
        DO 40 K=1,NDIMSI
          EPS(K) = EPSM(K) - (  EPSTH(1)
     &                       - KDESS * (SREF-SECHM)
     &                       - BENDO *  HYDRM  )     * KRON(K)
40      CONTINUE
      ENDIF

      DO 45 K=4,NDIMSI
        EPS(K) = EPS(K)/RAC2
45    CONTINUE
      IF (NDIMSI.LT.6) THEN
        DO 46 K=NDIMSI+1,6
          EPS(K)=0.D0
46      CONTINUE
      ENDIF

      PHI = NONLOC(1)
      R   = NONLOC(2)

C -- DIAGONALISATION DES DEFORMATIONS

      TR(1) = EPS(1)
      TR(2) = EPS(4)
      TR(3) = EPS(5)
      TR(4) = EPS(2)
      TR(5) = EPS(6)
      TR(6) = EPS(3)
      CALL DIAGP3(TR,VECP,EPSP)

C -- CALCUL DES CONTRAINTES ELAS POSITIVES ET DE L'ENERGIE POSITIVE

      TREPS = EPS(1)+EPS(2)+EPS(3)
      IF (TREPS.GE.0.D0) THEN
        DO 60 K=1,3
          SIGEL(K) = LAMBDA*TREPS
 60     CONTINUE
      ELSE
        DO 61 K=1,3
          SIGEL(K) = 0.D0
 61     CONTINUE
      ENDIF
      DO 15 K=1,3
        IF (EPSP(K).GE.0.D0) THEN
          SIGEL(K) = SIGEL(K) + DEUXMU*EPSP(K)
        ENDIF
15    CONTINUE
      ENER = 0.5D0 * DDOT(3,EPSP,1,SIGEL,1)


C -- CALCUL (OU RECUPERATION) DE L'ENDOMMAGEMENT
      D    = VIM(1)
      ETAT = VIM(2)
      ELAS=.TRUE.

      IF (.NOT.RESI) GOTO 5000
C    ESTIMATION DU CRITERE
      IF (ETAT.EQ.2) GOTO 2000


C      WRITE(6,*) 'ener=     ',ENER
C      WRITE(6,*) 'phi=      ',PHI
C      WRITE(6,*) 'seuil=    ',SEUIL/(1.D0+GAMMA)**2



      FEL = (1+GAMMA)*ENER/(1+GAMMA*D)**2+PHI-R*D-SEUIL


C      WRITE(6,*) 'FEL=    ',FEL


C    CAS ELASTIQUE ?
      IF (FEL.LE.0) THEN
        ETAT = 0
        D    = VIM(1)
        GOTO 2000
      END IF
C    CAS SATURE ?

      FSAT = (1+GAMMA)*ENER/(1+GAMMA)**2+PHI-R-SEUIL

      IF (FSAT.GE.0) THEN
        ETAT = 2
        D    = 1.D0

        GOTO 2000
      END IF

C     ON RESOUD SI NON ELASTIQUE ET NON SATURE


      ELAS=.FALSE.

      Q2 = (2.D0*GAMMA*R-(PHI-SEUIL)*GAMMA**2.D0)/R/GAMMA**2
      Q1 = (R-2.D0*GAMMA*(PHI-SEUIL))/R/GAMMA**2
      Q0 = -((PHI-SEUIL)+(1+GAMMA)*ENER)/R/GAMMA**2

      CALL ZEROP3(Q2,Q1,Q0,RAC,NRAC)

      ETAT = 1
      D = RAC(NRAC)
      IF (D.LT.VIM(1)) THEN
        D = VIM(1)
        ELAS=.TRUE.
      ELSEIF (D.GT.(1.D0-TOLD)) THEN
        D = 1.D0
        ELAS=.TRUE.
        ETAT = 2
      ENDIF

C      WRITE(6,*) 'deltaD=         ', D-VIM(1)



 2000 CONTINUE

C -- CALCUL DES CONTRAINTES



      FD  = (1-D)/(1+GAMMA*D)

      TREPS=EPSP(1)+EPSP(2)+EPSP(3)
      CALL R8INIR(3,0.D0,SIGP,1)

      IF (TREPS.GE.0.D0) THEN
        LAMBDD=LAMBDA * FD
      ELSE
        LAMBDD=LAMBDA
      ENDIF
      DO 201 I=1,3
        IF (EPSP(I).GE.0.D0) THEN
          DEUMUD(I)=DEUXMU*FD
        ELSE
          DEUMUD(I)=DEUXMU
        ENDIF
      SIGP(I)=LAMBDD*TREPS+DEUMUD(I)*EPSP(I)
 201  CONTINUE

      CALL R8INIR(6,0.D0,SIG,1)
      DO 1010 I=1,3
        RTEMP=SIGP(I)
        SIG(1)=SIG(1)+VECP(1,I)*VECP(1,I)*RTEMP
        SIG(2)=SIG(2)+VECP(2,I)*VECP(2,I)*RTEMP
        SIG(3)=SIG(3)+VECP(3,I)*VECP(3,I)*RTEMP
        SIG(4)=SIG(4)+VECP(1,I)*VECP(2,I)*RTEMP
        SIG(5)=SIG(5)+VECP(1,I)*VECP(3,I)*RTEMP
        SIG(6)=SIG(6)+VECP(2,I)*VECP(3,I)*RTEMP
1010  CONTINUE
      DO 18 K=4,NDIMSI
        SIG(K)=RAC2*SIG(K)
18    CONTINUE


      VIP(1) = D
      VIP(2) = ETAT

 5000 CONTINUE


C -- CALCUL DE LA MATRICE TANGENTE

      IF (.NOT.RIGI) GOTO 9000

      FD=(1-D)/(1+GAMMA*D)


      TREPS=EPSP(1)+EPSP(2)+EPSP(3)
      IF (TREPS.GE.0.D0) THEN
        LAMBDD=LAMBDA * FD
      ELSE
        LAMBDD=LAMBDA
      ENDIF
      DO 203 I=1,3
        IF (EPSP(I).GE.0.D0) THEN
          DEUMUD(I)=DEUXMU*FD
        ELSE
          DEUMUD(I)=DEUXMU
        ENDIF
 203  CONTINUE

      IF (OPTION(11:14).EQ.'ELAS') SECANT=.TRUE.
        CALL R8INIR(36, 0.D0, DSPDEP, 1)
        CALL R8INIR(36*4, 0.D0, DSIDEP, 1)

        IF (FD.LT.RIGMIN) THEN
          IF (TREPS.GE.0.D0) THEN
            LAMBDD=LAMBDA * RIGMIN
          ENDIF
          DO 202 I=1,3
            IF (EPSP(I).GE.0.D0) THEN
              DEUMUD(I)=DEUXMU*RIGMIN
            ENDIF
 202      CONTINUE
        ENDIF

        DO 100 K = 1,3
          DO 110 L = 1,3
            DSPDEP(K,L) = LAMBDD
 110      CONTINUE
 100    CONTINUE
        DO 120 K = 1,3
          DSPDEP(K,K) = DSPDEP(K,K) + DEUMUD(K)
 120    CONTINUE
        IF (EPSP(1)*EPSP(2).GE.0.D0) THEN
          DSPDEP(4,4)=DEUMUD(1)
        ELSE
          DSPDEP(4,4)=(DEUMUD(1)*EPSP(1)-DEUMUD(2)*EPSP(2))
     &                                    /(EPSP(1)-EPSP(2))
        ENDIF
        IF (EPSP(1)*EPSP(3).GE.0.D0) THEN
          DSPDEP(5,5)=DEUMUD(1)
        ELSE
          DSPDEP(5,5)=(DEUMUD(1)*EPSP(1)-DEUMUD(3)*EPSP(3))
     &                                    /(EPSP(1)-EPSP(3))
        ENDIF
        IF (EPSP(3)*EPSP(2).GE.0.D0) THEN
          DSPDEP(6,6)=DEUMUD(3)
        ELSE
          DSPDEP(6,6)=(DEUMUD(3)*EPSP(3)-DEUMUD(2)*EPSP(2))
     &                                    /(EPSP(3)-EPSP(2))
        ENDIF

        DO 20 I=1,3
          DO 21 J=I,3
            IF (I.EQ.J) THEN
              RTEMP3=1.D0
            ELSE
              RTEMP3=RAC2
            ENDIF
            DO 22 K=1,3
              DO 23 L=1,3
                IF (T(I,J).GE.T(K,L)) THEN
                IF (K.EQ.L) THEN
                  RTEMP4=RTEMP3
                ELSE
                  RTEMP4=RTEMP3/RAC2
                ENDIF
                RTEMP2=0.D0
                DO 24 M=1,3
                  DO 25 N=1,3
        RTEMP2=RTEMP2+VECP(K,M)*
     &        VECP(I,N)*VECP(J,N)*VECP(L,M)*DSPDEP(N,M)
25                CONTINUE
24              CONTINUE
       RTEMP2=RTEMP2+VECP(I,1)*VECP(J,2)*VECP(K,1)*VECP(L,2)*DSPDEP(4,4)
       RTEMP2=RTEMP2+VECP(I,2)*VECP(J,1)*VECP(K,2)*VECP(L,1)*DSPDEP(4,4)
       RTEMP2=RTEMP2+VECP(I,1)*VECP(J,3)*VECP(K,1)*VECP(L,3)*DSPDEP(5,5)
       RTEMP2=RTEMP2+VECP(I,3)*VECP(J,1)*VECP(K,3)*VECP(L,1)*DSPDEP(5,5)
       RTEMP2=RTEMP2+VECP(I,2)*VECP(J,3)*VECP(K,2)*VECP(L,3)*DSPDEP(6,6)
       RTEMP2=RTEMP2+VECP(I,3)*VECP(J,2)*VECP(K,3)*VECP(L,2)*DSPDEP(6,6)
          DSIDEP(T(I,J),T(K,L),1)=DSIDEP(T(I,J),T(K,L),1)+RTEMP2*RTEMP4
               ENDIF
23            CONTINUE
22          CONTINUE
21        CONTINUE
20      CONTINUE

       DO 26 I=1,6
         DO 27 J=I+1,6
           DSIDEP(I,J,1)=DSIDEP(J,I,1)
27       CONTINUE
26     CONTINUE


C -- CONTRIBUTION DISSIPATIVE
        IF ((.NOT. ELAS).OR.(ETAT.EQ.1.D0)) THEN



C      TREPS = EPS(1)+EPS(2)+EPS(3)
C        DO 960 K=1,3
C          SIGEL(K) = LAMBDA*TREPS
C 960     CONTINUE
C        DO 915 K=1,6
C          SIGEL(K) = SIGEL(K) + DEUXMU*EPS(K)
C 915    CONTINUE

          TR(1) = SIGEL(1)
          TR(2) = SIGEL(2)
          TR(3) = SIGEL(3)
          CALL R8INIR(6,0.D0,SIGEL,1)
          DO 1020 I=1,3
            RTEMP=TR(I)
            SIGEL(1)=SIGEL(1)+VECP(1,I)*VECP(1,I)*RTEMP
            SIGEL(2)=SIGEL(2)+VECP(2,I)*VECP(2,I)*RTEMP
            SIGEL(3)=SIGEL(3)+VECP(3,I)*VECP(3,I)*RTEMP
            SIGEL(4)=SIGEL(4)+VECP(1,I)*VECP(2,I)*RTEMP
            SIGEL(5)=SIGEL(5)+VECP(1,I)*VECP(3,I)*RTEMP
            SIGEL(6)=SIGEL(6)+VECP(2,I)*VECP(3,I)*RTEMP
1020      CONTINUE
          DO 28 K=4,NDIMSI
            SIGEL(K)=RAC2*SIGEL(K)
28        CONTINUE

          COEF1=(1.D0+GAMMA)/(1.D0+GAMMA*D)**2

          COEF2=(1.D0+GAMMA)/(R*(1.D0+GAMMA*D)**2
     &              +2.D0*GAMMA*(1.D0+GAMMA)*ENER/(1.D0+GAMMA*D))
          COEF3=(1.D0+GAMMA*D)**3/(R*(1.D0+GAMMA*D)**3
     &              +2.D0*GAMMA*(1.D0+GAMMA)*ENER)


C dans le cas de la matrice secante, on enleve la partie dissipative
C seulement sur la partie meca/meca
          IF (.NOT.SECANT) THEN
            DO 200 K = 1,NDIMSI
              DO 210 L = 1, NDIMSI
               DSIDEP(K,L,1)=DSIDEP(K,L,1)-COEF1*COEF2*SIGEL(K)*SIGEL(L)
 210          CONTINUE
 200        CONTINUE
          ENDIF


C les autres termes ne sont pas annules car ils permettent de faire
C converger sur la regularisation
          DO 220 K = 1,NDIMSI
             DSIDEP(K,1,3) = COEF2*SIGEL(K)
             DSIDEP(K,1,2) = - DSIDEP(K,1,3)
 220      CONTINUE
          DSIDEP(1,1,4) = COEF3


        ENDIF


 9000 CONTINUE

      END
