      SUBROUTINE CALCMD(NP1,A,A0,N,M,TYPJ,VG,VGT,VGT0,VD,VD0,RR,RR0,RI,
     &                  N2,IER,ICHOC,PREMAC,PREREL,MTMP1,MTMP2,TTR,
     &                  U,W,D,INTGE1,INTGE2,INDX,INDXF,LOC)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C TOLE  CRP_21
C-----------------------------------------------------------------------
C DESCRIPTION : DIAGONALISATION DE LA MATRICE DE RAIDEUR DU SYSTEME
C -----------   A L'INSTANT N+1
C
C               APPELANTS : ALITMI, NEWTON
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER    NP1
      REAL*8     A(NP1,*), A0(NP1,*)
      INTEGER    N, M, TYPJ
      REAL*8     VG(NP1,*), VGT(NP1,*), VGT0(NP1,*),
     &           VD(NP1,*), VD0(NP1,*),
     &           RR(*), RR0(*), RI(*)
      INTEGER    N2, IER, ICHOC
      REAL*8     PREMAC, PREREL,
     &           MTMP1(NP1,*), MTMP2(NP1,*),
     &           TTR(N2,*), U(*), W(*), D(*)
      INTEGER    INTGE1(*), INTGE2(*), INDX(*), INDXF(*)
      LOGICAL    LOC(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER    IB, K, L, I, J, IFAIL, IIMA, INEG, IPROD
      REAL*8     TEMP, TEMP1, TOL, EPS
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  ABS, SIGN, SQRT
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL   PRMAMA, TSTJAC,
C    &           VPZBAA, VPZHEA, VPZVPH, INDEXX, VPZQRS, VPZHEB, VPZBAB,
C    &           TRVPMD
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C 0. INITIALISATIONS
C    ---------------
      IER = 0
      INEG = 0
      IIMA = 0
      TYPJ = 0
      IB = 2
      TOL = 1.0D-08
      EPS = PREMAC
C
C---- CAS DU SYSTEME EN CHOC A L'INSTANT N+1
C----  => DIAGONALISATION DE LA MATRICE DE RAIDEUR
C
      IF ( ICHOC.EQ.1 ) THEN
C
C======================================================================
C 1.     TEST SUR LA VARIATION DU JACOBIEN
C======================================================================
C
         CALL TSTJAC(NP1,N,TYPJ,A,A0)
C
C------- VARIATION DE LA MATRICE DE RAIDEUR ENTRE LES INSTANTS N ET N+1
C
         IF ( TYPJ.EQ.1 ) THEN
C
C---------- SI UN SEUL VECTEUR PROPRE EST SOUHAITE
C
            IF ( (N.EQ.1).AND.(M.EQ.1) ) THEN
               RR(1) = SQRT(A(1,1))
               RR0(1) = SQRT(A(1,1))
               VD(1,1) = 1.0D0
               VD0(1,1) = 1.0D0
               VGT(1,1) = 1.0D0
               VGT0(1,1) = 1.0D0
C
C---------- SINON : CALCUL DES N VALEURS PROPRES ET DES M PREMIERS
C---------- VECTEURS PROPRES A DROITE ET A GAUCHE
C
            ELSE
C
C=======================================================================
C 2.           MATRICE A ET VECTEURS PROPRES A DROITE VD
C=======================================================================
C
C............. STOCKAGE DE LA MATRICE A DANS LA MATRICE MTMP2
               DO 1 J = 1, N
                  DO 2 I = 1, N
                     MTMP2(I,J) = A(I,J)
 2                CONTINUE
 1             CONTINUE
C
C............. CONDITIONNEMENT DE LA MATRICE MTMP2
               CALL VPZBAA(N,IB,MTMP2,NP1,K,L,D)
C
C............. STOCKAGE DE LA MATRICE CONDITIONNEE MTMP2 DANS MTMP1
               DO 3 J = 1, N
                  DO 4 I = 1, N
                     MTMP1(I,J) = MTMP2(I,J)
 4                CONTINUE
 3             CONTINUE
C
C............. CALCUL DE LA MATRICE DE HESSENBERG, ECRASEMENT DE MTMP1
               CALL VPZHEA(N,K,L,MTMP1,NP1,INTGE1)
C
C............. STOCKAGE DE LA MATRICE MTMP1 DANS LA MATRICE MTMP2
               DO 5 J = 1, N
                  DO 6 I = 1, N
                     MTMP2(I,J) = MTMP1(I,J)
 6                CONTINUE
 5             CONTINUE
C
C............. CALCUL DES VALEURS PROPRES DE LA MATRICE MTMP2
               CALL VPZVPH(N,EPS,PREREL,MTMP2,NP1,RR,RI,INTGE2,IFAIL)
               IF ( IFAIL.NE.0 )
     &            CALL U2MESS('F','ALGORITH_62')
C
C............. VERIFICATION DES VALEURS PROPRES DE LA MATRICE MTMP2
C............. (VALEURS PROPRES REELLES)
               DO 100 I = 1, N
                  IF ( RI(I).GT.(TOL*RR(I)) ) THEN
                     IER = 1
                     IF ( RR(I).LT.0.0D0 ) INEG = 1
                     IF ( RI(I).GT.ABS(TOL*RR(I)) ) IIMA = 1
                  ENDIF
 100           CONTINUE
C
               IF ( IER.NE.0 ) THEN
C
                  IF ( IIMA.EQ.1 )
     &               CALL U2MESS('A','ALGORITH_63')
C
                  IF ( INEG.EQ.1 )
     &               CALL U2MESS('A','ALGORITH_64')
C
                  GO TO 999
C
               ENDIF
C
C............. RECHERCHE DES M PREMIERS MODES (TRI SUR LA PARTIE REELLE
C............. DES VALEURS PROPRES DE LA MATRICE MTMP2)
               DO 7 I = 1, N
                  LOC(I) = .FALSE.
 7             CONTINUE
               CALL INDEXX(N,RR,INDX)
               DO 8 I = 1, M
                  LOC(INDX(I)) = .TRUE.
 8             CONTINUE
C
C............. CALCUL DES M PREMIERS VECTEURS PROPRES
C............. DE LA MATRICE MTMP1
               CALL VPZQRS(N,M,MTMP1,NP1,LOC,RI,RR,VD,NP1,TTR,N2,U,W,
     &                     EPS,IFAIL)
               IF ( IFAIL.NE.0 )
     &            CALL U2MESS('F','ALGORITH_65')
C
C............. RETOUR AUX M PREMIERS VECTEURS PROPRES
C............. DE LA MATRICE CONDITIONNEE
               CALL VPZHEB(K,L,M,MTMP1,NP1,INTGE1,VD,NP1,N)
C
C............. RETOUR AUX M PREMIERS VECTEURS PROPRES
C............. DE LA MATRICE A (VD)
               CALL VPZBAB(N,K,L,M,D,VD,NP1)
C
C............. RECUPERATION DES M VALEURS PROPRES REELLES
C............. DE LA MATRICE A
               CALL TRVPMD(NP1,N,M,RR,LOC,INDXF,INTGE1,INTGE2,U,W)
C
C............. CALCUL DE LA MATRICE D'ORDRE M CONSTITUEE DES M PREMIERS
C............. VECTEURS PROPRES A DROITE (VD)
               DO 11 J = 1, N
                  DO 12 I =1, N
                     MTMP1(I,J) = VD(I,INDXF(J))
 12               CONTINUE
 11            CONTINUE
               DO 13 J = 1, N
                  DO 14 I = 1, N
                     VD(I,J) = MTMP1(I,J)
 14               CONTINUE
 13            CONTINUE
C
C=======================================================================
C 3.           TRANSPOSEE DE LA MATRICE A ET VECTEURS PROPRES
C              A GAUCHE VG
C=======================================================================
C
C............. STOCKAGE DE LA MATRICE AT (TRANSPOSEE DE LA MATRICE A)
C............. DANS LA MATRICE MTMP2
               DO 15 J = 1, N
                  DO 16 I = 1, N
                     MTMP2(I,J) = A(J,I)
 16               CONTINUE
 15            CONTINUE
C
C............. CONDITIONNEMENT DE LA MATRICE MTMP2
               CALL VPZBAA(N,IB,MTMP2,NP1,K,L,D)
C
C............. STOCKAGE DE LA MATRICE CONDITIONNEE MTMP2
C............. DANS LA MATRICE MTMP1
               DO 19  J = 1, N
                  DO 20 I = 1, N
                     MTMP1(I,J) = MTMP2(I,J)
 20               CONTINUE
 19            CONTINUE
C
C............. CALCUL DE LA MATRICE DE HESSENBERG, ECRASEMENT DE MTMP1
               CALL VPZHEA(N,K,L,MTMP1,NP1,INTGE1)
C
C............. STOCKAGE DE LA MATRICE MTMP1 DANS LA MATRICE MTMP2
               DO 21 J = 1, N
                  DO 22 I = 1, N
                     MTMP2(I,J) = MTMP1(I,J)
 22               CONTINUE
 21            CONTINUE
C
C............. CALCUL DES VALEURS PROPRES DE LA MATRICE MTMP2
               CALL VPZVPH(N,EPS,PREREL,MTMP2,NP1,RR,RI,INTGE2,IFAIL)
               IF ( IFAIL.NE.0 )
     &            CALL U2MESS('F','ALGORITH_62')
C
C............. VERIFICATION DES VALEURS PROPRES DE LA MATRICE MTMP2
C............. (VALEURS PROPRES REELLES)
               IER = 0
               INEG = 0
               IIMA = 0
               DO 200 I = 1, N
                  IF ( RI(I).GT.(TOL*RR(I)) ) THEN
                     IER = 1
                     IF ( RR(I).LT.0.0D0 ) INEG = 1
                     IF ( RI(I).GT.ABS(TOL*RR(I)) ) IIMA = 1
                  ENDIF
 200           CONTINUE
C
               IF ( IER.NE.0 ) THEN
C
                  IF ( IIMA.EQ.1 )
     &               CALL U2MESS('A','ALGORITH_63')
C
                  IF ( INEG.EQ.1 )
     &               CALL U2MESS('A','ALGORITH_64')
C
                  GO TO 999
C
               ENDIF
C
C............. RECHERCHE DES M PREMIERS MODES (TRI SUR LA PARTIE REELLE
C............. DES VALEURS PROPRES DE LA MATRICE MTMP2)
               DO 23 I = 1, N
                  LOC(I) = .FALSE.
 23            CONTINUE
               CALL INDEXX(N,RR,INDX)
               DO 24 I = 1, M
                  LOC(INDX(I)) = .TRUE.
 24            CONTINUE
C
C............. CALCUL DES M PREMIERS VECTEURS PROPRES
C............. DE LA MATRICE MTMP1
               CALL VPZQRS(N,M,MTMP1,NP1,LOC,RI,RR,VG,NP1,TTR,N2,U,W,
     &                     EPS,IFAIL)
               IF ( IFAIL.NE.0 )
     &            CALL U2MESS('F','ALGORITH_65')
C
C............. RETOUR AUX M PREMIERS VECTEURS PROPRES
C............. DE LA MATRICE CONDITIONNEE
               CALL VPZHEB(K,L,M,MTMP1,NP1,INTGE1,VG,NP1,N)
C
C............. RETOUR AUX M PREMIERS VECTEURS PROPRES
C............. DE LA MATRICE AT (VG)
               CALL VPZBAB(N,K,L,M,D,VG,NP1)
C
C............. RECUPERATION DES M VALEURS PROPRES REELLES
C............. DE LA MATRICE AT
               CALL TRVPMD(NP1,N,M,RR,LOC,INDXF,INTGE1,INTGE2,U,W)
C
C............. CALCUL DE LA MATRICE D'ORDRE M CONSTITUEE DES M PREMIERS
C............. VECTEURS PROPRES A GAUCHE (VG)
               DO 27 J = 1, N
                  DO 28 I = 1, N
                     MTMP2(I,J) = VG(I,INDXF(J))
 28               CONTINUE
 27            CONTINUE
               DO 29 J = 1, N
                  DO 30 I = 1, N
                     VG(I,J) = MTMP2(I,J)
 30               CONTINUE
 29            CONTINUE
C
C............. CALCUL DE LA MATRICE DIAGONALE B = (TRANSPOSEE DE VG)*VD
               DO 31 J = 1, N
                  DO 32 I = 1, N
                     MTMP2(J,I) = VG(I,J)
 32               CONTINUE
 31            CONTINUE
               IPROD = 1
               CALL PRMAMA(IPROD,MTMP2,NP1,N,N,VD,NP1,N,N,
     &                           MTMP1,NP1,N,N,IER)
               IF ( IER.NE.0 )
     &            CALL U2MESS('F','ALGORITH_66')
C
C............. NORMALISATION DES VECTEURS PROPRES VD ET VG
               DO 33 J = 1, M
                  TEMP = ABS(MTMP1(J,J))
                  TEMP1 = SIGN(TEMP,MTMP1(J,J))
                  DO 34 I = 1, N
                     VD(I,J) = VD(I,J)/TEMP1
 34               CONTINUE
 33            CONTINUE
               DO 35 J = 1, N
                  DO 36 I = 1, N
                     IF ( J.LE.M ) THEN
                        VGT(J,I) = VG(I,J)
                     ELSE
                        VGT(J,I) = 0.0D0
                     ENDIF
 36               CONTINUE
 35            CONTINUE
               DO 37 I = 1, N
                  RR(I) = SQRT(RR(I))
 37            CONTINUE
C
C............. SAUVEGARDE DES RESULTATS DANS RR0, VGT0, ET VD0
               DO 38 I = 1, N
                  RR0(I) = RR(I)
 38            CONTINUE
               DO 39 J = 1, N
                  DO 40 I = 1, M
                     VGT0(I,J) = VGT(I,J)
 40               CONTINUE
 39            CONTINUE
               DO 41 J = 1, M
                  DO 42 I = 1, N
                     VD0(I,J) = VD(I,J)
 42               CONTINUE
 41            CONTINUE
C
            ENDIF
C
C---------- FIN DU CALCUL DES ELEMENTS PROPRES
C
C------- PAS DE VARIATION DE LA MATRICE DE RAIDEUR
C------- ENTRE LES INSTANTS N ET N+1
C
         ELSE
C
C.......... RECUPERATION DES VALEURS CALCULEES A LA DIAGONALISATION
C.......... PRECEDENTE
C
            DO 43 I = 1, N
               RR(I) = RR0(I)
 43         CONTINUE
            DO 44 J = 1, N
               DO 45 I = 1, M
                  VGT(I,J) = VGT0(I,J)
 45            CONTINUE
 44         CONTINUE
            DO 46 J = 1, M
               DO 47 I = 1, N
                  VD(I,J) = VD0(I,J)
 47            CONTINUE
 46         CONTINUE
C
         ENDIF
C
C------- FIN DU CAS SYSTEME EN CHOC
C
C---- CAS PARTICULIER OU LE SYSTEME EST EN VOL A L'INSTANT N+1
C----  => PAS DE DIAGONALISATION
C
      ELSE
C
         DO 48 I = 1, N
            RR(I) = SQRT(A(I,I))
 48      CONTINUE
C
      ENDIF
C
 999  CONTINUE
C
C --- FIN DE CALCMD.
      END
