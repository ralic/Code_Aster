      SUBROUTINE MGAUSS(CARA,A,B,DIM,NORDRE,NB,DET,IRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 22/04/2008   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_4

      IMPLICIT NONE

      CHARACTER*(*) CARA
      INTEGER DIM,NB,NORDRE,IRET
      REAL*8 A(DIM,DIM),B(DIM,NB),DET

C ----------------------------------------------------------------------
C  RESOLUTION PAR FACTORISATION LU D'UN SYSTEME LINEAIRE
C ----------------------------------------------------------------------
C  VARIABLES D'ENTREE
C  K4  CARA : CHAINE DE CARACTERES PRECISANT CERTAINES CARACTERISTIQUES
C             DE LA RESOLUTION :

C       CARA(1:1) = /'N' : SYSTEME "NORMAL"    : A*X=B
C                   /'T' : SYSTEME "TRANSPOSE" : A'*X=B

C       CARA(2:2) = /'F' : ON S'ARRETE EN ERREUR <F> EN CAS DE PROBLEME
C                   /'C' : SI PROBLEME, ON "CONTINUE" AVEC IRET > 0

C       CARA(3:3) = /'S' : ON VEUT ALLER SUREMENT : LA METHODE DE
C                          RESOLUTION EST EN PRINCIPE PLUS PRECISE
C                          ET SI INFO=2, ON IMPRIME DES INFORMATIONS
C                          SUR LA QUALITE DE LA SOLUTION:
C                          REMARQUE: LA MATRICE 'A' N'EST PAS MODIFIEE
C                         (POUR EVITER UNE MODIFICATION DE CETTE MATRICE
C                         ,ON FAIT PASSER DANS LA ROUTINE DE RESOLUTION
C                         UNE COPIE DE 'A', QUI ELLE PEUT ETRE MODIFIEE
C                         SI UN EQUILIBRAGE EST NECESSAIRE)
C                   /'V' : ON VEUT ALLER VITE, ON NE CONTROLE PAS
C                          LA QUALITE DE LA SOLUTION
C                   /'W' : ON VEUT ALLER ENCORE PLUS VITE ET UTILISER
C                          L'ANCIENNE ROUTINE MGAUSS DE G. RATEAU
C                          (RENOMMEE MGAUSW.F)
C                    ATTENTION : SI 'W', LA MATRICE A EST MODIFIEE


C       CARA(4:4) = /'D' : ON VEUT AUSSI LE DETERMINANT DE A
C                   /'P' : ON NE VEUT PAS CALCULER LE DETERMINANT DE A
C                   ATTENTION : LE CALCUL DU DETERMINANT PEUT PROVOQUER
C                                ASSEZ FACILEMENT DES "OVERFLOW"

C  REAL*8      A(DIM, DIM) : MATRICE CARREE PLEINE
C                            A n'est pas modifiee sauf si cara(3:3)='W'
C  REAL*8      B(DIM, NB)  : SECONDS MEMBRES
C  INTEGER     DIM         : DIMENSION DE A
C  INTEGER     NORDRE      : ORDRE DE LA MATRICE (NORDRE<DIM)
C  INTEGER     NB          : NOMBRE DE SECONDS MEMBRES

C  VARIABLES DE SORTIE
C  REAL*8      B(DIM, NB)  : A-1 * B
C  REAL*8      DET         : DETERMINANT DE A
C  INTEGER     IRET        : CODE RETOUR
C                              IRET = 0 : OK
C                              IRET > 0 : PB

C ----------------------------------------------------------------------
      INTEGER N,NRHS,LDB,LDX,IFM,NIV,I,J,LDA,LDAF,INDI
      INTEGER*4 IPIV4(DIM),INF4,IWORK4(DIM)
      INTEGER VALI(2)
      REAL*8 AF(DIM,DIM),R(DIM),C(DIM),X(DIM,NB),RCOND
      REAL*8 WORK(4*DIM),FERR(NB),BERR(NB),R8NNEM,AA(DIM,DIM)
      CHARACTER*1 FACT,EQUED,TRANS2
      CHARACTER*4 CARA2
      CHARACTER*24 VALK(2)
      LOGICAL LTRANS,LSTOP,LDET,LRET
C----------------------------------------------------------------------
      CALL MATFPE(-1)
C
      CARA2 = CARA
      CALL ASSERT((CARA2(1:1).EQ.'N') .OR. (CARA2(1:1).EQ.'T'))
      CALL ASSERT((CARA2(2:2).EQ.'F') .OR. (CARA2(2:2).EQ.'C'))
      CALL ASSERT((CARA2(3:3).EQ.'V') .OR. (CARA2(3:3).EQ.'S') .OR.
     &            (CARA2(3:3).EQ.'W'))
      CALL ASSERT((CARA2(4:4).EQ.'D') .OR. (CARA2(4:4).EQ.'P'))

      LTRANS = (CARA2(1:1).EQ.'T')
      LSTOP = (CARA2(3:3).EQ.'F')
      LDET = (CARA2(4:4).EQ.'D')

      DET = R8NNEM()


C     -- 1. ON VEUT ALLER SUREMENT (QUITE A PERDRE DU TEMPS):
C     ---------------------------------------------------------
      IF (CARA2(3:3).EQ.'S') THEN
C ---   DEFINITION DES PARAMETRES D'ENTREE POUR L'APPEL A LA ROUTINE
C       LAPACK : DGESVX
C       FACT : PERMET D'EQUILIBRER LA MATRICE (SI BESOIN)
        FACT = 'E'
C       TRANS2 : CARACTERE PERMETTANT DE TRANSPOSER A
        TRANS2 = 'N'
        IF (LTRANS) TRANS2 = 'T'
C       N : ORDRE DE LA MATRICE A
        N = NORDRE
C       NRHS : NOMBRE DE COLONNES DE X
        NRHS = NB
        LDA = DIM
        LDAF = DIM
        LDB = DIM
        LDX = DIM

C       SAUVEGARDE DE LA MATRICE 'A' DANS UNE MATRICE DE TRAVAIL: 'AA'
        DO 1 I = 1,DIM
          DO 2 J = 1,DIM
            AA(I,J) = A(I,J)
   2      CONTINUE
   1    CONTINUE

C --- RESOLUTION
        CALL DGESVX(FACT,TRANS2,N,NRHS,AA,LDA,AF,LDAF,IPIV4,EQUED,R,C,B,
     &              LDB,X,LDX,RCOND,FERR,BERR,WORK,IWORK4,INF4)
        IRET = INF4

C       -- RECOPIE DE X DANS B :
        DO 20 I = 1,N
          DO 10 J = 1,NB
            B(I,J) = X(I,J)
   10     CONTINUE
   20   CONTINUE

        IF (LDET) THEN
          DET = 1.D0
          DO 25 I = 1,N-1
            INDI = I+1
            DO 25 J = INDI, N  
              IF(IPIV4(I).GE.IPIV4(J)) DET = DET*(-1.D0)              
   25     CONTINUE
          DO 30 I = 1,N
            DET = DET*AF(I,I)
   30     CONTINUE
        END IF


C     -- 2. ON VEUT ALLER VITE :
C     ---------------------------------------------------------
      ELSE IF (CARA2(3:3).EQ.'V') THEN
C ---   DEFINITION DES PARAMETRES D'ENTREE POUR L'APPEL A LA ROUTINE
C       LAPACK : DGESV
C       N : ORDRE DE LA MATRICE A
        N = NORDRE
C       NRHS : NOMBRE DE COLONNES DE X
        NRHS = NB
        LDA = DIM
        LDB = DIM

        IF (LTRANS) THEN
          DO 50,I = 1,N
            DO 40,J = 1,N
              AF(J,I) = A(I,J)
   40       CONTINUE
   50     CONTINUE
        ELSE
          DO 70,I = 1,N
            DO 60,J = 1,N
              AF(I,J) = A(I,J)
   60       CONTINUE
   70     CONTINUE
        END IF

C       ---   RESOLUTION
        CALL DGESV(N,NRHS,AF,LDA,IPIV4,B,LDB,INF4)
        IRET = INF4
        IF (LDET) THEN
          DET = 1.D0
          DO 75 I = 1,N-1
            INDI = I+1
            DO 75 J = INDI, N  
              IF(IPIV4(I).GE.IPIV4(J)) DET = DET*(-1.D0)
   75     CONTINUE
          DO 80 I = 1,N
            DET = DET*AF(I,I)
   80     CONTINUE
        END IF


C     -- 3. ON VEUT ALLER ENCORE PLUS VITE :
C     ---------------------------------------------------------
      ELSE IF (CARA2(3:3).EQ.'W') THEN
        N = NORDRE
        IF (LTRANS) THEN
          DO 100,I = 1,N
            DO 90,J = 1,N
              AF(J,I) = A(I,J)
   90       CONTINUE
  100     CONTINUE
        END IF
        IF (LDET) THEN
          DET = 1.D0
        ELSE
          DET = 0.D0
        END IF
        LRET = .TRUE.
        IF (LTRANS) THEN
          CALL MGAUSW(AF,B,DIM,NORDRE,NB,DET,LRET)
        ELSE
          CALL MGAUSW(A,B,DIM,NORDRE,NB,DET,LRET)
        END IF
        IRET = 0
        IF (.NOT.LRET) IRET = 1
      END IF


C     -- 5. EN CAS DE PROBLEME : IRET > 0
C     ---------------------------------------
      IF (IRET.GT.0) THEN
        IF (LSTOP) THEN
          IF (CARA2(3:3).EQ.'S') THEN
            IF (IRET.EQ.N+1) THEN
              CALL U2MESS('F','CALCULEL3_79')
            ELSE
              VALI (1) = IRET
              VALI (2) = IRET
              VALK (1) = ' '
              VALK (2) = ' '
              CALL U2MESG('F', 'CALCULEL6_15',2,VALK,2,VALI,0,0.D0)
            END IF
          ELSE
            CALL U2MESS('F','CALCULEL3_79')
          END IF
        ELSE
C         -- ON CONTINUE
        END IF
      END IF


C     -- 6. IMPRESSIONS (SI LE MOT CLE 'INFO' = 2): RCOND, BERR, FERR
C     ---------------------------------------------------------------
      IF (CARA2(3:3).EQ.'S') THEN
        CALL INFNIV(IFM,NIV)
        IF (IRET.NE.0 .OR. NIV.LE.1) GO TO 110
        WRITE (IFM,1001) 'DEBUT DE MGAUSS'
        IF (EQUED.EQ.'N') THEN
          WRITE (IFM,*) 'L''EQUILIBRAGE DE LA MATRICE ''A'' '//
     &      ' N''A PAS ETE NECESSAIRE'
        ELSE IF (EQUED.EQ.'R') THEN
          WRITE (IFM,*) 'LA MATRICE ''A'' A ETE EQUILIBREE SOUS LA'//
     &      ' FORME : DIAG(R)*A'
        ELSE IF (EQUED.EQ.'C') THEN
          WRITE (IFM,*) 'LA MATRICE ''A'' A ETE EQUILIBREE SOUS LA'//
     &      ' FORME : A*DIAG(C)'
        ELSE IF (EQUED.EQ.'B') THEN
          WRITE (IFM,*) 'LA MATRICE ''A'' A ETE EQUILIBREE SOUS LA'//
     &      ' FORME : DIAG(R)*A*DIAG(C)'
        END IF
        WRITE (IFM,*) 'ESTIMATION DE LA VALEUR DU CONDITIONNEMENT '//
     &    'DE A :',RCOND
C       L'ERREUR ARRIERE (BACKWARD ERROR) EST L'ERREUR FAITE EN
C       ASSIMILANT LA MATRICE 'A' AU PRODUIT 'LU'
        WRITE (IFM,*) 'ERREUR ARRIERE : ',BERR
C       L'ERREUR AVANT (FORWARD ERROR) EST LA DIFFERENCE NORMALISEE
C       ENTRE LA VALEUR CALCULEE X ET SA VALEUR EXACTE
        WRITE (IFM,*) 'ERREUR AVANT : ',FERR
        WRITE (IFM,1001) 'FIN DE MGAUSS'
      END IF


  110 CONTINUE


  120 CONTINUE
 1001 FORMAT (10 ('='),A,10 ('='))
C
      CALL MATFPE(1)
C
      END
