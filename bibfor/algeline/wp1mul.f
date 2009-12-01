      SUBROUTINE WP1MUL(LMASSE,LAMOR,LRAIDE,PTORIG,TOLF,NITF,NBFREQ,
     &                  MXRESF,NPREC,RESUFI,RESUFR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LMASSE,LAMOR,LRAIDE,NITF,NBFREQ,RESUFI(MXRESF,*)
      COMPLEX*16                            PTORIG(3,*)
      REAL*8                                       TOLF,RESUFR(MXRESF,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 24/03/2009   AUTEUR REZETTE C.REZETTE 
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
C     CALCUL DES VALEURS PROPRES COMPLEXES DU SYSTEME QUADRATIQUE
C                         2
C                        L (M) Y + L (C) Y + (K) Y = 0
C     PAR RECHERCHE DES ZEROS DU POLYNOME CARACTERISTIQUE PAR UNE
C     METHODE COURBE D'INTERPOLATION, METHODE A 3 POINTS DE MULLER AVEC
C     DEFLATION
C     ------------------------------------------------------------------
C OUT RESUFR : R : ZERO DU POLYNOME CARACTERISTIQUE
C            POUR IMODE = 1, NBFREQ
C          (IMODE,1) : NUMERO D'ORDRE DU ZERO DU POLYNOME
C          (IMODE,2) : PARTIE IMAGINAIRE DU ZERO DU POLYNOME
C          (IMODE,3) : PARTIE REELLE DU ZERO DU POLYNOME
C     ------------------------------------------------------------------
C     REMARQUE: LES MATRICES QUE L'ON TRAITE SONT SYMETRIQUES ET DONC
C     LES VALEURS PROPRES COMPLEXES SE PRESENTENT PAR PAIRES CONJUGUEES,
C     ON NE RETIENT QUE CELLE A PARTIE IMAGINAIRE POSITIVE ET L'ON
C     ELIMINE L'AUTRE PAR DEFLATION.
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      COMPLEX*16    H0,H1,LAMBDA,DELTA,ZZ,G0,GG,GG1,GG2
      REAL*8        RN1,RN2,ERR,ERRZ,DIST
      CHARACTER*1   TYPCST(3)
      CHARACTER*8   NOMDDL
      CHARACTER*19  MATDYN
      CHARACTER*24  NMAT(3),NDYNAM
      COMPLEX*16    Z0,    Z1,    Z2
      REAL*8        CONST(6)
      COMPLEX*16    RES0,  RES1, RES2
      REAL*8        DET0,  DET1, DET2
      INTEGER       IDET0, IDET1,IDET2
      REAL*8        DEPI,R8DEPI,EPS
C     ------------------------------------------------------------------
      DATA          NOMDDL /'        '/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      MATDYN = '&&WP1MUL.MAT.DYNA'
C
C
C     --- CREATION DE LA MATRICE DYNAMIQUE A VALEUR COMPLEXE ---
      NEQ = ZI(LMASSE+2)
      CALL MTDEFS(MATDYN,ZK24(ZI(LMASSE+1)),'V','C')
      CALL MTDSCR(MATDYN)
      NDYNAM=MATDYN(1:19)//'.&INT'
      CALL JEVEUO(MATDYN(1:19)//'.&INT','E',LDYNAM)
C
C      --- DEFINITION DES TYPES DE CONSTANTES ET DES MATRICES ---
      NMAT(1) = ZK24(ZI(LMASSE+1))
      NMAT(2) = ZK24(ZI(LAMOR +1))
      NMAT(3) = ZK24(ZI(LRAIDE+1))
      DO 10 ICOMB = 1, 3
         TYPCST(ICOMB) = 'C'
 10   CONTINUE
      CONST(5) = 1.D0
      CONST(6) = 0.D0
C
C     --- CREATION D'UN OBJET DE TAVAIL POUR CONTENIR LES ZEROS ---
      CALL WKVECT('&&WP1MUL.ZERO.POLYNOME','V V C',NBFREQ,LZERO)
C
C     --- BOUCLE SUR LE NOMBRE DE MODES DEMANDE ----
      DO 100 IMODE=1,NBFREQ
C
         Z2 = PTORIG(3,IMODE)
         CONST(1) = DBLE(Z2*Z2)
         CONST(2) = DIMAG(Z2*Z2)
         CONST(3) = DBLE(Z2)
         CONST(4) = DIMAG(Z2)
         CALL MTCMBL(3,TYPCST,CONST,NMAT,NDYNAM,NOMDDL,' ','ELIM=')
         CALL TLDLGG(2,LDYNAM,1,NEQ,NPREC,NDECI,ISINGU,NPVNEG,IER)
         CALL WP1DFT(LDYNAM,IMODE,ZC(LZERO),Z2,RES2,DET2,IDET2,ISTU)
C
         Z1 = PTORIG(2,IMODE)
         CONST(1) = DBLE(Z1*Z1)
         CONST(2) = DIMAG(Z1*Z1)
         CONST(3) = DBLE(Z1)
         CONST(4) = DIMAG(Z1)
         CALL MTCMBL(3,TYPCST,CONST,NMAT,NDYNAM,NOMDDL,' ','ELIM=')
         CALL TLDLGG(2,LDYNAM,1,NEQ,NPREC,NDECI,ISINGU,NPVNEG,IER)
         CALL WP1DFT(LDYNAM,IMODE,ZC(LZERO),Z1,RES1,DET1,IDET1,ISTU)
C
C         --- BOUCLE JUSQU'A LA CONVERGENCE ---
         Z0 = PTORIG(1,IMODE)
         DO 110 I=1,NITF
C
            CONST(1) = DBLE(Z0*Z0)
            CONST(2) = DIMAG(Z0*Z0)
            CONST(3) = DBLE(Z0)
            CONST(4) = DIMAG(Z0)
            CALL MTCMBL(3,TYPCST,CONST,NMAT,NDYNAM,NOMDDL,' ','ELIM=')
            CALL TLDLGG(2,LDYNAM,1,NEQ,NPREC,NDECI,ISINGU,NPVNEG,IER)
            CALL WP1DFT(LDYNAM,IMODE,ZC(LZERO),Z0,RES0,DET0,IDET0,ISTU)
C
C           --- CALCUL DES COEFFICIENTS DE L'EQUATION DU 2ND DEGRE --
            H0     = Z0-Z1
            H1     = Z1-Z2
            LAMBDA = H0/H1
            DELTA  = 1.D0+LAMBDA
            G0 = RES2/RES0*DET2/DET0*10.D0**(IDET2-IDET0)*LAMBDA*LAMBDA
     &         - RES1/RES0*DET1/DET0*10.D0**(IDET1-IDET0)*DELTA*DELTA
     &         + LAMBDA + DELTA
            GG = RES2/RES0*DET2/DET0*10.D0**(IDET2-IDET0)*LAMBDA
     &         - RES1/RES0*DET1/DET0*10.D0**(IDET1-IDET0)*DELTA + 1.D0
            GG  = GG*4.D0*DELTA*LAMBDA
            GG1 = G0 + SQRT(G0*G0-GG)
            GG2 = G0 - SQRT(G0*G0-GG)
            RN1 = ABS( GG1 )
            RN2 = ABS( GG2 )
            IF (RN1.GE.RN2) THEN
               ZZ = -2.D0*DELTA/GG1
            ELSE
               ZZ = -2.D0*DELTA/GG2
            ENDIF
C
C           --- CORRECTION DE LA VALEUR PROPRE POUR CONVERGER VERS LA
C           --- FREQUENCE POSITIVE (SOLUTION CONJUGUEE)
            ZZ  = Z0 + ZZ * H0
            ZZ  = DCMPLX(DBLE(ZZ),ABS(DIMAG(ZZ)))
C
C           --- CALCUL DE L'ERREUR  ---
            ERRZ = ABS(Z0)
            ERR  = ABS(ZZ-Z0)
            ERR  = SQRT(ERR/ERRZ)
            IF (ERR .GE. TOLF) THEN
C
C              --- INCREMENTATION PAR PERMUTATION DES OBJETS ---
               RES2   =  RES1
               RES1   =  RES0
               DET2   =  DET1
               DET1   =  DET0
               IDET2  = IDET1
               IDET1  = IDET0
               Z2     = Z1
               Z1     = Z0
               Z0     = ZZ
            ELSE
               ITER   = I
               GOTO 120
            ENDIF
  110    CONTINUE
C
C         --- FIN DES ITERATIONS ---
         ITER = -NITF
  120    CONTINUE
C
         ZC(LZERO+IMODE-1) = ZZ
         RESUFR(IMODE,2)  = DIMAG(ZZ)
         RESUFR(IMODE,3)  = DBLE(ZZ)
         RESUFR(IMODE,14) = ERR
         RESUFI(IMODE,2)  = ITER
C
  100 CONTINUE
C
      CALL JEDETC('V','&&WP1MUL',1)
      CALL JEDEMA()
      END
