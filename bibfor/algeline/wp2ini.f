      SUBROUTINE WP2INI(APPR,LMASSE,LAMOR,LRAIDE,LMATRA,LMTPSC,SIGMA,
     &                  XH,XB,OPTIOF,PRORTO,NBORTO,NBVECT,NEQ,LBLOQ,
     &                  LDDL,ALPHA,BETA,SIGNE,YH,YB,SOLVEU)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*1       APPR
      INTEGER           LMASSE,LAMOR,LRAIDE,LMATRA,LMTPSC
      COMPLEX*16        SIGMA
      REAL*8            XH(*),XB(*)
      CHARACTER*(*)     OPTIOF
      INTEGER           NBORTO,NBVECT,NEQ,LBLOQ(*),LDDL(*)
      REAL*8            PRORTO
      REAL*8            ALPHA(*),BETA(*),SIGNE(*),YH(NEQ,*),YB(NEQ,*)
      CHARACTER*19      SOLVEU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_21
C     GENERATION DES VECTEURS DE LANCZOS ET DE LA TRIADIAGONALE
C     ASSOCIEE POUR LE PROBLEME QUADRATIQUE AUX VALEURS PROPRES
C     ------------------------------------------------------------------
C     1. REDUCTION A UN PROBLEME GENERALISE
C
C         !K   0! !P!          !-C   -M! !P!
C         !     ! ! ! = LAMBDA !       ! ! ! <=> K.G*Z = LAMBDA*M.G*Z
C         !0  -M! !Q!          !-M    0! !Q!
C
C     2. DECALAGE SPECTRAL K.G_S = K.G - SIGMA*M.G
C     3. DEDUCTION D' UN OPERATEUR REEL A
C
C              A = RE(K.G_S**-1 * M.G)
C          OU  A = IM(K.G_S**-1 * M.G)
C
C     4. CHOIX D' UN PSEUDO PRODUIT SCALAIRE SUR R
C
C              B = RE(K.G_S**-1)**-1
C          OU  B = IM(K.G_S**-1)**-1
C
C     ------------------------------------------------------------------
C IN  APPR   : K : INDICATEUR DE L' APPROCHE POUR A ( 'R' OU 'I')
C IN  LMASSE : I : MATRICE DE MASSE
C IN  LAMOR  : I : MATRICE D' AMORTISSEMENT
C IN  LRAIDE : I : MATRICE DE RAIDEUR
C IN  LMATRA : I : MATRICE DYNAMIQUE(S) FACTORISEE LDLT
C IN  LMTPSC : I : MATRICE DYNAMIQUE(RE(S)) FACTORISEE LDLT
C IN  SIGMA  : C : VALEUR DU PARAMETRE DE DECALAGE SPECTRAL
C IN  XH     : R : PARTIE SUPERIEURE DU VECTEURS INITIAL
C IN  XB     : R : PARTIE INFERIEURE DU VECTEURS INITIAL
C IN  NBORTO : I : NOMBRE MAXIMAL DE REORTHOGONALISATION AUTORISEE
C IN  PRORTO : R : PRECISION DE LA REORTHOGONALISATION
C IN  NBVECT : I : NOMBRE DE VECTEURS A GENERER
C IN  NEQ    : I : DIMENSION DE L' ESPACE DE DEPART
C IN  LBLOQ  : I : TYPE DES DDL (LBOLOQ(I) = 0 <=> DDL(I) = BLOQUE)
C IN  LDDL   : I : TYPE DES DDL (LDDL(I) = 0 <=> DDL(I) = LAGRANGE)
C OUT ALPHA  : R : DIAGONALE DE LA TRIDIAGONALE
C OUT BETA   : R : SUR-DIAGONALE DE LA TRIDIAGONALE
C OUT SIGNE  : R : SIGNE DES TERMES DE LA SOUS-DIAGONALE
C OUT YH     : R : PARTIE SUPERIEURE DES VECTEURS DE LANCZOS (P)
C OUT YB     : R : PARTIE INFERIEURE DES VECTEURS DE LANCZOS (Q)
C IN  SOLVEU : K19: SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C     ------------------------------------------------------------------
C
      CHARACTER*8     KBID
C
C     ------------------------------------------------------------------
      CHARACTER*12 STRG
      CHARACTER*24 VALK
      INTEGER      AU1,AU2,AU3,AU4,AV,ABAYH,ABAYB,APTBYH,APTBYB
      INTEGER VALI(4)
      INTEGER      I,J,K,ABYH,ABYB,IO
      REAL*8       A,B,C,SR,SI,DEUXSR,MODS2,INVSI,SI2,D1,D2
      LOGICAL      OC,RO
C     ------------------------------------------------------------------
      CALL JEMARQ()
      SI     = DIMAG(SIGMA)
      SI2    = SI*SI
      SR     = DBLE(SIGMA)
      DEUXSR = 2.0D0*SR
      MODS2  = SI2 + SR*SR
C
      IF ( OPTIOF .EQ. 'CENTRE' ) THEN
         INVSI = 1.D0/SI
      ELSE
         INVSI = 0.D0
      ENDIF
C
C     ---- ALLOCATION DES ZONES DE TRAVAIL ---
      CALL WKVECT('&&WP2INI.VECTEUR.AUX.U1R','V V R',NEQ,AU1  )
      CALL WKVECT('&&WP2INI.VECTEUR.AUX.U2R','V V R',NEQ,AU2  )
      CALL WKVECT('&&WP2INI.VECTEUR.AUX.U3R','V V R',NEQ,AU3  )
      CALL WKVECT('&&WP2INI.VECTEUR.AUX.U4R','V V R',NEQ,AU4  )
      IF ( SI .NE. 0.D0 ) THEN
         CALL WKVECT('&&WP2INI.VECTEUR.AUX.VC ','V V C',NEQ,AV   )
      ELSE
         AV = 0
      ENDIF
      CALL WKVECT('&&WP2INI.B_A.VECT.LANC.H','V V R',NEQ,ABAYH)
      CALL WKVECT('&&WP2INI.B_A.VECT.LANC.B','V V R',NEQ,ABAYB)
C
      CALL WKVECT('&&WP2INI.PT.B.LANCZO.H','V V I',NBVECT,APTBYH)
      CALL WKVECT('&&WP2INI.PT.B.LANCZO.B','V V I',NBVECT,APTBYB)
C
      DO 10, I = 1, NBVECT, 1
         CALL CODENT(I,'G',STRG)
         CALL JECREO('&&WP2INI.BYH'//STRG,'V V R')
         CALL JEECRA('&&WP2INI.BYH'//STRG,'LONMAX',NEQ,KBID)
         CALL JEECRA('&&WP2INI.BYH'//STRG,'LONUTI',NEQ,KBID)
         CALL JEVEUT('&&WP2INI.BYH'//STRG,'E',ZI(APTBYH + I-1))
         CALL JECREO('&&WP2INI.BYB'//STRG,'V V R')
         CALL JEECRA('&&WP2INI.BYB'//STRG,'LONMAX',NEQ,KBID)
         CALL JEECRA('&&WP2INI.BYB'//STRG,'LONUTI',NEQ,KBID)
         CALL JEVEUT('&&WP2INI.BYB'//STRG,'E',ZI(APTBYB + I-1))
  10  CONTINUE
C
      DSEED = 773218.D0
      CALL GGUBS(DSEED,NEQ,XB)
      DO 15 II = 1, NEQ
         XH(II) = 0.D0
         XB(II) = LBLOQ(II)*LDDL(II)*XB(II)
  15  CONTINUE
C
C     1 - GENERATION DU PREMIER VECTEUR
C
C     --- 1.1. DIRECTION
      CALL WP2AYL(APPR,LMATRA,LMASSE,LAMOR,SIGMA,LBLOQ,XH,XB,
     &            YH(1,1),YB(1,1),
     &            ZR(AU1),ZR(AU2),ZR(AU3),ZR(AU4),ZC(AV),NEQ,SOLVEU)
C
      DO 100 I = 1, NEQ
         ZR(ABAYH + I-1) = -ZR(AU1 + I-1) - ZR(AU2 + I-1)
         ZR(ABAYB + I-1) = -ZR(AU3 + I-1)
  100 CONTINUE
C
C     --- 1.2. B_NORMALISATION
      C = 0.D0
      DO 110 IPS = 1, NEQ
         C = C + ZR(ABAYH+IPS-1)*YH(IPS,1) + ZR(ABAYB+IPS-1)*YB(IPS,1)
  110 CONTINUE
      A = 1.D0/SQRT(ABS(C))
      IF ( C .GT. 0.D0 ) THEN
         SIGNE(1) =  1.D0
      ELSE
         SIGNE(1) = -1.D0
         A        = -A
      ENDIF
C
      ABYH = ZI(APTBYH + 1-1)
      ABYB = ZI(APTBYB + 1-1)
      DO 120 I = 1, NEQ
         YH(I,1) = A*YH(I,1)
         YB(I,1) = A*YB(I,1)
         ZR(ABYH + I-1) = A*ZR(ABAYH + I-1)
         ZR(ABYB + I-1) = A*ZR(ABAYB + I-1)
 120  CONTINUE
C
C     --- 1.3. COEFFICIENT DE LA TRIDIAGONALE
      CALL MRMULT('ZERO',LAMOR,YH(1,1),ZR(AU1),1,.FALSE.)
      CALL MRMULT('ZERO',LMASSE,YB(1,1),ZR(AU2),1,.FALSE.)
      CALL MRMULT('ZERO',LMASSE,YH(1,1),ZR(AU3),1,.FALSE.)
C
      A = 0.D0
      DO 130 I = 1, NEQ
         A = A - YH(I,1)*(ZR(AU1 + I-1) + ZR(AU2 + I-1))
     &         - YB(I,1)* ZR(AU3 + I-1)
 130  CONTINUE
      ALPHA(1) = A
      BETA(1)  = 0.D0
C
C     2  -  GENERATION DES VECTEURS 2, 3, .. , NBVECT
      DO 200 J = 2, NBVECT
C
C        --- 2.1. DIRECTION
         CALL WP2AYL(APPR,LMATRA,LMASSE,LAMOR,SIGMA,LBLOQ,
     &               YH(1,J-1),YB(1,J-1),
     &               YH(1,J),YB(1,J),
     &               ZR(AU1),ZR(AU2),ZR(AU3),ZR(AU4),ZC(AV),NEQ,SOLVEU)
C
         DO 210 I = 1, NEQ
            ZR(ABAYH + I-1) = -ZR(AU1 + I-1) - ZR(AU2 + I-1)
            ZR(ABAYB + I-1) = -ZR(AU3 + I-1)
  210    CONTINUE
C
         A = 0.D0
         DO 215 IPS = 1, NEQ
            A = A + ZR(ABAYH+IPS-1)*YH(IPS,J-1)
     &            + ZR(ABAYB+IPS-1)*YB(IPS,J-1)
  215   CONTINUE
C
         D1 = SIGNE(J-1)
         A  = D1*A
C
         IF ( J .EQ. 2 ) THEN
            DO 220 I = 1, NEQ
               YH(I,J) = YH(I,J) - A*YH(I,J-1)
               YB(I,J) = YB(I,J) - A*YB(I,J-1)
  220       CONTINUE
         ELSE
            B = 0.D0
            DO 225 IPS = 1, NEQ
               B = B + ZR(ABAYH+IPS-1)*YH(IPS,J-2)
     &               + ZR(ABAYB+IPS-1)*YB(IPS,J-2)
  225      CONTINUE
            D2 = SIGNE(J-2)
            B  = D2*B
            DO 230 I = 1, NEQ
               YH(I,J) = YH(I,J) - A*YH(I,J-1) - B*YH(I,J-2)
               YB(I,J) = YB(I,J) - A*YB(I,J-1) - B*YB(I,J-2)
  230       CONTINUE
         ENDIF
C
C        --- 2.2. NORMALISATION
         ABYH = ZI(APTBYH + J-1)
         ABYB = ZI(APTBYB + J-1)
         IF ( APPR .EQ. 'R' ) THEN
            CALL WP2BRY(LMTPSC,LMASSE,LAMOR,LRAIDE,SR,SI2,
     &                  YH(1,J),YB(1,J),ZR(ABYH),ZR(ABYB),
     &                  ZR(AU1),ZR(AU2),ZR(AU3),ZR(AU4),NEQ,SOLVEU)
         ELSE
            CALL WP2BIY(LMASSE,LAMOR,LRAIDE,MODS2,DEUXSR,INVSI,
     &                  YH(1,J),YB(1,J),ZR(ABYH),ZR(ABYB),LBLOQ,
     &                  ZR(AU1),ZR(AU2),ZR(AU3),ZR(AU4),NEQ)
         ENDIF
         C = 0.D0
         DO 235 IPS = 1, NEQ
            C = C + ZR(ABYH+IPS-1)*YH(IPS,J)
     &            + ZR(ABYB+IPS-1)*YB(IPS,J)
  235   CONTINUE
C
         A = 1.D0/SQRT(ABS(C))
         IF ( C .GT. 0.D0 ) THEN
            SIGNE(J) =  1.D0
         ELSE
            SIGNE(J) = -1.D0
            A        = -A
         ENDIF
C
         DO 240 I = 1, NEQ
            ZR(ABYH + I-1) = A*ZR(ABYH + I-1)
            ZR(ABYB + I-1) = A*ZR(ABYB + I-1)
            YH(I,J)        = A*YH(I,J)
            YB(I,J)        = A*YB(I,J)
240      CONTINUE
C
C        --- 2.3. REORTHOGONALISTION
         RO = .FALSE.
         DO 300 I = 1, J-1
            ABYH = ZI(APTBYH + I-1)
            ABYB = ZI(APTBYB + I-1)
            A = 0.D0
            DO 310 IPS = 1, NEQ
               A = A + ZR(ABYH+IPS-1)*YH(IPS,J)
     &               + ZR(ABYB+IPS-1)*YB(IPS,J)
  310      CONTINUE
            OC =  ( ABS(A) .LT. PRORTO )
            RO =  (.NOT. OC) .OR. RO
C
            IO = 1
600         CONTINUE
            IF ( (.NOT. OC) .AND. (IO .LE. NBORTO) ) THEN
               A = A*SIGNE(I)
               DO 315 K = 1, NEQ
                  YH(K,J) =  YH(K,J) - A*YH(K,I)
                  YB(K,J) =  YB(K,J) - A*YB(K,I)
  315          CONTINUE
               B = 0.D0
               DO 320 IPS = 1, NEQ
                  B = B + ZR(ABYH+IPS-1)*YH(IPS,J)
     &                  + ZR(ABYB+IPS-1)*YB(IPS,J)
  320         CONTINUE
               IF ( ABS(B) .GT. ABS(A) ) THEN
                  VALI (1) = IO
                  VALI (2) = IO
                  VALI (3) = J
                  VALI (4) = I
                  VALK = '"ENNUI" POSSIBLE'
                  CALL U2MESG('I', 'ALGELINE4_86',1,VALK,4,VALI,0,0.D0)
                  OC = .TRUE.
               ELSE
                  A  = B
                  IO = IO + 1
                  OC = ( ABS(B) .LT. PRORTO )
               ENDIF
               GOTO 600
            ENDIF
  300    CONTINUE
C
C        --- 2.4. REACTUALISATION
         IF ( RO ) THEN
            ABYH = ZI(APTBYH + J-1)
            ABYB = ZI(APTBYB + J-1)
            IF ( APPR .EQ. 'R' ) THEN
               CALL WP2BRY(LMTPSC,LMASSE,LAMOR,LRAIDE,SR,SI2,
     &                     YH(1,J),YB(1,J),ZR(ABYH),ZR(ABYB),
     &                     ZR(AU1),ZR(AU2),ZR(AU3),ZR(AU4),NEQ,SOLVEU)
            ELSE
               CALL WP2BIY(LMASSE,LAMOR,LRAIDE,MODS2,DEUXSR,INVSI,
     &                     YH(1,J),YB(1,J),ZR(ABYH),ZR(ABYB),LBLOQ,
     &                     ZR(AU1),ZR(AU2),ZR(AU3),ZR(AU4),NEQ)
            ENDIF
            C = 0.D0
            DO 350 IPS = 1, NEQ
               C = C + ZR(ABYH+IPS-1)*YH(IPS,J)
     &               + ZR(ABYB+IPS-1)*YB(IPS,J)
  350      CONTINUE
            A = 1.D0/SQRT(ABS(C))
            IF ( C .GT. 0.D0 ) THEN
               SIGNE(J) =  1.D0
            ELSE
               SIGNE(J) = -1.D0
               A        = -A
            ENDIF
C
            DO 400 I = 1, NEQ
               ZR(ABYH + I-1) = A*ZR(ABYH + I-1)
               ZR(ABYB + I-1) = A*ZR(ABYB + I-1)
               YH(I,J)        = A*YH(I,J)
               YB(I,J)        = A*YB(I,J)
  400       CONTINUE
         ENDIF
C
C        --- 2.5. COEFFICIENTS DE LA TRIDIAGONALE
         CALL MRMULT('ZERO',LAMOR,YH(1,J),ZR(AU1),1,.FALSE.)
         CALL MRMULT('ZERO',LMASSE,YB(1,J),ZR(AU2),1,.FALSE.)
         CALL MRMULT('ZERO',LMASSE,YH(1,J),ZR(AU3),1,.FALSE.)
C
         A = 0.D0
         B = 0.D0
         DO 500, I = 1, NEQ, 1
            A = A - YH(I,J)*(ZR(AU1+ I-1) + ZR(AU2 + I-1))
     &            - YB(I,J)* ZR(AU3 + I-1)
            B = B - YH(I,J-1)*(ZR(AU1 + I-1) + ZR(AU2 + I-1))
     &            - YB(I,J-1)* ZR(AU3 + I-1)
  500    CONTINUE
         ALPHA(J) = A
         BETA (J) = B
  200 CONTINUE
C
C     --- DESTRUCTION DES OJB TEMPORAIRES
      CALL JEDETC('V','&&WP2INI',1)
C
      CALL JEDEMA()
      END
