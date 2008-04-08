      SUBROUTINE ARLAS0(NUM1  ,NUM2  ,CNX   ,CNXC  ,
     &                  INO1  ,NI    ,INO2  ,INO2C ,
     &                  IJ)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ======================================================================
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      INTEGER NUM1,NUM2,CNX(*),CNXC(*)
      INTEGER INO1(*),NI,INO2(*),INO2C(*)
      INTEGER IJ(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C GESTION DES RELATIONS LINEAIRES
C POINTEURS DANS LA MATRICE MORSE D'UN COUPLE DE MAILLE (NUM1,NUM2)
C
C ----------------------------------------------------------------------
C
C
C IN  NUM1    : NUMERO DE MAILLE LIGNE (INDEX CNXC)
C IN  NUM2    : NUMERO DE MAILLE COLONNE (INDEX CNXC)
C IN  CNX     : COLLECTION CONNECTIVITE DU MAILLAGE
C IN  CNXC    : LONGUEUR CUMULEE ASSOCIEE A CNX
C IN  INO1    : LISTE NOEUDS LIGNES (CF ARLFAC)
C IN  NI      : LONGUEUR DU VECTEUR INO1
C IN  INO2    : COLLECTION NOEUDS COLONNES
C IN  INO2C   : LONGUEUR CUMULEE ASSOCIEE A INO2
C OUT IJ(*)   : POINTEURS DANS LA MATRICE MORSE,
C                  DEFINIE PAR INO1 ET INO2, POUR LES
C                  NOEUDS DES MAILLES NUM1 ET NUM2
C                 ( POINTEUR (NUM1.NO1,NUM2.NO1),
C                   POINTEUR (NUM1.NO1,NUM2.NO2), ...,
C                   POINTEUR (NUM1.NO2,NUM2.NO1), ... )
C
C ----------------------------------------------------------------------
C
      INTEGER NR1,NR2,P,N1,Q,Q0,N2,D,D0,F,F0,M,NT,I,J,K
C
C ----------------------------------------------------------------------
C
      K = 0

      P   = CNXC(NUM1)
      NR1 = CNXC(NUM1+1) - P
      Q0  = CNXC(NUM2)
      NR2 = CNXC(NUM2+1) - Q0


      DO 10 I = 1, NR1

        D = 1
        F = NI + 1
        N1 = CNX(P)
        P = P + 1

C ----- RECHERCHE PAR DICHOTOMIE

 20     CONTINUE
        M = (D + F)/2
        NT = INO1(M)

        IF (N1.NE.NT) THEN
          IF (N1.LT.NT) THEN
            F = M
          ELSE
            D = M
          ENDIF

          GOTO 20
        ENDIF

        D0 = INO2C(M)
        F0 = INO2C(M+1)
        Q = Q0

        DO 10 J = 1, NR2

          D = D0
          F = F0
          N2 = CNX(Q)
          Q = Q + 1

C ------- RECHERCHE PAR DICHOTOMIE

 30       CONTINUE
          M = (D + F)/2
          NT = INO2(M)

          IF (N2.NE.NT) THEN
            IF (N2.LT.NT) THEN
              F = M
            ELSE
              D = M
            ENDIF
            GOTO 30
          ENDIF
C ------- STOCKAGE
          K = K + 1
          IJ(K) = M

 10   CONTINUE

      END
