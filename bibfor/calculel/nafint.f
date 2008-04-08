      SUBROUTINE NAFINT(INT   ,NINT  ,LIMA  ,BDIM  ,BPAN  ,
     &                  DIME  ,CINE  ,TMA   ,NO    ,NORM  ,
     &                  CNX   ,CNXC  ,NTM   ,DICO  ,CNO   ,
     &                  PRECFR,NNO   ,NOP)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      INTEGER     DIME
      INTEGER     INT(2,*),NINT
      INTEGER     LIMA(*)
      INTEGER     BDIM(2,*)
      REAL*8      BPAN(DIME+2,*)
      CHARACTER*8 CINE
      INTEGER     TMA(*)
      REAL*8      NO(3,*)
      REAL*8      NORM(DIME,*)
      INTEGER     CNX(*),CNXC(*)
      CHARACTER*8 NTM(*)
      INTEGER     DICO(*)
      REAL*8      CNO(DIME,*)
      INTEGER     NNO
      INTEGER     NOP(*)
      REAL*8      PRECFR
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C RETOURNE ARETES/FACES (CONNECTIVITES ET NOEUDS) DE LA FRONTIERE
C D'UN DOMAINE
C
C ----------------------------------------------------------------------
C
C
C IN  INT    : FACES DE LA FRONTIERE (CF ELBORD)
C              ( MAILLE SUPPORT, NUMERO DE LA FACE, ... )
C                              INDEX DANS LIMA
C IN  NINT   : NOMBRE DE FACES DE LA FRONTIERE
C IN  LIMA   : NUMERO DES MAILLES
C IN  BDIM   : STRUCTURE BOITE.DIME (CF BOITE)
C IN  BPAN   : STRUCTURE BOITE.NOEPAN (CF BOITE)
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  CINE   : CINEMATIQUE (COQUE OU SOLIDE)
C IN  TMA    : NUMERO DE TYPEMA DES MAILLES DU MAILLAGE
C IN  NO     : COORDONNEES DES NOEUDS DU MAILLAGE
C IN  NORM   : NORMALE LISSEE (CF. LISNOR)
C IN  CNX    : CONNECTIVITE DES MAILLES
C IN  CNXC   : INDEX DANS CNX
C IN  NTM    : VECTEUR NOMS TYPES DE MAILLE
C I/O DICO   : VECTEUR DE TRAVAIL
C IN  PRECFR : PRECISION ABSOLUE POUR CERNER LA FRONTIERE
C OUT CNO    : COORDONNEES DES NOEUDS DE LA FRONTIERE
C OUT NNO    : NOMBRE DE NOEUDS DE LA FRONTIERE
C OUT NOP    : CONNECTIVITE DES FACES DE LA FRONTIERE
C                 (NB NOEUDS FACE.1,NOEUD.1.FACE.1,
C                  NOEUD.2.FACE.1,...,NB NOEUDS FACE.2, ...)
C
C ----------------------------------------------------------------------
C
      CHARACTER*8 TYPEMA
      INTEGER     IPAN,INO,IMA,IMA0,NUMA,NNP
      INTEGER     CNXMA(27),NOEPAN(60),COQNOE(2,18)
      INTEGER     P0,P1,P2,P3,Q0,I,J,IDIME,LCOQUE
      REAL*8      PREC,R
C
C ----------------------------------------------------------------------
C
      IMA0 = 0
      NNO  = 0
      Q0   = 0

      IF (CINE(1:6).EQ.'SOLIDE') THEN
        DO 10 I = 1, NINT
          IMA  = INT(1,I)
          IPAN = INT(2,I)
          IF (IMA.NE.IMA0) THEN

            IMA0   = IMA
            NUMA   = LIMA(IMA)
            TYPEMA = NTM(TMA(NUMA))
            CALL NOPAN(TYPEMA,NOEPAN)
C
C --- COORDONNEES ET RENUMEROTATION DES NOEUDS
C
            P0 = CNXC(NUMA)
            P1 = CNXC(NUMA+1) - 1

            DO 20 J = P0, P1

              INO = CNX(J)
              IF (DICO(INO).NE.0) GOTO 20

              NNO = NNO + 1
              DICO(INO) = NNO
              CALL DCOPY(DIME,NO(1,INO),1,CNO(1,NNO),1)

 20         CONTINUE

C --------- REORIENTATION DE LA MAILLE

            CALL ORIEM3(NUMA  ,TYPEMA,NO    ,CNX   ,CNXC  ,
     &                  CNXMA)

          ENDIF

C ------- CONNECTIVITE DES PANS

          P3 = 1
          DO 30 J = 2, IPAN
            P3 = P3 + ABS(NOEPAN(P3)) + 1
 30       CONTINUE

          Q0      = Q0 + 1
          NNP     = NOEPAN(P3)
          NOP(Q0) = NNP

          PREC = 0.D0
          P2 = BDIM(1,IMA0+1) - 1 + IPAN

          DO 40 IDIME = 1, DIME
            R    = BPAN(IDIME,P2)
            PREC = PREC + R*R
 40       CONTINUE

          PREC = PRECFR/PREC

          DO 11 J = 1, ABS(NNP)
            P3      = P3 + 1
            INO     = DICO(CNXMA(NOEPAN(P3)))
            Q0      = Q0 + 1
            NOP(Q0) = INO
            DO 12 IDIME = 1, DIME
              CNO(IDIME,INO) = CNO(IDIME,INO) + PREC*BPAN(IDIME,P2)
 12         CONTINUE
 11       CONTINUE
 10     CONTINUE

C --- CAS DES MAILLAGES COQUE

      ELSEIF (CINE(1:5).EQ.'COQUE') THEN

        DO 50 I = 1, NINT

          IMA  = INT(1,I)
          IPAN = INT(2,I)

          IF (IMA.NE.IMA0) THEN

            IMA0   = IMA
            NUMA   = LIMA(IMA)
            TYPEMA = NTM(TMA(NUMA))
            CALL TMACOQ(TYPEMA,DIME,LCOQUE)
            CALL NOPAN(TYPEMA,NOEPAN)

C --------- COORDONNEES ET RENUMEROTATION DES NOEUDS

            P0 = CNXC(NUMA)
            P1 = CNXC(NUMA+1) - 1
            CALL COQUNO(DIME,P1-P0+1,COQNOE)

            DO 60 J = P0, P1
              INO = CNX(J)
              IF (DICO(INO).NE.0) GOTO 60
              DO 70 IDIME = 1, DIME
                CNO(IDIME,NNO+1) = NO(IDIME,INO) - NORM(IDIME,INO)
                CNO(IDIME,NNO+2) = NO(IDIME,INO) + NORM(IDIME,INO)
 70           CONTINUE
              DICO(INO) = NNO+1
              NNO       = NNO+2
 60         CONTINUE
          ENDIF

C ------- CONNECTIVITE DES PANS

          P3 = 1
          DO 80 J = 2, IPAN
            P3 = P3 + ABS(NOEPAN(P3)) + 1
 80       CONTINUE

          Q0 = Q0 + 1
          NNP = NOEPAN(P3)
          NOP(Q0) = NNP

          PREC = 0.D0
          P2 = BDIM(1,IMA0+1) - 1 + IPAN

          DO 90 IDIME = 1, DIME
            R    = BPAN(IDIME,P2)
            PREC = PREC + R*R
 90       CONTINUE

          PREC = PRECFR/PREC

          DO 51 J = 1, ABS(NNP)
            P3 = P3 + 1
            INO = NOEPAN(P3)
            INO = DICO(CNX(P0-1+COQNOE(1,INO)))+COQNOE(2,INO)
            Q0 = Q0 + 1
            NOP(Q0) = INO
            DO 52 IDIME = 1, DIME
              CNO(IDIME,INO) = CNO(IDIME,INO) + PREC*BPAN(IDIME,P2)
 52         CONTINUE
 51       CONTINUE
 50     CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      END
