      SUBROUTINE RVCHL3(VALE,PADR,PNSP,PNBN,MA,NBMA,ITYPM,
     &                  NBCO,NBSP,NBPT,NBCP,FACE,CREF,NBNDF,CLOCF,
     &                  CONEC,VLCCNC,VAL,PTADR,TABAUX)
      IMPLICIT NONE
C
      INTEGER     PADR(*),PNBN(*),PNSP(*)
      INTEGER     MA(*),FACE(*),CONEC(*),VLCCNC(*),NBNDF(6,*)
      INTEGER     NBCO,NBSP,NBPT,NBMA,NBCP,PTADR,ITYPM,CLOCF(6,4,*)
      REAL*8      VALE(*),CREF(2,*),VAL(*),TABAUX(4,*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C     ------------------------------------------------------------------
C     CALCUL DU VALE D' UN SS_CHAM_ELEM SUR 1 MAILLE DE NBPT
C     A NBCO COUCHE, NBSP SOUS-POINT POUR NBCP
C     RQ : LE CAS NBPT = 1 CORRESPOND A UN SS_CHAM_NO
C     ------------------------------------------------------------------
C IN  VALE   : R :  VALE DU SSCH19
C IN  PADR   : I :  PADR DU SSCH19
C IN  PNBN   : I :  PNBN DU SSCH19
C IN  PNSP   : I :  PNSP DU SSCH19
C IN  MA     : I :  TABLE  DE MAILLE 3D CONTRIBUANT
C IN  NBMA   : I :  NOMBRE DE MAILLE 3D CONTRIBUANT
C IN  NBCO   : I :  NBCO DU SS_CHAM_ELEM
C IN  NBSP   : I :  NBSP DU SS_CHAM_ELEM
C IN  NBCP   : I :  NBCP DU SS_CHAM_ELEM
C IN  NBPT   : I :  NOMBRE DE POINT DE LA MAILLE DU SS_CHAM_ELEM
C IN  FACE   : I :  TABLE DES FACE (POUR MA(1))CONTENANT LES POINT
C IN  CREF   : R :  TABLE(1..2,1..NBPT) DES C_REF DES PTS DANS LEUR FACE
C IN  NBNDF  : I :  TABLE(1..6,1..3)     --+
C IN  CLOCF  : I :  TABLE(1..6,1..4,1..3)--+ --> DESC_TYPE_MAILLE_3D
C IN  CONEC  : I :  CONNECTIVITE DU MAILLAGE
C IN  VLCCNC : I :  LONGUEUR CUMULEE DE LA CONNECTIVITE DU MAILLAGE
C OUT VAL    : R :  VALE DU SS_CHAM_ELEM (ET DE LA SD_EVAL SOUS-JACENTE)
C VAR PTADR  : I :  POINTEUR SUR LA PARTIE DE VALE NON AFFECTEE
C     ------------------------------------------------------------------
C     NBPT > 1 => LES POINT SONT SUR  UNE MEME FACE OU ARETE DE MA(1)
C                 MA(I), I>1,ONT EN COMMUN CETTE FACE OU ARETE
C     ------------------------------------------------------------------
      INTEGER M,F,IPT,ICO,I,J,K,NDGLO1(4),NDLOC(4),NLOC
      INTEGER NSPM,NBNM,DEC1,DEC2,DEC3,NBNF
      REAL*8  C1,C2,C3,C4,C5,R,S,UNSUR4,R8VIDE
C
C======================================================================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      UNSUR4 = 0.25D0
C
      DO 300, IPT = 1, NBPT, 1
         M      = MA(1)
         F      = FACE(IPT)
         NBNF   = NBNDF(F,ITYPM)
         NBNM   = PNBN(M)
         NSPM   = PNSP(M)
         DO 10, I = 1, NBNF, 1
            NLOC      = CLOCF(F,I,ITYPM)
            NDGLO1(I) = CONEC(VLCCNC(M) + NLOC-1)
            NDLOC (I) = NLOC
            DEC1      = NSPM*NBCP*(NLOC-1)
            DO 11, ICO = 1, NBCO, 1
               DEC2 = NBCP*NBSP*(ICO-1)
               DEC3 = NBCP*NSPM*NBNM*(ICO-1)
               DO 12, J = 1, NBSP*NBCP, 1
                  TABAUX(I,DEC2+J) = VALE(PADR(M)+DEC1+DEC3+J-1)
12             CONTINUE
11          CONTINUE
10       CONTINUE
         DO 100, K = 2, NBMA, 1
            M    = MA(K)
            NBNM = PNBN(M)
            NSPM = PNSP(M)
            CALL I3NLOC ( NDGLO1, CONEC(VLCCNC(M)), NBNF, NBNM, NDLOC )
            DEC1 = 0
            DO 101, I = 1, NBNF, 1
               DEC1 = DEC1 + MAX(0,MIN(1,NDLOC(I)))
101         CONTINUE
            IF ( DEC1 .LT. 2 ) CALL U2MESS('F','POSTRELE_16')
            DO 110, I = 1, NBNF, 1
               NLOC = NDLOC(I)
               IF ( NLOC .GT. 0 ) THEN
                  DEC1 = NSPM*NBCP*(NLOC-1)
                  DO 111, ICO = 1, NBCO, 1
                     DEC2 = NBCP*NBSP*(ICO-1)
                     DEC3 = NBCP*NSPM*NBNM*(ICO-1)
                     DO 112, J = 1, NBSP*NBCP, 1
                        IF ( TABAUX(I,DEC2+J) .NE. R8VIDE() ) THEN
                         TABAUX(I,DEC2+J) = VALE(PADR(M)+DEC1+DEC3+J-1)
     &                                      + TABAUX(I,DEC2+J)
                        ENDIF
112                  CONTINUE
111               CONTINUE
               ENDIF
110         CONTINUE
100      CONTINUE
C
         DO 200, I = 1, NBNF, 1
            DO 210, J = 1, NBCO*NBSP*NBCP, 1
               IF ( TABAUX(I,J) .NE. R8VIDE() ) THEN
                  TABAUX(I,J) = TABAUX(I,J)/NBMA
               ENDIF
210         CONTINUE
200      CONTINUE
C
         R    = CREF(1,IPT)
         S    = CREF(2,IPT)
         DEC1 = NBCP*NBSP*(IPT-1)
         DO 310, ICO = 1, NBCO, 1
            DEC2 = NBCP*NBPT*NBSP*(ICO-1)
            DO 320, I = 1, NBCP*NBSP, 1
              IF ( TABAUX(1,I) .EQ. R8VIDE() ) THEN
                C5 = R8VIDE()
              ELSE
                C1 = TABAUX(1,I)
                C2 = TABAUX(2,I)
                C3 = TABAUX(3,I)
                IF ( NBNF .EQ. 3 ) THEN
                  C5 = C1 + R*(C2-C1) + S*(C3-C1)
                ELSE
                  C4 = TABAUX(4,I)
                  C5 = R*((C2+C3-C1-C4) + S*(C1+C3-C2-C4))+C1+C2+C3+C4
                  C5 = UNSUR4*(C5 + S*(C3+C4-C1-C2))
                ENDIF
              ENDIF
              VAL(PTADR+DEC1+DEC2+I-1) = C5
320         CONTINUE
310      CONTINUE
C
300   CONTINUE
C
      PTADR = PTADR + NBPT*NBCP*NBSP*NBCO
C
      END
