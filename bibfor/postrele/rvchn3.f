      SUBROUTINE RVCHN3(VALE,PADR,MA,ITYPM,NBPT,NBCP,FACE,CREF,NBNDF,
     +                  CLOCF,CONEC,VLCCNC,VAL,PTADR,TABAUX)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER PADR(*),MA(*),FACE(*),CONEC(*),VLCCNC(*),NBNDF(6,*)
      INTEGER NBPT,NBCP,PTADR,ITYPM,CLOCF(6,4,*)
      REAL*8  VALE(*),CREF(2,*),VAL(*),TABAUX(4,*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ------------------------------------------------------------------
C     CALCUL DU VALE D' UN SS_CHAM_NO EN UN NBPT POINT
C     ------------------------------------------------------------------
C IN  VALE   : R :  VALE DU SSCH19
C IN  PADR   : I :  PADR DU SSCH19
C IN  MA     : I :  TABLE  DE MAILLE 3D CONTRIBUANT
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
      INTEGER M,F,IPT,I,J,NLOC,NGLO,NBNF
      REAL*8  C1,C2,C3,C4,C5,R,S,UNSUR4
C
C======================================================================
C
      UNSUR4 = 0.25D0
      M      = MA(1)
      F      = FACE(1)
      NBNF   = NBNDF(F,ITYPM)
      DO 10, I = 1, NBNF, 1
         NLOC = CLOCF(F,I,ITYPM)
         NGLO = CONEC(VLCCNC(M) + NLOC-1)
         DO 12, J = 1, NBCP, 1
            TABAUX(I,J) = VALE(PADR(NGLO)+J-1)
12       CONTINUE
10    CONTINUE
      DO 300, IPT = 1, NBPT, 1
         R = CREF(1,IPT)
         S = CREF(2,IPT)
         DO 320, I = 1, NBCP, 1
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
            VAL(PTADR+NBCP*(IPT-1)+I-1) = C5
320      CONTINUE
300   CONTINUE
      PTADR = PTADR + NBPT*NBCP
      END
