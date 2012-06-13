      SUBROUTINE MTCOPY(MATIN,MATOUT,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*(*) MATIN,MATOUT
      INTEGER       IER
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
C     ------------------------------------------------------------------
C     RECOPIE LES VALEURS DE LA MATRICE MATIN  DANS LA MATRICE MATOUT
C     ------------------------------------------------------------------
C     PRECAUTION D'EMPLOI :
C        1) LA MATRICE "MATOUT" DOIT EXISTER ET AVOIR LA MEME STRUCTURE
C     QUE "MATIN"
C        2) ON RECOPIE LE .CCID DE MATIN DANS
C     MATOUT, SI MATOUT POSSEDAIT DEJA CE CHAMP ON LE DETRUIT.
C     ------------------------------------------------------------------
C     RAPPEL :   UNE MATRICE  "MAT" EXISTE
C          S'IL EXISTE UN OBJET SIMPLE  MAT//"REFE"
C          ET UNE COLLECTION NUMEROTEE  MAT//"VALE"
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
      INTEGER LMATOU,LMATIN,NIMPOU
      REAL*8 UN
      CHARACTER*8 NOMDDL
      CHARACTER*19 MATI19,MATO19
      CHARACTER*24 NMATOU,NMATIN
      CHARACTER*24 VALK(2)
C     ------------------------------------------------------------------
      DATA NOMDDL/'        '/
C     ------------------------------------------------------------------
C
C     --- CONTROLE DES REFERENCES ---
      CALL JEMARQ()
      CALL VRREFE(MATIN,MATOUT,IER)
      MATI19 = MATIN
      MATO19 = MATOUT
      IF (IER.NE.0) THEN
         VALK(1) = MATI19
         VALK(2) = MATO19
         CALL U2MESK('F','ALGELINE2_11', 2 ,VALK)

      ELSE
C        --- TYPE DES VALEURS, NOMBRE DE BLOCS, LONGUEUR D'UN BLOC ---
        CALL MTDSCR(MATIN)
        NMATIN = MATIN(1:19)//'.&INT'
        CALL JEVEUO(MATIN(1:19)//'.&INT','E',LMATIN)
        CALL MTDSCR(MATOUT)
        NMATOU = MATOUT(1:19)//'.&INT'
        CALL JEVEUO(MATOUT(1:19)//'.&INT','E',LMATOU)
C
C --- GESTION DES .CCID .CCLL .CCVA
C
        NIMPOU = ZI(LMATOU+7)
        IF (NIMPOU.NE.0) THEN
          CALL JEDETR(MATO19//'.CCID')
          CALL JEDETR(MATO19//'.CCLL')
          CALL JEDETR(MATO19//'.CCII')
          CALL JEDETR(MATO19//'.CCVA')
          ZI(LMATOU+7) = 0
          ZI(LMATOU+15) = 0
          ZI(LMATOU+16) = 0
        END IF

        UN = 1.D0
C
C --- RECOPIE DU .VALE ET DE .CCID, .CCLL, .CCVA
        CALL MTCMBL(1,'R',UN,NMATIN,NMATOU,NOMDDL,' ','ELIM=')
      END IF
C
      CALL JEDEMA()
      END
