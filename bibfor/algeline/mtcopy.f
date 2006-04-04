      SUBROUTINE MTCOPY(MATIN,MATOUT,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) MATIN,MATOUT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 04/04/2006   AUTEUR VABHHTS J.PELLET 
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
C     RECOPIE LES VALEURS DE LA MATRICE MATIN  DANS LA MATRICE MATOUT
C     ------------------------------------------------------------------
C     PRECAUTION D'EMPLOI :
C        1) LA MATRICE "MATOUT" DOIT EXISTER ET AVOIR LA MEME STRUCTURE
C     QUE "MATIN"
C        2) ON RECOPIE LES .CCID, .CCLL, .CCVA DE MATIN DANS
C     MATOUT, SI MATOUT POSSEDAIT DEJA CES CHAMPS ON LES DETRUITS.
C     ------------------------------------------------------------------
C     RAPPEL :   UNE MATRICE  "MAT" EXISTE
C          S'IL EXISTE UN OBJET SIMPLE  MAT//"REFE"
C          ET UNE COLLECTION NUMEROTEE  MAT//"VALE"
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      INTEGER LMATOU,LMATIN
      REAL*8 UN
      CHARACTER*1 TYPE(2)
      CHARACTER*8 NOMDDL
      CHARACTER*19 MATI19,MATO19,KBID
      CHARACTER*24 NMATOU,NMATIN
C     ------------------------------------------------------------------
      DATA TYPE/'R','C'/
      DATA NOMDDL/'        '/
C     ------------------------------------------------------------------
C
C     --- CONTROLE DES REFERENCES ---
      CALL JEMARQ()
      CALL VRREFE(MATIN,MATOUT,IER)
      MATI19 = MATIN
      MATO19 = MATOUT
      IF (IER.NE.0) THEN
        CALL UTMESS('F','MTCOPY','LES "MATASS" "'//MATI19//'"  ET  "'//
     &              MATO19//'"  N''ONT LE MEME DOMAINE DE DEFINITION.')

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
          CALL JEDETR(MATO19//'.CCVA')
          ZI(LMATOU+7) = 0
          ZI(LMATOU+15) = 0
          ZI(LMATOU+16) = 0
        END IF

        UN = 1.D0
C
C --- RECOPIE DU .VALE ET DE .CCID, .CCLL, .CCVA
        CALL MTCOMB(1,'R',UN,TYPE(ZI(LMATIN+3)),NMATIN,
     &              TYPE(ZI(LMATOU+3)),NMATOU,NOMDDL,'V')
C
C        --- LIBERATION DES DESCRIPTEURS ---
      END IF
C
      CALL JEDEMA()
      END
