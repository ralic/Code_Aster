      SUBROUTINE VTCREC(CHAMP,CHMOD,BASE,TYPC,NEQ)
      IMPLICIT NONE
      CHARACTER*(*) CHAMP,BASE,TYPC,CHMOD
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET J.PELLET
C     ------------------------------------------------------------------
C     CREATION D'UNE STRUCTURE CHAM_NO A PARTIR D'UN MODELE : CHMOD
C     LE CHAM_NO MODELE NE DOIT PAS ETRE A REPRESENTATION CONSTANTE.
C     ------------------------------------------------------------------
C     IN  CHAMP  : K19 : NOM DU CHAM_NO A CREER
C     IN  CHMOD  : K29 : NOM DU CHAMP MODELE
C     IN  BASE   : CH1 : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT
C                        ETRE CREE
C     IN  TYPC   :     : TYPE DES VALEURS DU CHAM_NO A CREER
C              'R'  ==> COEFFICIENTS REELS
C              'C'  ==> COEFFICIENTS COMPLEXES
C              'K8' ==> COEFFICIENTS CARACTERE*8
C     REMARQUE:  AUCUN CONTROLE SUR LE "TYPC" QUE L'ON PASSE TEL QUEL
C                A JEVEUX
C     ------------------------------------------------------------------
C     DETAILS :
C       1) cette routine ne fonctione pas avec les cham_ni a
C          representation constante
C       2) LES COEFFICIENTS DU CHAM_NO "CHAMP" NE SONT PAS AFFECTES
C                 (I.E.  LE .VALE EST VIERGE)
C     ------------------------------------------------------------------
C
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
C     ------------------------------------------------------------------
      INTEGER LCHAMP
      CHARACTER*1 CLASSE
      CHARACTER*1 TYPE
      CHARACTER*8 KBID
      CHARACTER*24 VALE,REFE,DESC
C     ------------------------------------------------------------------
      INTEGER  IBID,NUGDSI,IED,NEQ
      CHARACTER*19 CHMOD2
C     ------------------------------------------------------------------
      DATA VALE/'                   .VALE'/
      DATA REFE/'                   .REFE'/
      DATA DESC/'                   .DESC'/
C     DEB --------------------------------------------------------------
      CALL JEMARQ()

      REFE(1:19) = CHAMP
      DESC(1:19) = CHAMP
      VALE(1:19) = CHAMP
      CHMOD2=CHMOD

      CLASSE = BASE(1:1)
      IF (TYPC(1:1).EQ.'K') THEN
        TYPE   = 'F'
      ELSE
        TYPE   = TYPC(1:1)
      END IF

C     -- RECOPIE DE L'OBJET .REFE MODELE :
      CALL JEDUPO(CHMOD2//'.REFE', CLASSE, REFE, .FALSE.)
C
C     -- CREATION DE L'OBJET .DESC :
      CALL WKVECT(DESC,CLASSE//' V I',2,LCHAMP)
      CALL JEECRA(DESC,'DOCU',IBID,'CHNO')
      CALL DISMOI('F','NUM_GD',CHMOD2,'CHAM_NO',NUGDSI,KBID,IED)
      ZI(LCHAMP-1+1)=NUGDSI
      ZI(LCHAMP-1+2) = 1

C     -- CREATION DE L'OBJET .VALE :
      CALL WKVECT(VALE,CLASSE//' V '//TYPE,NEQ,LCHAMP)

C     -- CHANGER LE TYPE SCALAIRE DE LA GRANDEUR ---
      CALL SDCHGD(CHAMP,TYPE)

      CALL JEDEMA()
      END
