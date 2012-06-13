      SUBROUTINE VTCREA(CHAMP,CREFE,BASE,TYPC,NEQ)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     CHAMP,      BASE,TYPC
      CHARACTER*24            CREFE(*)
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
C     CREATION D'UNE STRUCTURE CHAM_NO A PARTIR D'UN MODELE : CREFE
C     LE CHAM_NO MODELE NE DOIT PAS ETRE A REPRESENTATION CONSTANTE.
C     ------------------------------------------------------------------
C     IN  CHAMP  : K19 : NOM DU CHAM_NO A CREER
C     IN  CREFE  : K24 : CONTENU DE L'OBJET .REFE D'UN CHAM_NO MODELE
C                (1) :  K8  : MODELE
C                (2) :  K19 : PROF_CHNO
C IN  BASE   : CH1 : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT ETRE
C                    CREER
C     IN  TYPC   :     : TYPE DES VALEURS DU CHAM_NO A CREER
C              'R'  ==> COEFFICIENTS REELS
C              'C'  ==> COEFFICIENTS COMPLEXES
C              'K8' ==> COEFFICIENTS CARACTERE*8
C     REMARQUE:  AUCUN CONTROLE SUR LE "TYPC" QUE L'ON PASSE TEL QUEL
C                A JEVEUX
C     ------------------------------------------------------------------
C     PRECAUTIONS D'EMPLOI :
C       1) LE CHAM_NO "CHAMP" NE DOIT PAS EXISTER
C       2) LES COEFFICIENTS DU CHAM_NO "CHAMP" NE SONT PAS AFFECTES
C                 (I.E.  LE .VALE EST VIERGE)
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
      INTEGER LCHAMP
      CHARACTER*1 CLASSE
      CHARACTER*1 TYPE
      CHARACTER*8 KBID
      CHARACTER*14  NU
      CHARACTER*24 VALE,REFE,DESC
C     ------------------------------------------------------------------
      DATA VALE/'                   .VALE'/
      DATA REFE/'                   .REFE'/
      DATA DESC/'                   .DESC'/
C     DEB --------------------------------------------------------------
      CALL JEMARQ()
      CLASSE = BASE(1:1)
      IF (TYPC(1:1).EQ.'K') THEN
        TYPE   = 'F'
      ELSE
        TYPE   = TYPC(1:1)
      END IF
C
C     --- RECOPIE DE L'OBJET .REFE MODELE :
      REFE(1:19) = CHAMP
      CALL WKVECT(REFE,CLASSE//' V K24',4,LCHAMP)
      DO 10 I = 1,2
         ZK24(LCHAMP-1+I) = CREFE(I)
   10 CONTINUE
C
C     -- CREATION DE L'OBJET .DESC :
      DESC(1:19) = CHAMP
      CALL WKVECT(DESC,CLASSE//' V I',2,LCHAMP)
      CALL JEECRA(DESC,'DOCU',IBID,'CHNO')
      NU= CREFE(2)(1:14)
      CALL DISMOI('F','NUM_GD_SI',NU,'NUME_DDL',NUGDSI,KBID,IED)
      ZI(LCHAMP-1+1)=NUGDSI
      ZI(LCHAMP-1+2) = 1
C
C     -- CREATION DE L'OBJET .VALE :
      VALE(1:19) = CHAMP
      CALL WKVECT(VALE,CLASSE//' V '//TYPE,NEQ,LCHAMP)
C
C     -- CHANGER LE TYPE SCALAIRE DE LA GRANDEUR ---
      CALL SDCHGD(CHAMP,TYPE)
      CALL JEDEMA()
      END
