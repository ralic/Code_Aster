      SUBROUTINE CRCHNO(CHAMP,PRNO,GRAN,NOMA,BASE,TYPC,NBNOEU,LONVAL)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     CHAMP,PRNO,GRAN,NOMA,BASE,TYPC
      INTEGER                                          NBNOEU,LONVAL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     CREATION D'UNE STRUCTURE CHAMNO "CHAMP"
C IN  CHAMP  : CH8 : NOM DU CHAMNO A CREER
C IN  PRNO   : CH24: NOM DU PROFCHNO ASSOCIE
C IN  GRAN   : CH8 : NOM DE LA GRANDEUR ASSOCIEE
C IN  NOMA   : CH8 : NOM DU MAILLAGE ASSOCIE AU CHAMNO
C IN  BASE   : CH1 : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT ETRE
C                    CREE
C IN  TYPC   : CH1 : TYPE DES VALEURS DU CHAMNO A CREER
C              'R'  ==> COEFFICIENTS REELS
C              'C'  ==> COEFFICIENTS COMPLEXES
C              'K..'==> COEFFICIENTS K..
C IN  NBNOEU : I   : NOMBRE DE NOEUDS DU MAILLAGE ASSOCIE AU CHAMNO
C IN  LONVAL : I   : DIMENSION DU .VALE
C     ------------------------------------------------------------------
C     PRECAUTIONS D'EMPLOI :
C       1) LE CHAMNO "CHAMP" NE DOIT PAS EXISTER
C       2) LES COEFFICIENTS DU CHAMNO "CHAMP" NE SONT PAS AFFECTES
C ----------------------------------------------------------------------
C
C
C
C
      INTEGER       NBVAL, LCHAMP
      CHARACTER*1   CLASSE
      CHARACTER*3   TYPE
      CHARACTER*8   CBID
      CHARACTER*24  VALE, REFE, DESC
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DATA  VALE  /'                   .VALE'/
      DATA  REFE  /'                   .REFE'/
      DATA  DESC  /'                   .DESC'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CLASSE = BASE(1:1)
C
C     --------------------------- REFE --------------------------------
C     --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP ---
      NBVAL = 4
      REFE(1:19) = CHAMP
      CALL WKVECT(REFE,CLASSE//' V K24',NBVAL,LCHAMP)
      ZK24(LCHAMP)   = NOMA
      ZK24(LCHAMP+1) = PRNO
C
C     --------------------------- DESC --------------------------------
C     --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP ---
      NBVAL = 2
      DESC(1:19) = CHAMP
      CALL WKVECT(DESC,CLASSE//' V I',NBVAL,LCHAMP)
      CALL JEECRA(DESC,'DOCU',NBVAL,'CHNO')
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',GRAN),ZI(LCHAMP))
      ZI(LCHAMP+1) = NBNOEU
C
C     --------------------------- VALE --------------------------------
C     ------------- CREATION DE L'OBJET SIMPLE DES VALEURS -------------
C     --- TYPE DES VALEURS, LONGUEUR D'UN VECTEUR ---
C
      VALE(1:19) = CHAMP
      TYPE       = TYPC
      CALL JECREO(VALE,CLASSE//' V '//TYPE )
      CALL JEECRA(VALE,'LONMAX',LONVAL,CBID)
C
      CALL JEDEMA()
      END
