      SUBROUTINE CRCHNO(CHAMP,PRNO,GRAN,NOMA,BASE,TYPC,NBNOEU,LONVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CHAMP,PRNO,GRAN,NOMA,BASE,TYPC
      INTEGER                                          NBNOEU,LONVAL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/04/97   AUTEUR VABHHTS J.PELLET 
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
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER       NBVAL,IVAL, LCHAMP
      CHARACTER*1   CLASSE
      CHARACTER*3   TYPE
      CHARACTER*8   CBID
      CHARACTER*24  VALE, REFE, DESC
C     ------------------------------------------------------------------
      DATA  VALE  /'                   .VALE'/
      DATA  REFE  /'                   .REFE'/
      DATA  DESC  /'                   .DESC'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CLASSE = BASE(1:1)
C
C     --------------------------- REFE --------------------------------
C     --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP ---
      NBVAL = 2
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
