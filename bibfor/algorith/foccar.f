      SUBROUTINE FOCCAR ( NOMFON, EXPO1, SORTIE, BASE )
      IMPLICIT   NONE
      INTEGER             EXPO1
      CHARACTER*1         BASE
      CHARACTER*19        NOMFON, SORTIE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/08/2004   AUTEUR CIBHHLV L.VIVAN 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C      CALCUL F(X)=G(X)**N
C
C IN  : NOMFON : NOM DE LA FONCTION A TRAITER
C IN  : EXPO1  : EXPOSANT
C IN  : BASE   : BASE OU EST STOCKEE LA FONCTION PRODUITE
C OUT : SORTIE : NOM DE LA FONCTION PRODUITE
C
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       I, K, LPRO, NBPARF, NBVAL, LONVAL
      INTEGER       LVAL, LVAR, LONT
      CHARACTER*8   CBID
      CHARACTER*16  TYPREF
      CHARACTER*24  O1, O2
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO ( NOMFON//'.PROL', 'L', LPRO )
      TYPREF =  ZK16(LPRO)
C
      O1 = NOMFON//'.PROL'
      O2 = SORTIE//'.PROL'
      CALL JEDUPO ( O1, BASE, O2, .FALSE. )
C
C     -- ON TRAITE UNE NAPPE ----
 
      IF ( TYPREF .EQ. 'NAPPE' ) THEN

         O1 = NOMFON//'.PARA'
         O2 = SORTIE//'.PARA'
         CALL JEDUPO ( O1, BASE, O2, .FALSE. )

         CALL JELIRA ( NOMFON//'.PARA', 'LONMAX', NBPARF, CBID )
         CALL JELIRA ( NOMFON//'.VALE', 'LONT', LONT, CBID )
         CALL JECREC ( SORTIE//'.VALE', BASE//' V R', 'NU', 'CONTIG',
     +                                        'VARIABLE', NBPARF )
         CALL JEECRA ( SORTIE//'.VALE', 'LONT', LONT, ' ' )

C        -- BOUCLE SUR CHAQUE PARAMETRE DE LA NAPPE

         DO 100 I=1,NBPARF
            CALL JELIRA(JEXNUM(NOMFON//'.VALE',I),'LONUTI',LONVAL,CBID)
            NBVAL = LONVAL / 2
            CALL JEVEUO ( JEXNUM(NOMFON//'.VALE',I), 'L', LVAL )

            CALL JECROC(JEXNUM(SORTIE//'.VALE',I))
            CALL JEECRA(JEXNUM(SORTIE//'.VALE',I),'LONMAX',LONVAL,' ')
            CALL JEECRA(JEXNUM(SORTIE//'.VALE',I),'LONUTI',LONVAL,' ')
            CALL JEVEUO(JEXNUM(SORTIE//'.VALE',I),'E',LVAR)
            DO 110 K = 1 , NBVAL
               ZR(LVAR+K-1) = ZR(LVAL+K-1)
               ZR(LVAR+NBVAL+K-1) = ZR(LVAL+NBVAL+K-1)**EXPO1
 110        CONTINUE
            
 100     CONTINUE
C
C
C     -- ON TRAITE UNE FONCTION
C
      ELSEIF ( TYPREF .EQ. 'FONCTION' ) THEN
 
         CALL JELIRA( NOMFON//'.VALE', 'LONUTI', LONVAL, CBID )
         NBVAL = LONVAL / 2
         CALL JEVEUO ( NOMFON//'.VALE', 'L', LVAL )
C
         CALL WKVECT ( SORTIE//'.VALE', BASE//' V R', LONVAL, LVAR )
         DO 200 K = 1 , NBVAL
            ZR(LVAR+K-1) = ZR(LVAL+K-1)
            ZR(LVAR+NBVAL+K-1) = ZR(LVAL+NBVAL+K-1)**EXPO1
 200     CONTINUE

      ELSE
         CALL UTMESS('F','FOCCAR','PUISSANCE POUR '//TYPREF//
     +                            ' NON IMPLEMENTEE')
      ENDIF
C
      CALL JEDEMA()
C
      END
