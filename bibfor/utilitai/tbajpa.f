      SUBROUTINE TBAJPA ( NOMTA, NBPAR, NOMPAR, TYPPAR )
      IMPLICIT   NONE
      INTEGER                    NBPAR
      CHARACTER*(*)       NOMTA,        NOMPAR(*), TYPPAR(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/11/2004   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C      AJOUTER DES PARAMETRES A UNE TABLE.
C ----------------------------------------------------------------------
C IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
C IN  : NBPAR  : NOMBRE DE PARAMETRES.
C IN  : NOMPAR : NOMS DES PARAMETRES.
C IN  : TYPPAR : TYPES DES PARAMETRES.
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ----------------------------------------------------------------------
      INTEGER      IRET, NBPARA, NBLIGN, JTBBA, JTBNP, NBPM, NBPU
      INTEGER      NDIM, JTBLP, I, J, K, IDEB, JNJV, NBPAR1
      CHARACTER*1  BASE
      CHARACTER*3  TYPE
      CHARACTER*4  KNUME
      CHARACTER*8  K8B
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, INPAR, JNPAR
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = ' '
      NOMTAB = NOMTA
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('F','TBAJPA','LA TABLE N''EXISTE PAS') 
      ENDIF
      IF ( NOMTAB(18:19) .NE. '  ' ) THEN
         CALL UTMESS('F','TBAJPA','NOM DE TABLE INCORRECT') 
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBBA' , 'L' , JTBBA )
      BASE = ZK8(JTBBA)(1:1)
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'E', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = MAX ( ZI(JTBNP+1) , 10 )
C
C ----------------------------------------------------------------------
C
C                   --- ON INITIALISE LA TABLE ---
C
      IF ( NBPARA .EQ. 0 ) THEN
C
         ZI(JTBNP) = NBPAR
         NDIM = 4 * NBPAR
C
         CALL JECREO ( NOMTAB//'.TBLP', BASE//' V K24')
         CALL JEECRA ( NOMTAB//'.TBLP', 'LONMAX', NDIM, ' ')
         CALL JEECRA ( NOMTAB//'.TBLP', 'LONUTI', NDIM, ' ')
         CALL JEVEUO ( NOMTAB//'.TBLP' , 'E', JTBLP )
C
         DO 10 I = 1 , NBPAR
            ZK24(JTBLP+4*(I-1)  ) = NOMPAR(I)
            ZK24(JTBLP+4*(I-1)+1) = TYPPAR(I)
            CALL CODENT ( I ,'D0',KNUME)
            NOMJV = NOMTAB//'.'//KNUME
            TYPE = '   '
            TYPE = TYPPAR(I)
            CALL JECREO ( NOMJV , BASE//' V '//TYPE )
            CALL JEECRA ( NOMJV , 'LONMAX' , NBLIGN , ' ' )
            CALL JEECRA ( NOMJV , 'LONUTI' ,  0 , ' ' )
            CALL JEVEUO ( NOMJV , 'E', IRET )
            ZK24(JTBLP+4*(I-1)+2) = NOMJV
            NOMJV = NOMTAB(1:17)//'LG.'//KNUME
            CALL JECREO ( NOMJV ,BASE//' V I' )
            CALL JEECRA ( NOMJV , 'LONMAX' , NBLIGN , ' ' )
            CALL JEVEUO ( NOMJV , 'E', JNJV )
            DO 12 J = 1 , NBLIGN
               ZI(JNJV+J-1) = 0
 12         CONTINUE
            ZK24(JTBLP+4*(I-1)+3) = NOMJV
 10      CONTINUE
C
C ----------------------------------------------------------------------
C
C               --- ON AJOUTE DES PARAMETRES A LA TABLE ---
C
      ELSE
C
         CALL JELIRA ( NOMTAB//'.TBLP', 'LONMAX', NBPM, K8B)
         CALL JELIRA ( NOMTAB//'.TBLP', 'LONUTI', NBPU, K8B)
         CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
C
C        IL FAUT INITIALISER LES COLONNES AU LONMAX ET NON PAS A NBLIGN
C        QUI EST LE NOMBRE DE LIGNES EVENTUELLEMENT REMPLI
C        ON RECUPERE LE PREMIER PARAMETRE DE LA TABLE
C
         J = 1
         CALL CODENT ( J ,'D0',KNUME)
         NOMJV = NOMTAB(1:17)//'LG.'//KNUME
         CALL JELIRA ( NOMJV, 'LONMAX', NBLIGN, K8B)
C
C        --- ON VERIFIE QUE LES PARAMETRES N'EXISTENT PAS ---
         NBPAR1 = 0
         DO 20 I = 1 , NBPAR
            INPAR = NOMPAR(I)
            DO 22 J = 1 , NBPARA
               JNPAR = ZK24(JTBLP+4*(J-1))
               IF ( INPAR .EQ. JNPAR ) GOTO 20 
 22         CONTINUE
            NBPAR1 = NBPAR1 + 1
 20      CONTINUE
         IF ( NBPAR1 .EQ. 0 ) GOTO 9999
C
         IDEB = NBPARA
         NBPARA = NBPARA + NBPAR1
         ZI(JTBNP) = NBPARA
         NDIM = 4*NBPARA
C
         IF ( NDIM .GT. NBPM ) THEN
            CALL JUVECA ( NOMTAB//'.TBLP', NDIM)
         ENDIF
         CALL JEECRA ( NOMTAB//'.TBLP','LONUTI',NDIM,' ')
         CALL JEVEUO ( NOMTAB//'.TBLP' , 'E', JTBLP )
         DO 30 I = 1 , NBPAR
            INPAR = NOMPAR(I)
            DO 32 J = 1 , NBPARA
               JNPAR = ZK24(JTBLP+4*(J-1))
               IF ( INPAR .EQ. JNPAR ) GOTO 30
 32         CONTINUE
            IDEB = IDEB + 1
            J = IDEB
            ZK24(JTBLP+4*(J-1)  ) = NOMPAR(I)
            ZK24(JTBLP+4*(J-1)+1) = TYPPAR(I)
            CALL CODENT ( J ,'D0',KNUME)
            NOMJV = NOMTAB//'.'//KNUME
            TYPE = '   '
            TYPE = TYPPAR(I)
            CALL JECREO ( NOMJV ,BASE//' V '//TYPE)
            CALL JEECRA ( NOMJV , 'LONMAX' , NBLIGN , ' ' )
            CALL JEECRA ( NOMJV , 'LONUTI' ,  0 , ' ' )
            CALL JEVEUO ( NOMJV , 'E', IRET )
            ZK24(JTBLP+4*(J-1)+2) = NOMJV
            NOMJV = NOMTAB(1:17)//'LG.'//KNUME
            CALL JECREO ( NOMJV ,BASE//' V I' )
            CALL JEECRA ( NOMJV , 'LONMAX' , NBLIGN , ' ' )
            CALL JEVEUO ( NOMJV , 'E', JNJV )
            DO 34 K = 1 , NBLIGN
               ZI(JNJV+K-1) = 0
 34         CONTINUE
            ZK24(JTBLP+4*(J-1)+3) = NOMJV
 30      CONTINUE
C
      ENDIF
 9999 CONTINUE
C
      CALL JEDEMA()
      END
