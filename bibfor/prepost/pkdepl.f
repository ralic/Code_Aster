      SUBROUTINE PKDEPL ( NOMA, FOND, DEPSUP, DEPINF )
      IMPLICIT   NONE
      CHARACTER*8         NOMA, FOND
      CHARACTER*24        DEPSUP, DEPINF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 18/11/2003   AUTEUR CIBHHLV L.VIVAN 
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

C     OPERATEUR POST_K1_K2_K3 : RECUPERATION DES DEPLACEMENTS
C                               - DES NOEUDS DE LA LEVRE SUPERIEURE
C                               - DES NOEUDS DE LA LEVRE INFERIEURE

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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      INTEGER      IBID, N1, N2, NBPAR, IRET, NBORDR, JORDR, NBNOE,
     +             JNOLS, JNOLI, NBNOLS, NBNOLI, NBMA, JLIMA, IM, 
     +             IORD, NORDR, IAD, NCMP, IN, INO, ICP, IDLINO, 
     +             IADRMA, JCNSD, JCNSC, JCNSV, JCNSL
      PARAMETER  ( NBPAR=5 )
      REAL*8       PREC, VALE(4), DX, DY, DZ
      COMPLEX*16   CBID
      LOGICAL      LDX, LDY, LDZ
      CHARACTER*2  TYPPAR(NBPAR)
      CHARACTER*8  K8B, NOMRES, CRIT, NOEUD, NOCMP, RESU
      CHARACTER*16 NOMCMD, CONCEP, NOMPAR(NBPAR)
      CHARACTER*19 CHAMP, CHAMS
      CHARACTER*24 FONLSU, FONLIN, KNUM

      DATA  NOMPAR / 'INST' , 'NOEUD' , 'DX' , 'DY' , 'DZ' /
      DATA  TYPPAR / 'R'    , 'K8'    , 'R'  , 'R'  , 'R'  /
C DEB ------------------------------------------------------------------
      CALL JEMARQ ( )

      CALL GETRES ( NOMRES , CONCEP , NOMCMD )

C     ------------------------------------------------------------------
C             LA TABLE DE DEPLACEMENT DE LA LEVRE SUPERIEURE
C     ------------------------------------------------------------------

      CALL GETVID ( ' ', 'TABL_DEPL_SUP', 1,1,1, DEPSUP, N1 )

C     ------------------------------------------------------------------
C             LA TABLE DE DEPLACEMENT DE LA LEVRE INFERIEURE
C     ------------------------------------------------------------------

      CALL GETVID ( ' ', 'TABL_DEPL_INF', 1,1,1, DEPINF, N2 )

      IF ( N1*N2 .NE. 0 )  GOTO 9999

C     ------------------------------------------------------------------
C                  RECUPERATION D'UN RESULTAT
C     ------------------------------------------------------------------

      CALL GETVID ( ' ', 'RESULTAT' , 1,1,1, RESU, N1 )

      CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, PREC, N1 )
      CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT, N2 )

      KNUM = '&&PKDEPL.NUME_ORDRE'
      CALL RSUTNU ( RESU, ' ', 1, KNUM, NBORDR, PREC, CRIT, IRET )
      IF (IRET.NE.0) THEN
         CALL UTMESS('F',NOMCMD,'ERREUR(S) DANS LES DONNEES')
      ENDIF
      CALL JEVEUO ( KNUM, 'L', JORDR )

C     ------------------------------------------------------------------

      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOE,K8B,IRET)

      CALL WKVECT ( '&&PKDEPL_LIST_NOEUD'  , 'V V I', NBNOE, IDLINO )
      CALL WKVECT ( '&&PKDEPL_NOEU_LEV_SUP', 'V V I', NBNOE, JNOLS  )
      CALL WKVECT ( '&&PKDEPL_NOEU_LEV_INF', 'V V I', NBNOE, JNOLI  )

C --- GROUP_MA LEVRE_SUP --> GROUP_NO LEVRE_SUP

      FONLSU = FOND//'.LEVRESUP  .MAIL'
      CALL JELIRA ( FONLSU, 'LONMAX', NBMA, K8B )
      CALL JEVEUO ( FONLSU, 'L', IADRMA )
      CALL WKVECT ( '&&PKDEPL_MAILLE_LEV_SUP', 'V V I', NBMA, JLIMA )
      DO 10 IM = 1 , NBMA
         CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(IADRMA+IM-1)),
     +                                                 ZI(JLIMA+IM-1) )
 10   CONTINUE
      CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), NBMA,
     +                           ZI(JNOLS), NBNOLS, 'TOUS' )

C --- GROUP_MA LEVRE_INF --> GROUP_NO LEVRE_INF
C
      FONLIN = FOND//'.LEVREINF  .MAIL'
      CALL JELIRA ( FONLIN, 'LONMAX', NBMA, K8B )
      CALL JEVEUO ( FONLIN, 'L', IADRMA )
      CALL WKVECT ( '&&PKDEPL_MAILLE_LEV_INF', 'V V I', NBMA, JLIMA )
      DO 20 IM = 1 , NBMA
         CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(IADRMA+IM-1)),
     +                                                 ZI(JLIMA+IM-1) )
 20   CONTINUE
      CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), NBMA,
     +                           ZI(JNOLI), NBNOLI, 'TOUS' )

C     ------------------------------------------------------------------

C --- ON CREE LA TABLE DES DEPLACEMENTS DES LEVRES
C
      DEPSUP = '&&PKDEPL.DEPL_SUP       '
      DEPINF = '&&PKDEPL.DEPL_INF       '

      CALL TBCRSD ( DEPSUP, 'V' )
      CALL TBAJPA ( DEPSUP, NBPAR, NOMPAR, TYPPAR )

      CALL TBCRSD ( DEPINF, 'V' )
      CALL TBAJPA ( DEPINF, NBPAR, NOMPAR, TYPPAR )

      DO 30 IORD = 1, NBORDR
         NORDR = ZI(JORDR+IORD-1)

         CALL RSEXCH ( RESU, 'DEPL', NORDR, CHAMP, IRET )
         IF (IRET.NE.0) THEN
            CALL UTMESS('F',NOMCMD,'PB RECUP CHAMP DANS RESULTAT')
         ENDIF

         CALL RSADPA ( RESU, 'L', 1, 'INST', NORDR, 0, IAD, K8B )
         VALE(1) = ZR(IAD)

         CHAMS = '&&PKDEPL_CNO_S'
         CALL CNOCNS ( CHAMP, 'V', CHAMS )
         CALL JEVEUO ( CHAMS//'.CNSD', 'L', JCNSD )
         CALL JEVEUO ( CHAMS//'.CNSC', 'L', JCNSC )
         CALL JEVEUO ( CHAMS//'.CNSV', 'L', JCNSV )
         CALL JEVEUO ( CHAMS//'.CNSL', 'L', JCNSL )
         NCMP = ZI(JCNSD-1+2)

         DO 32 IN = 1, NBNOLS
            INO = ZI(JNOLS+IN-1)
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO), NOEUD )
            LDX = .FALSE.
            LDY = .FALSE.
            LDZ = .FALSE.
            DO 321 ICP = 1,NCMP
               NOCMP = ZK8(JCNSC-1+ICP)
               IF ( NOCMP .EQ. 'DX' ) THEN
                  IF (ZL(JCNSL-1+(INO-1)*NCMP+ICP)) THEN
                     LDX = .TRUE.
                     DX = ZR(JCNSV-1+(INO-1)*NCMP+ICP)
                  ENDIF
               ELSEIF ( NOCMP .EQ. 'DY' ) THEN
                  IF (ZL(JCNSL-1+(INO-1)*NCMP+ICP)) THEN
                     LDY = .TRUE.
                     DY = ZR(JCNSV-1+(INO-1)*NCMP+ICP)
                  ENDIF
               ELSEIF ( NOCMP .EQ. 'DZ' ) THEN
                  IF (ZL(JCNSL-1+(INO-1)*NCMP+ICP)) THEN
                     LDZ = .TRUE.
                     DZ = ZR(JCNSV-1+(INO-1)*NCMP+ICP)
                  ENDIF
               ENDIF
 321        CONTINUE
            IF ( .NOT. LDX ) THEN
               CALL UTMESS('F',NOMCMD,'MANQUE LA COMPOSANTE "DX" '//
     +                                ' POUR LE NOEUD '//NOEUD)
            ELSE
               VALE(2) = DX
            ENDIF
            IF ( .NOT. LDY ) THEN
               CALL UTMESS('F',NOMCMD,'MANQUE LA COMPOSANTE "DY" '//
     +                                ' POUR LE NOEUD '//NOEUD)
            ELSE
               VALE(3) = DY
            ENDIF
            IF ( .NOT. LDZ ) THEN
               CALL UTMESS('F',NOMCMD,'MANQUE LA COMPOSANTE "DZ" '//
     +                                ' POUR LE NOEUD '//NOEUD)
            ELSE
               VALE(4) = DZ
            ENDIF
            CALL TBAJLI ( DEPSUP, NBPAR, NOMPAR, IBID, VALE, CBID,
     +                       NOEUD, 0 )
 32      CONTINUE

         DO 34 IN = 1, NBNOLI
            INO = ZI(JNOLI+IN-1)
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO), NOEUD )
            LDX = .FALSE.
            LDY = .FALSE.
            LDZ = .FALSE.
            DO 341 ICP = 1,NCMP
               NOCMP = ZK8(JCNSC-1+ICP)
               IF ( NOCMP .EQ. 'DX' ) THEN
                  IF (ZL(JCNSL-1+(INO-1)*NCMP+ICP)) THEN
                     LDX = .TRUE.
                     DX = ZR(JCNSV-1+(INO-1)*NCMP+ICP)
                  ENDIF
               ELSEIF ( NOCMP .EQ. 'DY' ) THEN
                  IF (ZL(JCNSL-1+(INO-1)*NCMP+ICP)) THEN
                     LDY = .TRUE.
                     DY = ZR(JCNSV-1+(INO-1)*NCMP+ICP)
                  ENDIF
               ELSEIF ( NOCMP .EQ. 'DZ' ) THEN
                  IF (ZL(JCNSL-1+(INO-1)*NCMP+ICP)) THEN
                     LDZ = .TRUE.
                     DZ = ZR(JCNSV-1+(INO-1)*NCMP+ICP)
                  ENDIF
               ENDIF
 341        CONTINUE
            IF ( .NOT. LDX ) THEN
               CALL UTMESS('F',NOMCMD,'MANQUE LA COMPOSANTE "DX" '//
     +                                ' POUR LE NOEUD '//NOEUD)
            ELSE
               VALE(2) = DX
            ENDIF
            IF ( .NOT. LDY ) THEN
               CALL UTMESS('F',NOMCMD,'MANQUE LA COMPOSANTE "DY" '//
     +                                ' POUR LE NOEUD '//NOEUD)
            ELSE
               VALE(3) = DY
            ENDIF
            IF ( .NOT. LDZ ) THEN
               CALL UTMESS('F',NOMCMD,'MANQUE LA COMPOSANTE "DZ" '//
     +                                ' POUR LE NOEUD '//NOEUD)
            ELSE
               VALE(4) = DZ
            ENDIF
            CALL TBAJLI ( DEPINF, NBPAR, NOMPAR, IBID, VALE, CBID,
     +                       NOEUD, 0 )
 34      CONTINUE

         CALL DETRSD ( 'CHAM_NO_S', CHAMS )

 30   CONTINUE

 9999 CONTINUE

      CALL JEDETR ( KNUM )
      CALL JEDETR ( '&&PKDEPL_LIST_NOEUD'   )
      CALL JEDETR ( '&&PKDEPL_NOEU_LEV_SUP' )
      CALL JEDETR ( '&&PKDEPL_NOEU_LEV_INF' )
      CALL JEDETR ( '&&PKDEPL_MAILLE_LEV_SUP' )
      CALL JEDETR ( '&&PKDEPL_MAILLE_LEV_INF' )
 
      CALL JEDEMA ( )
      END
