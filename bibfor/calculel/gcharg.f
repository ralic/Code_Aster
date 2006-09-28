      SUBROUTINE GCHARG ( MODELE, NCHAR, LCHAR, CHVOLU, CF1D2D, CF2D3D,
     &                 CHPRES,CHEPSI,CHPESA,CHROTA,FONC,EPSI,TIME,IORD)
      IMPLICIT NONE
      INTEGER         NCHAR, IORD
      CHARACTER*8     MODELE, LCHAR(*)
      CHARACTER*19    CHVOLU,CF1D2D,CF2D3D,CHPRES,CHEPSI,CHPESA,CHROTA
      LOGICAL         FONC, EPSI
      REAL*8  TIME
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ----------------------------------------------------------------------
C
      INTEGER       IBID, IER, I, IF3D3D, IF2D2D, IF1D2D, IF2D3D,
     &              IPRESS, IEPSIN, IROTA, IPESA, IRET, N1, ICHA0,
     &              NBVALE, IN, IVAL, JVAL, NEXCI, JPARA, JFCHA, NRES
      INTEGER       NVOLU, N1D2D, N2D3D, NPRESS, NEPSI, NROTA, NPESA
      CHARACTER*8   K8B, RESU
      CHARACTER*16  TYPE, OPER
      CHARACTER*24  VCHAR, NOMFCT
      CHARACTER*24  EXCISD
      REAL*8  CONST
      LOGICAL LCHSD, FONCI
C     ------------------------------------------------------------------
C
      CALL JEMARQ()

      CALL GETRES ( K8B, TYPE, OPER )
      FONC   = .FALSE.
      EPSI   = .FALSE.
      NVOLU = 0
      N1D2D = 0
      N2D3D = 0
      NPRESS = 0
      NEPSI = 0
      NROTA = 0
      NPESA = 0
C
      CALL GETFAC('EXCIT',NEXCI)
      CALL GETVID(' ','RESULTAT',0,1,1,RESU,NRES)
      LCHSD=.FALSE.
      IF(NRES.NE.0.AND.NEXCI.EQ.0) LCHSD=.TRUE.
C
C--- LECTURE DES INFORMATIONS CONTENUES DANS LA SD RESULTAT
C
      IF(LCHSD) THEN
        CALL RSADPA(RESU,'L',1,'EXCIT',IORD,0,JPARA,K8B)
        EXCISD = ZK24(JPARA)
        CALL JEVEUO(EXCISD(1:19)//'.FCHA','L',JFCHA)
      ENDIF
C
      IER = 0
      DO 10 I = 1 , NCHAR
C
        IF(LCHAR(I).NE.' ') THEN
         CALL DISMOI('F','TYPE_CHARGE',LCHAR(I),'CHARGE',IBID,K8B,IRET)
         IF ( K8B(5:7) .EQ. '_FO' ) THEN
            FONC = .TRUE.
            FONCI = .TRUE.
         ELSE
           FONCI = .FALSE.
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F3D3D.DESC', IF3D3D )
         IF ( IF3D3D .NE. 0 ) THEN
           IF ( NVOLU .EQ. 0) THEN
             NVOLU = NVOLU + 1
             CALL COPISD('CHAMP_GD','V',LCHAR(I)//'.CHME.F3D3D',CHVOLU)

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0 .AND.(.NOT. FONCI)) THEN
              VCHAR = LCHAR(I)//'.CHME.F3D3D.VALE'
              CALL JEVEUO(VCHAR,'L',IVAL)
              CALL JEVEUO(CHVOLU // '.VALE','E',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 101 IN = 1,NBVALE
                  ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  101         CONTINUE
             ELSEIF (N1. NE. 0 .AND. FONCI) THEN
               IER = IER + 1
               CALL UTDEBM('E',OPER,'ON NE PEUT ASSOCIER DES CHARGE')
               CALL UTIMPK('S','MENTS FONCTION (ICI :',1,LCHAR(I)(1:8))
               CALL UTIMPK('S','AVEC UNE',1,'FONCTION MULTIPLICATRICE')
               CALL UTFINM()
             ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'FORCE VOLUMIQUE')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHVOLU(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F2D2D.DESC', IF2D2D )
         IF ( IF2D2D .NE. 0 ) THEN
           IF ( NVOLU .EQ. 0 ) THEN
             NVOLU = NVOLU + 1
             CALL COPISD('CHAMP_GD','V',LCHAR(I)//'.CHME.F2D2D',CHVOLU)

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0 .AND.(.NOT. FONCI)) THEN
              VCHAR = LCHAR(I)//'.CHME.F2D2D.VALE'
              CALL JEVEUO(VCHAR,'L',IVAL)
              CALL JEVEUO(CHVOLU // '.VALE','E',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 102 IN = 1,NBVALE
                  ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  102         CONTINUE
             ELSEIF (N1. NE. 0 .AND. FONCI) THEN
               IER = IER + 1
               CALL UTDEBM('E',OPER,'ON NE PEUT ASSOCIER DES CHARGE')
               CALL UTIMPK('S','MENTS FONCTION (ICI :',1,LCHAR(I)(1:8))
               CALL UTIMPK('S','AVEC UNE',1,'FONCTION MULTIPLICATRICE')
               CALL UTFINM()
             ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'FORCE VOLUMIQUE')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHVOLU(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F1D2D.DESC', IF1D2D )
         IF ( IF1D2D .NE. 0 ) THEN
           IF ( N1D2D .EQ. 0 ) THEN
             N1D2D = N1D2D + 1
             CALL JEVEUO(LCHAR(I)//'.CHME.F1D2D.VALE','L',JVAL)
             CALL COPISD('CHAMP_GD','V',LCHAR(I)//'.CHME.F1D2D',CF1D2D)

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0 .AND.(.NOT. FONCI)) THEN
              VCHAR = LCHAR(I)//'.CHME.F1D2D.VALE'
              CALL JEVEUO(VCHAR,'L',IVAL)
              CALL JEVEUO(CF1D2D // '.VALE','E',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 103 IN = 1,NBVALE
                  ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  103         CONTINUE
             ELSEIF (N1. NE. 0 .AND. FONCI) THEN
               IER = IER + 1
               CALL UTDEBM('E',OPER,'ON NE PEUT ASSOCIER DES CHARGE')
               CALL UTIMPK('S','MENTS FONCTION (ICI :',1,LCHAR(I)(1:8))
               CALL UTIMPK('S','AVEC UNE',1,'FONCTION MULTIPLICATRICE')
               CALL UTFINM()
             ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'FORCE_...')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CF1D2D(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F2D3D.DESC', IF2D3D )
         IF ( IF2D3D .NE. 0 ) THEN
           IF ( N2D3D .EQ. 0 ) THEN
             N2D3D = N2D3D +1
             CALL COPISD('CHAMP_GD','V',LCHAR(I)//'.CHME.F2D3D',CF2D3D)

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0 .AND.(.NOT. FONCI)) THEN
              VCHAR = LCHAR(I)//'.CHME.F2D3D.VALE'
              CALL JEVEUO(VCHAR,'L',IVAL)
              CALL JEVEUO(CF2D3D // '.VALE','E',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 104 IN = 1,NBVALE
                  ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  104         CONTINUE
             ELSEIF (N1. NE. 0 .AND. FONCI) THEN
               IER = IER + 1
               CALL UTDEBM('E',OPER,'ON NE PEUT ASSOCIER DES CHARGE')
               CALL UTIMPK('S','MENTS FONCTION (ICI :',1,LCHAR(I)(1:8))
               CALL UTIMPK('S','AVEC UNE',1,'FONCTION MULTIPLICATRICE')
               CALL UTFINM()
             ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'FORCE_...')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CF2D3D(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.PRESS.DESC', IPRESS )
         IF ( IPRESS .NE. 0 ) THEN
           IF ( NPRESS .EQ. 0 ) THEN
             NPRESS = NPRESS + 1
             CALL COPISD('CHAMP_GD','V',LCHAR(I)//'.CHME.PRESS',CHPRES)

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0 .AND.(.NOT. FONCI)) THEN
              VCHAR = LCHAR(I)//'.CHME.PRESS.VALE'
              CALL JEVEUO(VCHAR,'L',IVAL)
              CALL JEVEUO(CHPRES // '.VALE','E',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 105 IN = 1,NBVALE
                  ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  105         CONTINUE
             ELSEIF (N1. NE. 0 .AND. FONCI) THEN
               IER = IER + 1
               CALL UTDEBM('E',OPER,'ON NE PEUT ASSOCIER DES CHARGE')
               CALL UTIMPK('S','MENTS FONCTION (ICI :',1,LCHAR(I)(1:8))
               CALL UTIMPK('S','AVEC UNE',1,'FONCTION MULTIPLICATRICE')
               CALL UTFINM()
             ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'PRESSION')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHPRES(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.EPSIN.DESC', IEPSIN )
         IF ( IEPSIN .NE. 0 ) THEN
           EPSI = .TRUE.
           IF ( NEPSI .EQ. 0 ) THEN
             NEPSI = NEPSI + 1
             CALL COPISD('CHAMP_GD','V',LCHAR(I)//'.CHME.EPSIN',CHEPSI)

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0 .AND.(.NOT. FONCI)) THEN
              VCHAR = LCHAR(I)//'.CHME.EPSIN.VALE'
              CALL JEVEUO(VCHAR,'L',IVAL)
              CALL JEVEUO(CHEPSI // '.VALE','E',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 106 IN = 1,NBVALE
                  ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  106         CONTINUE
             ELSEIF (N1. NE. 0 .AND. FONCI) THEN
               IER = IER + 1
               CALL UTDEBM('E',OPER,'ON NE PEUT ASSOCIER DES CHARGE')
               CALL UTIMPK('S','MENTS FONCTION (ICI :',1,LCHAR(I)(1:8))
               CALL UTIMPK('S','AVEC UNE',1,'FONCTION MULTIPLICATRICE')
               CALL UTFINM()
             ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'EPSI_INIT')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHEPSI(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.PESAN.DESC', IPESA )
         IF ( IPESA .NE. 0 ) THEN
           IF ( NPESA .EQ. 0 ) THEN
             NPESA = NPESA + 1
             CALL COPISD('CHAMP_GD','V',LCHAR(I)//'.CHME.PESAN',CHPESA)

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0 .AND.(.NOT. FONCI)) THEN
              VCHAR = LCHAR(I)//'.CHME.PESAN.VALE'
              CALL JEVEUO(VCHAR,'L',IVAL)
              CALL JEVEUO(CHPESA // '.VALE','E',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 107 IN = 1,NBVALE
                  ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  107         CONTINUE
             ELSEIF (N1. NE. 0 .AND. FONCI) THEN
               IER = IER + 1
               CALL UTDEBM('E',OPER,'ON NE PEUT ASSOCIER DES CHARGE')
               CALL UTIMPK('S','MENTS FONCTION (ICI :',1,LCHAR(I)(1:8))
               CALL UTIMPK('S','AVEC UNE',1,'FONCTION MULTIPLICATRICE')
               CALL UTFINM()
             ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'PESANTEUR')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHPESA(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.ROTAT.DESC', IROTA )
         IF ( IROTA .NE. 0 ) THEN
           IF ( NROTA .EQ. 0 ) THEN
             NROTA = NROTA + 1
             CALL COPISD('CHAMP_GD','V',LCHAR(I)//'.CHME.ROTAT',CHROTA)

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0 .AND.(.NOT. FONCI)) THEN
              VCHAR = LCHAR(I)//'.CHME.ROTAT.VALE'
              CALL JEVEUO(VCHAR,'L',IVAL)
              CALL JEVEUO(CHROTA // '.VALE','E',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 108 IN = 1,NBVALE
                  ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  108         CONTINUE
             ELSEIF (N1. NE. 0 .AND. FONCI) THEN
               IER = IER + 1
               CALL UTDEBM('E',OPER,'ON NE PEUT ASSOCIER DES CHARGE')
               CALL UTIMPK('S','MENTS FONCTION (ICI :',1,LCHAR(I)(1:8))
               CALL UTIMPK('S','AVEC UNE',1,'FONCTION MULTIPLICATRICE')
               CALL UTFINM()
             ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'ROTATION')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHROTA(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
       ENDIF
C
 10   CONTINUE
C
      IF ( IER .NE. 0 ) THEN
         CALL U2MESS('F','CALCULEL2_60')
      ENDIF
C
C  -  SI ABSENCE D'UN CHAMP DE FORCES, CREATION D'UN CHAMP NUL
C
      IF ( NVOLU .EQ. 0 ) THEN
         CALL MEFOR0 ( MODELE, CHVOLU, FONC )
      ENDIF
      IF ( N1D2D .EQ. 0 ) THEN
         CALL MEFOR0 ( MODELE, CF1D2D, FONC )
      ENDIF
      IF ( N2D3D .EQ. 0 ) THEN
         CALL MEFOR0 ( MODELE, CF2D3D, FONC )
      ENDIF
      IF ( NPRESS .EQ. 0 ) THEN
         CALL MEPRES ( MODELE, CHPRES, FONC )
      ENDIF
C
      CALL JEDEMA()
      END
