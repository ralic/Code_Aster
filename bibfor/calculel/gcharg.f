      SUBROUTINE GCHARG ( MODELE, NCHAR, LCHAR, CHVOLU, CF1D2D, CF2D3D,
     +                 CHPRES,CHEPSI,CHPESA,CHROTA,FONC,EPSI,TIME,IORD)
      IMPLICIT NONE
      INTEGER         NCHAR, IORD
      CHARACTER*8     MODELE, LCHAR(*)
      CHARACTER*24    CHVOLU,CF1D2D,CF2D3D,CHPRES,CHEPSI,CHPESA,CHROTA
      LOGICAL         FONC, EPSI
      REAL*8  TIME
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/10/2004   AUTEUR LEBOUVIE F.LEBOUVIER 
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
     +              IPRESS, IEPSIN, IROTA, IPESA, IRET, N1, ICHA0, 
     +              NBVALE, IN, JVAL, NEXCI, JPARA, JFCHA, NRES
      CHARACTER*8   K8B, RESU
      CHARACTER*16  TYPE, OPER
      CHARACTER*24  BLANC, VCHAR, VOLU0, C1D2D0, C2D3D0, PRES0,
     +              EPSI0, PESA0, ROTA0, NOMFCT
      CHARACTER*24  EXCISD
      REAL*8  CONST 
      LOGICAL LCHSD  
C     ------------------------------------------------------------------
C
      CALL GETRES ( K8B, TYPE, OPER )
      FONC   = .FALSE.
      EPSI   = .FALSE.
      BLANC = '                        '
      CHVOLU = BLANC
      CF1D2D = BLANC
      CF2D3D = BLANC
      CHPRES = BLANC
      CHEPSI = BLANC
      CHPESA = BLANC
      CHROTA = BLANC
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
         CALL DISMOI('F','TYPE_CHARGE',LCHAR(I),'CHARGE',IBID,K8B,IRET)
         IF ( K8B(5:7) .EQ. '_FO' )  FONC = .TRUE.
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F3D3D.DESC', IF3D3D )
         IF ( IF3D3D .NE. 0 ) THEN
           IF ( CHVOLU .EQ. BLANC ) THEN
             CHVOLU = LCHAR(I) // '.CHME.F3D3D.DESC'

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0) THEN
              VCHAR = LCHAR(I) // '.CHME.F3D3D.VALE'
              CALL JEVEUO(VCHAR,'L',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL JEEXIN(LCHAR(I) // '.F3D3D.INIT',IRET)
              IF (IRET .EQ. 0) THEN
                VOLU0 = LCHAR(I) // '.F3D3D.INIT'
                CALL WKVECT(VOLU0,'V V R',NBVALE,ICHA0)
                DO 101 IN = 1,NBVALE
                  ZR(ICHA0+IN-1)  = ZR(JVAL +IN-1)
  101           CONTINUE
              ELSE
                VOLU0 = LCHAR(I) // '.F3D3D.INIT'
                CALL JEVEUO(VOLU0,'L',ICHA0)
              ENDIF  
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 102 IN = 1,NBVALE
                ZR(JVAL+IN-1)  = CONST* ZR(ICHA0 +IN-1)
  102         CONTINUE
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
           IF ( CHVOLU .EQ. BLANC ) THEN
             CHVOLU = LCHAR(I)//'.CHME.F2D2D.DESC'
             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0) THEN
              VCHAR = LCHAR(I) // '.CHME.F2D2D.VALE'
              CALL JEVEUO(VCHAR,'L',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL JEEXIN(LCHAR(I) // '.F2D2D.INIT',IRET)
              IF (IRET .EQ. 0) THEN
                VOLU0 = LCHAR(I) // '.F2D2D.INIT'
                CALL WKVECT(VOLU0,'V V R',NBVALE,ICHA0)
                DO 103 IN = 1,NBVALE
                  ZR(ICHA0+IN-1)  = ZR(JVAL +IN-1)
  103           CONTINUE
              ELSE
                VOLU0 = LCHAR(I) // '.F2D2D.INIT'
                CALL JEVEUO(VOLU0,'L',ICHA0)
              ENDIF  
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 104 IN = 1,NBVALE
                ZR(JVAL+IN-1)  = CONST* ZR(ICHA0 +IN-1)
  104         CONTINUE
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
           IF ( CF1D2D .EQ. BLANC ) THEN
             CF1D2D = LCHAR(I)//'.CHME.F1D2D.DESC'

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0) THEN
              VCHAR = LCHAR(I) // '.CHME.F1D2D.VALE'
              CALL JEVEUO(VCHAR,'L',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL JEEXIN(LCHAR(I) // '.F1D2D.INIT',IRET)
              IF (IRET .EQ. 0) THEN
                C1D2D0 = LCHAR(I) // '.F1D2D.INIT'
                CALL WKVECT(C1D2D0,'V V R',NBVALE,ICHA0)
                DO 105 IN = 1,NBVALE
                  ZR(ICHA0+IN-1)  = ZR(JVAL +IN-1)
  105           CONTINUE
              ELSE
                C1D2D0 = LCHAR(I) // '.F1D2D.INIT'
                CALL JEVEUO(C1D2D0,'L',ICHA0)
              ENDIF  
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 106 IN = 1,NBVALE
                ZR(JVAL+IN-1)  = CONST* ZR(ICHA0 +IN-1)
  106         CONTINUE
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
           IF ( CF2D3D .EQ. BLANC ) THEN
             CF2D3D = LCHAR(I)//'.CHME.F2D3D.DESC'

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0) THEN
              VCHAR = LCHAR(I) // '.CHME.F2D3D.VALE'
              CALL JEVEUO(VCHAR,'L',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL JEEXIN(LCHAR(I) // '.F2D3D.INIT',IRET)
              IF (IRET .EQ. 0) THEN
                C2D3D0 = LCHAR(I) // '.F2D3D.INIT'
                CALL WKVECT(C2D3D0,'V V R',NBVALE,ICHA0)
                DO 107 IN = 1,NBVALE
                  ZR(ICHA0+IN-1)  = ZR(JVAL +IN-1)
  107           CONTINUE
              ELSE
                C2D3D0 = LCHAR(I) // '.F2D3D.INIT'
                CALL JEVEUO(C2D3D0,'L',ICHA0)
              ENDIF  
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 108 IN = 1,NBVALE
                ZR(JVAL+IN-1)  = CONST* ZR(ICHA0 +IN-1)
  108         CONTINUE
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
           IF ( CHPRES .EQ. BLANC ) THEN
             CHPRES = LCHAR(I)//'.CHME.PRESS.DESC'

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF
             IF (N1. NE. 0) THEN
              VCHAR = LCHAR(I) // '.CHME.PRESS.VALE'
              CALL JEVEUO(VCHAR,'L',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL JEEXIN(LCHAR(I) // '.PRESS.INIT',IRET)
              IF (IRET .EQ. 0) THEN
                PRES0 = LCHAR(I) // '.PRESS.INIT'
                CALL WKVECT(PRES0,'V V R',NBVALE,ICHA0)
                DO 109 IN = 1,NBVALE
                  ZR(ICHA0+IN-1)  = ZR(JVAL +IN-1)
  109           CONTINUE
              ELSE
                PRES0 = LCHAR(I) // '.PRESS.INIT'
                CALL JEVEUO(PRES0,'L',ICHA0)
              ENDIF  
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 110 IN = 1,NBVALE
                ZR(JVAL+IN-1)  = CONST* ZR(ICHA0 +IN-1)
  110         CONTINUE
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
           IF ( CHEPSI .EQ. BLANC ) THEN
             CHEPSI = LCHAR(I)//'.CHME.EPSIN.DESC'

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0) THEN
              VCHAR = LCHAR(I) // '.CHME.EPSIN.VALE'
              CALL JEVEUO(VCHAR,'L',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL JEEXIN(LCHAR(I) // '.EPSIN.INIT',IRET)
              IF (IRET .EQ. 0) THEN
                EPSI0 = LCHAR(I) // '.EPSIN.INIT'
                CALL WKVECT(EPSI0,'V V R',NBVALE,ICHA0)
                DO 111 IN = 1,NBVALE
                  ZR(ICHA0+IN-1)  = ZR(JVAL +IN-1)
  111           CONTINUE
              ELSE
                EPSI0 = LCHAR(I) // '.EPSIN.INIT'
                CALL JEVEUO(EPSI0,'L',ICHA0)
              ENDIF  
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 112 IN = 1,NBVALE
                ZR(JVAL+IN-1)  = CONST* ZR(ICHA0 +IN-1)
  112         CONTINUE
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
           IF ( CHPESA .EQ. BLANC ) THEN
             CHPESA = LCHAR(I)//'.CHME.PESAN.DESC'

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0) THEN
              VCHAR = LCHAR(I) // '.CHME.PESAN.VALE'
              CALL JEVEUO(VCHAR,'L',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL JEEXIN(LCHAR(I) // '.PESAN.INIT',IRET)
              IF (IRET .EQ. 0) THEN
                PESA0 = LCHAR(I) // '.PESAN.INIT'
                CALL WKVECT(PESA0,'V V R',NBVALE,ICHA0)
                DO 113 IN = 1,NBVALE
                  ZR(ICHA0+IN-1)  = ZR(JVAL +IN-1)
  113           CONTINUE
              ELSE
                PESA0 = LCHAR(I) // '.PESAN.INIT'
                CALL JEVEUO(PESA0,'L',ICHA0)
              ENDIF  
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 114 IN = 1,NBVALE
                ZR(JVAL+IN-1)  = CONST* ZR(ICHA0 +IN-1)
  114         CONTINUE
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
           IF ( CHROTA .EQ. BLANC ) THEN
             CHROTA = LCHAR(I)//'.CHME.ROTAT.DESC'

             IF(LCHSD) THEN
                N1 = 1
                NOMFCT = ZK24(JFCHA-1+I)
                IF(NOMFCT(1:2).EQ.'&&') N1 = 0
             ELSE
                CALL GETVID('EXCIT','FONC_MULT',I,1,1,NOMFCT,N1)
             ENDIF

             IF (N1. NE. 0) THEN
              VCHAR = LCHAR(I) // '.CHME.ROTAT.VALE'
              CALL JEVEUO(VCHAR,'L',JVAL)
              CALL JELIRA(VCHAR  ,'LONMAX',NBVALE,K8B)
              CALL JEEXIN(LCHAR(I) // '.ROTAT.INIT',IRET)
              IF (IRET .EQ. 0) THEN
                ROTA0 = LCHAR(I) // '.ROTAT.INIT'
                CALL WKVECT(ROTA0,'V V R',NBVALE,ICHA0)
                DO 115 IN = 1,NBVALE
                  ZR(ICHA0+IN-1)  = ZR(JVAL +IN-1)
  115           CONTINUE
              ELSE
                ROTA0 = LCHAR(I) // '.ROTAT.INIT'
                CALL JEVEUO(ROTA0,'L',ICHA0)
              ENDIF  
              CALL FOINTE('A',NOMFCT, 1,'INST',TIME,CONST,IRET)
              DO 116 IN = 1,NBVALE
                ZR(JVAL+IN-1)  = CONST* ZR(ICHA0 +IN-1)
  116         CONTINUE
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
C
 10   CONTINUE
C
      IF ( IER .NE. 0 ) THEN
         CALL UTMESS('F',OPER,'******* ERREUR DONNEES *******')
      ENDIF
C
C  -  SI ABSENCE D'UN CHAMP DE FORCES, CREATION D'UN CHAMP NUL
C
      IF ( CHVOLU .EQ. BLANC ) THEN
         CALL MEFOR0 ( MODELE, CHVOLU, FONC )
      ENDIF
      IF ( CF1D2D .EQ. BLANC ) THEN
         CALL MEFOR1 ( MODELE, CF1D2D, FONC )
      ENDIF
      IF ( CF2D3D .EQ. BLANC ) THEN
         CALL MEFOR0 ( MODELE, CF2D3D, FONC )
      ENDIF
      IF ( CHPRES .EQ. BLANC ) THEN
         CALL MEPRES ( MODELE, CHPRES, FONC )
      ENDIF
C
      END
