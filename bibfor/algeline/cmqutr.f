      SUBROUTINE CMQUTR ( BASZ, NOMAIN, NOMAOU, NBMA, NUMMAI, PREFIX,
     +                    NDINIT )
      IMPLICIT   NONE
      INTEGER             NBMA, NUMMAI(*), NDINIT
      CHARACTER*8         NOMAIN, NOMAOU, PREFIX
      CHARACTER*(*)       BASZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 24/06/2003   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPTION = 'QUAD_TRIA3' 
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      I, IMA, NBMAT, NBMAIL, TYPTRI, NBTRI, IRET, NBGRNO,
     +             NBNOMX, NBPT, INO, IMA2, IMAV, IATYMA, JREFE, JVG,
     +             JTYPM, JDIME, JOPT, JNPT, NBNO, IER, JGG, IM, J, 
     +             LGPREF, LXLGUT, LGND, NBGRMA, NBMAG, NBGRM, JGROUP,
     +             IFM, NIV, IQ4, IQ8, IQ9
      INTEGER      TQUA4(2,3), TQUA8(6,3), TQUA9(6,3)
      LOGICAL      LOGIC
      CHARACTER*1  K1B, BASE
      CHARACTER*8  K8B, NOMG, TYPM
      CHARACTER*16 KNUME
      CHARACTER*24 NOMMAI, TYPMAI, CONNEX, NODIME, NOMNOE, GRPNOE,
     &             COOVAL, COODSC, COOREF, GRPMAI
      CHARACTER*24 TYPMAV, CONNEV, NODIMV, NOMNOV, GRPNOV, 
     &             COOVAV, COODSV, COOREV, NOMMAV, GRPMAV
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- ECLATEMENT QUAD4 EN 2 TRIA3
      TQUA4(1,1) = 1
      TQUA4(1,2) = 2
      TQUA4(1,3) = 3
      TQUA4(2,1) = 1
      TQUA4(2,2) = 3
      TQUA4(2,3) = 4
C
C --- ECLATEMENT QUAD8 EN 6 TRIA3
      TQUA8(1,1) = 1
      TQUA8(1,2) = 5
      TQUA8(1,3) = 8
      TQUA8(2,1) = 5
      TQUA8(2,2) = 2
      TQUA8(2,3) = 6
      TQUA8(3,1) = 6
      TQUA8(3,2) = 3
      TQUA8(3,3) = 7
      TQUA8(4,1) = 7
      TQUA8(4,2) = 4
      TQUA8(4,3) = 8
      TQUA8(5,1) = 5
      TQUA8(5,2) = 7
      TQUA8(5,3) = 8
      TQUA8(6,1) = 5
      TQUA8(6,2) = 6
      TQUA8(6,3) = 7
C
C --- ECLATEMENT QUAD9 EN 6 TRIA3 COMME QUAD8 
C --- CAR DX DY DZ INCONNUS SUR LE NOEUD 9 EN COQUE_3D
      TQUA9(1,1) = 1
      TQUA9(1,2) = 5
      TQUA9(1,3) = 8
      TQUA9(2,1) = 5
      TQUA9(2,2) = 2
      TQUA9(2,3) = 6
      TQUA9(3,1) = 6
      TQUA9(3,2) = 3
      TQUA9(3,3) = 7
      TQUA9(4,1) = 7
      TQUA9(4,2) = 4
      TQUA9(4,3) = 8
      TQUA9(5,1) = 5
      TQUA9(5,2) = 7
      TQUA9(5,3) = 8
      TQUA9(6,1) = 5
      TQUA9(6,2) = 6
      TQUA9(6,3) = 7
C
      BASE = BASZ
C
      NOMMAV = NOMAIN//'.NOMMAI         '
      NOMNOV = NOMAIN//'.NOMNOE         '
      TYPMAV = NOMAIN//'.TYPMAIL        '
      CONNEV = NOMAIN//'.CONNEX         '
      GRPNOV = NOMAIN//'.GROUPENO       '
      GRPMAV = NOMAIN//'.GROUPEMA       '
      NODIMV = NOMAIN//'.DIME           '
      COOVAV = NOMAIN//'.COORDO    .VALE'
      COODSV = NOMAIN//'.COORDO    .DESC'
      COOREV = NOMAIN//'.COORDO    .REFE'
C
      NOMMAI = NOMAOU//'.NOMMAI         '
      NOMNOE = NOMAOU//'.NOMNOE         '
      TYPMAI = NOMAOU//'.TYPMAIL        '
      CONNEX = NOMAOU//'.CONNEX         '
      GRPNOE = NOMAOU//'.GROUPENO       '
      GRPMAI = NOMAOU//'.GROUPEMA       '
      NODIME = NOMAOU//'.DIME           '
      COOVAL = NOMAOU//'.COORDO    .VALE'
      COODSC = NOMAOU//'.COORDO    .DESC'
      COOREF = NOMAOU//'.COORDO    .REFE'
C
      CALL JEVEUO ( TYPMAV, 'L', JTYPM )
      CALL JEVEUO ( NODIMV, 'L', JDIME )
      NBMAT = ZI(JDIME+3-1)
C
      LOGIC = .FALSE.
      NBTRI = 0
C
      IQ4 = 0
      IQ8 = 0
      IQ9 = 0
      NBMAIL = NBMAT
      DO 10 IM = 1 , NBMA
         IMA = NUMMAI(IM)
C
         CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM+IMA-1)),TYPM)
C
         IF (TYPM .EQ. 'QUAD4') THEN
            NBMAIL =  NBMAIL- 1
            NBTRI = NBTRI + 2
            IQ4 = IQ4 + 1
C
         ELSE IF (TYPM .EQ. 'QUAD8') THEN
            NBMAIL =  NBMAIL- 1
            NBTRI = NBTRI + 6
            IQ8 = IQ8 + 1
C
         ELSE IF (TYPM .EQ. 'QUAD9') THEN
            NBMAIL =  NBMAIL- 1
            NBTRI = NBTRI + 6
            IQ9 = IQ9 + 1
         ENDIF
 10   CONTINUE
C
      IF ( NIV .GE. 1 ) THEN
         WRITE(IFM,1000) 1
         IF ( IQ4 .NE. 0 ) WRITE(IFM,1002) IQ4, 'QUAD4', 2*IQ4, 'TRIA3'
         IF ( IQ8 .NE. 0 ) WRITE(IFM,1002) IQ8, 'QUAD8', 6*IQ8, 'TRIA3'
         IF ( IQ9 .NE. 0 ) WRITE(IFM,1002) IQ9, 'QUAD9', 6*IQ9, 'TRIA3'
      ENDIF
C
      NBMAIL = NBMAIL + NBTRI
C
      CALL JEDUPO ( NODIMV, BASE, NODIME, LOGIC )
      CALL JEDUPO ( NOMNOV, BASE, NOMNOE, LOGIC )
      CALL JEDUPO ( COOVAV, BASE, COOVAL, LOGIC )
      CALL JEDUPO ( COODSV, BASE, COODSC, LOGIC )
      CALL JEDUPO ( COOREV, BASE, COOREF, LOGIC )
C
      CALL JEVEUO ( COOREF, 'E', JREFE )
      ZK24(JREFE) = NOMAOU
C
      CALL JEVEUO ( NODIME, 'E', JDIME )
      ZI(JDIME+3-1) = NBMAIL

C ----------------------------------------------------------------------
C     LE '.NOMMAI' ET LE '.CONNEX'
C ----------------------------------------------------------------------

      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'TRIA3'  ), TYPTRI )

      CALL JECREO ( NOMMAI, BASE//' N K8' )
      CALL JEECRA ( NOMMAI, 'NOMMAX', NBMAIL, ' ' )

      CALL WKVECT ( TYPMAI, BASE//' V I', NBMAIL, IATYMA )

C     NBNOMX = NBRE DE NOEUDS MAX. POUR UNE MAILLE :
      CALL DISMOI('F','NB_NO_MAX','&CATA','CATALOGUE',NBNOMX,K1B,IER)

      CALL JECREC ( CONNEX, BASE//' V I', 'NU', 'CONTIG', 'VARIABLE',
     +                                                    NBMAIL )
      CALL JEECRA ( CONNEX, 'LONT', NBNOMX*NBMAIL, ' ' )
C
      LGPREF = LXLGUT(PREFIX)
      IMAV  = NDINIT - 1
C
      DO 100 IMA = 1 , NBMAT
C
         CALL JENUNO ( JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM+IMA-1)), TYPM )
         CALL JEVEUO ( JEXNUM(CONNEV,IMA), 'L', JOPT )
         CALL JELIRA ( JEXNUM(CONNEV,IMA), 'LONMAX', NBPT, K1B )
C
         DO 210 IM = 1 , NBMA
            IF ( IMA .EQ. NUMMAI(IM) ) GOTO 300
 210     CONTINUE
C
         CALL JENUNO ( JEXNUM(NOMMAV,IMA), NOMG )
         CALL JEEXIN ( JEXNOM(NOMMAI,NOMG), IRET )
         IF (IRET.EQ.0) THEN
            CALL JECROC(JEXNOM(NOMMAI,NOMG))
         ELSE
            CALL UTDEBM('F','CMQUTR','ERREUR DONNEES')
            CALL UTIMPK('L','MAILLE DEJA EXISTANTE : ',1,NOMG)
            CALL UTFINM
         END IF

         CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
         ZI(IATYMA-1+IMA2) = ZI(JTYPM+IMA-1)

         CALL JEECRA(JEXNUM(CONNEX,IMA2),'LONMAX',NBPT,K8B)
         CALL JEVEUO(JEXNUM(CONNEX,IMA2),'E',JNPT)
         DO 140 INO = 1 , NBPT
            ZI(JNPT-1+INO) = ZI(JOPT+INO-1)
 140     CONTINUE
         GOTO 100
C
 300     CONTINUE
         IF ( TYPM .EQ. 'QUAD4' ) THEN
C             -----------------
            NBPT = 3
            DO 110 I = 1 , 2
               IMAV = IMAV + 1
               CALL CODENT ( IMAV, 'G', KNUME )
               LGND = LXLGUT(KNUME)
               IF (LGND+LGPREF.GT.8) CALL UTMESS('F','CMQUTR',
     &         'PREF_MAILLE EST TROP LONG OU PREF_NUME EST TROP GRAND')
               NOMG = PREFIX(1:LGPREF)//KNUME
               IF (IRET.EQ.0) THEN
                   CALL JECROC(JEXNOM(NOMMAI,NOMG))
               ELSE
                  CALL UTDEBM('F','CMQUTR','ERREUR DONNEES')
                  CALL UTIMPK('L','MAILLE DEJA EXISTANTE : ',1,NOMG)
                  CALL UTFINM
               END IF
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IATYMA-1+IMA2) = TYPTRI
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 112 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TQUA4(I,INO))
 112           CONTINUE
 110        CONTINUE
C
         ELSEIF ( TYPM .EQ. 'QUAD8' ) THEN
C                 -----------------
            NBPT = 3
            DO 120 I = 1 , 6
               IMAV = IMAV + 1
               CALL CODENT ( IMAV, 'G', KNUME )
               LGND = LXLGUT(KNUME)
               IF (LGND+LGPREF.GT.8) CALL UTMESS('F','CMQUTR',
     &         'PREF_MAILLE EST TROP LONG OU PREF_NUME EST TROP GRAND')
               NOMG = PREFIX(1:LGPREF)//KNUME
               IF (IRET.EQ.0) THEN
                   CALL JECROC(JEXNOM(NOMMAI,NOMG))
               ELSE
                  CALL UTDEBM('F','CMQUTR','ERREUR DONNEES')
                  CALL UTIMPK('L','MAILLE DEJA EXISTANTE : ',1,NOMG)
                  CALL UTFINM
               END IF
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IATYMA-1+IMA2) = TYPTRI
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 122 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TQUA8(I,INO))
 122           CONTINUE
 120        CONTINUE
C
         ELSEIF ( TYPM .EQ. 'QUAD9' ) THEN
C                 -----------------
            NBPT = 3
            DO 130 I = 1 , 6
               IMAV = IMAV + 1
               CALL CODENT ( IMAV, 'G', KNUME )
               LGND = LXLGUT(KNUME)
               IF (LGND+LGPREF.GT.8) CALL UTMESS('F','CMQUTR',
     &         'PREF_MAILLE EST TROP LONG OU PREF_NUME EST TROP GRAND')
               NOMG = PREFIX(1:LGPREF)//KNUME
               CALL JEEXIN ( JEXNOM(NOMMAI,NOMG), IRET )
               IF (IRET.EQ.0) THEN
                   CALL JECROC(JEXNOM(NOMMAI,NOMG))
               ELSE
                  CALL UTDEBM('F','CMQUTR','ERREUR DONNEES')
                  CALL UTIMPK('L','MAILLE DEJA EXISTANTE : ',1,NOMG)
                  CALL UTFINM
               END IF
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IATYMA-1+IMA2) = TYPTRI
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 132 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TQUA9(I,INO))
 132           CONTINUE
 130        CONTINUE
C
         ENDIF

 100  CONTINUE
C
C ----------------------------------------------------------------------
C     LE '.GROUPENO'
C ----------------------------------------------------------------------
C
      CALL JEEXIN ( GRPNOV, IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL JELIRA ( GRPNOV, 'NOMUTI', NBGRNO, K1B )
         CALL JECREC ( GRPNOE, BASE//' V I', 'NO', 'DISPERSE',
     +                 'VARIABLE', NBGRNO )
         DO 20 I = 1,NBGRNO
            CALL JENUNO ( JEXNUM(GRPNOV,I), NOMG )
            CALL JEVEUO ( JEXNUM(GRPNOV,I), 'L', JVG )
            CALL JELIRA ( JEXNUM(GRPNOV,I), 'LONMAX', NBNO, K1B )
            CALL JEEXIN ( JEXNOM(GRPNOE,NOMG), IRET )
            IF (IRET.EQ.0) THEN
               CALL JECROC ( JEXNOM( GRPNOE, NOMG ) )
            ELSE
               CALL UTDEBM('F','CMQUTR','ERREUR DONNEES')
               CALL UTIMPK('L','GROUP_NO DEJA EXISTANT : ',1,NOMG)
               CALL UTFINM
            END IF
            CALL JEECRA(JEXNOM(GRPNOE,NOMG),'LONMAX',NBNO,' ')
            CALL JEVEUO(JEXNOM(GRPNOE,NOMG),'E',JGG)
            DO 22 J = 0,NBNO - 1
               ZI(JGG+J) = ZI(JVG+J)
  22        CONTINUE
  20     CONTINUE
      END IF
C
C ----------------------------------------------------------------------
C     LE '.GROUPEMA' : ON ELIMINE LES GROUP_MA CONTENANT NUMMAI(I)
C ----------------------------------------------------------------------
C
      CALL JEEXIN ( GRPMAV, IRET )
      IF ( IRET .NE. 0 ) THEN
         NBGRMA = 0
         CALL JELIRA ( GRPMAV, 'NOMUTI', NBGRM, K1B )
         CALL WKVECT ( '&&CMQUTR.GROUP_MA', 'V V I', NBGRM, JGROUP )
         DO 30 I = 1,NBGRM
            ZI(JGROUP+I-1) = 0
            CALL JEVEUO ( JEXNUM(GRPMAV,I), 'L', JVG )
            CALL JELIRA ( JEXNUM(GRPMAV,I), 'LONMAX', NBMAG, K1B )
            DO 32 J = 0,NBMAG - 1
               IMA = ZI(JVG+J)
               DO 200 IM = 1 , NBMA
                  IF ( IMA .EQ. NUMMAI(IM) ) GOTO 30
 200           CONTINUE
  32        CONTINUE
            NBGRMA = NBGRMA + 1
            ZI(JGROUP+I-1) = 1
  30     CONTINUE

         IF ( NBGRMA .NE. 0 ) THEN
         CALL JECREC ( GRPMAI, BASE//' V I', 'NO', 'DISPERSE',
     +                 'VARIABLE', NBGRMA )
         DO 34 I = 1,NBGRM
            IF ( ZI(JGROUP+I-1) .EQ. 0 ) GOTO 34
            CALL JENUNO ( JEXNUM(GRPMAV,I), NOMG )
            CALL JEVEUO ( JEXNUM(GRPMAV,I), 'L', JVG )
            CALL JELIRA ( JEXNUM(GRPMAV,I), 'LONMAX', NBMAG, K1B )
            CALL JEEXIN ( JEXNOM(GRPMAI,NOMG), IRET )
            IF (IRET.EQ.0) THEN
               CALL JECROC ( JEXNOM( GRPMAI, NOMG ) )
            ELSE
               CALL UTDEBM('F','CMQUTR','ERREUR DONNEES')
               CALL UTIMPK('L','GROUP_MA DEJA EXISTANT : ',1,NOMG)
               CALL UTFINM
            END IF
            CALL JEECRA(JEXNOM(GRPMAI,NOMG),'LONMAX',NBMAG,' ')
            CALL JEVEUO(JEXNOM(GRPMAI,NOMG),'E',JGG)
            DO 36 J = 0,NBMAG - 1
               CALL JENUNO ( JEXNUM(NOMMAV,ZI(JVG+J)), NOMG )
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), ZI(JGG+J) )
  36        CONTINUE
  34     CONTINUE
         END IF
         CALL JEDETR ( '&&CMQUTR.GROUP_MA' )
      END IF
C
 1000 FORMAT('MOT CLE FACTEUR "MODI_MAILLE", OCCURRENCE ',I4)
 1002 FORMAT('  MODIFICATION DE ',I6,' MAILLES ',A8,
     +                     ' EN ',I6,' MAILLES ',A8)
C
      CALL JEDETR ( '&&CMQUTR.NUME_MAILLE' )
C
      CALL JEDEMA()
C
      END
