      SUBROUTINE CNONOR ( NOMO, GRAN, BASE, CNO )
      IMPLICIT NONE
      CHARACTER*1  BASE
      CHARACTER*8  NOMO, GRAN, CNO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C BUT :     COMMANDES :    AFFE_CHAM_NO + CREA_CHAMP/OPERATION:'NORMALE'
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32 JEXNUM, JEXNOM, JEXR8, JEXATR
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER       IBID, IER, NEC, IACMP, IAV, I, IRET, II, INO, JJ,
     +              NCMPMX, NUMGD, NDIM, NBNO, NBNOEU, IDIM, NN, NBMA,
     +              NBCOMP, NBTYP, LONVAL, ICOMP, IC, IEC, IAND, JLMA,
     +              JNUNOE, JNORM, JNNO, JVAL, JNBCA, JDESC
      REAL*8        RBID, VALR(3)
      CHARACTER*2   TYPVAL
      CHARACTER*8   K8B, RESU, NOMA, ZCST,TYPMCL(4),NOCMP(3),LISTYP(10)
      CHARACTER*16  MOTCLF, MOTCLE(2)
      CHARACTER*24  NOMNOE, MESMAI
      CHARACTER*24 VALK(2)
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
      RESU = CNO
C
      CALL JENONU ( JEXNOM('&CATA.GD.NOMGD',GRAN), NUMGD )
      IF (NUMGD.EQ.0) THEN
            VALK (1) = GRAN
        CALL U2MESG('F', 'UTILITAI6_10',1,VALK,0,0,0,0.D0)
      END IF
      CALL DISMOI ( 'F', 'NB_EC', GRAN, 'GRANDEUR', NEC , K8B, IER )
      CALL JEVEUO ( JEXNOM('&CATA.GD.NOMCMP',GRAN), 'L', IACMP )
      CALL JEVEUO ( JEXATR('&CATA.GD.NOMCMP','LONCUM'), 'L', IAV  )
      NCMPMX = ZI(IAV+NUMGD) - ZI(IAV+NUMGD-1)
C
      CALL DISMOI ( 'F', 'NOM_MAILLA', NOMO, 'MODELE', IBID, NOMA, IER )
C
C --- DIMENSION DU PROBLEME
C
      NDIM = 3
      CALL DISMOI ( 'F', 'Z_CST', NOMO, 'MODELE', IBID, ZCST, IER )
      IF (ZCST(1:3).EQ.'OUI')  NDIM = 2
C
C --- DEFINITION DES COMPOSANTES ET DES TYPES DE MAILLE A TRAITER
C
      IF (NDIM.EQ.2) THEN
        NBCOMP = 2
        NOCMP(1) = 'X'
        NOCMP(2) = 'Y'
        NBTYP = 3
        LISTYP(1) = 'SEG2'
        LISTYP(2) = 'SEG3'
        LISTYP(3) = 'SEG4'
      ELSE
        NBCOMP = 3
        NOCMP(1) = 'X'
        NOCMP(2) = 'Y'
        NOCMP(3) = 'Z'
        NBTYP = 10
        LISTYP(1) = 'TRIA3'
        LISTYP(2) = 'TRIA6'
        LISTYP(3) = 'TRIA9'
        LISTYP(4) = 'QUAD4'
        LISTYP(5) = 'QUAD8'
        LISTYP(6) = 'QUAD9'
        LISTYP(7) = 'QUAD12'
        LISTYP(8) = 'SEG2'
        LISTYP(9) = 'SEG3'
        LISTYP(10) = 'SEG4'
      END IF
C
C --- VERIFICATION QUE LES COMPOSANTES APPARTIENNENT A LA GRANDEUR
C
      DO 10 I = 1,NBCOMP
         CALL VERICP ( ZK8(IACMP), NOCMP(I), NCMPMX, IRET )
         IF (IRET.NE.0) THEN
            VALK (1) = GRAN
            VALK (2) = NOCMP(I)
            CALL U2MESG('F', 'UTILITAI6_11',2,VALK,0,0,0,0.D0)
         END IF
 10   CONTINUE
C
C --- LISTE DES MAILLES A TRAITER
C
      MESMAI = '&&CNONOR.MES_MAILLES'
      MOTCLF = ' '
      MOTCLE(1) = 'MAILLE'
      MOTCLE(2) = 'GROUP_MA'
      TYPMCL(1) = 'MAILLE'
      TYPMCL(2) = 'GROUP_MA'
C
      CALL RELIEM ( ' ', NOMA, 'NU_MAILLE', MOTCLF, 1, 2,
     +                                    MOTCLE, TYPMCL, MESMAI, NBMA )
      CALL JEVEUO ( MESMAI, 'L', JLMA )
C
      CALL NBNLMA ( NOMA, NBMA, ZI(JLMA), NBTYP, LISTYP, NBNO )
      CALL JEVEUO ( '&&NBNLMA.LN' , 'L', JNUNOE )
C
C --- DETERMINATION DES NORMALES
C
      CALL CANORT ( NOMA, NBMA, ZI(JLMA), K8B, NDIM, NBNO,
     &                                         ZI(JNUNOE), 1 )
C
      CALL JEVEUO ( '&&CANORT.NORMALE', 'L', JNORM )
C
C-----------------------------------------------------------------------
C
      NOMNOE = NOMA//'.NOMNOE'
      CALL JELIRA ( NOMNOE, 'NOMMAX', NBNOEU, K8B )
      TYPVAL = 'R'
C
C     AFFE DU CHAMP AUX NOEUDS
C     ------------------------
C --- ALLOCATION DE 4 OBJETS INTERMEDIAIRES SERVANT AUX CALCULS
C     DE .PRNO ET .VALE
C
      CALL WKVECT('&&CNONOR.NOMS_NOEUDS','V V K8',NBNOEU,JNNO)
      CALL WKVECT('&&CNONOR.VALCOMPNO'  ,'V V R' ,NBNOEU*NCMPMX,JVAL)
      CALL WKVECT('&&CNONOR.NCMPMX_AFFE','V V I ',NBNOEU,JNBCA)
      CALL WKVECT('&&CNONOR.DESC_NOEUD' ,'V V I' ,NEC*NBNOEU,JDESC)
C
      DO 110 II = 1,NBNO
         INO = ZI(JNUNOE-1+II)
         CALL JENUNO ( JEXNUM(NOMNOE, INO ), ZK8(JNNO+INO-1) )
C
         DO 112 IDIM = 1 , NDIM
            VALR(IDIM) = ZR(JNORM-1+NDIM*(II-1)+IDIM)
 112     CONTINUE
C
         CALL AFFENO ( 1,INO,NOCMP,NBCOMP,ZK8(IACMP),NCMPMX,
     &                 VALR,K8B,ZI(JDESC),ZR(JVAL),K8B,TYPVAL,NEC)
C
 110  CONTINUE

C --- CALCUL DU NOMBRE TOTAL DE CMP AFFECTEES (SOMMEES SUR LES NOEUDS)

      LONVAL = 0
      DO 140 INO = 1,NBNOEU
        ICOMP = 0
        DO 130 IC = 1,NCMPMX
          IEC = (IC-1)/30 + 1
          JJ = IC - 30* (IEC-1)
          II = 2**JJ
          NN = IAND(ZI(JDESC+ (INO-1)*NEC+IEC-1),II)
          IF (NN.GT.0) THEN
            ICOMP = ICOMP + 1
          END IF
  130   CONTINUE
        ZI(JNBCA-1+INO) = ICOMP
        LONVAL = LONVAL + ICOMP
  140 CONTINUE
C
      CALL AFCHNO ( RESU,BASE,GRAN,NOMA,NBNOEU,ZI(JNBCA),ZI(JDESC),
     &                LONVAL,TYPVAL,ZR(JVAL),ZC(JVAL),K8B)
C
      CALL JEDEMA()
      END
