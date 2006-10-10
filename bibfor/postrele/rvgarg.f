      SUBROUTINE RVGARG ( NXDNOM, NXDNUM, NVCHEF, NVCODO, NXDVAR )
      IMPLICIT   NONE
C
      CHARACTER*24 NXDNOM,NXDNUM,NVCHEF,NVCODO, NXDVAR
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 10/10/2006   AUTEUR MCOURTOI M.COURTOIS 
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
C     ------------------------------------------------------------------
C      SAISIE ET PREPA VERIF COHERENCE DES ARGUMENTS D 'APPEL DE OP0051
C          1. VERIFICATION D' EXISTENCE DES OJB MIS EN JEU
C          2. VERIFICATION DE LEGALITE DES CMP MISES EN JEU
C          3. VERIFICATION DE COHERENCE SUR LES MAILLAGES SOUS-JACENTS
C     CES VERIFICATIONS SERONT EFFECTUEES PAR RVCOHE
C     SAISIE DES CMP MISES EN JEU ET DES OPERATION DEMANDEE
C     ------------------------------------------------------------------
C OUT NXDNOM : K : XD V K8 NOM DES CMP UTILISATEURS (OC(I) = OCCUR(I))
C OUT NXDNUM : K : XD V K8 NUM DES CMP UTILISATEURS (OC(I) = OCCUR(I))
C OUT NVCHEF : K : S V K24 NOM D' UN CHAMP EFFECTIFS(TYPE CHAM_GD)
C OUT NVCODO : K : S V I   CODE OPERATION POST
C     ------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      CHARACTER*80 TEXT80,TEXT1
      CHARACTER*24 NAUX24,KORDRE, NOMOBJ
      CHARACTER*19 NCHP19
      CHARACTER*16 NCHSYM
      CHARACTER*8  K8B, NRESU,NCHGD, GRANCH,NOMCP(50)
      CHARACTER*4  TYPECH
      CHARACTER*1  K1BID
      LOGICAL      EXISTE
      INTEGER      ANOMCP,ANUMCP,ANCPU1,ANCPU2,ADESC,ACPGD,AVCHEF
      INTEGER      N1,N2,I,IOCC,GD,N3,ADR,NBELP,NBINV,IBID,IE,AVCODO
      INTEGER      NBPOST,NBCHGD,NBCPGD,NBCMP,NBRESU,NBTCP,NBSOM
      INTEGER      IFR,IUNIFI,J,JORDR,JXVAR,N4,NBC,NNC,NBNC,NUMECP(50)
      REAL*8       RBID
      COMPLEX*16   CBID
C
C=================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      IFR = IUNIFI('RESULTAT')
C
      RBID = 1.0D-6
      CBID = DCMPLX(RBID,RBID)
      CALL GETFAC ( 'ACTION', NBPOST )
      CALL JECREC(NXDNOM,'V V K8','NU','DISPERSE','VARIABLE',NBPOST)
      CALL JECREC(NXDVAR,'V V I ','NU','DISPERSE','VARIABLE',NBPOST)
      CALL JECREC(NXDNUM,'V V I ','NU','DISPERSE','VARIABLE',NBPOST)
      CALL WKVECT(NVCHEF,'V V K24',NBPOST,AVCHEF)
      CALL WKVECT(NVCODO,'V V I',NBPOST,AVCODO)
      KORDRE = '&&RVGARG.NUMEORDR'
      DO 100, IOCC = 1, NBPOST, 1
         N1 = 0
         N2 = 0
         CALL GETVTX('ACTION','OPERATION',IOCC,1,0,K8B,N3)
         N3 = -N3
         CALL WKVECT('&&RVGARG.NOM.OPERATION','V V K80',N3,ADR)
         CALL GETVTX('ACTION','OPERATION',IOCC,1,N3,ZK80(ADR),N4)
         IF ( N3 .EQ. 1 ) THEN
            TEXT1 = ZK80(ADR + 1-1)
            IF ( TEXT1(1:1) .EQ. 'E' ) THEN
               ZI(AVCODO + IOCC-1) = 1
            ELSE
               ZI(AVCODO + IOCC-1) = 3
            ENDIF
         ELSE
               ZI(AVCODO + IOCC-1) = 2
         ENDIF
         CALL JEDETR('&&RVGARG.NOM.OPERATION')
         CALL GETVID('ACTION','RESULTAT',IOCC,1,0,K8B,NBRESU)
         CALL GETVID('ACTION','CHAM_GD' ,IOCC,1,0,K8B,NBCHGD)
         NBRESU = -NBRESU
         NBCHGD = -NBCHGD
         IF ( NBRESU .NE. 0 ) THEN
C        /* CAS D' UN RESULTAT COMPOSE */
            CALL GETVID('ACTION','RESULTAT',IOCC,1,1,NRESU,N1)
            CALL GETVTX('ACTION','NOM_CHAM',IOCC,1,1,TEXT80,N1)
            NCHSYM = TEXT80(1:16)
            CALL JENONU(JEXNOM(NRESU//'           .DESC',NCHSYM),N1)
            IF ( N1 .NE. 0 ) THEN
C           /* LE CHAMP SYMBOLIQUE EXISTE (POTENTIELLEMENT)*/
               CALL RSORAC(NRESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,
     &                    'RELATIF',N3,1,IBID)
               IF ( N3 .GT. 0 ) THEN
                   CALL WKVECT(KORDRE,'V V I',N3,JORDR)
                   CALL RSORAC(NRESU,'TOUT_ORDRE',IBID,RBID,K8B,CBID,
     &                        RBID,'RELATIF',ZI(JORDR),N3,IBID)
                   DO 10 J = 1 , N3
                     CALL RSEXCH(NRESU,NCHSYM,ZI(JORDR+J-1),NAUX24,N2)
                     IF ( N2 .EQ. 0 ) GOTO 12
 10                CONTINUE
 12                CONTINUE
                   CALL JEDETR(KORDRE)
               ELSE
                  N2 = 1
               ENDIF
            ELSE
C           /* LE CHAMP SYMBOLIQUE N' EXISTE PAS */
               N2 = 1
               WRITE(IFR,*)'CHAMP SYMBOLIQUE >',NCHSYM,'< NON '//
     &                      'AUTORISE POUR LE RESULTAT >',NRESU,'<'
               WRITE(IFR,*)'LES CHAMPS SYMBOLIQUES AUTORISES SONT :'
              CALL JEIMPO(IFR,NRESU//'           .DESC',' ',' ')
            ENDIF
            IF ( (N1 .EQ. 0) .OR. (N2 .NE. 0) )THEN
C           /* ALTERNATIVE :                              */
C           /* LE CHAMPS SYMBOLIQUE EST ILEGAL OU         */
C           /* AUCUN CHAMP EFFECTIF ASSOCIE N' A ETE CREE */
               EXISTE = .FALSE.
            ELSE
               EXISTE = .TRUE.
               NCHP19 = NAUX24(1:19)
            ENDIF
         ELSE
C        /* CAS D'UN CHAMP_GD */
            CALL GETVID('ACTION','CHAM_GD',IOCC,1,1,NCHGD,N1)
            EXISTE = .TRUE.
            NCHP19 = NCHGD//'           '
         ENDIF
         CALL JECROC(JEXNUM(NXDNOM,IOCC))
         CALL JECROC(JEXNUM(NXDNUM,IOCC))
C
         IF ( .NOT. EXISTE ) THEN
C        /* LE CHAMPS SYMBOLIQUE EST ILEGAL, OU                 */
C        /* IL EST LEGAL, MAIS IL N' ADMET AUCUN CHAMP EFFECTIF */
            CALL JEECRA(JEXNUM(NXDNOM,IOCC),'LONMAX',1,' ')
            CALL JEVEUO(JEXNUM(NXDNOM,IOCC),'E',ANOMCP)
            ZK8(ANOMCP) = '&NOEXIST'
            CALL JEECRA(JEXNUM(NXDNUM,IOCC),'LONMAX',1,' ')
            CALL JEVEUO(JEXNUM(NXDNUM,IOCC),'E',ANUMCP)
            ZI(ANUMCP) = 0
            ZK24(AVCHEF + IOCC-1) = '&NONEXISTEOUNONCREE     '
         ELSE
            ZK24(AVCHEF + IOCC-1) = NCHP19//'     '
            CALL DISMOI('F','TYPE_CHAMP',NCHP19,'CHAMP',IBID,TYPECH,IE)
            CALL DISMOI('F','NOM_GD'    ,NCHP19,'CHAMP',IBID,GRANCH,IE)
            CALL JEEXIN(NCHP19//'.DESC',IBID)
            IF (IBID.GT.0) THEN
              CALL JEVEUO(NCHP19//'.DESC','L',ADESC)
            ELSE
              CALL JEVEUO(NCHP19//'.CELD','L',ADESC)
            END IF

            GD = ZI(ADESC + 1-1)
            CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',
     &                  NBCPGD,K1BID)
            CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',ACPGD)
            CALL GETVTX('ACTION','NOM_CMP'        ,IOCC,1,0,K8B,NBCMP)
            CALL GETVTX('ACTION','TOUT_CMP'       ,IOCC,1,0,K8B,NBTCP)
            CALL GETVTX('ACTION','INVARIANT'      ,IOCC,1,0,K8B,NBINV)
            CALL GETVTX('ACTION','ELEM_PRINCIPAUX',IOCC,1,0,K8B,NBELP)
            CALL GETVTX('ACTION','RESULTANTE'     ,IOCC,1,0,K8B,NBSOM)
            NBCMP = -NBCMP
            NBTCP = -NBTCP
            NBINV = -NBINV
            NBELP = -NBELP
            NBSOM = -NBSOM
            IF ( (NBCMP .NE. 0) .OR. (NBSOM .NE. 0) ) THEN
C           /* PASSAGE D' UNE OU DEUX LISTE DE NOM DE CMPS    */
C           /* MOT-CLE (NOM_CMP) OU (RESULTANTE ET/OU MOMENT) */
               IF ( NBCMP .NE. 0 ) THEN
                  CALL WKVECT('&&OP0051.NOMCMP.USER','V V K8',NBCMP,
     &                         ANCPU1)
                  CALL GETVTX('ACTION','NOM_CMP',IOCC,1,
     &                        NBCMP,ZK8(ANCPU1),N1)
               ELSE
                  IF (TYPECH.EQ.'ELNO' .AND. GRANCH.EQ.'VARI_R') THEN
                    CALL U2MESS('F','POSTRELE_52')
                  ENDIF
                  CALL GETVTX('ACTION','RESULTANTE',IOCC,1,0,K8B,N1)
                  CALL GETVTX('ACTION','MOMENT'    ,IOCC,1,0,K8B,N2)
                  N1    = -N1
                  N2    = -N2
                  NBCMP =  N1+N2
                  CALL WKVECT('&&OP0051.NOMCMP.USER','V V K8',NBCMP,
     &                         ANCPU1)
                  CALL GETVTX('ACTION','RESULTANTE',IOCC,1,
     &                        N1,ZK8(ANCPU1),N1)
                  CALL GETVTX('ACTION','MOMENT',IOCC,1,
     &                        N2,ZK8(ANCPU1+N1),N2)
               ENDIF
               IF (TYPECH.EQ.'ELNO' .AND. GRANCH.EQ.'VARI_R') THEN
                  CALL UTCMP2 ( GRANCH, 'ACTION', IOCC, NOMCP, NBCMP,
     &                                                  NUMECP, NBNC )
                  CALL JEECRA(JEXNUM(NXDVAR,IOCC),'LONMAX',NBNC,' ')
                  CALL JEECRA(JEXNUM(NXDVAR,IOCC),'LONUTI',NBNC,' ')
                  CALL JEVEUO(JEXNUM(NXDVAR,IOCC),'E',JXVAR)
                  DO 112 I = 1 , NBNC
                     ZI(JXVAR+I-1) = NUMECP(I)
 112              CONTINUE
                  NBCMP      = 1
                  ZK8(ANCPU1) = 'VARI'
               ELSE
                  CALL JEECRA(JEXNUM(NXDVAR,IOCC),'LONMAX',NBCMP,' ')
                  CALL JEECRA(JEXNUM(NXDVAR,IOCC),'LONUTI',0,' ')
               ENDIF
               CALL JEECRA(JEXNUM(NXDNOM,IOCC),'LONMAX',NBCMP,' ')
               CALL JEVEUO(JEXNUM(NXDNOM,IOCC),'E',ANOMCP)
               CALL JEECRA(JEXNUM(NXDNUM,IOCC),'LONMAX',NBCMP,' ')
               CALL JEVEUO(JEXNUM(NXDNUM,IOCC),'E',ANUMCP)
               DO 110, I = 1, NBCMP, 1
                  ZK8(ANOMCP + I-1) = ZK8(ANCPU1 + I-1)
110            CONTINUE
               CALL NUMEK8(ZK8(ACPGD),ZK8(ANOMCP),NBCPGD,NBCMP,
     &                    ZI(ANUMCP))
               CALL JEDETR('&&OP0051.NOMCMP.USER')
            ELSE IF ( NBTCP .NE. 0 ) THEN
C
               NOMOBJ = '&&OP0051.NOMCMP.USER'
               CALL UTNCMP ( NCHP19, NBC, NOMOBJ )
               IF (NBC.EQ.0) CALL U2MESS('F','CHAMPS_1')
               CALL JEVEUO ( NOMOBJ, 'L', ANCPU2 )
               CALL JEECRA(JEXNUM(NXDNOM,IOCC),'LONMAX',NBC,' ')
               CALL JEVEUO(JEXNUM(NXDNOM,IOCC),'E',ANOMCP)
               CALL JEECRA(JEXNUM(NXDNUM,IOCC),'LONMAX',NBC,' ')
               CALL JEVEUO(JEXNUM(NXDNUM,IOCC),'E',ANUMCP)
               DO 120, I = 1, NBC, 1
                  ZK8(ANOMCP + I-1) = ZK8(ANCPU2 + I-1)
120            CONTINUE
               CALL NUMEK8( ZK8(ACPGD), ZK8(ANOMCP), NBCPGD, NBC,
     &                      ZI(ANUMCP) )
               CALL JEDETR ( NOMOBJ )
               IF (TYPECH.EQ.'ELNO' .AND. GRANCH.EQ.'VARI_R') THEN
                  CALL JEECRA(JEXNUM(NXDVAR,IOCC),'LONMAX',NBC,' ')
                  CALL JEECRA(JEXNUM(NXDVAR,IOCC),'LONUTI',NBC,' ')
                  CALL JEVEUO(JEXNUM(NXDVAR,IOCC),'E',JXVAR)
                  ZI(JXVAR) = -1
               ELSE
                  CALL JEECRA(JEXNUM(NXDVAR,IOCC),'LONUTI',0,' ')
               ENDIF
            ELSE
C           /* PASSAGE DE CMPS IMPLICITES */
               CALL JEECRA(JEXNUM(NXDNOM,IOCC),'LONMAX',1,' ')
               CALL JEVEUO(JEXNUM(NXDNOM,IOCC),'E',ANOMCP)
               CALL JEECRA(JEXNUM(NXDNUM,IOCC),'LONMAX',1,' ')
               CALL JEVEUO(JEXNUM(NXDNUM,IOCC),'E',ANUMCP)
               ZK8(ANOMCP) = 'IMPLICIT'
               ZI (ANUMCP) = -1
            ENDIF
         ENDIF
100   CONTINUE
      CALL JEDEMA()
      END
