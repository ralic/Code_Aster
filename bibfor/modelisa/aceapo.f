      SUBROUTINE ACEAPO(NOMA,NOMO,LMAX,NPOUTR,NBOCC,MCLF,
     &                  NBEPO,NTYELE,IVR,IFM,JDLM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER           LMAX,NPOUTR,NBOCC,NBEPO,IFM,JDLM
      INTEGER           NTYELE(*),IVR(*)
      CHARACTER*8       NOMA,NOMO
      CHARACTER*(*)     MCLF
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/02/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT POUTRE
C ----------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : NOMO   : NOM DU MODELE
C IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
C IN  : NPOUTR : NOMBRE DE POUTRE DU MODELE
C IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE POUTRE
C IN  : NBEPO  : NOMBRE D'ELEMENT DE TYPE POUTRE
C IN  : NTYELE : TABLEAU DES TYPES D'ELEMENTS
C IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
C IN  : JDLM   : ADRESSE DES MAILLES
C ----------------------------------------------------------------------
C     ------------------------------------------------------------------
      CHARACTER*1  K1BID
      CHARACTER*6  KIOC
      CHARACTER*8  K8B, NOMU, FCX,NOMSEC
      CHARACTER*16 K16B, SEC, CONCEP, CMD, VARSEC
      CHARACTER*19 CARTPO, CARTGE,CARTPF,TABCAR, NAPCIS, FONCIS
      CHARACTER*24 TMPNPO ,TMPVPO ,TMPGEN ,TMPNGE,TMPVGE,TYPCA,NOMMAI
      CHARACTER*24 TMPNPF ,TMPVPF ,TMPGEF, MODMAI, MLGGMA, MLGNMA
      CHARACTER*24 VMESSK(2)
      INTEGER      IARG
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IDW ,IER ,IISEC ,IIVAR ,IOC ,ISEC
      INTEGER ITABL ,ITBLP ,ITBNP ,IVAR ,IVECT ,IXMA ,J
      INTEGER JCAR ,JCARA ,JDCGE ,JDCPO ,JDCPOF ,JDGE ,JDGEF
      INTEGER JDGM ,JDLS ,JDME ,JDVGE ,JDVPO ,JDVPOF ,JEXP
      INTEGER JJ ,JPARA ,JSECT ,JTAB ,JTYPE ,JVALE ,K
      INTEGER NBCAR ,NBCOLO ,NBLIGN ,NBMAGR ,NBMAIL ,NBO ,NBVAL
      INTEGER NCAR ,NCARAC ,NDIM ,NFCX ,NG ,NM ,NNOSEC
      INTEGER NPOAFF ,NSEC ,NSECPO ,NTAB ,NTYPSE ,NUMMAI ,NUTYEL
      INTEGER NVAL ,NVSEC
      REAL*8 EPY1 ,HY1
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
C
      CALL WKVECT('&&ACEAPO.TAB_PARA','V V I',10,JPARA)
      CALL ACEDAT('POUTRE',0,ZI(JPARA),K16B,K8B,K8B,K8B)
      NSECPO = ZI(JPARA  )
      NTYPSE = ZI(JPARA+1)
      NBO    = ZI(JPARA+2)
      NBCAR  = ZI(JPARA+3)
      NBVAL  = ZI(JPARA+4)

      CALL WKVECT('&&ACEAPO.NCP','V V I',NTYPSE,JTYPE)
      DO 2 I = 1,NTYPSE
         ZI(JTYPE+I-1) = ZI(JPARA+4+I)
 2    CONTINUE
      NDIM = ZI(JTYPE) * ( NSECPO + 1 )
      CALL WKVECT('&&ACEAPO.TYP_SECT','V V K16',NTYPSE     ,JSECT)
      CALL WKVECT('&&ACEAPO.EXPPOU'  ,'V V K8 ',NBO        ,JEXP )
      CALL WKVECT('&&ACEAPO.TABPOU'  ,'V V K8 ',NBO        ,JTAB )
      CALL WKVECT('&&ACEAPO.CARPOU'  ,'V V K8 ',NDIM*NTYPSE,JCAR )
      CALL ACEDAT('POUTRE',1,ZI(JPARA),ZK16(JSECT),ZK8(JEXP),ZK8(JTAB),
     &                                                      ZK8(JCAR))
      CALL WKVECT('&&ACEAPO.CARA'  ,'V V K8',NBCAR ,JCARA)
      CALL WKVECT('&&ACEAPO.VALE'  ,'V V R8',NBVAL ,JVALE)
C
      MODMAI = NOMO//'.MAILLE'
      MLGNMA = NOMA//'.NOMMAI'
      MLGGMA = NOMA//'.GROUPEMA'
      IER = 0
      CALL JELIRA(MLGNMA,'NOMMAX',NBMAIL,K1BID)
      CALL JEEXIN(MODMAI,IXMA)
      IF (IXMA.NE.0) CALL JEVEUO(MODMAI,'L',JDME)
C
C --- CONSTRUCTION DES CARTES
      TMPGEN = NOMU//'.POUTRE'
      CARTPO = NOMU//'.CARGENPO'
      CARTGE = NOMU//'.CARGEOPO'
      TMPNPO = CARTPO//'.NCMP'
      TMPVPO = CARTPO//'.VALV'
      TMPNGE = CARTGE//'.NCMP'
      TMPVGE = CARTGE//'.VALV'

      TMPGEF = NOMU//'.VENT'
      CARTPF = NOMU//'.CVENTCXF'
      TMPNPF = CARTPF//'.NCMP'
      TMPVPF = CARTPF//'.VALV'
C
C --- CREATION D UN OBJET TAMPON (SURDIMENSIONNE A NBO*NPOUTR)  :
      CALL JECREC(TMPGEN,'V V R','NO','CONTIG','CONSTANT',NPOUTR)
      CALL JEECRA(TMPGEN,'LONMAX',NBO,' ')
      CALL JECREC(TMPGEF,'V V K8','NO','CONTIG','CONSTANT',NPOUTR)
      CALL JEECRA(TMPGEF,'LONMAX',1,' ')
      CALL WKVECT('&&ACEAPO.POUTRE','V V K24',LMAX,JDLS)
C
C --- LECTURE ET STOCKAGE DES DONNEES  DANS L OBJET TAMPON
      DO 10 IOC = 1 , NBOCC
         CALL CODENT(IOC,'G',KIOC)
         CALL GETVEM(NOMA,'GROUP_MA','POUTRE','GROUP_MA',
     &            IOC,IARG,LMAX,ZK24(JDLS),NG)
         CALL GETVEM(NOMA,'MAILLE','POUTRE','MAILLE',
     &          IOC,IARG,LMAX,ZK24(JDLS),NM)
         CALL GETVTX('POUTRE','SECTION',IOC,IARG,1,
     &               SEC       ,NSEC)
         CALL GETVTX('POUTRE','VARI_SECT',IOC,IARG,1,
     &               VARSEC    ,NVSEC)


         CALL GETVID('POUTRE','TABLE_CARA',IOC,IARG,1,TABCAR,NTAB)
         IF (NTAB.EQ.1)THEN
             CALL GETVTX('POUTRE','NOM_SEC',IOC,IARG,1,
     &               NOMSEC       ,NNOSEC)
             CALL ASSERT(NNOSEC.EQ.1)

             CALL JEVEUO(TABCAR//'.TBNP','L',ITBNP)
C            NOMBRE DE COLONNES
             NBCOLO = ZI(ITBNP)
C            ON RECHERCHE NOMSEC DANS LA 1ER COLONNE
             CALL JEVEUO(TABCAR//'.TBLP','L',ITBLP)
             TYPCA=ZK24(ITBLP+1)
             IF (TYPCA(1:2).NE.'K8'.AND.TYPCA(1:3).NE.'K24')
     &                       CALL U2MESK('F','MODELISA8_17',1,TABCAR)
             CALL JEVEUO(ZK24(ITBLP+2),'L',ITABL)
             NBLIGN = ZI(ITBNP+1)
             IF (TYPCA.EQ.'K8')THEN
               DO 95 I=1,NBLIGN
                 IF (ZK8(ITABL-1+I).EQ.NOMSEC) THEN
                   IISEC=I
                   GOTO 97
                 ENDIF
 95            CONTINUE
             ELSE
               DO 94 I=1,NBLIGN
                 IF (ZK24(ITABL-1+I)(1:8).EQ.NOMSEC) THEN
                   IISEC=I
                   GOTO 97
                 ENDIF
 94            CONTINUE
             ENDIF
             VMESSK(1)=TABCAR
             VMESSK(2)=NOMSEC
             CALL U2MESK('F','MODELISA8_18',2,VMESSK)
 97          CONTINUE
             JJ=0
             DO 96 I=1,NBCOLO-1
               IF (ZK24(ITBLP+4*I+1).NE.'R')GOTO 96
               DO 102 K=1,NBO
                 IF (ZK24(ITBLP+4*I).EQ.ZK8(JEXP-1+K)) GOTO 103
 102           CONTINUE
               GOTO 96
 103           CONTINUE
               JJ=JJ+1
               ZK8(JCARA-1+JJ) = ZK24(ITBLP+4*I)(1:8)
               CALL JEVEUO(ZK24(ITBLP+4*I+2),'L',IVECT)
               ZR(JVALE-1+JJ)=ZR(IVECT-1+IISEC)
 96          CONTINUE
             NCARAC=JJ
         ELSE
           CALL GETVTX('POUTRE','CARA',IOC,IARG,NBCAR,
     &               ZK8(JCARA),NCAR)
           CALL GETVR8('POUTRE','VALE',IOC,IARG,NBVAL,
     &               ZR(JVALE) ,NVAL)
           CALL ASSERT(NCAR.GT.0)
           NCARAC=NCAR
         ENDIF

         FCX = '.'
         CALL GETVID('POUTRE','FCX'       ,IOC,IARG,1    ,FCX ,NFCX)
C
         IVAR = 2
C
C ----  TYPE DE SECTION ET DE VARIATION DE SECTION POUR CETTE OCCURENCE
C        TEST DE ZK8(JCARA) SEUL > VERIFICATION D HOMOGENEITE DEJA FAITE

         IF ( VARSEC(1:4) .EQ. 'AFFI' ) THEN
            IVAR = 1
            HY1 = 0.D0
            EPY1 = 0.D0
            DO 120 K = 1 , NBCAR
               IF (ZK8(JCARA-1+K)(1:3).EQ.'HY ')  THEN
                  HY1  = ZR(JVALE-1+K)
                  ZK8(JCARA-1+K) = 'HY1'
               ENDIF
               IF (ZK8(JCARA-1+K)(1:4).EQ.'EPY ') THEN
                  EPY1 = ZR(JVALE-1+K)
                  ZK8(JCARA-1+K) = 'EPY1'
               ENDIF
 120        CONTINUE
            NCAR = NCAR + 1
            ZK8(JCARA-1+NCAR) = 'HY2'
             ZR(JVALE-1+NCAR) = HY1
            IF ( EPY1 .NE. 0.D0 ) THEN
               NCAR = NCAR + 1
               ZK8(JCARA-1+NCAR) = 'EPY2'
                ZR(JVALE-1+NCAR) = EPY1
            ENDIF
            NCARAC = NCAR
         ENDIF
C
C
         IF (NTAB.EQ.0) THEN
          DO 20 I = 1 , NTYPSE
            IF (SEC.EQ.ZK16(JSECT+I-1)) THEN
               ISEC = I - 1
               DO 22 J = 1 , ZI(JTYPE+I-1)
                  IF (ZK8(JCARA).EQ.ZK8(JCAR+J+NDIM*(I-1)-1)) THEN
                     IVAR = 0
                     GOTO 24
                  ENDIF
 22            CONTINUE
            ENDIF
 20       CONTINUE
         ELSE
C         SI ON A DONNE TABLE_CARA LA SECTION EST CONSTANTE
          IVAR = 0
          ISEC = 0
         ENDIF
 24      CONTINUE
         IIVAR = IVAR
C
C ---    "GROUP_MA" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DES
C                                                    GROUPES DE MAILLES
         IF (NG.GT.0) THEN
            DO 40 I = 1 , NG
               CALL JEVEUO(JEXNOM(MLGGMA,ZK24(JDLS+I-1)),'L',JDGM)
               CALL JELIRA(JEXNOM(MLGGMA,ZK24(JDLS+I-1)),'LONUTI',
     &                                                  NBMAGR,K1BID)
               DO 42 J = 1,NBMAGR
                  NUMMAI = ZI(JDGM+J-1)
                  CALL JENUNO(JEXNUM(MLGNMA,NUMMAI),NOMMAI)
                  NUTYEL = ZI(JDME+NUMMAI-1)
                  DO 44 K = 1 , NBEPO
                     IF (NUTYEL.EQ.NTYELE(K)) THEN
                        IF (K.EQ.4) IIVAR = 10
                        CALL AFFPOU(TMPGEN,TMPGEF,FCX,NOMMAI,
     &                              ISEC  ,IIVAR ,ZK8(JCARA),
     &                              NCARAC,ZR(JVALE),ZK8(JTAB),
     &                              ZK8(JEXP),NBO,KIOC,IER)
                        IIVAR = IVAR
                        GOTO 42
                     ENDIF
 44               CONTINUE
                  VMESSK(1) = MCLF
                  VMESSK(2) = NOMMAI
                  CALL U2MESK('F','MODELISA_8',2,VMESSK)
 42            CONTINUE
 40         CONTINUE
         ENDIF
C
C ---    "MAILLE" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DE MAILLES
         IF (NM.GT.0) THEN
            DO 50 I = 1 , NM
               NOMMAI = ZK24(JDLS+I-1)
               CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUMMAI)
               NUTYEL = ZI(JDME+NUMMAI-1)
               DO 52 J = 1 , NBEPO
                  IF (NUTYEL.EQ.NTYELE(J)) THEN
                     IF (J.EQ.4) IIVAR = 10
                     CALL AFFPOU(TMPGEN,TMPGEF,FCX,NOMMAI,ISEC,IIVAR,
     &                           ZK8(JCARA),NCARAC,ZR(JVALE),ZK8(JTAB),
     &                           ZK8(JEXP),NBO,KIOC,IER)
                     IIVAR = IVAR
                     GOTO 50
                  ENDIF
 52            CONTINUE
               VMESSK(1) = MCLF
               VMESSK(2) = NOMMAI
               CALL U2MESK('F','MODELISA_8',2,VMESSK)
 50         CONTINUE
         ENDIF
C
 10   CONTINUE
      IF (IER.NE.0) THEN
         CALL U2MESS('F','MODELISA_14')
      ENDIF
C
      CALL JELIRA(TMPGEN,'NUTIOC',NPOAFF,K1BID)
C
C --- VERIFICATION DES OBLIGATIONS ET AFFECTATION DES DEFAUTS
      DO 60 I = 1 ,NPOAFF
         CALL JENUNO(JEXNUM(TMPGEN,I),NOMMAI)
         CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUMMAI)
         NUTYEL = ZI(JDME+NUMMAI-1)
         CALL AFFDEF(TMPGEN,NOMMAI,NUTYEL,NTYELE,ZK8(JTAB),IER)
 60   CONTINUE
      IF (IER.NE.0) THEN
         CALL U2MESS('F','MODELISA_15')
      ENDIF
C
C --- CALCUL DES DONNEES GENERALES A PARTIR DES DONNEES GEOMETRIQUES
C                            (CERC+RECT) ET AFFECTATIONS DANS LE TAMPON
C
C --- UTILISATION DE LA NAPPE POUR LES SECTIONS RECTANGULAIRES
C --- ET DE LA FONCTION POUR LES SECTIONS CIRCULAIRES
C --- AFIN DINTERPOLER LES COEFFICIENTS DE CISAILLEMENT
C
      CALL COECIS(NAPCIS,FONCIS)
C
      DO 62 I = 1 ,NPOAFF
         CALL JENUNO(JEXNUM(TMPGEN,I),NOMMAI)
         CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUMMAI)
         NUTYEL = ZI(JDME+NUMMAI-1)
         CALL AFFGEN(TMPGEN,NOMMAI,NUTYEL,NTYELE,NAPCIS,FONCIS)
 62   CONTINUE
C
C --- IMPRESSION DES VALEURS AFFECTEES DANS LE TAMPON SI DEMANDE
      IF (IVR(3).EQ.1) THEN
C
C ---    IMPRESSION DES DONNEES GENERALES
         WRITE(IFM,2000)
         DO 64 I = 1 , NPOAFF
            CALL JENUNO(JEXNUM(TMPGEN,I),NOMMAI)
            CALL JEVEUO(JEXNUM(TMPGEN,I),'L',JDGE)
            IVAR = NINT(ZR(JDGE+22))
            ISEC = NINT(ZR(JDGE+35))
            WRITE(IFM,2001)NOMMAI,(ZR(JDGE+J-1),J=1,22),
     &                     (ZR(JDGE+J-1),J=37,44),IVAR,ISEC
            CALL JENUNO(JEXNUM(TMPGEF,I),NOMMAI)
            CALL JEVEUO(JEXNUM(TMPGEF,I),'L',JDGEF)
            WRITE(IFM,*)'CX : ',ZK8(JDGEF)
 64      CONTINUE
C
C ---    IMPRESSION DES DONNEES GEOMETRIQUES
         IDW = 0
         DO 66 I = 1,NPOAFF
            CALL JENUNO(JEXNUM(TMPGEN,I),NOMMAI)
            CALL JEVEUO(JEXNUM(TMPGEN,I),'L',JDGE)
            ISEC = NINT(ZR(JDGE+35))
            IF (ISEC.GT.0) THEN
               IF (IDW.EQ.0) THEN
                  WRITE(IFM,2002)
                  IDW = 1
               ENDIF
               WRITE(IFM,2003)NOMMAI,(ZR(JDGE+J-1),J=24,35),ISEC
            ENDIF
 66      CONTINUE
      ENDIF
C
 2000   FORMAT(/,3X,'<SECTION> ',
     &      'VALEURS DE TYPE GENERALE AFFECTEES AUX POUTRES',//,3X,
     &      'MAILLE   ',
     &      'A1  ',8X,'IY1  ',7X,'IZ1  ',7X,'AY1  ',7X,'AZ1  ',/,
     &  12X,'EY1 ',8X,'EZ1  ',7X,'JX1  ',7X,'RY1  ',7X,'RZ1  ',/,
     &  12X,'RT1 ',8X,'A2   ',7X,'IY2  ',7X,'IZ2  ',7X,'AY2  ',/,
     &  12X,'AZ2 ',8X,'EY2  ',7X,'EZ2  ',7X,'JX2  ',7X,'RY2  ',/,
     &  12X,'RZ2 ',8X,'RT2  ',7X,'AI1  ',7X,'AI2  ',7X,'JG1  ',/,
     &  12X,'JG2 ',8X,'IYR21',7X,'IYR22',7X,'IZR21',7X,'IZR22',/,
     &  12X,'TVAR',8X,'TSEC ')
 2001   FORMAT(/,1P,3X,A8,1X,5(1PD12.5,1X),5(/,12X,5(1PD12.5,1X)),
     &         /,12X,I6,6X,I6)
 2002   FORMAT(/,3X,'<SECTION> ',
     &  'VALEURS DE TYPE GEOMETRIQUE AFFECTEES AUX POUTRES',//,3X,
     &  'MAILLE   HY1         HZ1         EPY1        EPZ1'
     &  ,/,12X,         'HY2         HZ2         EPY2        EPZ2'
     &  ,/,12X,         'R1          EP1         R2          EP2',9X
     &  ,'TSEC')
 2003   FORMAT(/,1P,3X,A8,1X,4(1PD12.5,1X),2(/,12X,4(1PD12.5,1X)),I6)
C
C --- ALLOCATION DES CARTES
      CALL ALCART('G',CARTPO,NOMA,'CAGNPO')
      CALL ALCART('G',CARTGE,NOMA,'CAGEPO')
      CALL JEVEUO(TMPNPO,'E',JDCPO)
      CALL JEVEUO(TMPVPO,'E',JDVPO)
      CALL JEVEUO(TMPNGE,'E',JDCGE)
      CALL JEVEUO(TMPVGE,'E',JDVGE)
      CALL JEVEUO(TMPNPF,'E',JDCPOF)
      CALL JEVEUO(TMPVPF,'E',JDVPOF)
C
C --- AFFECTATIONS DES DONNEES GENERALES
      DO 68 I = 1 , 23
         ZK8(JDCPO+I-1) = ZK8(JTAB+I-1)
 68   CONTINUE
      DO 69 I = 24, 31
         ZK8(JDCPO+I-1) = ZK8(JTAB+I+13-1)
 69   CONTINUE
C     POUR LA CARTE DE VENT ==> FCXP
      ZK8(JDCPOF) = 'FCXP'

      DO 70 I = 1 , NPOAFF
         CALL JENUNO(JEXNUM(TMPGEN,I),NOMMAI)
         CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUMMAI)
         ZI(JDLM+NUMMAI-1) = -1
         CALL JEVEUO(JEXNUM(TMPGEN,I),'L',JDGE)
         DO 72 J = 1 , 23
            ZR(JDVPO+J-1)  = ZR(JDGE+J-1)
 72      CONTINUE
         DO 73 J = 24, 31
            ZR(JDVPO+J-1)  = ZR(JDGE+J+13-1)
 73      CONTINUE
         CALL JEVEUO(JEXNUM(TMPGEF,I),'L',JDGEF)
         ZK8(JDVPOF) = ZK8(JDGEF)
         CALL NOCART(CARTPO ,3,' ','NOM',1,NOMMAI,0,' ',31)
         CALL NOCART(CARTPF,3,' ','NOM',1,NOMMAI,0,' ',1)
 70   CONTINUE
C
C --- AFFECTATIONS DONNEES GEOMETRIQUES (ON AFFECTE TOUTES LES CMPS)
      DO 74 I = 1 , 13
         ZK8(JDCGE+I-1) = ZK8(JTAB+I+23-1)
 74   CONTINUE
      DO 76 J = 1 , NPOAFF
         CALL JENUNO(JEXNUM(TMPGEN,J),NOMMAI)
         CALL JEVEUO(JEXNUM(TMPGEN,J),'L',JDGE)
         ISEC = NINT(ZR(JDGE+35))
         IF (ISEC.EQ.0) THEN
C ---       GENERALE
            DO 78 I = 1 , 13
               ZR (JDVGE+I-1) = 0.D0
 78         CONTINUE
            CALL NOCART(CARTGE,3,' ','NOM',1,NOMMAI,0,' ',13)
         ELSE
C ---       RECTANGLE OU CERCLE
            DO 80 I = 1 , 13
               ZR (JDVGE+I-1) = ZR(JDGE+I+22)
 80         CONTINUE
            CALL NOCART(CARTGE,3,' ','NOM',1,NOMMAI,0,' ',13)
         ENDIF
 76   CONTINUE

C --- COMPACTAGE DES CARTES (MAIS ON NE CHERCHE PAS DE REMANENCE) :
      CALL TECART(CARTPO)
      CALL TECART(CARTGE)

C --- NETTOYAGE
      CALL JEDETR('&&ACEAPO.TAB_PARA')
      CALL JEDETR('&&ACEAPO.NCP')
      CALL JEDETR('&&ACEAPO.TYP_SECT')
      CALL JEDETR('&&ACEAPO.EXPPOU')
      CALL JEDETR('&&ACEAPO.TABPOU')
      CALL JEDETR('&&ACEAPO.CARPOU')
      CALL JEDETR('&&ACEAPO.CARA')
      CALL JEDETR('&&ACEAPO.VALE')
      CALL JEDETR('&&ACEAPO.POUTRE')
      CALL JEDETR(TMPGEN)
      CALL JEDETR(TMPGEF)
      CALL JEDETR(TMPNPO)
      CALL JEDETR(TMPVPO)
      CALL JEDETR(TMPNGE)
      CALL JEDETR(TMPVGE)
C
      CALL JEDEMA()
      END
