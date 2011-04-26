      SUBROUTINE IRCERL(IFI,NBEL,LIGREL,NBGREL,LONGR,NCMPMX,VALE,NOMCMP,
     &                  NOMEL,LOC,CELD,CONNEX,POINT,NOMNOS,NBCMPT,
     &                  NUCMPU,NBNOT,NUMNOE,NBMAT,NUMMAI,LSUP,BORSUP,
     &                  LINF,BORINF,LMAX,LMIN,LCOR,NDIM,COOR,NOLILI,
     &                  FORMR,NCMPV,NUCMP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER           IFI,NBEL,LIGREL(*),NBGREL,LONGR(*),NCMPMX,NBNOT,
     &                  NBCMPT,NUCMPU(*),CELD(*),CONNEX(*),POINT(*),
     &                  NUMNOE(*),NBMAT,NDIM, NUMMAI(*),NCMPV,NUCMP(*)
      REAL*8            BORSUP,BORINF,COOR(*),              VALE(*)
      CHARACTER*(*)     NOMCMP(*),NOMEL(*),LOC,NOMNOS(*),FORMR
      CHARACTER*19      NOLILI
      LOGICAL           LSUP,LINF,     LMAX,LMIN,LCOR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE  CRP_20  CRP_21
C        ECRITURE D'UN CHAMELEM SUR LISTING
C        A VALEURS REELLES
C  ENTREE:
C     IFI   : UNITE LOGIQUE DU FICHIER
C     NBEL  : NOMBRE D'ELEMENTS DU LIGREL ( DU MAILLAGE)
C     LIGREL: LIGREL COMPLET
C     NBGREL: NOMBRE DE GRELS
C     LONGR : POINTEUR DE LONGUEUR DE LIGREL
C     NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
C     VALE  : VALEURS DU CHAM_NO
C     NOMCMP: NOMS DES CMP
C     NOMEL : NOMS DES MAILLES SUPPORTS DES ELEMENTS
C     LOC   : LOCALISATION DES VALEURS (ELNO OU ELGA OU ELEM)
C     CELD  : DESCRIPTEUR DU CHAM_ELEM (MODES LOCAUX,ADRESSES->.CELV)
C     CONNEX: CONNECTIVITES DES MAILLES
C     POINT : POINTEUR DANS LES CONNECTIVITES
C     NOMNOS: NOMS DES NOEUDS
C     NBCMPT: NOMBRE DE COMPOSANTES A IMPRIMER
C     NUCMPU: NUMEROS DES COMPOSANTES A IMPRIMER
C     NBMAT : NOMBRE DE MAILLES OU ON DESIRE IMPRIMER LE CHAMELEM
C     NUMMAI: NUMEROS DES MAILLES OU ON DESIRE IMPRIMER LE CHAMELEM
C     LSUP  : =.TRUE.  INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
C     BORSUP: VALEUR DE LA BORNE SUPERIEURE
C     LINF  : =.TRUE.  INDIQUE PRESENCE D'UNE BORNE INFERIEURE
C     BORINF: VALEUR DE LA BORNE INFERIEURE
C     LMAX  : =.TRUE.  INDIQUE IMPRESSION VALEUR MAXIMALE
C     LMIN  : =.TRUE.  INDIQUE IMPRESSION VALEUR MINIMALE
C     LCOR  : =.TRUE.  IMPRESSION DES COORDONNEES DE NOEUDS DEMANDEE
C     NDIM  : DIMENSION DU PROBLEME
C     COOR  : TABLEAU DES COORDONNEES DE NOEUDS
C     NOLILI: NOM DU LIGREL
C     FORMR : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       DIGDEL,IMODEL,ILONG
      REAL*8        RUNDF
      CHARACTER*7   CBID
      CHARACTER*8   NOMNO, NOMCP, FORCMP, NOMCOR(3)
      CHARACTER*10  FORMAT
      CHARACTER*24  NREPE
      CHARACTER*50  FMT, FMV, FMT1, FMT2, FORM1
      LOGICAL       EXISDG, LIMPR
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
      RUNDF = R8VIDE()
      NOMCOR(1) = 'X'
      NOMCOR(2) = 'Y'
      NOMCOR(3) = 'Z'
      FORMAT = FORMR
      LGR = LXLGUT( FORMAT )
      ID = 0
      IF = 0
      CALL JEVEUO ('&CATA.TE.MODELOC', 'L', IMODEL )
      CALL JEVEUO (JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',ILONG)
      DO 2 I = 1 , LGR-1
         IF ( FORMAT(I:I) .EQ. 'D' .OR. FORMAT(I:I) .EQ. 'E' .OR.
     &        FORMAT(I:I) .EQ. 'F' .OR. FORMAT(I:I) .EQ. 'G' ) THEN
            ID = I+1
            GOTO 2
         ENDIF
         IF ( FORMAT(I:I) .EQ. '.' ) THEN
            IF = I-1
            GOTO 2
         ENDIF
 2    CONTINUE
      IF ( ID.NE.0 .AND. IF.GE.ID ) THEN
         FORCMP = 'A'//FORMAT(ID:IF)
      ELSE
         FORCMP = 'A12'
      ENDIF
C
C  -- DETERMINATION DU NOMBRE MAXIMUM DE SOUS_POINTS ---
      ICOMAX = 0
      DO 4 IGRE = 1 , NBGREL
         ICOEF=MAX(1,CELD(4))
         IF ( ICOEF .GT. ICOMAX ) ICOMAX = ICOEF
 4    CONTINUE
      NCMP = NCMPV
      IF ( NCMP .GT. 0 ) THEN
         NCMP = 0
         DO 141 I = 1 , NCMPV
            IF ( NUCMP(I) .LE. ICOMAX ) THEN
               NCMP = NCMP + 1
            ELSE
               CALL CODENT ( NUCMP(I), 'G', CBID )
               NOMCP = 'V'//CBID
               CALL U2MESK('A','PREPOST_74',1,NOMCP)
            ENDIF
 141     CONTINUE
         IF ( NCMP .EQ. 0 ) THEN
            CALL U2MESS('A','PREPOST_75')
            GOTO 9999
         ENDIF
         ICOMAX = NCMP
      ENDIF
C
      IF ( LMAX .OR. LMIN ) THEN
        CALL JEDETR('&&IRCERL.NCMT')
        CALL WKVECT('&&IRCERL.NCMT','V V K16',NCMPMX*ICOMAX,INOT)
        DO 6 I=1,NCMPMX
          IF (ICOMAX.GT.1 .OR. NCMP.GE.1) THEN
             DO 7 JCO = 1,ICOMAX
                IF ( NCMP .GT. 0 ) THEN
                   CALL CODENT ( NUCMP(JCO), 'G', CBID )
                ELSE
                   CALL CODENT ( JCO, 'G', CBID )
                ENDIF
                NOMCP = NOMCMP(I)
                ZK16(INOT-1+(I-1)*ICOMAX+JCO)='V'//CBID
  7          CONTINUE
          ELSE
             ZK16(INOT-1+I) = NOMCMP(I)
          ENDIF
  6      CONTINUE
      ENDIF
      IF ( LMAX ) THEN
        CALL JEDETR ( '&&IRCERL.MAX')
        CALL WKVECT ( '&&IRCERL.MAX', 'V V R', NCMPMX*ICOMAX, IMAX )
        CALL JEDETR ( '&&IRCERL.MAIMAX' )
        CALL WKVECT ( '&&IRCERL.MAIMAX', 'V V K8',NCMPMX*ICOMAX,INMAX)
        CALL JEDETR ( '&&IRCERL.NBVMAX' )
        CALL WKVECT ( '&&IRCERL.NBVMAX','V V I',NCMPMX*ICOMAX,IVMAX)
        DO 90 I=1,NCMPMX*ICOMAX
           ZR(IMAX-1+I)=RUNDF
 90     CONTINUE
      ENDIF
      IF ( LMIN ) THEN
        CALL JEDETR ( '&&IRCERL.MIN' )
        CALL WKVECT ( '&&IRCERL.MIN','V V R',NCMPMX*ICOMAX,IMIN)
        CALL JEDETR ( '&&IRCERL.MAIMIN' )
        CALL WKVECT ( '&&IRCERL.MAIMIN','V V K8',NCMPMX*ICOMAX,INMIN)
        CALL JEDETR ( '&&IRCERL.NBVMIN' )
        CALL WKVECT ( '&&IRCERL.NBVMIN','V V I',NCMPMX*ICOMAX,IVMIN)
        DO 91 I=1,NCMPMX*ICOMAX
           ZR(IMIN-1+I)=RUNDF
 91     CONTINUE
      ENDIF
      IF (LOC.EQ.'ELGA' .OR. LOC.EQ.'ELEM' .OR. .NOT.LCOR) NDIM = 0
      NREPE = NOLILI//'.REPE'
      CALL JEVEUO(NREPE,'L',IREPE)
      IF (NBMAT.NE.0) NBEL=NBMAT
      MODSAU = 0
      DO 12 IMAI =1,NBEL
         IF (NBMAT.NE.0) THEN
            IMAIL = NUMMAI(IMAI)
         ELSE
            IMAIL = IMAI
         ENDIF
         IGREL = ZI(IREPE+2*(IMAIL-1)+1-1)
         IF( IGREL.EQ.0) GOTO 12
         IELG  = ZI(IREPE+2*(IMAIL-1)+2-1)
         MODE=CELD(CELD(4+IGREL)+2)
         IF(MODE.EQ.0) GO TO 12
         IF (MODE.NE.MODSAU) THEN
           IPOIN1=LONGR(IGREL)
           JMOD = IMODEL+ZI(ILONG-1+MODE)-1
           NEC = NBEC(ZI(JMOD-1+2))
           CALL JEDETR('&&IRCERL.ENT_COD')
           CALL WKVECT('&&IRCERL.ENT_COD','V V I',NEC,IAEC)
           CALL DGMODE(MODE,IMODEL,ILONG,NEC,ZI(IAEC))
           IAD=CELD(CELD(4+IGREL)+8)
           NSCAL = DIGDEL(MODE)
           ICOEF=MAX(1,CELD(4))
           NSCA  = NSCAL*ICOEF
           ICOEF2 = ICOEF
           IF ( NCMP .GT. 0 ) ICOEF2 = NCMP
           NCMPP = 0
           NCMP2 = 0
C
C -- IPOSG : POSITION DE LA COMPOSANTE DANS LA GRANDEUR
C -- IPOSV : POSITION DE LA COMPOSANTE DANS LE .VALE
C
           CALL JEDETR('&&IRCERL.POSG')
           CALL WKVECT('&&IRCERL.POSG','V V I',NCMPMX*ICOEF2,IPOSG)
           CALL JEDETR('&&IRCERL.POSV')
           CALL WKVECT('&&IRCERL.POSV','V V I',NCMPMX,IPOSV)
           CALL JEDETR('&&IRCERL.COEF')
           CALL WKVECT('&&IRCERL.COEF','V V I',NCMPMX*ICOEF2,ICOE)
           CALL JEDETR('&&IRCERL.NCMP')
           CALL WKVECT('&&IRCERL.NCMP','V V K16',NCMPMX*ICOEF2,INOM)
           IF (LSUP.OR.LINF) THEN
            CALL JEDETR('&&IRCERL.NCPP')
            CALL WKVECT('&&IRCERL.NCPP','V V K16',NCMPMX*ICOEF2,INOP)
            CALL JEDETR('&&IRCERL.PO2')
            CALL WKVECT('&&IRCERL.PO2','V V I',NCMPMX*ICOEF2,IPO2)
           ENDIF
           CALL JEDETR('&&IRCERL.VAL')
           CALL WKVECT('&&IRCERL.VAL','V V R',NCMPMX*ICOEF2,IVAL)
           DO 5 I=1,NCMPMX*ICOEF2
              ZI(IPOSG-1+I)=0
  5        CONTINUE
           DO 26 I=1,NCMPMX
              ZI(IPOSV-1+I)=0
 26        CONTINUE
           DO 23 I=1,NCMPMX
            IF (EXISDG(ZI(IAEC),I)) THEN
               NCMPP=NCMPP+1
               IF(NBCMPT.NE.0) THEN
                  DO 8 ICM=1,NBCMPT
                    ICMP2=NUCMPU(ICM)
                    IF (I.EQ.ICMP2) THEN
                       NCMP2=NCMP2+1
                       DO 92 JCO=1,ICOEF2
                         ZI(IPOSG-1+(ICM-1)*ICOEF2+JCO)=I
  92                   CONTINUE
                       ZI(IPOSV-1+ICM)=NCMPP
                    ENDIF
 8                CONTINUE
               ELSE
                  DO 93 JCO = 1 , ICOEF2
                     ZI(IPOSG-1+(NCMPP-1)*ICOEF2+JCO)=I
  93              CONTINUE
               ENDIF
            END IF
   23      CONTINUE
           IF(NBCMPT.EQ.0) NCMP2=NCMPP
           NPCALC = NSCAL/NCMPP
C
C --- RETASSAGE DU TABLEAU DES POSITIONS DES COMPOSANTES DANS GRANDEUR-
C
           IF(NBCMPT.NE.0) THEN
             I2=0
             DO 9 I=1,NBCMPT*ICOEF2
               IF (ZI(IPOSG-1+I).NE.0) THEN
                 I2=I2+1
                 ZI(IPOSG-1+I2)= ZI(IPOSG-1+I)
               ENDIF
 9           CONTINUE
           ENDIF
C
C --- STOCKAGE DES NOMS DE COMPOSANTES ---
           DO 42 I=1,NCMP2
            IF(ICOEF2.GT.1 .OR. NCMP.GE.1) THEN
              DO 43 JCO=1,ICOEF2
                IF ( NCMP .GT. 0 ) THEN
                   CALL CODENT ( NUCMP(JCO), 'G', CBID )
                ELSE
                   CALL CODENT ( JCO, 'G', CBID )
                ENDIF
                NOMCP = NOMCMP(ZI(IPOSG-1+I))
                ZK16(INOM-1+(I-1)*ICOEF2+JCO)='V'//CBID
   43         CONTINUE
            ELSE
              ZK16(INOM-1+I)=NOMCMP(ZI(IPOSG-1+I))
            ENDIF
   42      CONTINUE
C
C --- CREATION DES FORMATS D'ECRITURE ---
C
           IF (.NOT.LMAX.AND..NOT.LMIN) THEN
             ILIG=(NCMP2*ICOEF2+NDIM)/6
             IRES=(NCMP2*ICOEF2+NDIM)-ILIG*6
             FMT = ' '
             FMV = ' '
             IF (IRES.NE.0) THEN
               FMT = '(1X,A8,6(1X,'//FORCMP//'),30(/,9X,6(1X,'//
     &                               FORCMP//')))'
               IF(LOC.EQ.'ELNO') THEN
                 FMV = '(1X,A8,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     &                                 FORMAT//')))'
               ELSEIF(LOC.EQ.'ELGA') THEN
                 FMV = '(2X,I7,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     &                                 FORMAT//')))'
               ELSEIF(LOC.EQ.'ELEM') THEN
                 FMV = '(9X,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     &                              FORMAT//')))'
               ENDIF
             ELSEIF (IRES.EQ.0.AND.ILIG.EQ.1) THEN
               FMT = '(1X,A8,6(1X,'//FORCMP//'))'
               IF(LOC.EQ.'ELNO') THEN
                 FMV = '(1X,A8,6(1X,'//FORMAT//'))'
               ELSEIF(LOC.EQ.'ELGA') THEN
                 FMV = '(2X,I7,6(1X,'//FORMAT//'))'
               ELSEIF(LOC.EQ.'ELEM') THEN
                 FMV = '(9X,6(1X,'//FORMAT//'))'
               ENDIF
             ELSE
               WRITE(FMT,'(A,A8,A,I2,A,A8,A)') '(1X,A8,6(1X,', FORCMP,
     &                   '),', (ILIG-1),'(/,9X,6(1X,', FORCMP, ')))'
               IF (LOC.EQ.'ELNO') THEN
                 WRITE(FMV,'(A,A10,A,I2,A,A10,A)') '(1X,A8,6(1X,',
     &               FORMAT,'),', (ILIG-1), '(/,9X,6(1X,', FORMAT, ')))'
               ELSEIF(LOC.EQ.'ELGA') THEN
                 WRITE(FMV,'(A,A10,A,I2,A,A10,A)') '(2X,I7,6(1X,',
     &               FORMAT,'),', (ILIG-1), '(/,9X,6(1X,', FORMAT, ')))'
               ELSEIF(LOC.EQ.'ELEM') THEN
                 WRITE(FMV,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',
     &               FORMAT,'),', (ILIG-1), '(/,9X,6(1X,', FORMAT, ')))'
               ENDIF
             ENDIF
           ENDIF
         ENDIF
C
C --- BOUCLE SUR LES ELEMENTS ---
C
         IEL=LIGREL(IPOIN1+IELG-1)
         LIMPR = .TRUE.
         IF(.NOT.LSUP.AND..NOT.LINF.AND..NOT.LMAX.AND..NOT.LMIN) THEN
             IF (NDIM.EQ.0) THEN
               WRITE(IFI,FMT) NOMEL(IEL),(ZK16(INOM-1+I)(1:11),
     &                                                 I=1,ICOEF2*NCMP2)
             ELSE
               WRITE(IFI,FMT) NOMEL(IEL),(NOMCOR(I),I=1,NDIM),
     &                           (ZK16(INOM-1+I)(1:11),I=1,ICOEF2*NCMP2)
             ENDIF
         ENDIF
         IACHML = IAD + NSCA * (IELG-1)
         IF (LOC.EQ.'ELGA' .OR. LOC.EQ.'ELEM') THEN
              DO 16 IPCA=1,NPCALC
                 J=IACHML-1+NCMPP*ICOEF*(IPCA-1)
                 IF (NBCMPT.EQ.0) THEN
                    DO 10 I=1,NCMP2
                      IF ( NCMP .GT. 0 ) THEN
                         DO 551 JCO=1,ICOEF2
                           ZR(IVAL-1+(I-1)*ICOEF2+JCO)=
     &                                    VALE(J+I+(NUCMP(JCO)-1)*NCMPP)
                           ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
  551                    CONTINUE
                      ELSE
                         DO 55 JCO=1,ICOEF2
                           ZR(IVAL-1+(I-1)*ICOEF2+JCO)=
     &                                           VALE(J+I+(JCO-1)*NCMPP)
                           ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
   55                    CONTINUE
                      ENDIF
   10               CONTINUE
                 ELSE
                    DO 20 I=1,NCMP2
                      INU=ZI(IPOSV-1+I)
                      IF ( NCMP .GT. 0 ) THEN
                         DO 301 JCO=1,ICOEF2
                           ZR(IVAL-1+(I-1)*ICOEF2+JCO)=
     &                                  VALE(J+INU+(NUCMP(JCO)-1)*NCMPP)
                           ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
  301                    CONTINUE
                      ELSE
                         DO 30 JCO=1,ICOEF2
                           ZR(IVAL-1+(I-1)*ICOEF2+JCO)=
     &                                         VALE(J+INU+(JCO-1)*NCMPP)
                           ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
   30                    CONTINUE
                      ENDIF
   20               CONTINUE
                 ENDIF
C
C --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
C
                 IF ( LSUP .OR. LINF ) THEN
                   DO 35 IVA=1,ICOEF2*NCMP2
                     IF(LSUP) THEN
                       IF((ZR(IVAL-1+IVA)-BORSUP).GT.0.D0)
     &                    ZI(ICOE-1+IVA)=0
                     ENDIF
                     IF(LINF) THEN
                       IF((ZR(IVAL-1+IVA)-BORINF).LT.0.D0)
     &                    ZI(ICOE-1+IVA)=0
                     ENDIF
 35                CONTINUE
C
C --- RETASSAGE POUR IMPRIMER COMPOSANTES PRESENTES DANS L'INTERVALLE --
C
                   ICOMP2=0
                   DO 36 I=1,ICOEF2*NCMP2
                     IF(ZI(ICOE-1+I).NE.0) THEN
                          ICOMP2=ICOMP2+1
                          ZI(ICOE-1+ICOMP2)=ZI(ICOE-1+I)
                          ZI(IPO2-1+ICOMP2)=ZI(IPOSG-1+I)
                          ZR(IVAL-1+ICOMP2)=ZR(IVAL-1+I)
                          ZK16(INOP-1+ICOMP2)=ZK16(INOM-1+I)
                     ENDIF
   36              CONTINUE
                   IF(ICOMP2.EQ.0)  GOTO 16
C
C -- IMPRESSION ----
C
                   IF (.NOT.LMAX.AND..NOT.LMIN) THEN
                     ILIG=(ICOMP2)/6
                     IRES=(ICOMP2)-ILIG*6
                     FMT1 = ' '
                     FMT2 = ' '
                     IF (LOC.EQ.'ELGA') THEN
                       IF (IRES.NE.0) THEN
                         FMT1 = '(9X,6(1X,'//FORCMP//'),30(/,9X,6(1X,'
     &                                     //FORCMP//')))'
                         FMT2 = '(2X,I7,6(1X,'//FORMAT//
     &                          '),30(/,9X,6(1X,'//FORMAT//')))'
                       ELSEIF (IRES.EQ.0.AND.ILIG.EQ.1) THEN
                         FMT1 = '(9X,6(1X,'//FORCMP//'))'
                         FMT2 = '(2X,I7,6(1X,'//FORMAT//'))'
                       ELSE
                        WRITE(FMT1,'(A,A8,A,I2,A,A8,A)')'(1X,A8,6(1X,',
     &                   FORCMP,'),',(ILIG-1),'(/,9X,6(1X,',FORCMP,')))'
                       WRITE(FMT2,'(A,A10,A,I2,A,A10,A)')'(2X,I7,6(1X,',
     &                   FORMAT,'),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
                       ENDIF
                     ELSE
                       IF (IRES.NE.0) THEN
                         FMT1 = '(9X,6(1X,'//FORCMP//'),30(/,9X,6(1X,'
     &                                     //FORCMP//')))'
                         FMT2 = '(9X,6(1X,'//FORMAT//'),30(/,9X,6(1X,'
     &                                     //FORMAT//')))'
                       ELSEIF (IRES.EQ.0.AND.ILIG.EQ.1) THEN
                         FMT1 = '(9X,6(1X,'//FORCMP//'))'
                         FMT2 = '(9X,6(1X,'//FORMAT//'))'
                       ELSE
                       WRITE(FMT1,'(A,A8,A,I2,A,A8,A)')'(1X,A8,6(1X,',
     &                   FORCMP,'),',(ILIG-1),'(/,9X,6(1X,',FORCMP,')))'
                       WRITE(FMT2,'(A,A10,A,I2,A,A10,A)')'(9X,6(1X,',
     &                   FORMAT,'),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
                       ENDIF
                     ENDIF
                     IF (LSUP.OR.LINF) THEN
                       IF (LIMPR) THEN
                         WRITE(IFI,'(A,I2,A)') NOMEL(IEL)
                         LIMPR=.FALSE.
                       ENDIF
                     ENDIF
                     IF (LOC.EQ.'ELGA') THEN
                       WRITE(IFI,FMT1) (ZK16(INOP-1+I)(1:11),I=1,ICOMP2)
                       WRITE(IFI,FMT2) IPCA,(ZR(IVAL-1+ICMP),
     &                                              ICMP=1,ICOMP2)
                     ELSE
                       WRITE(IFI,FMT1) (ZK16(INOP-1+I)(1:11),I=1,ICOMP2)
                       WRITE(IFI,FMT2) (ZR(IVAL-1+ICMP),ICMP=1,ICOMP2)
                     ENDIF
                   ENDIF
                   NBCPT=ICOMP2
                 ELSE
                   IF (.NOT.LMAX.AND..NOT.LMIN) THEN
                     IF (LOC.EQ.'ELGA') THEN
                       WRITE(IFI,FMV) IPCA,(ZR(IVAL-1+ICMP),
     &                                            ICMP=1,ICOEF2*NCMP2)
                     ELSE
                       WRITE(IFI,FMV) (ZR(IVAL-1+ICMP),
     &                                            ICMP=1,ICOEF2*NCMP2)
                     ENDIF
                   ENDIF
                   NBCPT=ICOEF2*NCMP2
                 ENDIF
C
C -- RECHERCHE DE LA VALEUR MAXIMALE ---
C
                  IF ( LMAX ) THEN
                    DO 101 I=1,NBCPT
                     IF(LSUP.OR.LINF) THEN
                       IADR=(ZI(IPO2-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                     ELSE
                       IADR=(ZI(IPOSG-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                     ENDIF
                     IF(ZR(IMAX-1+IADR).EQ.RUNDF) THEN
                       ZR(IMAX-1+IADR) = ZR(IVAL-1+I)
                       ZK8(INMAX-1+IADR) = NOMEL(IEL)
                       ZI(IVMAX-1+IADR) = 1
                     ELSEIF(ZR(IVAL-1+I).GT.ZR(IMAX-1+IADR)) THEN
                       ZR(IMAX-1+IADR)= ZR(IVAL-1+I)
                       ZK8(INMAX-1+IADR) = NOMEL(IEL)
                       ZI(IVMAX-1+IADR) = 1
                     ELSEIF(ZR(IVAL-1+I).EQ.ZR(IMAX-1+IADR)) THEN
                       ZI(IVMAX-1+IADR)=ZI(IVMAX-1+IADR)+1
                     ENDIF
  101               CONTINUE
                  ENDIF
C
C -- RECHERCHE DE LA VALEURE MINIMALE ---
C
                  IF(LMIN) THEN
                    DO 102 I=1,NBCPT
                     IF(LSUP.OR.LINF) THEN
                       IADR=(ZI(IPO2-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                     ELSE
                       IADR=(ZI(IPOSG-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                     ENDIF
                     IF(ZR(IMIN-1+IADR).EQ.RUNDF) THEN
                       ZR(IMIN-1+IADR) = ZR(IVAL-1+I)
                       ZK8(INMIN-1+IADR) = NOMEL(IEL)
                       ZI(IVMIN-1+IADR) = 1
                     ELSEIF(ZR(IVAL-1+I).LT.ZR(IMIN-1+IADR)) THEN
                       ZR(IMIN-1+IADR)= ZR(IVAL-1+I)
                       ZK8(INMIN-1+IADR) = NOMEL(IEL)
                       ZI(IVMIN-1+IADR) = 1
                     ELSEIF(ZR(IVAL-1+I).EQ.ZR(IMIN-1+IADR)) THEN
                       ZI(IVMIN-1+IADR)=ZI(IVMIN-1+IADR)+1
                     ENDIF
  102               CONTINUE
                  ENDIF
   16          CONTINUE
CCCCCC
            ELSE IF (LOC.EQ.'ELNO') THEN
               IPOIN=POINT(IEL)
               NBNO=POINT(IEL+1)-IPOIN
               NCOU=NPCALC/NBNO
               DO 17 ICOU=1,NCOU
                  IF (NCOU.GT.1) THEN
                     IF (.NOT.LMAX.AND..NOT.LMIN) THEN
                       IF (NCOU.EQ.2) THEN
                        IF (ICOU.EQ.1) WRITE(IFI,'(A)') ' PEAU INTERNE'
                        IF (ICOU.EQ.2) WRITE(IFI,'(A)') ' PEAU EXTERNE'
                       ELSE
                        WRITE(IFI,'(A,I3)') ' COUCHE NUMERO:',ICOU
                       END IF
                     END IF
                  END IF
                  DO 18 IN=1,NBNO
                     NUNO = CONNEX(IPOIN-1+IN)
                     IF ( NBNOT .NE. 0 ) THEN
                        DO 187 IINO = 1 , NBNOT
                           IF ( NUNO .EQ. NUMNOE(IINO) ) GOTO 189
 187                    CONTINUE
                        GOTO 18
 189                    CONTINUE
                     ENDIF
                     NOMNO= NOMNOS(NUNO)
                     J=IACHML-1+NCMPP*ICOEF*(IN-1)
     &                                     +(ICOU-1)*NCMPP*ICOEF*NBNO
                     IF (NBCMPT.EQ.0) THEN
                       DO 50 I=1,NCMP2
                         IF ( NCMP .GT. 0 ) THEN
                            DO 511 JCO=1,ICOEF2
                              ZR(IVAL-1+(I-1)*ICOEF2+JCO)=
     &                                    VALE(J+I+(NUCMP(JCO)-1)*NCMPP)
                              ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
  511                       CONTINUE
                         ELSE
                            DO 51 JCO=1,ICOEF2
                              ZR(IVAL-1+(I-1)*ICOEF2+JCO)=
     &                                           VALE(J+I+(JCO-1)*NCMPP)
                              ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
   51                       CONTINUE
                         ENDIF
   50                  CONTINUE
                     ELSE
                       DO 60 I=1,NCMP2
                         INU=ZI(IPOSV-1+I)
                         IF ( NCMP .GT. 0 ) THEN
                            DO 701 JCO=1,ICOEF2
                              ZR(IVAL-1+(I-1)*ICOEF2+JCO)=
     &                                  VALE(J+INU+(NUCMP(JCO)-1)*NCMPP)
                              ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
  701                       CONTINUE
                         ELSE
                            DO 70 JCO=1,ICOEF2
                              ZR(IVAL-1+(I-1)*ICOEF2+JCO)=
     &                                         VALE(J+INU+(JCO-1)*NCMPP)
                              ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
   70                       CONTINUE
                         ENDIF
   60                  CONTINUE
                     ENDIF
C
C --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
C
                     IF(LSUP.OR.LINF) THEN
                       DO 65 IVA=1,ICOEF2*NCMP2
                         IF(LSUP) THEN
                           IF((ZR(IVAL-1+IVA)-BORSUP).GT.0.D0)
     &                       ZI(ICOE-1+IVA)=0
                         ENDIF
                         IF(LINF) THEN
                           IF((ZR(IVAL-1+IVA)-BORINF).LT.0.D0)
     &                       ZI(ICOE-1+IVA)=0
                         ENDIF
 65                    CONTINUE
C
C --- RETASSAGE POUR IMPRIMER COMPOSANTES PRESENTES DANS L'INTERVALLE --
C
                       ICOMP2=0
                       DO 66 I=1,ICOEF2*NCMP2
                         IF(ZI(ICOE-1+I).NE.0) THEN
                           ICOMP2=ICOMP2+1
                           ZI(ICOE-1+ICOMP2)=ZI(ICOE-1+I)
                           ZI(IPO2-1+ICOMP2)=ZI(IPOSG-1+I)
                           ZR(IVAL-1+ICOMP2)=ZR(IVAL-1+I)
                           ZK16(INOP-1+ICOMP2)=ZK16(INOM-1+I)
                         ENDIF
   66                  CONTINUE
                       IF(ICOMP2.EQ.0)  GOTO 18
C
C -- IMPRESSION  --
C
                       IF (.NOT.LMAX.AND..NOT.LMIN) THEN
                         ILIG=(ICOMP2+NDIM)/6
                         IRES=(ICOMP2+NDIM)-ILIG*6
                         FMT1 = ' '
                         FMT2 = ' '
                         IF (IRES.NE.0) THEN
                           FMT1 = '(9X,6(1X,'//FORCMP//
     &                            '),30(/,9X,6(1X,'//FORCMP//')))'
                           FMT2 = '(1X,A8,6(1X,'//FORMAT//
     &                            '),30(/,9X,6(1X,'//FORMAT//')))'
                         ELSEIF (IRES.EQ.0.AND.ILIG.EQ.1) THEN
                           FMT1 = '(9X,6(1X,'//FORCMP//'))'
                           FMT2 = '(1X,A8,6(1X,'//FORMAT//'))'
                         ELSE
                          WRITE(FMT1,'(A,A8,A,I2,A,A8,A)')'(9X,6(1X,',
     &                   FORCMP,'),',(ILIG-1),'(/,9X,6(1X,',FORCMP,')))'
                          WRITE(FMT2,'(A,A10,A,I2,A,A10,A)')
     &                               '(1X,A8,6(1X,', FORMAT, '),',
     &                          (ILIG-1), '(/,9X,6(1X,', FORMAT, ')))'
                         ENDIF
                         IF (LSUP.OR.LINF) THEN
                           IF (LIMPR) THEN
                              WRITE(IFI,'(A,I2,A)') NOMEL(IEL)
                              LIMPR=.FALSE.
                           ENDIF
                         ENDIF
                         IF (NDIM.EQ.0) THEN
                           WRITE(IFI,FMT1) (ZK16(INOP-1+I)(1:11),
     &                                                       I=1,ICOMP2)
                           WRITE(IFI,FMT2) NOMNO,(ZR(IVAL-1+ICMP),
     &                                             ICMP=1,ICOMP2)
                         ELSE
                           WRITE(IFI,FMT1) (NOMCOR(I),I=1,NDIM),
     &                                 (ZK16(INOP-1+I)(1:11),I=1,ICOMP2)
                           WRITE(IFI,FMT2) NOMNO,(COOR((NUNO-1)*3+I),
     &                     I=1,NDIM),(ZR(IVAL-1+ICMP),ICMP=1,ICOMP2)
                         ENDIF
                       ENDIF
                       NBCPT=ICOMP2
                     ELSE
                       IF (.NOT.LMAX.AND..NOT.LMIN) THEN
                         IF (NDIM.EQ.0) THEN
                           WRITE(IFI,FMV) NOMNO,(ZR(IVAL-1+ICMP),
     &                                        ICMP=1,ICOEF2*NCMP2)
                         ELSE
                           WRITE(IFI,FMV) NOMNO,(COOR((NUNO-1)*3+I),
     &                                   I=1,NDIM),(ZR(IVAL-1+ICMP),
     &                                              ICMP=1,ICOEF2*NCMP2)
                         ENDIF
                       ENDIF
                       NBCPT=ICOEF2*NCMP2
                     ENDIF
C
C -- RECHERCHE DE LA VALEUR MAXIMALE ---
C
                     IF(LMAX) THEN
                       DO 103 I=1,NBCPT
                        IF(LSUP.OR.LINF) THEN
                          IADR=(ZI(IPO2-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                        ELSE
                          IADR=(ZI(IPOSG-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                        ENDIF
                        IF(ZR(IMAX-1+IADR).EQ.RUNDF) THEN
                          ZR(IMAX-1+IADR) = ZR(IVAL-1+I)
                          ZK8(INMAX-1+IADR) = NOMEL(IEL)
                          ZI(IVMAX-1+IADR) = 1
                        ELSEIF(ZR(IVAL-1+I).GT.ZR(IMAX-1+IADR)) THEN
                          ZR(IMAX-1+IADR)= ZR(IVAL-1+I)
                          ZK8(INMAX-1+IADR)= NOMEL(IEL)
                          ZI(IVMAX-1+IADR)= 1
                        ELSEIF(ZR(IVAL-1+I).EQ.ZR(IMAX-1+IADR)) THEN
                          ZI(IVMAX-1+IADR)=ZI(IVMAX-1+IADR)+1
                        ENDIF
  103                  CONTINUE
                     ENDIF
C
C -- RECHERCHE DE LA VALEURE MINIMALE ---
C
                     IF(LMIN) THEN
                       DO 104 I=1,NBCPT
                        IF(LSUP.OR.LINF) THEN
                          IADR=(ZI(IPO2-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                        ELSE
                          IADR=(ZI(IPOSG-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                        ENDIF
                        IF(ZR(IMIN-1+IADR).EQ.RUNDF) THEN
                          ZR(IMIN-1+IADR) = ZR(IVAL-1+I)
                          ZK8(INMIN-1+IADR) = NOMEL(IEL)
                          ZI(IVMIN-1+IADR) = 1
                        ELSEIF(ZR(IVAL-1+I).LT.ZR(IMIN-1+IADR)) THEN
                          ZR(IMIN-1+IADR)= ZR(IVAL-1+I)
                          ZK8(INMIN-1+IADR)= NOMEL(IEL)
                          ZI(IVMIN-1+IADR)= 1
                        ELSEIF(ZR(IVAL-1+I).EQ.ZR(IMIN-1+IADR)) THEN
                          ZI(IVMIN-1+IADR)= ZI(IVMIN-1+IADR)+1
                        ENDIF
  104                  CONTINUE
                     ENDIF
   18             CONTINUE
   17          CONTINUE
            END IF
   12 CONTINUE
      WRITE (IFI,*) ' '
C
C --- IMPRESSION DE LA VALEUR MAXIMALE ---
C
      IF(LMAX) THEN
        DO 95 I=1,NCMPMX*ICOEF2
          IF(ZR(IMAX-1+I).NE.RUNDF) THEN
            FORM1 = '(1X,3A,1X,'//FORMAT//',A,I4,2A)'
            WRITE(IFI,FORM1) 'LA VALEUR MAXIMALE DE ', ZK16(INOT-1+I),
     &       ' EST ',ZR(IMAX-1+I),
     &       ' EN ',ZI(IVMAX-1+I),' MAILLE(S) : ',ZK8(INMAX-1+I)
          ENDIF
 95     CONTINUE
      ENDIF
C
C --- IMPRESSION DE LA VALEUR MINIMALE ---
C
      IF(LMIN) THEN
        DO 96 I=1,NCMPMX*ICOEF2
          IF(ZR(IMIN-1+I).NE.RUNDF) THEN
            FORM1 = '(1X,3A,1X,'//FORMAT//',A,I4,2A)'
            WRITE(IFI,FORM1) 'LA VALEUR MINIMALE DE ', ZK16(INOT-1+I),
     &       ' EST ',ZR(IMIN-1+I),
     &       ' EN ',ZI(IVMIN-1+I),' MAILLE(S) : ',ZK8(INMIN-1+I)
          ENDIF
 96     CONTINUE
      ENDIF
C
      CALL JEDETR('&&IRCERL.NCMT')
      CALL JEDETR('&&IRCERL.MAX')
      CALL JEDETR('&&IRCERL.MAIMAX')
      CALL JEDETR('&&IRCERL.NBVMAX')
      CALL JEDETR('&&IRCERL.MIN')
      CALL JEDETR('&&IRCERL.MAIMIN')
      CALL JEDETR('&&IRCERL.NBVMIN')
      CALL JEDETR('&&IRCERL.ENT_COD')
      CALL JEDETR('&&IRCERL.POSG')
      CALL JEDETR('&&IRCERL.POSV')
      CALL JEDETR('&&IRCERL.COEF')
      CALL JEDETR('&&IRCERL.NCMP')
      CALL JEDETR('&&IRCERL.NCPP')
      CALL JEDETR('&&IRCERL.PO2')
      CALL JEDETR('&&IRCERL.VAL')
C
 9999 CONTINUE
      CALL JEDEMA ( )
      END
