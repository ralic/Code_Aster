      SUBROUTINE IRCECL(IFI,NBEL,LIGREL,NBGREL,LONGR,NCMPMX,VALE,
     &                  NOMCMP,NOMEL,LOC,CELD,CONNEX,POINT,NOMNOS,
     &                  NBCMPT,NUCMPU,NBNOT,NUMNOE,NBMAT,NUMMAI,LSUP,
     &                  BORSUP,LINF,BORINF,LMAX,LMIN,LCOR,NDIM,COOR,
     &                  NOLILI,FORMR,NCMPV,NUCMP)
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER           IFI,NBEL,LIGREL(*),NBGREL,LONGR(*),NCMPMX,NBNOT,
     &                  NBCMPT,NUCMPU(*),CELD(*),CONNEX(*),POINT(*),
     &                  NUMNOE(*),NBMAT,NDIM,NUMMAI(*),NCMPV,NUCMP(*)
      REAL*8            BORSUP,BORINF,COOR(*)
      COMPLEX*16        VALE(*)
      CHARACTER*(*)     NOMCMP(*),NOMEL(*),LOC,NOMNOS(*),FORMR
      CHARACTER*19      NOLILI
      LOGICAL           LSUP,LINF,LMAX,LMIN,LCOR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C        A VALEURS COMPLEXES
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
      INTEGER       DIGDEL,ILONG,IMODEL
      REAL*8        RUNDF, VALUE, VALMAX, VALMIN
      CHARACTER*3   CBID
      CHARACTER*8   NOMNO, NOMCP, KBID, FORCMP, NOMCOR(3)
      CHARACTER*10  FORMAT
      CHARACTER*24  NREPE
      CHARACTER*50  FMT, FMV, FMT1, FMT2, FMT3, FMV2, FORM1
      LOGICAL       EXISDG, LIMPR
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,I2 ,IACHML ,IAD ,IADR ,IAEC ,ICM
      INTEGER ICMAX ,ICMIN ,ICMP ,ICMP2 ,ICOE ,ICOEF ,ICOEF2
      INTEGER ICOMAX ,ICOMP2 ,ICOU ,ICVAL ,ID ,IEL ,IELG
      INTEGER IF ,IGRE ,IGREL ,IINO ,ILIG ,IMAI ,IMAIL
      INTEGER IN ,INMAX ,INMIN ,INOM ,INOP ,INOT ,INU
      INTEGER IPCA ,IPO2 ,IPOIN ,IPOIN1 ,IPOSG ,IPOSV ,IREPE
      INTEGER IRES ,IRMAX ,IRMIN ,IRVAL ,IVA ,IVMAX ,IVMIN
      INTEGER J ,JCO ,JMOD ,LGR ,LXLGUT ,MODE ,MODSAU
      INTEGER NBCPT ,NBNO ,NCMP ,NCMP2 ,NCMPP ,NCOU ,NEC
      INTEGER NPCALC ,NSCA ,NSCAL ,NUNO,NBEC
      REAL*8 R8VIDE
C-----------------------------------------------------------------------
      CALL JEMARQ ( )
      KBID='        '
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
        CALL JEDETR('&&IRCECL.NCMT')
        CALL WKVECT('&&IRCECL.NCMT','V V K16',NCMPMX*ICOMAX,INOT)
        DO 6 I=1,NCMPMX
         IF ( ICOMAX.GT.1 .OR. NCMP.GE.1 ) THEN
             DO 7 JCO = 1 , ICOMAX
                IF ( NCMP .GT. 0 ) THEN
                   CALL CODENT ( NUCMP(JCO), 'G', CBID )
                ELSE
                   CALL CODENT ( JCO, 'G', CBID )
                ENDIF
                NOMCP = NOMCMP(I)
                ZK16(INOT-1+(I-1)*ICOMAX+JCO) = 'V'//CBID
  7          CONTINUE
          ELSE
             ZK16(INOT-1+I)=NOMCMP(I)
          ENDIF
  6      CONTINUE
      ENDIF
      IF ( LMAX ) THEN
        CALL JEDETR('&&IRCECL.MAXR')
        CALL WKVECT('&&IRCECL.MAXR','V V R',NCMPMX*ICOMAX,IRMAX)
        CALL JEDETR('&&IRCECL.MAXC')
        CALL WKVECT('&&IRCECL.MAXC','V V R',NCMPMX*ICOMAX,ICMAX)
        CALL JEDETR('&&IRCECL.MAIMAX')
        CALL WKVECT('&&IRCECL.MAIMAX','V V K8',NCMPMX*ICOMAX,INMAX)
        CALL JEDETR('&&IRCECL.NBVMAX')
        CALL WKVECT('&&IRCECL.NBVMAX','V V I',NCMPMX*ICOMAX,IVMAX)
        DO 90 I=1,NCMPMX*ICOMAX
           ZR(IRMAX-1+I)=RUNDF
 90     CONTINUE
      ENDIF
      IF ( LMIN ) THEN
        CALL JEDETR('&&IRCECL.MINR')
        CALL WKVECT('&&IRCECL.MINR','V V R',NCMPMX*ICOMAX,IRMIN)
        CALL JEDETR('&&IRCECL.MINC')
        CALL WKVECT('&&IRCECL.MINC','V V R',NCMPMX*ICOMAX,ICMIN)
        CALL JEDETR('&&IRCECL.MAIMIN')
        CALL WKVECT('&&IRCECL.MAIMIN','V V K8',NCMPMX*ICOMAX,INMIN)
        CALL JEDETR('&&IRCECL.NBVMIN')
        CALL WKVECT('&&IRCECL.NBVMIN','V V I',NCMPMX*ICOMAX,IVMIN)
        DO 91 I=1,NCMPMX*ICOMAX
           ZR(IRMIN-1+I)=RUNDF
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
           CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',MODE),'L',JMOD)
           NEC = NBEC(ZI(JMOD-1+2))
           CALL JEDETR('&&IRCECL.ENT_COD')
           CALL WKVECT('&&IRCECL.ENT_COD','V V I',NEC,IAEC)
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
           CALL JEDETR('&&IRCECL.POSG')
           CALL WKVECT('&&IRCECL.POSG','V V I',NCMPMX*ICOEF2,IPOSG)
           CALL JEDETR('&&IRCECL.POSV')
           CALL WKVECT('&&IRCECL.POSV','V V I',NCMPMX,IPOSV)
           CALL JEDETR('&&IRCECL.COEF')
           CALL WKVECT('&&IRCECL.COEF','V V I',NCMPMX*ICOEF2,ICOE)
           CALL JEDETR('&&IRCECL.NCMP')
           CALL WKVECT('&&IRCECL.NCMP','V V K16',NCMPMX*ICOEF2,INOM)
           IF (LSUP.OR.LINF) THEN
            CALL JEDETR('&&IRCECL.NCPP')
            CALL WKVECT('&&IRCECL.NCPP','V V K16',NCMPMX*ICOEF2,INOP)
            CALL JEDETR('&&IRCECL.PO2')
            CALL WKVECT('&&IRCECL.PO2','V V I',NCMPMX*ICOEF2,IPO2)
           ENDIF
           CALL JEDETR('&&IRCECL.VALR')
           CALL WKVECT('&&IRCECL.VALR','V V R',NCMPMX*ICOEF2,IRVAL)
           CALL JEDETR('&&IRCECL.VALC')
           CALL WKVECT('&&IRCECL.VALC','V V R',NCMPMX*ICOEF2,ICVAL)
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
                       ZI(IPOSV-1+ICM) = NCMPP
                    ENDIF
 8                CONTINUE
               ELSE
                  DO 93 JCO=1,ICOEF2
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
 9          CONTINUE
           ENDIF
C
C --- STOCKAGE DES NOMS DE COMPOSANTES ---
           DO 42 I=1,NCMP2
            IF( ICOEF2.GT.1 .OR. NCMP.GE.1 ) THEN
             DO 43 JCO=1,ICOEF2
                IF ( NCMP .GT. 0 ) THEN
                   CALL CODENT ( NUCMP(JCO), 'G', CBID )
                ELSE
                   CALL CODENT ( JCO, 'G', CBID )
                ENDIF
                NOMCP = NOMCMP(ZI(IPOSG-1+I))
                ZK16(INOM-1+(I-1)*ICOEF2+JCO) = 'V'//CBID
   43        CONTINUE
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
            FMT  = ' '
            FMV  = ' '
            FMV2 = ' '
            IF (IRES.NE.0) THEN
              FMT  = '(1X,A8,6(1X,'//FORCMP//'),30(/,9X,6(1X,'//
     &                               FORCMP//')))'
             IF(LOC.EQ.'ELNO') THEN
               FMV  = '(1X,A8,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     &                                FORMAT//')))'
             ELSEIF(LOC.EQ.'ELGA') THEN
               FMV  = '(2X,I7,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     &                                FORMAT//')))'
               FMV2 = '(9X,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     &                             FORMAT//')))'
             ELSEIF(LOC.EQ.'ELEM') THEN
               FMV  = '(9X,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     &                             FORMAT//')))'
               FMV2 = '(9X,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     &                             FORMAT//')))'
             ENDIF
            ELSEIF (IRES.EQ.0.AND.ILIG.EQ.1) THEN
             FMT = '(1X,A8,6(1X,'//FORCMP//'))'
             IF(LOC.EQ.'ELNO') THEN
               FMV = '(1X,A8,6(1X,'//FORMAT//'))'
             ELSEIF(LOC.EQ.'ELGA') THEN
               FMV  = '(2X,I7,6(1X,'//FORMAT//'))'
               FMV2 = '(9X,6(1X,'//FORMAT//'))'
             ELSEIF(LOC.EQ.'ELEM') THEN
               FMV  = '(9X,6(1X,'//FORMAT//'))'
               FMV2 = '(9X,6(1X,'//FORMAT//'))'
             ENDIF
            ELSE
             WRITE(FMT,'(A,A8,A,I2,A,A8,A)') '(1X,A8,6(1X,',FORCMP,
     &                 '),',(ILIG-1),'(/,9X,6(1X,',FORCMP,')))'
             IF (LOC.EQ.'ELNO') THEN
               WRITE(FMV,'(A,A10,A,I2,A,A10,A)') '(1X,A8,6(1X,',FORMAT,
     &                   '),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
             ELSEIF(LOC.EQ.'ELGA') THEN
               WRITE(FMV,'(A,A10,A,I2,A,A10,A)') '(2X,I7,6(1X,',FORMAT,
     &                   '),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
               WRITE(FMV2,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',FORMAT,
     &                    '),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
             ELSEIF(LOC.EQ.'ELEM') THEN
               WRITE(FMV ,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',FORMAT,
     &                    '),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
               WRITE(FMV2,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',FORMAT,
     &                    '),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
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
                          ZR(IRVAL-1+(I-1)*ICOEF2+JCO)=
     &                             DBLE(VALE(J+I+(NUCMP(JCO)-1)*NCMPP))
                          ZR(ICVAL-1+(I-1)*ICOEF2+JCO)=
     &                             DIMAG(VALE(J+I+(NUCMP(JCO)-1)*NCMPP))
                          ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
  551                   CONTINUE
                      ELSE
                        DO 55 JCO=1,ICOEF2
                          ZR(IRVAL-1+(I-1)*ICOEF2+JCO)=
     &                                    DBLE(VALE(J+I+(JCO-1)*NCMPP))
                          ZR(ICVAL-1+(I-1)*ICOEF2+JCO)=
     &                                    DIMAG(VALE(J+I+(JCO-1)*NCMPP))
                          ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
   55                   CONTINUE
                      ENDIF
   10               CONTINUE
                 ELSE
                    DO 20 I=1,NCMP2
                      INU=ZI(IPOSV-1+I)
                      IF ( NCMP .GT. 0 ) THEN
                        DO 301 JCO=1,ICOEF2
                          ZR(IRVAL-1+(I-1)*ICOEF2+JCO)=
     &                           DBLE(VALE(J+INU+(NUCMP(JCO)-1)*NCMPP))
                          ZR(ICVAL-1+(I-1)*ICOEF2+JCO)=
     &                           DIMAG(VALE(J+INU+(NUCMP(JCO)-1)*NCMPP))
                          ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
  301                   CONTINUE
                      ELSE
                        DO 30 JCO=1,ICOEF2
                          ZR(IRVAL-1+(I-1)*ICOEF2+JCO)=
     &                                  DBLE(VALE(J+INU+(JCO-1)*NCMPP))
                          ZR(ICVAL-1+(I-1)*ICOEF2+JCO)=
     &                                  DIMAG(VALE(J+INU+(JCO-1)*NCMPP))
                          ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
   30                   CONTINUE
                      ENDIF
   20               CONTINUE
                 ENDIF
C
C --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
C
                 IF( LSUP .OR. LINF ) THEN
                   DO 35 IVA=1,ICOEF2*NCMP2
                     VALUE = SQRT(ZR(IRVAL-1+IVA)**2+ZR(ICVAL-1+IVA)**2)
                     IF(LSUP) THEN
                       IF((VALUE-BORSUP).GT.0.D0) ZI(ICOE-1+IVA)=0
                     ENDIF
                     IF(LINF) THEN
                       IF((VALUE-BORINF).LT.0.D0) ZI(ICOE-1+IVA)=0
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
                          ZR(IRVAL-1+ICOMP2)=ZR(IRVAL-1+I)
                          ZR(ICVAL-1+ICOMP2)=ZR(ICVAL-1+I)
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
                     FMT3 = ' '
                     IF (LOC.EQ.'ELGA') THEN
                       IF (IRES.NE.0) THEN
                         FMT1 = '(9X,6(1X,'//FORCMP//'),30(/,9X,6(1X,'
     &                                     //FORCMP//')))'
                      FMT2 = '(2X,I7,6(1X,'//FORMAT//'),30(/,9X,6(1X,'
     &                                     //FORMAT//')))'
                         FMT3 = '(9X,6(1X,'//FORMAT//'),30(/,9X,6(1X,'
     &                                     //FORMAT//')))'
                       ELSEIF (IRES.EQ.0.AND.ILIG.EQ.1) THEN
                         FMT1 = '(9X,6(1X,'//FORCMP//'))'
                         FMT2 = '(2X,I7,6(1X,'//FORMAT//'))'
                         FMT3 = '(9X,6(1X,'//FORMAT//'))'
                       ELSE
                       WRITE(FMT1,'(A,A8,A,I2,A,A8,A)') '(1X,A8,6(1X,',
     &                   FORCMP,'),',(ILIG-1),'(/,9X,6(1X,',FORCMP,')))'
                       WRITE(FMT2,'(A,A10,A,I2,A,A10,A)')'(2X,I7,6(1X,',
     &                   FORMAT,'),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
                       WRITE(FMT3,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',
     &                   FORMAT,'),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
                       ENDIF
                     ELSE
                       IF (IRES.NE.0) THEN
                         FMT1 = '(9X,6(1X,'//FORCMP//'),30(/,9X,6(1X,'
     &                                     //FORCMP//')))'
                         FMT2 = '(9X,6(1X,'//FORMAT//'),30(/,9X,6(1X,'
     &                                     //FORMAT//')))'
                         FMT3 = '(9X,6(1X,'//FORMAT//'),30(/,9X,6(1X,'
     &                                     //FORMAT//')))'
                       ELSEIF (IRES.EQ.0.AND.ILIG.EQ.1) THEN
                         FMT1 = '(9X,6(1X,'//FORCMP//'))'
                         FMT2 = '(9X,6(1X,'//FORMAT//'))'
                         FMT3 = '(9X,6(1X,'//FORMAT//'))'
                       ELSE
                       WRITE(FMT1,'(A,A8,A,I2,A,A8,A)') '(1X,A8,6(1X,',
     &                   FORCMP,'),',(ILIG-1),'(/,9X,6(1X,',FORCMP,')))'
                       WRITE(FMT2,'(A,A10,A,I2,A,A10,A)')'(9X,6(1X,',
     &                   FORMAT,'),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
                       WRITE(FMT3,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',
     &                   FORMAT,'),',(ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
                       ENDIF
                     ENDIF
                     IF (LSUP.OR.LINF) THEN
                       IF (LIMPR) THEN
                         WRITE(IFI,'(A,I2,A)') NOMEL(IEL)
                         LIMPR=.FALSE.
                       ENDIF
                     ENDIF
                     WRITE(IFI,FMT1) (ZK16(INOP-1+I)(1:11),I=1,ICOMP2)
                     WRITE(IFI,FMT2) IPCA,(ZR(IRVAL-1+ICMP),
     &                                              ICMP=1,ICOMP2)
                     WRITE(IFI,FMT3) (ZR(ICVAL-1+ICMP),
     &                                              ICMP=1,ICOMP2)
                   ENDIF
                   NBCPT=ICOMP2
                 ELSE
                   IF (.NOT.LMAX.AND..NOT.LMIN) THEN
                     IF (LOC.EQ.'ELGA') THEN
                       WRITE(IFI,FMV) IPCA,(ZR(IRVAL-1+ICMP),
     &                                            ICMP=1,ICOEF2*NCMP2)
                       WRITE(IFI,FMV2) (ZR(ICVAL-1+ICMP),
     &                                            ICMP=1,ICOEF2*NCMP2)
                     ELSE
                       WRITE(IFI,FMV)  (ZR(IRVAL-1+ICMP),
     &                                            ICMP=1,ICOEF2*NCMP2)
                       WRITE(IFI,FMV2) (ZR(ICVAL-1+ICMP),
     &                                            ICMP=1,ICOEF2*NCMP2)
                     ENDIF
                   ENDIF
                   NBCPT=ICOEF2*NCMP2
                 ENDIF
C
C -- RECHERCHE DE LA VALEUR MAXIMALE ---
C
                  IF(LMAX) THEN
                    DO 101 I=1,NBCPT
                     IF(LSUP.OR.LINF) THEN
                       IADR=(ZI(IPO2-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                     ELSE
                       IADR=(ZI(IPOSG-1+I)-1)*ICOEF2+ZI(ICOE-1+I)
                     ENDIF
                     IF(ZR(IRMAX-1+IADR).EQ.RUNDF) THEN
                       ZR(IRMAX-1+IADR) = ZR(IRVAL-1+I)
                       ZR(ICMAX-1+IADR) = ZR(ICVAL-1+I)
                       ZK8(INMAX-1+IADR) = NOMEL(IEL)
                       ZI(IVMAX-1+IADR) = 1
                     ELSE
                       VALMAX = SQRT(ZR(IRMAX-1+IADR)**2 +
     &                             ZR(ICMAX-1+IADR)**2)
                       VALUE = SQRT(ZR(IRVAL-1+I)**2 +
     &                             ZR(ICVAL-1+I)**2)
                       IF(VALUE.GT.VALMAX) THEN
                         ZR(IRMAX-1+IADR)= ZR(IRVAL-1+I)
                         ZR(ICMAX-1+IADR)= ZR(ICVAL-1+I)
                         ZK8(INMAX-1+IADR) = NOMEL(IEL)
                         ZI(IVMAX-1+IADR) = 1
                       ELSEIF(VALUE.EQ.VALMAX) THEN
                         ZI(IVMAX-1+IADR)=ZI(IVMAX-1+IADR)+1
                       ENDIF
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
                     IF(ZR(IRMIN-1+IADR).EQ.RUNDF) THEN
                       ZR(IRMIN-1+IADR) = ZR(IRVAL-1+I)
                       ZR(ICMIN-1+IADR) = ZR(ICVAL-1+I)
                       ZK8(INMIN-1+IADR) = NOMEL(IEL)
                       ZI(IVMIN-1+IADR) = 1
                     ELSE
                       VALMIN = SQRT(ZR(IRMIN-1+IADR)**2 +
     &                             ZR(ICMIN-1+IADR)**2)
                       VALUE = SQRT(ZR(IRVAL-1+I)**2 +
     &                             ZR(ICVAL-1+I)**2)
                       IF(VALUE.LT.VALMIN) THEN
                         ZR(IRMIN-1+IADR)= ZR(IRVAL-1+I)
                         ZR(ICMIN-1+IADR)= ZR(ICVAL-1+I)
                         ZK8(INMIN-1+IADR) = NOMEL(IEL)
                         ZI(IVMIN-1+IADR) = 1
                       ELSEIF(VALUE.EQ.VALMIN) THEN
                         ZI(IVMIN-1+IADR)=ZI(IVMIN-1+IADR)+1
                       ENDIF
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
                             ZR(IRVAL-1+(I-1)*ICOEF2+JCO)=
     &                             DBLE(VALE(J+I+(NUCMP(JCO)-1)*NCMPP))
                             ZR(ICVAL-1+(I-1)*ICOEF2+JCO)=
     &                             DIMAG(VALE(J+I+(NUCMP(JCO)-1)*NCMPP))
                             ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
  511                      CONTINUE
                         ELSE
                           DO 51 JCO=1,ICOEF2
                             ZR(IRVAL-1+(I-1)*ICOEF2+JCO)=
     &                                    DBLE(VALE(J+I+(JCO-1)*NCMPP))
                             ZR(ICVAL-1+(I-1)*ICOEF2+JCO)=
     &                                    DIMAG(VALE(J+I+(JCO-1)*NCMPP))
                             ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
   51                      CONTINUE
                         ENDIF
   50                  CONTINUE
                     ELSE
                       DO 60 I=1,NCMP2
                         INU=ZI(IPOSV-1+I)
                         IF ( NCMP .GT. 0 ) THEN
                           DO 701 JCO=1,ICOEF2
                             ZR(IRVAL-1+(I-1)*ICOEF2+JCO)=
     &                           DBLE(VALE(J+INU+(NUCMP(JCO)-1)*NCMPP))
                             ZR(ICVAL-1+(I-1)*ICOEF2+JCO)=
     &                           DIMAG(VALE(J+INU+(NUCMP(JCO)-1)*NCMPP))
                             ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
  701                      CONTINUE
                         ELSE
                           DO 70 JCO=1,ICOEF2
                             ZR(IRVAL-1+(I-1)*ICOEF2+JCO)=
     &                                  DBLE(VALE(J+INU+(JCO-1)*NCMPP))
                             ZR(ICVAL-1+(I-1)*ICOEF2+JCO)=
     &                                  DIMAG(VALE(J+INU+(JCO-1)*NCMPP))
                             ZI(ICOE-1+(I-1)*ICOEF2+JCO)=JCO
   70                      CONTINUE
                         ENDIF
   60                  CONTINUE
                     ENDIF
C
C --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
C
                     IF(LSUP.OR.LINF) THEN
                       DO 65 IVA=1,ICOEF2*NCMP2
                         VALUE= SQRT(ZR(IRVAL-1+IVA)**2+
     &                               ZR(ICVAL-1+IVA)**2)
                         IF(LSUP) THEN
                           IF((VALUE-BORSUP).GT.0.D0)  ZI(ICOE-1+IVA)=0
                         ENDIF
                         IF(LINF) THEN
                           IF((VALUE-BORINF).LT.0.D0)  ZI(ICOE-1+IVA)=0
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
                           ZR(IRVAL-1+ICOMP2)=ZR(IRVAL-1+I)
                           ZR(ICVAL-1+ICOMP2)=ZR(ICVAL-1+I)
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
                          WRITE(FMT1,'(A,A8,A,I2,A,A8,A)')
     &                               '(9X,6(1X,',FORCMP,'),',
     &                               (ILIG-1),'(/,9X,6(1X,',FORCMP,')))'
                          WRITE(FMT2,'(A,A10,A,I2,A,A10,A)')
     &                               '(1X,A8,6(1X,',FORMAT,'),',
     &                              (ILIG-1),'(/,9X,6(1X,',FORMAT,')))'
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
                           WRITE(IFI,FMT2) NOMNO,(ZR(IRVAL-1+ICMP),
     &                                             ICMP=1,ICOMP2)
                           WRITE(IFI,FMT2) KBID,(ZR(ICVAL-1+ICMP),
     &                                             ICMP=1,ICOMP2)
                         ELSE
                           WRITE(IFI,FMT1) (NOMCOR(I),I=1,NDIM),
     &                                 (ZK16(INOP-1+I)(1:11),I=1,ICOMP2)
                           WRITE(IFI,FMT2) NOMNO,(COOR((NUNO-1)*3+I),
     &                     I=1,NDIM),(ZR(IRVAL-1+ICMP),ICMP=1,ICOMP2)
                           WRITE(IFI,FMT2) KBID,(COOR((NUNO-1)*3+I),
     &                     I=1,NDIM),(ZR(ICVAL-1+ICMP),ICMP=1,ICOMP2)
                         ENDIF
                       ENDIF
                       NBCPT=ICOMP2
                     ELSE
                       IF (.NOT.LMAX.AND..NOT.LMIN) THEN
                         IF (NDIM.EQ.0) THEN
                           WRITE(IFI,FMV) NOMNO,(ZR(IRVAL-1+ICMP),
     &                                        ICMP=1,ICOEF2*NCMP2)
                           WRITE(IFI,FMV) KBID,(ZR(ICVAL-1+ICMP),
     &                                        ICMP=1,ICOEF2*NCMP2)
                         ELSE
                           WRITE(IFI,FMV) NOMNO,(COOR((NUNO-1)*3+I),
     &                                   I=1,NDIM),(ZR(IRVAL-1+ICMP),
     &                                              ICMP=1,ICOEF2*NCMP2)
                           WRITE(IFI,FMV) KBID,(COOR((NUNO-1)*3+I),
     &                                   I=1,NDIM),(ZR(ICVAL-1+ICMP),
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
                        IF(ZR(IRMAX-1+IADR).EQ.RUNDF) THEN
                          ZR(IRMAX-1+IADR) = ZR(IRVAL-1+I)
                          ZR(ICMAX-1+IADR) = ZR(ICVAL-1+I)
                          ZK8(INMAX-1+IADR) = NOMEL(IEL)
                          ZI(IVMAX-1+IADR) = 1
                        ELSE
                          VALMAX=SQRT(ZR(IRMAX-1+IADR)**2 +
     &                                 ZR(ICMAX-1+IADR)**2)
                          VALUE=SQRT(ZR(IRVAL-1+I)**2 +
     &                                 ZR(ICVAL-1+I)**2)
                          IF(VALUE.GT.VALMAX) THEN
                            ZR(IRMAX-1+IADR)= ZR(IRVAL-1+I)
                            ZR(ICMAX-1+IADR)= ZR(ICVAL-1+I)
                            ZK8(INMAX-1+IADR) = NOMEL(IEL)
                            ZI(IVMAX-1+IADR) = 1
                          ELSEIF(VALUE.EQ.VALMAX) THEN
                            ZI(IVMAX-1+IADR)=ZI(IVMAX-1+IADR)+1
                          ENDIF
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
                        IF(ZR(IRMIN-1+IADR).EQ.RUNDF) THEN
                          ZR(IRMIN-1+IADR) = ZR(IRVAL-1+I)
                          ZR(ICMIN-1+IADR) = ZR(ICVAL-1+I)
                          ZK8(INMIN-1+IADR) = NOMEL(IEL)
                          ZI(IVMIN-1+IADR) = 1
                        ELSE
                          VALMIN=SQRT(ZR(IRMIN-1+IADR)**2 +
     &                                 ZR(ICMIN-1+IADR)**2)
                          VALUE=SQRT(ZR(IRVAL-1+I)**2 +
     &                                 ZR(ICVAL-1+I)**2)
                          IF(VALUE.LT.VALMIN) THEN
                            ZR(IRMIN-1+IADR)= ZR(IRVAL-1+I)
                            ZR(ICMIN-1+IADR)= ZR(ICVAL-1+I)
                            ZK8(INMIN-1+IADR) = NOMEL(IEL)
                            ZI(IVMIN-1+IADR) = 1
                          ELSEIF(VALUE.EQ.VALMIN) THEN
                            ZI(IVMIN-1+IADR)=ZI(IVMIN-1+IADR)+1
                          ENDIF
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
          IF(ZR(IRMAX-1+I).NE.RUNDF) THEN
           FORM1 = '(1X,3A,'//FORMAT//',1X,'//FORMAT//',A,I4,2A)'
           WRITE(IFI,FORM1)'LA VALEUR MAXIMALE DE ',ZK16(INOT-1+I),
     &       ' EST ',ZR(IRMAX-1+I),ZR(ICMAX-1+I),
     &       ' EN ',ZI(IVMAX-1+I),' MAILLE(S) : ',ZK8(INMAX-1+I)
          ENDIF
 95     CONTINUE
      ENDIF
C
C --- IMPRESSION DE LA VALEUR MINIMALE ---
C
      IF(LMIN) THEN
        DO 96 I=1,NCMPMX*ICOEF2
          IF(ZR(IRMIN-1+I).NE.RUNDF) THEN
           FORM1 = '(1X,3A,'//FORMAT//',1X,'//FORMAT//',A,I4,2A)'
           WRITE(IFI,FORM1)'LA VALEUR MINIMALE DE ',ZK16(INOT-1+I),
     &       ' EST ',ZR(IRMIN-1+I),ZR(ICMIN-1+I),
     &       ' EN ',ZI(IVMIN-1+I),' MAILLE(S) : ',ZK8(INMIN-1+I)
          ENDIF
 96     CONTINUE
      ENDIF
C
      CALL JEDETR('&&IRCECL.NCMT')
      CALL JEDETR('&&IRCECL.MAXR')
      CALL JEDETR('&&IRCECL.MAXC')
      CALL JEDETR('&&IRCECL.MAIMAX')
      CALL JEDETR('&&IRCECL.NBVMAX')
      CALL JEDETR('&&IRCECL.MINR')
      CALL JEDETR('&&IRCECL.MINC')
      CALL JEDETR('&&IRCECL.MAIMIN')
      CALL JEDETR('&&IRCECL.NBVMIN')
      CALL JEDETR('&&IRCECL.ENT_COD')
      CALL JEDETR('&&IRCECL.POSG')
      CALL JEDETR('&&IRCECL.POSV')
      CALL JEDETR('&&IRCECL.COEF')
      CALL JEDETR('&&IRCECL.NCMP')
      CALL JEDETR('&&IRCECL.NCPP')
      CALL JEDETR('&&IRCECL.PO2')
      CALL JEDETR('&&IRCECL.VALR')
      CALL JEDETR('&&IRCECL.VALC')
C
 9999 CONTINUE
      CALL JEDEMA ( )
      END
