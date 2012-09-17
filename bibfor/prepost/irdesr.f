      SUBROUTINE IRDESR(IFI,NBNO,PRNO,NUEQ,NEC,DG,NCMPMX,VALE,
     +              NOMCMP,TITR,NOMNOE,NOMSD,NOMSYM,IR,NUMNOE,LMASU,
     +              NBCMP,NCMPS,NOCMPL)
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER           IFI,NBNO,PRNO(*),NUEQ(*),NEC,DG(*),NCMPMX
      INTEGER           IR,NUMNOE(*),NCMPS(*),NBCMP
      REAL*8            VALE(*)
      CHARACTER*(*)     NOMCMP(*),NOCMPL(*)
      CHARACTER*(*)     TITR,NOMNOE(*),NOMSD,NOMSYM
      LOGICAL           LMASU
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 18/09/2012   AUTEUR LADIER A.LADIER 
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
C--------------------------------------------------------------------
C        ECRITURE D'UN CHAM_NO SUR FICHIER UNIVERSEL, DATASET TYPE 55
C        A VALEURS REELLES
C      ENTREE:
C         IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
C                NBNO  : NOMBRE DE NOEUDS DU LIGREL ( DU MAILLAGE)
C         PRNO  : OBJET .PRNO DU PROF_CHNO
C         NUEQ  : OBJET .NUEQ DU PROF_CHNO
C         NEC   : NOMBRE D'ENTIERS-CODES
C         DG    : ENTIERS CODES
C         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR
C         VALE  : VALEURS DU CHAM_NO
C         NOMCMP: NOMS DES CMP
C         TITR  : 1 LIGNE DE TITRE
C         NOMNOE: NOMS DES NOEUDS
C         NUMNOE: NUMEROS DES NOEUDS
C         NOMSD : NOM DU RESULTAT
C         NOMSYM: NOM SYMBOLIQUE
C         IR    : NUMERO D'ORDRE DU CHAMP
C         LMASU : INDIQUE SI MAILLAGE SUPERTAB  .TRUE. MAILLAGE SUPERTAB
C         NOCMPL: NOMS DES COMPOSANTES SELECTIONNEES
C         NBCMP : NOMBRE DE COMPOANTES SELECTIONNEES
C         NCMPS : NUMEROS DES COMPOSANTES SELECTIONNEES
C
      LOGICAL EXISDG
C     ------------------------------------------------------------------
      CHARACTER*8  NOCMP,NOMGS
      CHARACTER*24 NOMST
      CHARACTER*80 ENTETE(10),TITRE,TEXTE
      INTEGER      NBCHS,NBCMPT
      INTEGER      IMPRE,IENTE,IUTIL
      LOGICAL      AFAIRE,LCMP
C
C  --- INITIALISATIONS ----
C
C-----------------------------------------------------------------------
      INTEGER I ,IBCMPS ,IC ,ICHS ,ICMP ,ICMPS ,ICMS 
      INTEGER ICMSUP ,ICOMPT ,ICP ,IDA ,IDEBU ,IEC ,IER 
      INTEGER IFIN ,ILIG ,INDATS ,INNO ,INO ,INOCHS ,INOGDS 
      INTEGER IRES ,IRET ,IRVAL ,ITABL ,IVAL ,J ,JADM 
      INTEGER JJ ,JL ,JMAX ,JPOS ,JTITR ,K ,L 
      INTEGER LL ,LXLGUT ,NBDATS ,NCMP ,NI 
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL WKVECT('&&IRDESR.NOMGDS','V V K8',NCMPMX,INOGDS)
      CALL WKVECT('&&IRDESR.NOMCHS','V V K8',NCMPMX,INOCHS)
      CALL WKVECT('&&IRDESR.NBCMPS','V V I',NCMPMX,IBCMPS)
      CALL WKVECT('&&IRDESR.IPCMPS','V V I',NCMPMX*NCMPMX,ICMPS)
      CALL WKVECT('&&IRDESR.LTABL','V V L',NCMPMX,ITABL)
C
      NOMST= '&&IRECRI.SOUS_TITRE.TITR'
      CALL JEVEUO(NOMST,'L',JTITR)
      TITRE = ZK80(JTITR)
      DO 1 I=1,NCMPMX
        ZL(ITABL-1+I)=.FALSE.
   1  CONTINUE
C
C --- ALLOCATION DES TABLEAUX DE TRAVAIL ---
C
      CALL JEDETR('&&IRDESR.VAL')
      CALL WKVECT('&&IRDESR.VAL','V V R',NCMPMX,IRVAL)
C
C ---- RECHERCHE DES GRANDEURS SUPERTAB -----
C
      CALL IRGAGS(NCMPMX,NOMCMP,NOMSYM,NBCHS,ZK8(INOCHS),
     +               ZI(IBCMPS),ZK8(INOGDS),ZI(ICMPS))
C
C      ==================
C ---- PARTIE 1 : NBCMP=0
C      ===================
C
      IF(NBCMP.EQ.0)THEN
C
C ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
      DO 10 ICHS = 1,NBCHS
        IF ( ICHS .GT. 1 ) THEN
          AFAIRE=.FALSE.
          DO 2 ICP=1,ZI(IBCMPS-1+ICHS)
            AFAIRE = (AFAIRE.OR.ZL(ITABL-1+
     +                  ZI(ICMPS-1+(ICHS-1)*NCMPMX+ICP)))
   2      CONTINUE
          IF(.NOT. AFAIRE) GO TO 10
        ENDIF
        IENTE = 1
        IMPRE = 0
        LCMP=.FALSE.
        CALL ECRTES(NOMSD,TITR,ZK8(INOGDS-1+ICHS),IR,'NOEU',
     +            ZI(IBCMPS-1+ICHS),2,ENTETE,LCMP)
        IDEBU = 1
        ENTETE(4) = ' '
        TEXTE = ' '
        DO 5 ICP=1,ZI(IBCMPS-1+ICHS)
          NOCMP = NOMCMP(ZI(ICMPS-1+(ICHS-1)*NCMPMX+ICP))
          IUTIL = LXLGUT(NOCMP)
          IFIN  = IDEBU+IUTIL
          TEXTE(IDEBU:IFIN) = NOCMP(1:IUTIL)//' '
          IDEBU = IFIN+1
  5     CONTINUE
        IUTIL = LXLGUT(TEXTE)
        JMAX = LXLGUT(TITRE)
        JMAX  = MIN(JMAX,(80-IUTIL-2))
        ENTETE(4)= TITRE(1:JMAX)//' - '//TEXTE(1:IUTIL)

        DO 11 INNO = 1,NBNO
          INO = NUMNOE(INNO)
          DO 17 IEC=1,NEC
            DG(IEC)=PRNO((INO-1)*(NEC+2)+2+IEC)
  17      CONTINUE
C
C         NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
C         IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
          IVAL = PRNO((INO-1)* (NEC+2)+1)
          NCMP = PRNO((INO-1)* (NEC+2)+2)
          IF (NCMP.EQ.0) GO TO 11
C
          DO 25 IC = 1,ZI(IBCMPS-1+ICHS)
           ZR(IRVAL-1+IC) = 0.0D0
   25     CONTINUE

          ICOMPT = 0
          DO 12 ICMP = 1,NCMPMX
              IF (EXISDG(DG,ICMP)) THEN
                  IF(ICHS.EQ.1) ZL(ITABL-1+ICMP)= .TRUE.
                  ICOMPT = ICOMPT + 1
                  DO 13 ICMS = 1,ZI(IBCMPS-1+ICHS)
                     ICMSUP = ZI(ICMPS-1+(ICHS-1)*NCMPMX+ICMS)
                     IF (ICMP.EQ.ICMSUP) THEN
                       IMPRE = 1
                       ZR(IRVAL-1+ICMS) = VALE(NUEQ(IVAL-1+ICOMPT))
                       GOTO 12
                     ENDIF
   13             CONTINUE
              ENDIF
   12     CONTINUE

          IF (IMPRE.EQ.1) THEN
            IF(IENTE.EQ.1) THEN
              WRITE(IFI,'(A80)') (ENTETE(I),I=1,10)
              IENTE=0
            ENDIF
            IF (LMASU) THEN
                CALL LXLIIS(NOMNOE(INNO)(2:8),INO,IER)
            ENDIF
            WRITE (IFI,'(I10,5X,A,A)') INO,'% NOEUD ',NOMNOE(INNO)
            WRITE (IFI,'(6(1PE13.5E3))') (ZR(IRVAL-1+I),
     +                      I=1,ZI(IBCMPS-1+ICHS))
            IMPRE=0
          ENDIF
   11 CONTINUE
      IF (IENTE.EQ.0) WRITE (IFI,'(A)') '    -1'
   10 CONTINUE
C
C      =====================
C ---- PARTIE 2 : NBCMP.NE.0
C      =====================
C
      ELSE
C
C --- GRANDEUR SUPERTAB
      DO 897 I=1,NBCHS
         DO 898 J=1,ZI(IBCMPS+I-1)
            IF(NCMPS(1).EQ.ZI(ICMPS-1+(I-1)*NCMPMX+J)) GOTO 899
 898     CONTINUE
 897  CONTINUE
 899  CONTINUE
      NOMGS=ZK8(INOGDS-1+I)

C --- NOMBRE DE DATASET
      CALL WKVECT('&&IRDESR.CMP_DATS','V V I',NBCMP,INDATS)
      NBCMPT=6
      ILIG=NBCMP/6
      IRES=NBCMP-ILIG*6
      NI=0
      ZI(INDATS)=NI
      IF(IRES.EQ.0) THEN
         NBDATS=ILIG
         DO 901 I=1,NBDATS
            ZI(IBCMPS+I-1)=6
            NI=NI+6
            ZI(INDATS+I)=NI
 901     CONTINUE
      ELSE
         NBDATS=ILIG+1
         DO 902 I=1,NBDATS-1
            ZI(IBCMPS+I-1)=6
            NI=NI+6
            ZI(INDATS+I)=NI
 902     CONTINUE
         ZI(IBCMPS+NBDATS-1)=IRES
         ZI(INDATS+NBDATS)=NI+IRES
      ENDIF

C --- ECRITURE DE L'ENTETE SUPERTAB ----
      LCMP=.TRUE.
      CALL ECRTES(NOMSD,TITR,NOMGS,IR,'NOEU',NBCMPT,2,ENTETE,LCMP)

C --- BOUCLE SUR LES DATASETS
C     -----------------------
      DO 810 IDA=1,NBDATS

         IENTE = 1
         IDEBU = 1
         ENTETE(4) = ' '
         TEXTE = ' '

         DO 865 ICP=1,ZI(IBCMPS+IDA-1)
            NOCMP = NOCMPL(ICP+ZI(INDATS+IDA-1))
            IUTIL=LXLGUT(NOCMP)
            IFIN = IDEBU+IUTIL
            TEXTE(IDEBU:IFIN)=NOCMP(1:IUTIL)//' '
            IDEBU = IFIN + 1
 865     CONTINUE

         IUTIL = LXLGUT(TEXTE)
         JMAX = LXLGUT(TITRE)
         JMAX  = MIN(JMAX,(80-IUTIL-2))
         ENTETE(4)= TITRE(1:JMAX)//' - '//TEXTE(1:IUTIL)

         DO 811 INNO = 1,NBNO

            INO = NUMNOE(INNO)
            DO 817 IEC=1,NEC
               DG(IEC)=PRNO((INO-1)*(NEC+2)+2+IEC)
 817        CONTINUE

            NCMP = PRNO((INO-1)*(NEC+2)+2)
            IF (NCMP.EQ.0) GO TO 811


            DO 825 IC = 1,6
               ZR(IRVAL-1+IC) = 0.0D0
 825        CONTINUE

C ---       COMPOSANTES ADMISES
            CALL JEDETR('&&IRDESR.CMP')
            CALL WKVECT('&&IRDESR.CMP','V V I',NCMPMX,JADM)
            CALL JEDETR('&&IRDESR.POS')
            CALL WKVECT('&&IRDESR.POS','V V I',NBCMP,JPOS)
            K=0
            DO 777 ICMP=1,NCMPMX
               IF(EXISDG(DG,ICMP))THEN
                    ZI(JADM+K)=ICMP
                    K=K+1
               ENDIF
 777        CONTINUE

C ---       POSITIONS DES COMPOSANTES SELECTIONNEES PARMI LES
C           COMPOSANTES ADMISES
            L=0
            DO 778 J=1,ZI(IBCMPS+IDA-1)
               LL=0
               DO 779 JL=1,K
                 LL=LL+1
                 IF(ZI(JADM+JL-1).EQ.NCMPS(J+ZI(INDATS+IDA-1))) GOTO 780
 779           CONTINUE
 780           CONTINUE
               ZI(JPOS+L)=LL
               L=L+1
 778           CONTINUE

            IVAL = PRNO((INO-1)*(NEC+2)+1)
            DO 812 ICMP = 1,ZI(IBCMPS+IDA-1)
               JJ=ZI(JPOS+ICMP-1)
               ZR(IRVAL-1+ICMP)=VALE(NUEQ(IVAL-1+JJ))
 812        CONTINUE

            IF(IENTE.EQ.1) THEN
               WRITE(IFI,'(A80)') (ENTETE(I),I=1,10)
               IENTE=0
            ENDIF

            IF (LMASU) THEN
                CALL LXLIIS(NOMNOE(INNO)(2:8),INO,IER)
             ENDIF

             WRITE (IFI,'(I10,5X,A,A)') INO,'% NOEUD ',NOMNOE(INNO)
             WRITE (IFI,'(6(1PE13.5E3))') (ZR(IRVAL-1+I),I=1,6)

 811      CONTINUE
          IF (IENTE.EQ.0) WRITE (IFI,'(A)') '    -1'
 810   CONTINUE

      ENDIF
C
C --- MENAGE
C
      CALL JEDETR('&&IRDESR.NOMGDS')
      CALL JEDETR('&&IRDESR.NOMCHS')
      CALL JEDETR('&&IRDESR.NBCMPS')
      CALL JEDETR('&&IRDESR.IPCMPS')
      CALL JEDETR('&&IRDESR.LTABL')
      CALL JEDETR('&&IRDESR.VAL')
      CALL JEDETR('&&IRDESR.CMP_DATS')
      CALL JEDETR('&&IRDESR.CMP')
      CALL JEDETR('&&IRDESR.POS')

      CALL JEDEMA()
      END
