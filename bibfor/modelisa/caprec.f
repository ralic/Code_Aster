      SUBROUTINE CAPREC(CHARGE,MAILLA)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 23/05/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C ----------------------------------------------------------------------
C  DESCRIPTION : OPERATEUR AFFE_CHAR_MECA
C  -----------   TRAITEMENT DU MOT-CLE FACTEUR RELA_CINE_BP
C                APPELANT : CHARME

C  IN     : CHARGE : CHARACTER*8 , SCALAIRE
C                    NOM DU CONCEPT CHAR_MECA
C  IN     : MAILLA : CHARACTER*8 , SCALAIRE
C                    NOM DU CONCEPT MAILLAGE

C-------------------   DECLARATION DES VARIABLES   ---------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C     ----- FIN   COMMUNS NORMALISES  JEVEUX  --------------------------

C ARGUMENTS
C ---------
      CHARACTER*8 CHARGE,MAILLA

C VARIABLES LOCALES
C -----------------
      INTEGER NMOCL
      PARAMETER (NMOCL=300)
      INTEGER IAS,IAS1,IASM,IBID,ICMP,ICODE,ICODE1,ICODOK,IOCC,IRANN,
     &        IRET,JDESC,JDESC1,JDNBRE,JNCMP1,JPTMA,JPTMA1,JSIEF,JVALE,
     &        JVALE1,JVALV1,NBEC,NBMAMA,NBOCC,NBRELA,NSIEF,NUMAIL
      LOGICAL LCART1,LRELA,LSIGM,LPARA
      CHARACTER*1 K1B
      CHARACTER*2 TYPLAG
      CHARACTER*3 K3B
      CHARACTER*8 CABLPR,K8B,MODELE,NPARA,K8VIDE
      CHARACTER*19 LIGRMO,LIRELA,SIGCAB,SIGCHA,LISNOM,LISAN1,LISAN2
      CHARACTER*24 GRNOMA,LISNOE,LIGRNO,NOEUMA
      LOGICAL EXISDG
      INTEGER INDIK8,NDDLA
      INTEGER IDIMAX,JLIST,INO,JGRO,IN,INDNOE,LONLIS
      INTEGER JIND,IN1,INDLIS,JPRNM,NANC,JADD
      REAL*8 R8B,DMIN,R8GAEM
      COMPLEX*16 C16B
      CHARACTER*1 K1BID
      CHARACTER*8 K8TMP,NOMNOE,NOMANC,NOMG,NOMAN1,NOMAN2
      CHARACTER*8 CMP,NOMCMP(NMOCL)
      CHARACTER*8 CMP4,CMP5,CMP6,K8BID
      CHARACTER*9 NOMTE
      INTEGER NTYPEL(NMOCL)
      INTEGER ICMP4,ICMP5,ICMP6,IDRXYZ,I,IDRZ,NDIMMO
      INTEGER ILISNO,IER,INOM,NBCMP,IERD
      INTEGER NBNOM,NBAN1,NBAN2,JLSNOM,JLSAN1,JLSAN2,INOMC
      INTEGER NUMCAB,NUMCA0
      CHARACTER*8 EFFNOR
      CHARACTER*19 LRLTMP,LISREL,TABLE

      DATA EFFNOR/'N       '/
      DATA LRLTMP/'&&CAPREC.LIRELA    '/

C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------

      CALL JEMARQ()
      K8VIDE = '        '

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     DECOMPTE DES OCCURRENCES DU MOT-CLE FACTEUR 'RELA_CINE_BP'
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      CALL GETFAC('RELA_CINE_BP',NBOCC)

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     TRAITEMENT SI AU MOINS UNE OCCURRENCE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      IF (NBOCC.GT.0) THEN

C ---  RECUPERATION DES INFORMATIONS UTILES - INITIALISATIONS
        CALL DISMOI('F','NOM_MODELE',CHARGE,'CHARGE',IBID,MODELE,IRET)
        LIGRMO = MODELE//'.MODELE    '

        CALL DISMOI('F','NB_MA_MAILLA',MAILLA,'MAILLAGE',NBMAMA,K8B,
     &              IRET)

C     CALCUL D'UNE DIMENSION DE REFERENCE : DMIN
      CALL LTNOTB ( MAILLA, 'CARA_GEOM' , TABLE )
      CALL TBLIVA(TABLE,1,'APPLAT_Z',IBID,0.D0,C16B,K1BID,'ABSO',
     +             R8GAEM(),'AR_MIN',K1BID,IBID,DMIN,C16B,K1BID,IER )


C --- NOM DE LA LISTE DE RELATIONS
        LISREL = '&&CAPREC.RLLISTE'

C --- DIMENSION ASSOCIEE AU MODELE
        CALL DISMOI('F','Z_CST',MODELE,'MODELE',NDIMMO,K8B,IER)
        NDIMMO = 3
        IF (K8B.EQ.'OUI') NDIMMO = 2

C --- RECUPERATION DES NOMS DES DDLS ET DES NUMEROS
C --- D'ELEMENTS DE LAGRANGE ASSOCIES

        NOMG = 'DEPL_R'
        NOMTE = 'D_DEPL_R_'

        CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMG),'L',INOM)
        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMG),'LONMAX',NBCMP,K1BID)
        NDDLA = NBCMP - 1
        IF (NDDLA.GT.NMOCL) THEN
          CALL UTDEBM('F','CAPREC','NOMBRE DE CMPS SUPERIEUR AU MAX')
          CALL UTIMPI('L','NMAXCMP= ',1,NMOCL)
          CALL UTIMPI('L','NCMP   = ',1,NDDLA)
          CALL UTFINM()
        END IF
        DO 10 I = 1,NDDLA
          NOMCMP(I) = ZK8(INOM-1+I)
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE',NOMTE//NOMCMP(I) (1:7)),
     &                NTYPEL(I))
   10   CONTINUE
        CALL DISMOI('F','NB_EC',NOMG,'GRANDEUR',NBEC,K8BID,IERD)

C --- ACCES A L'OBJET .PRNM

        IF (NBEC.GT.10) THEN
          CALL UTMESS('F','CAPREC',
     &                'LE DESCRIPTEUR_GRANDEUR DES DEPLACEMENTS'//
     &                ' NE TIENT PAS SUR DIX ENTIERS CODES')
        ELSE
          CALL JEVEUO(LIGRMO//'.PRNM','L',JPRNM)
        END IF


C....... DETERMINATION DU RANG DE LA COMPOSANTE <N>
C....... DE LA GRANDEUR <SIEF_R>

        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP','SIEF_R'),'LONMAX',NSIEF,
     &              K1B)
        CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP','SIEF_R'),'L',JSIEF)
        IRANN = 0
        DO 20 ICMP = 1,NSIEF
          IF (ZK8(JSIEF+ICMP-1).EQ.EFFNOR) THEN
            IRANN = ICMP
            GO TO 30
          END IF
   20   CONTINUE
   30   CONTINUE
        IF (IRANN.EQ.0) CALL UTMESS('F','CAPREC',
     &                              'PROBLEME POUR DETERMINER LE '//
     &               'RANG DE LA COMPOSANTE <N> DE LA GRANDEUR <SIEF_R>'
     &                              )

        IF (IRANN.GT.30) CALL VERI32()
        ICODOK = 2**IRANN

        LCART1 = .TRUE.
        SIGCHA = CHARGE//'.CHME.SIGIN'
        CALL DISMOI('F','NB_EC','SIEF_R','GRANDEUR',NBEC,K8B,IRET)

C        BOUCLE SUR LE NOMBRE D'OCCURRENCES DE RELA_CINE_BP
C ---
        DO 160 IOCC = 1,NBOCC

C ---  LECTURE DES ARGUMENTS DES MOT-CLES 'SIGM_BPEL' ET
C ---  'RELA_CINE' POUR DEFINITION DES DONNEES A RECUPERER

          CALL GETVTX('RELA_CINE_BP','SIGM_BPEL',IOCC,1,1,K3B,IBID)
          LSIGM = (K3B.EQ.'OUI')
          CALL GETVTX('RELA_CINE_BP','RELA_CINE',IOCC,1,1,K3B,IBID)
          LRELA = (K3B.EQ.'OUI')

C ---  RECUPERATION DES DONNEES LE CAS ECHEANT
          IF (LSIGM .OR. LRELA) THEN

C ---  RECUPERATION DU NOM DU CONCEPT DE TYPE CABL_PRECONT

            CALL GETVID('RELA_CINE_BP','CABLE_BP',IOCC,1,1,CABLPR,IBID)

C ---  RECUPERATION D'UNE CARTE DE CONTRAINTES INITIALES LE CAS ECHEANT

            IF (LSIGM) THEN
              CALL DISMOI('F','NB_EC','SIEF_R','GRANDEUR',NBEC,K8B,IRET)
              SIGCAB = CABLPR//'.CHME.SIGIN'
              CALL JEEXIN(SIGCAB//'.DESC',IRET)
              IF (IRET.EQ.0) CALL UTMESS('F','CAPREC',
     &                                   'LE CONCEPT '//CABLPR//
     &                                   ' N EST PAS UN CONCEPT DE TYPE'
     &                                   //' CABL_PRECONT')
              CALL JEVEUO(SIGCAB//'.DESC','L',JDESC)

              IASM = ZI(JDESC+1)

C ---  RECOPIAGE DE LA PREMIERE CARTE

              IF (LCART1) THEN

                CALL COPISD('CHAMP_GD','G',SIGCAB,SIGCHA)
                LCART1 = .FALSE.
                CALL ETENCA(SIGCHA,LIGRMO,IRET)
                IF (IRET.NE.0) CALL UTMESS('F','CAPREC','ERREUR '//
     &                            'A L APPEL DE LA ROUTINE ETENCA POUR '
     &                                     //'EXTENSION DE LA CARTE '//
     &                                     SIGCHA)
                CALL JEVEUO(SIGCHA//'.DESC','L',JDESC1)
                CALL JEVEUO(SIGCHA//'.VALE','L',JVALE1)
                CALL JEVEUO(SIGCHA//'.PTMA','L',JPTMA1)
                CALL JECREO(SIGCHA//'.NCMP','V V K8')
                CALL JEECRA(SIGCHA//'.NCMP','LONMAX',NSIEF,' ')
                CALL JEVEUO(SIGCHA//'.NCMP','E',JNCMP1)
                DO 40 ICMP = 1,NSIEF
C                  ZK8(JNCMP1+ICMP-1) = '        '
                  ZK8(JNCMP1+ICMP-1) = K8VIDE
   40           CONTINUE
                ZK8(JNCMP1) = EFFNOR
                CALL JECREO(SIGCHA//'.VALV','V V R')
                CALL JEECRA(SIGCHA//'.VALV','LONMAX',NSIEF,' ')
                CALL JEVEUO(SIGCHA//'.VALV','E',JVALV1)

C ---  RECUPERATION DES DONNEES APPORTEES PAR LES CARTES SUIVANTES

              ELSE

                CALL ETENCA(SIGCAB,LIGRMO,IRET)
                IF (IRET.NE.0) CALL UTMESS('F','CAPREC','ERREUR '//
     &                            'A L APPEL DE LA ROUTINE ETENCA POUR '
     &                                     //'EXTENSION DE LA CARTE '//
     &                                     SIGCAB)
                CALL JEVEUO(SIGCAB//'.DESC','L',JDESC)
                CALL JEVEUO(SIGCAB//'.VALE','L',JVALE)
                CALL JEVEUO(SIGCAB//'.PTMA','L',JPTMA)
                DO 50 NUMAIL = 1,NBMAMA
                  IAS1 = ZI(JPTMA1+NUMAIL-1)
                  IAS = ZI(JPTMA+NUMAIL-1)
                  ICODE1 = ZI(JDESC1+3+2*IASM+NBEC* (IAS1-1))
                  ICODE = ZI(JDESC+3+2*IASM+NBEC* (IAS-1))
                  IF (ICODE.EQ.ICODOK) THEN
                    ZR(JVALV1) = ZR(JVALE+NSIEF* (IAS-1))
                    IF (ICODE1.EQ.ICODOK) ZR(JVALV1) = ZR(JVALV1) +
     &                  ZR(JVALE1+NSIEF* (IAS1-1))
                    CALL NOCART(SIGCHA,3,K1B,'NUM',1,K1B,NUMAIL,' ',1)
                  END IF
   50           CONTINUE
                CALL JELIBE(SIGCAB//'.DESC')
                CALL JELIBE(SIGCAB//'.VALE')
                CALL JELIBE(SIGCAB//'.PTMA')
                CALL JEDETC('V',SIGCAB,1)

              END IF

            END IF

C ---  RECUPERATION D'UNE LISTE DE RELATIONS CINEMATIQUES
C ---  LE CAS ECHEANT

            IF (LRELA) THEN
              CALL DISMOI('F','NB_EC','DEPL_R','GRANDEUR',NBEC,K8B,IRET)
              LIRELA = CABLPR//'.LIRELA    '
              CALL JEEXIN(LIRELA//'.RLNR',IRET)
              IF (IRET.EQ.0) CALL UTMESS('F','CAPREC',
     &                                   'LE CONCEPT '//CABLPR//
     &                                   ' N EST PAS UN CONCEPT DE TYPE'
     &                                   //' CABL_PRECONT')
              CALL JEVEUO(LIRELA//'.RLNR','L',JDNBRE)
              NBRELA = ZI(JDNBRE)
              CALL JELIBE(LIRELA//'.RLNR')
              IF (NBRELA.GT.0) THEN
                CALL COPISD(' ','V',LIRELA,LRLTMP)
                CALL AFLRCH(LRLTMP,CHARGE)
              END IF


C ---  ACQUISITION DE LA LISTE DES NOEUDS A LIER PAR UN EQUIVALENT
C ---  A LIAISON_SOLIDE (CETTE LISTE EST NON REDONDANTE)

              CALL JEVEUO(CABLPR//'           .LTNS','L',JADD)
              TABLE = ZK24(JADD)

         LISNOM = '&&CAPREC.NUME_CABLE'
         LISAN1 = '&&CAPREC.NOM_ANCRA1'
         LISAN2 = '&&CAPREC.NOM_ANCRA2'

         CALL TBEXIP ( TABLE, 'NUME_CABLE', LPARA, NPARA )
         CALL TBEXIP ( TABLE, 'NOM_ANCRAGE1', LPARA, NPARA )

         CALL TBEXVE ( TABLE, 'NUME_CABLE', LISNOM, 'V', NBNOM, K8B )
         CALL JEVEUO ( LISNOM, 'L', JLSNOM )

         CALL TBEXVE ( TABLE, 'NOM_ANCRAGE1', LISAN1, 'V', NBAN1, K8B )
         CALL JEVEUO ( LISAN1, 'L', JLSAN1 )

         CALL TBEXVE ( TABLE, 'NOM_ANCRAGE2', LISAN2, 'V', NBAN2, K8B )
         CALL JEVEUO ( LISAN2, 'L', JLSAN2 )

         NUMCA0 = 0

C         NOMANC = ''
         NOMANC = K8VIDE

         DO 145 INOMC = 1,NBNOM
           NUMCAB = ZI(JLSNOM-1+INOMC)
           NOMAN1 = ZK8(JLSAN1-1+INOMC)
           NOMAN2 = ZK8(JLSAN2-1+INOMC)

           IF (NUMCAB.NE.NUMCA0) THEN
             NUMCA0 = NUMCAB

             DO 150 NANC = 1,2
C               NOMANC = '        '
               NOMANC = K8VIDE

                IF (NANC.EQ.1) THEN
                  NOMANC = NOMAN1
                END IF

                IF (NANC.EQ.2) THEN
                  NOMANC = NOMAN2
                END IF

C                IF (NOMANC.NE.'        ') THEN
                IF (NOMANC.NE.K8VIDE) THEN

                  LISNOE = '&&CAPREC.LISTNOE'
                  TYPLAG = '12'
                  LIGRNO = CABLPR//'           '//'.LTNT      '
                  CALL JEEXIN(LIGRNO,IRET)

                  GRNOMA = MAILLA//'.GROUPENO'
                  NOEUMA = MAILLA//'.NOMNOE'

                  CALL JEVEUO(JEXNOM(GRNOMA,NOMANC),'L',JGRO)
                  CALL JELIRA(JEXNOM(GRNOMA,NOMANC),'LONMAX',IDIMAX,K1B)
                  CALL WKVECT(LISNOE,'V V K8',IDIMAX,JLIST)

                  INDNOE = 0
                  DO 60 INO = 1,IDIMAX
                    IN = ZI(JGRO+INO-1)
                    INDNOE = INDNOE + 1
                    CALL JENUNO(JEXNUM(NOEUMA,IN),NOMNOE)

                    ZK8(JLIST+INDNOE-1) = NOMNOE
   60             CONTINUE

C ---  ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS DE LA LISTE
                  CALL WKVECT('&&CAPREC.INDICE','V V I',IDIMAX,JIND)

                  DO 80 INO = 1,IDIMAX
                    DO 70 IN1 = INO + 1,IDIMAX
                      IF (ZK8(JLIST+IN1-1).EQ.ZK8(JLIST+INO-1)) THEN
                        ZI(JIND+IN1-1) = 1
                      END IF
   70               CONTINUE
   80             CONTINUE

                  INDLIS = 0
                  DO 90 INO = 1,IDIMAX
                    IF (ZI(JIND+INO-1).EQ.0) THEN
                      INDLIS = INDLIS + 1
                      ZK8(JLIST+INDLIS-1) = ZK8(JLIST+INO-1)
                    END IF
   90             CONTINUE

                  LONLIS = INDLIS

                  CALL JEVEUO(LISNOE,'L',ILISNO)

C ---  CAS OU LA LISTE DES NOEUDS A LIER EST UN SINGLETON

                  IF (LONLIS.EQ.1) THEN
                    CALL UTMESS('I','CALISO',
     &                          'ATTENTION, LA LISTE DES NOEUDS '//
     &                    'EST REDUITE A UN SEUL TERME ET L''ON NE FAIT'
     &                          //' AUCUN TRAITEMENT')
                    GO TO 140
                  END IF

C ---  CAS OU LA DIMENSION DU MODELE EST EGALE A 2

                  IF (NDIMMO.EQ.2) THEN

C ---  ON REGARDE S'IL Y A UN NOEUD DE LA LISTE PORTANT LE DDL DRZ

                    CMP = 'DRZ'
                    ICMP = INDIK8(NOMCMP,CMP,1,NDDLA)
                    IDRZ = 0
                    DO 100 I = 1,LONLIS
C ---  NUMERO DU NOEUD COURANT DE LA LISTE
                      CALL JENONU(JEXNOM(MAILLA//'.NOMNOE',
     &                            ZK8(ILISNO+I-1)),IN)

                      IF (EXISDG(ZI(JPRNM-1+ (IN-1)*NBEC+1),ICMP)) THEN
                        IDRZ = 1
                        GO TO 110
                      END IF
  100               CONTINUE

  110               CONTINUE

C ---  CAS OU L'ON A UN NOEUD DE LA LISTE PORTANT LE DDL DRZ

                    IF (IDRZ.EQ.1) THEN
                      CALL DRZ12D(LISNOE,LONLIS,CHARGE,TYPLAG,LISREL)

C ---  CAS OU AUCUN NOEUD DE LA LISTE NE PORTE LE DDL DRZ

                    ELSE IF (IDRZ.EQ.0) THEN
                      CALL DRZ02D(LISNOE,LONLIS,CHARGE,TYPLAG,LISREL
     &                            ,DMIN)

C ---  FIN DU CAS 2D SANS DDL DE ROTATION
                    END IF

C ---  CAS OU LA DIMENSION DU MODELE EST EGALE A 3

                  ELSE IF (NDIMMO.EQ.3) THEN

C ---  ON REGARDE S'IL Y A UN NOEUD DE LA LISTE PORTANT LES 3 DDLS
C ---  DE ROTATION

                    CMP4 = 'DRX'
                    CMP5 = 'DRY'
                    CMP6 = 'DRZ'
                    ICMP4 = INDIK8(NOMCMP,CMP4,1,NDDLA)
                    ICMP5 = INDIK8(NOMCMP,CMP5,1,NDDLA)
                    ICMP6 = INDIK8(NOMCMP,CMP6,1,NDDLA)
                    IDRXYZ = 0
                    DO 120 I = 1,LONLIS
C ---  NUMERO DU NOEUD COURANT DE LA LISTE
                      CALL JENONU(JEXNOM(MAILLA//'.NOMNOE',
     &                            ZK8(ILISNO+I-1)),IN)

                      IF ((EXISDG(ZI(JPRNM-1+ (IN-1)*NBEC+1),
     &                    ICMP4)) .AND. (EXISDG(ZI(JPRNM-1+ (IN-1)*NBEC+
     &                    1),ICMP5)) .AND. (EXISDG(ZI(JPRNM-1+ (IN-
     &                    1)*NBEC+1),ICMP6))) THEN
                        IDRXYZ = 1
                        GO TO 130
                      END IF
  120               CONTINUE

  130               CONTINUE

C ---  CAS OU L'ON A UN NOEUD DE LA LISTE PORTANT LES 3 DDLS
C ---  DE ROTATION
                    IF (IDRXYZ.EQ.1) THEN
                      CALL DRZ13D(LISNOE,LONLIS,CHARGE,TYPLAG,LISREL)

C ---  CAS MASSIF (PAS DE COMPOSANTES DE ROTATION)
                    ELSE IF (IDRXYZ.EQ.0) THEN
                      CALL DRZ03D(LISNOE,LONLIS,CHARGE,TYPLAG,LISREL
     &                           ,DMIN)

C ---  FIN DU CAS 3D MASSIF (IDRXYZ=0)
                    END IF
C ---  FIN DU CAS 3D
                  END IF
  140             CONTINUE
C ---  DESTRUCTION DE LA LISTE DES NOEUDS A LIER
                  CALL JEDETR(LISNOE)
                  CALL JEDETR('&&CAPREC.INDICE')

C ---  AFFECTATION DE LA LISTE_RELA A LA CHARGE :
                  CALL AFLRCH(LISREL,CHARGE)

C ---  FIN IF NOMANC = NOM_ANCRAGE1 OU NOM_ANCRAGE2
                END IF

C ---  FIN BCL SUR NUMERO ANCRAGE (NANC = 1 OU 2)
  150         CONTINUE


C ---  FIN BCL SUR NUM_CABLE DANS LA TABLE
           END IF
  145    CONTINUE

         CALL JEDETR ( LISNOM )
         CALL JEDETR ( LISAN1 )
         CALL JEDETR ( LISAN2 )

C ---  FIN IF : LRELA
            END IF

C ---  FIN IF : LSIGM .OR. LRELA
          END IF

C ---  FIN BOUCLE SUR LE NOMBRE D'OCCURENCES DE RELA_CINE_BP
  160   CONTINUE


C ---  DESTRUCTION DES OBJETS DE TRAVAIL ATTACHES A LA CARTE
C ---  DES CONTRAINTES INITIALES

        IF (.NOT.LCART1) CALL JEDETC('V',SIGCHA,1)

      END IF

      CALL JEDEMA()

C ---  FIN DE CAPREC.
      END
