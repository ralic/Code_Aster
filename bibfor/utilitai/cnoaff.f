      SUBROUTINE CNOAFF ( NOMA, GRAN, BASE, CNO )
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C RESPONSABLE VABHHTS J.PELLET
C TOLE CRP_20

C BUT :     COMMANDES :     AFFE_CHAM_NO + CREA_CHAMP/OPERATION:'AFFE'
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
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

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER      NBEC,GD
      INTEGER VALI
      REAL*8       RBID
      REAL*8 VALR
      LOGICAL      FCT
      CHARACTER*1  K1BID,BASE
      CHARACTER*2  TYPVAL
      CHARACTER*2  TSCA
      CHARACTER*8  RESU,NOMA,KBID,GRAN,NOMGD,NOMN,NOCHNO,NOGDSI,CNO,
     +             TYPMCL(4)
      CHARACTER*14 NONUME
      CHARACTER*16 TYPE,OPER,TYPCO, MOTCLF, MOTCLE(4)
      CHARACTER*24 NOMNOE,REFE,VALE,NUEQ,CHAMNO,PRCHNO,MESNOE
      CHARACTER*24 VALK(2)
C
      DATA REFE/'                   .REFE'/
      DATA VALE/'                   .VALE'/
      DATA NUEQ/'                   .NUEQ'/
      DATA CHAMNO/'                        '/
      DATA PRCHNO/'                        '/
C ----------------------------------------------------------------------

      CALL JEMARQ()

C --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
      CALL GETRES(RESU,TYPE,OPER)
      RESU = CNO

      IF (OPER.EQ.'AFFE_CHAM_NO') THEN
        CALL GETVID(' ','CHAM_NO_AFFE',1,1,0,KBID,NBV)
        IF (NBV.NE.0) THEN
          CALL AFFEN0(IBID)
          GO TO 190
        END IF
      END IF

      FCT = .FALSE.

      CALL DISMOI('F','NB_EC',GRAN,'GRANDEUR',NEC,KBID,IE)
      CALL DISMOI('F','TYPE_SCA',GRAN,'GRANDEUR',IBID,TSCA,IE)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',GRAN),NUMGD)

      IF (NUMGD.EQ.0) THEN
                  VALK (1) = GRAN
        CALL U2MESG('F', 'UTILITAI6_1',1,VALK,0,0,0,0.D0)

      ELSE
        CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',IACMP)
        CALL JEVEUO(JEXATR('&CATA.GD.NOMCMP','LONCUM'),'L',IAV)
        NBCOMP = ZI(IAV+NUMGD) - ZI(IAV+NUMGD-1)
      END IF

C --- NUME_DDL

      INUME = 0
      CALL GETVID(' ','NUME_DDL',0,1,1,NONUME,INUME)

C --- CHAM_NO

      ICHNO = 0
      NBV = 0
      CALL GETVID(' ','CHAM_NO',0,1,1,NOCHNO,NBV)
      IF (NBV.GT.0) THEN
        ICHNO = 1
      ENDIF

C --- AFFE

      MESNOE = '&&CNOAFF.MES_NOEUDS'
      MOTCLF = 'AFFE'
      MOTCLE(1) = 'NOEUD'
      MOTCLE(2) = 'GROUP_NO'
      MOTCLE(3) = 'MAILLE'
      MOTCLE(4) = 'GROUP_MA'
      TYPMCL(1) = 'NOEUD'
      TYPMCL(2) = 'GROUP_NO'
      TYPMCL(3) = 'MAILLE'
      TYPMCL(4) = 'GROUP_MA'
C
      NOMNOE = NOMA//'.NOMNOE'
C
      CALL GETFAC ( MOTCLF, NOCC )
C
C    VERIFICATIONS DANS LE MOT-CLE FACTEUR AFFE
C    __________________________________________

      DO 20 IOCC = 1 , NOCC
C
        CALL GETVTX ( MOTCLF, 'NOM_CMP', IOCC,1,0, KBID, NBCMP )
C
        CALL GETVR8 ( MOTCLF,'VALE'    , IOCC,1,0, RBID, NBVAR )
C
        CALL GETVID ( MOTCLF,'VALE_F'  , IOCC,1,0, KBID, NBFCT )
C
        IF (NBFCT.NE.0) THEN
          FCT = .TRUE.
          IF (TSCA.NE.'K8') THEN
                  VALK (1) = GRAN
            CALL U2MESG('F', 'UTILITAI6_2',1,VALK,0,0,0,0.D0)
          END IF
        END IF
C
        NBCMP = -NBCMP
        NBVAR = MAX(-NBVAR,-NBFCT)
C
        IF (NBVAR.GT.NBCMP) THEN
                  VALI = IOCC
          CALL U2MESG('F', 'UTILITAI6_3',0,' ',1,VALI,0,0.D0)
        END IF
C
        IF (NBCMP.NE.0) THEN
          CALL WKVECT('&&CNOAFF.LISTE_COMP','V V K8',NBCMP,JCMP)
          CALL GETVTX(MOTCLF,'NOM_CMP',IOCC,1,NBCMP,ZK8(JCMP),NBCMP)
          DO 10 I = 1,NBCMP
            CALL VERICP(ZK8(IACMP),ZK8(JCMP+I-1),NBCOMP,IRET)
            IF (IRET.NE.0) THEN
                  VALI = IOCC
                  VALK (1) = GRAN
                  VALK (2) = ZK8(JCMP+I-1)
              CALL U2MESG('F', 'UTILITAI6_4',2,VALK,1,VALI,0,0.D0)
            END IF
   10     CONTINUE
          CALL JEDETR('&&CNOAFF.LISTE_COMP')
        END IF
C
   20 CONTINUE

C-----------------------------------------------------------------------

      IF (INUME.EQ.1) THEN

C ---   VERIFICATION QUE LA GRANDEUR ASSOCIEE AU NUME_DDL
C ---   EST LA MEME QUE CELLE DE LA COMMANDE

        CALL DISMOI('F','NOM_GD',NONUME,'NUME_DDL',IBID,NOMGD,IERD)
        CALL DISMOI('F','NOM_GD_SI',NOMGD,'GRANDEUR',IBID,NOGDSI,IERD)
        IF (NOGDSI.NE.GRAN) THEN
                  VALK (1) = NOGDSI
                  VALK (2) = GRAN
          CALL U2MESG('F', 'UTILITAI6_5',2,VALK,0,0,0,0.D0)
        END IF
      END IF

      CALL JELIRA ( NOMNOE, 'NOMMAX', NBNOEU, K1BID )
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',GRAN),'L',IACMP)

C    AFFE DU CHAMP AUX NOEUDS
C    -------------------------------

C    ALLOCATION DE 4 OBJETS INTERMEDIAIRES SERVANT AUX CALCULS
C    DE .PRNO ET .VALE


C    ALLOCATION DU VECTEUR (K8) CONTENANT LES NOMS DES NOEUDS

      CALL WKVECT('&&CNOAFF.NOMS_NOEUDS','V V K8',NBNOEU,JNNO)

C    ALLOCATION DU TABLEAU DES VALEURS DES COMPOSANTES DES NOEUDS
C    DIM NBNOEU * NBCOMP

      IF (FCT) THEN
        CALL WKVECT('&&CNOAFF.VALCOMPNO','V V K8',NBNOEU*NBCOMP,JVAL)
      ELSE
        CALL WKVECT('&&CNOAFF.VALCOMPNO','V V R' ,NBNOEU*NBCOMP,JVAL)
      END IF

C    ALLOCATION DU VECTEUR (IS) CONTENANT LE NOMBRE DES COMPOSANTES
C    AFFECTEES PAR NOEUD

      CALL WKVECT('&&CNOAFF.NBCOMP_AFFE','V V I',NBNOEU,JNBCA)

C    ALLOCATION DU VECTEUR (IS) CONTENANT LE DESCRIPTEUR GRANDEUR
C    PAR NOEUD

      CALL WKVECT('&&CNOAFF.DESC_NOEUD','V V I',NEC*NBNOEU,JDESC)

C   BOUCLE SUR LES OCCURRENCES DE AFFE

      DO 120 IOCC = 1,NOCC
C
         NBNOE = 0
C
         CALL GETVTX ( MOTCLF, 'NOM_CMP', IOCC,1,0, KBID, NBCMP )
         CALL GETVR8 ( MOTCLF, 'VALE'   , IOCC,1,0, RBID, NBVAL )
         CALL GETVID ( MOTCLF, 'VALE_F' , IOCC,1,0, KBID, NBFCT )
C
         CALL GETVTX ( MOTCLF, 'TOUT'   , IOCC,1,1, KBID, NBTOU )
C
         IF ( NBTOU .EQ. 0 ) THEN
C
            CALL RELIEM(' ', NOMA, 'NO_NOEUD', MOTCLF, IOCC, 4,
     +                                  MOTCLE, TYPMCL, MESNOE, NBNOE )
            CALL JEVEUO ( MESNOE, 'L', JLNO )

         ENDIF
C
         NBCMP = -NBCMP
         NBVAL = -NBVAL
         NBFCT = -NBFCT
C
         CALL WKVECT ( '&&CNOAFF.LISTE_COMP', 'V V K8', NBCMP, JCMP )
         CALL GETVTX ( MOTCLF, 'NOM_CMP', IOCC,1,NBCMP,ZK8(JCMP),NBCMP)

         IF ( FCT ) THEN
            CALL WKVECT('&&CNOAFF.LISTE_VAL','V V K8',NBCMP,JFCT)
            IF ( NBFCT .NE. 0  ) THEN
              CALL GETVID(MOTCLF,'VALE_F',IOCC,1,NBFCT,ZK8(JFCT),NBFCT)
            ENDIF

C  PROLONGEMENT DANS LE CAS D'UNE MEME FONCTION POUR TTES LES CMP

            IF (NBCMP.GT.NBFCT) THEN
               DO 60 ICMP = NBFCT+1,NBCMP
                  ZK8(JFCT-1+ICMP) = ZK8(JFCT+NBFCT-1)
                  VALK (1) = ZK8(JFCT+NBFCT-1)
                  VALK (2) = ZK8(JCMP+ICMP-1)
                  CALL U2MESG('A', 'UTILITAI6_6',2,VALK,0,0,0,0.D0)
   60          CONTINUE
            END IF
         ELSE
           CALL WKVECT('&&CNOAFF.LISTE_VAL','V V R',NBCMP,JVALR)
           IF ( NBVAL .NE. 0 ) THEN
             CALL GETVR8(MOTCLF,'VALE'  ,IOCC,1,NBVAL,ZR(JVALR),NBVAL)
           ENDIF

C  PROLONGEMENT DANS LE CAS D'UNE MEME VALEUR POUR TTES LES CMP

           IF (NBCMP.GT.NBVAL) THEN
              DO 70 ICMP = NBVAL+1,NBCMP
                 ZR(JVALR-1+ICMP) = ZR(JVALR+NBVAL-1)
                  VALR = ZR(JVALR+NBVAL-1)
                  VALK (1) = ZK8(JCMP+ICMP-1)
                 CALL U2MESG('A', 'UTILITAI6_7',1,VALK,0,0,1,VALR)
   70         CONTINUE
           END IF
        END IF

C        CAS D'UNE AFFE SUR TOUS LES NOEUDS (MOT-CLE TOUT)
C        -----------------------------------------

        IF ( NBTOU .NE. 0 ) THEN
          DO 80 INO = 1,NBNOEU
            CALL JENUNO(JEXNUM(NOMNOE,INO),NOMN)
            ZK8(JNNO+INO-1) = NOMN
            IF (FCT) THEN
              CALL AFFENO(IOCC,INO,ZK8(JCMP),NBCMP,ZK8(IACMP),NBCOMP,
     &                    RBID,ZK8(JFCT),ZI(JDESC),RBID,ZK8(JVAL),
     &                    'K',NEC)
            ELSE
              CALL AFFENO(IOCC,INO,ZK8(JCMP),NBCMP,ZK8(IACMP),NBCOMP,
     &                    ZR(JVALR),KBID,ZI(JDESC),ZR(JVAL),KBID,
     &                    'R',NEC)
            END IF
   80     CONTINUE
        END IF
C
        IF (NBNOE.NE.0) THEN
          DO 110 II = 1,NBNOE
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JLNO-1+II)),INO)
            ZK8(JNNO+INO-1) = ZK8(JLNO+II-1)
            IF (FCT) THEN
              CALL AFFENO(IOCC,INO,ZK8(JCMP),NBCMP,ZK8(IACMP),NBCOMP,
     &                    RBID,ZK8(JFCT),ZI(JDESC),RBID,ZK8(JVAL),
     &                    'K',NEC)
            ELSE
              CALL AFFENO(IOCC,INO,ZK8(JCMP),NBCMP,ZK8(IACMP),NBCOMP,
     &                    ZR(JVALR),KBID,ZI(JDESC),ZR(JVAL),KBID,
     &                    'R',NEC)
            END IF
  110     CONTINUE
          CALL JEDETR ( MESNOE )
        END IF

        CALL JEDETR ( '&&CNOAFF.LISTE_VAL' )
        CALL JEDETR ( '&&CNOAFF.LISTE_COMP')

C    FIN DE BOUCLE SUR LES OCCURRENCES DE AFFE

  120 CONTINUE

C  CALCUL DU NOMBRE TOTAL DE CMP AFFECTEES (SOMMEES SUR LES NOEUDS)

      LONVAL = 0
      DO 140 INO = 1,NBNOEU
        ICOMP = 0
        DO 130 IC = 1,NBCOMP
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

      IF (FCT) THEN
        TYPVAL = 'K8'
      ELSE
        TYPVAL = 'R'
      END IF

      IF (INUME.EQ.0 .AND. ICHNO.EQ.0) THEN

C        CAS OU LE PROF_CHNO EST A CONSTRUIRE
C        ************************************

        CALL AFCHNO(RESU,BASE,GRAN,NOMA,NBNOEU,ZI(JNBCA),ZI(JDESC),
     &              LONVAL,TYPVAL,ZR(JVAL),ZC(JVAL),ZK8(JVAL))
      ELSE

C        CAS D'UN PROF_CHNO RECUPERE A PARTIR DE NUME_DDL OU CHNO
C        ********************************************************

        IF (INUME.EQ.1) THEN
          PRCHNO = NONUME//'.NUME'
          CALL DISMOI('F','NB_EQUA',NONUME,'NUME_DDL',NEQ,KBID,IERD)
        ELSE IF (ICHNO.EQ.1) THEN
          REFE(1:8) = NOCHNO
          CALL JEVEUO(REFE,'L',JREF)
          PRCHNO = ZK24(JREF+1)
          CALL DISMOI('F','NB_EQUA',NOCHNO,'CHAM_NO',NEQ,KBID,IERD)
        END IF

C        --- CREATION DU CHAMNO ---

        CALL CRCHNO(RESU,PRCHNO,GRAN,NOMA,BASE,TYPVAL,NBNOEU,NEQ)
        CHAMNO(1:8) = RESU
        CALL JEVEUO(CHAMNO(1:19)//'.DESC','L',IADESC)
        GD = ZI(IADESC)

C        --- AFFE DU .VALE DE L'OBJET CHAMNO ---

        VALE(1:8) = RESU
        CALL JEVEUO(VALE,'E',LVALE)
        NUEQ(1:19) = PRCHNO(1:19)
        CALL JEVEUO(NUEQ,'E',LNUEQ)
        CALL JEVEUO(PRCHNO(1:19)//'.PRNO','L',LPRNOI)
        NEC = NBEC(GD)
        NEC2 = NEC + 2

        IF (FCT) THEN
          DO 160 INO = 1,NBNOEU
            I1 = ZI(LPRNOI-1+ (INO-1)*NEC2+1) + LNUEQ - 1
            DO 150 IC = 1,NBCOMP
              IEC = (IC-1)/30 + 1
              JJ = IC - 30* (IEC-1)
              II = 2**JJ
              NN = IAND(ZI(JDESC+ (INO-1)*NEC+IEC-1),II)
              NNI = IAND(ZI(LPRNOI-1+ (INO-1)*NEC2+2+IEC),II)
C     SI LA COMPOSANTE EST AFFECTEE DANS LA COMMANDE
              IF (NN.GT.0) THEN
C     SI LA COMPOSANTE FIGURE DANS LE PROF_CHNO
                IF (NNI.GT.0) THEN
                  ZK8(LVALE-1+ZI(I1)) = ZK8(JVAL+ (INO-1)*NBCOMP-1+IC)
                  I1 = I1 + 1
                ELSE
                  VALK (1) = NOMN
                  VALK (2) = ZK8(IACMP-1+IC)
                  CALL U2MESG('F', 'UTILITAI6_8',2,VALK,0,0,0,0.D0)
                END IF
              ELSE
C     LA COMPOSANTE N'EST PAS AFFECTEE DANS LA COMMANDE
                IF (NNI.GT.0) THEN
                  I1 = I1 + 1
                END IF
              END IF
  150       CONTINUE
  160     CONTINUE
        ELSE
          DO 180 INO = 1,NBNOEU
            I1 = ZI(LPRNOI-1+ (INO-1)*NEC2+1) + LNUEQ - 1
            DO 170 IC = 1,NBCOMP
              IEC = (IC-1)/30 + 1
              JJ = IC - 30* (IEC-1)
              II = 2**JJ
              NN = IAND(ZI(JDESC+ (INO-1)*NEC+IEC-1),II)
              NNI = IAND(ZI(LPRNOI-1+ (INO-1)*NEC2+2+IEC),II)
C     SI LA COMPOSANTE EST AFFECTEE DANS LA COMMANDE
              IF (NN.GT.0) THEN
C     SI LA COMPOSANTE FIGURE DANS LE PROF_CHNO
                IF (NNI.GT.0) THEN
                  ZR(LVALE-1+ZI(I1)) = ZR(JVAL+ (INO-1)*NBCOMP-1+IC)
                  I1 = I1 + 1
                ELSE
                  VALK (1) = NOMN
                  VALK (2) = ZK8(IACMP-1+IC)
                  CALL U2MESG('F', 'UTILITAI6_8',2,VALK,0,0,0,0.D0)
                END IF
              ELSE
C     LA COMPOSANTE N'EST PAS AFFECTEE DANS LA COMMANDE
                IF (NNI.GT.0) THEN
                  I1 = I1 + 1
                END IF
              END IF
  170       CONTINUE
  180     CONTINUE
        END IF
        LONVAL = NEQ
      END IF

      CALL JEDETR('&&CNOAFF.NOMS_NOEUDS')
      CALL JEDETR('&&CNOAFF.VALCOMPNO')
      CALL JEDETR('&&CNOAFF.NBCOMP_AFFE')
      CALL JEDETR('&&CNOAFF.DESC_NOEUD')

  190 CONTINUE
  999 FORMAT (5X,I4,12X,A4,12X,A5)

      CALL JEDEMA()
      END
