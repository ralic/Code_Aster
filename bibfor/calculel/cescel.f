      SUBROUTINE CESCEL(CESZ,LIGREZ,OPTINI,NOMPAZ,PROL0,NNCP,BASEZ,CELZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/08/2005   AUTEUR VABHHTS J.PELLET 
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

C RESPONSABLE                            VABHHTS J.PELLET
      IMPLICIT NONE
      CHARACTER*(*) CESZ,CELZ,BASEZ,LIGREZ,OPTINI,NOMPAZ
      CHARACTER*3 PROL0
      INTEGER NNCP
C ------------------------------------------------------------------
C BUT : TRANSFORMER UN CHAM_ELEM_S (CESZ) EN CHAM_ELEM (CELZ)
C ------------------------------------------------------------------
C ARGUMENTS:
C ==========
C CESZ    IN/JXIN  K19 : SD CHAM_ELEM_S A TRANSFORMER
C LIGREZ  IN/JXIN  K19 : SD LIGREL QUI SERA ASSOCIE A CELZ
C OPTINI  IN       K16 : OPTION QUI SERA ASSOCIEE A CELZ
C         SI OPTINI=' ' : ON PREND OPTINI='TOU_INI_ELNO'
C                         OU OPTINI='TOU_INI_ELGA'
C                         SELON LE TYPE DE CESZ
C NOMPAZ  IN       K8  : NOM DU PARAMETRE "IN" OU "OUT" DANS OPTINI
C         SI NOMPAZ=' ' : ON CHERCHE LE BON CANDIDAT DANS LES
C                         PARAMETRES "IN" ET "OUT" DE OPTINI
C         ATTENTION : SI NOMPAZ=' ' ET QU'IL EXISTE PLUSIEURS
C                     PARAMETRES ASSOCIES A LA MEME GRANDEUR
C                     CELA CONDUIRA A UNE ERREUR <F>
C                     => IL VAUT MIEUX FOURNIR NOMPAZ !
C PROL0   IN       K3  :
C    /'NON' : ERREUR <F> SI IL EXISTE DES
C             DES VALEURS DE CEL QUI NE SONT PAS AFFECTEES PAR CES.
C             => ON N'INVENTE AUCUNE VALEUR
C    /'OUI' : LE CHAM_ELEM CEL EST PROLONGE
C             PAR DES VALEURS NULLES LA OU CES N'EST PAS DEFINI.
C    /'CHL' : (UTILISE PAR CHLIGR)
C             PROLONGE PAR "ZERO" LES MAILLES DE CEL QUI NE SONT
C             PAS DU TOUT AFFECTEES DANS CES (NOUVELLES MAILLES)
C             ARRETE EN ERREUR <F> SI DES MAILLES DE CEL PORTENT
C             DES CMPS INCONNUES DANS CES
C NNCP   OUT       I   : NOMBRE DE VALEURS DE CESZ NON RECOPIEES
C                        DANS CELZ
C BASEZ   IN       K1  : BASE DE CREATION POUR CELZ : G/V/L
C CELZ    IN/JXOUT K19 : SD CHAM_ELEM A CREER

C-----------------------------------------------------------------------

C---- COMMUNS NORMALISES  JEVEUX
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
C     ------------------------------------------------------------------
      INTEGER ICMP,NEC,JCESK,JCESD,JCESV,JCESL,GD
      INTEGER IRET,IBID,JNUCMP,JNUCM1,JCESC
      INTEGER NCMPMX,NCMP1,JCMPGD,ICMP1,K,IOPT,IADG
      INTEGER JCELV,INDIK8,NEQ,NBVCES,JCOPI,NBVCOP
      INTEGER IGR,IEL,IALIEL,ILLIEL,JCELD,NBGR,IMOLO,JMOLO
      INTEGER NBPT,ICO,IPT,NUMA,IAD,IEQ,NUMAIL,NBELEM,IAD2
      INTEGER JDCELD,JDCELL,JDCELV,IMA,NBMA,NBSPT,ISPT,ICMPMX
      INTEGER ADIEL,JLPT,JLCUPT,LGCATA,NCDYN,CUMU,NBEL,NPTMX
      INTEGER NBSP,NBCMP,ISP,NBPT2
      LOGICAL EXISDG,DIFF,PROL,PROL2
      CHARACTER*1 BASE,KBID
      CHARACTER*8 MA,NOMGD,NOMCMP,NOMPAR,NOMMA,LICMP(2),NOPAR2
      CHARACTER*3 TSCA
      CHARACTER*4 TYPCES
      CHARACTER*16 OPTION
      CHARACTER*19 CES,CEL,LIGREL,DCEL
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR

      NUMAIL(IGR,IEL) = ZI(IALIEL-1+ZI(ILLIEL+IGR-1)+IEL-1)
C     ------------------------------------------------------------------
      CALL JEMARQ()

      BASE = BASEZ
      CES = CESZ
      CEL = CELZ
      OPTION = OPTINI
      NOMPAR = NOMPAZ
      LIGREL = LIGREZ

      IF (PROL0.EQ.'OUI') THEN
        PROL = .TRUE.
        PROL2= .TRUE.
      ELSE IF (PROL0.EQ.'NON') THEN
        PROL = .FALSE.
        PROL2= .FALSE.
      ELSE IF (PROL0.EQ.'CHL') THEN
        PROL = .FALSE.
        PROL2= .TRUE.
      ELSE
        CALL UTMESS('F','CESCEL','ARGUMENT PROL0 INVALIDE.')
      END IF


C     -- SI CEL EXISTE DEJA, ON LE DETRUIT :
      CALL DETRSD('CHAM_ELEM',CEL)

      CALL JEVEUO(CES//'.CESK','L',JCESK)
      CALL JEVEUO(CES//'.CESD','L',JCESD)
      CALL JEVEUO(CES//'.CESC','L',JCESC)
      CALL JEVEUO(CES//'.CESV','L',JCESV)
C     -- OBJET .COPI TEMPORAIRE POUR VERIFIER QUE TOUTES LES
C        COMPOSANTES DE CES ONT ETE RECOPIEES
      CALL JELIRA(CES//'.CESV','LONMAX',NBVCES,KBID)
      CALL WKVECT('&&CESCEL.COPI','V V I',NBVCES,JCOPI)
      CALL JEVEUO(CES//'.CESL','L',JCESL)

      MA = ZK8(JCESK-1+1)
      NOMGD = ZK8(JCESK-1+2)
      TYPCES = ZK8(JCESK-1+3)

      NBMA = ZI(JCESD-1+1)
      NCMP1 = ZI(JCESD-1+2)

      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,KBID,IBID)
      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)
      CALL DISMOI('F','NB_CMP_MAX',NOMGD,'GRANDEUR',NCMPMX,KBID,IBID)
      CALL DISMOI('F','NUM_GD',NOMGD,'GRANDEUR',GD,KBID,IBID)


C     1- REMPLISSAGE DE .TMP_NUCMP ET .TMP_NUCM1 (SI NOMGD /='VARI_R'):
C     -----------------------------------------------------------------
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',JCMPGD)
      IF (NOMGD.NE.'VARI_R') THEN
        CALL WKVECT('&&CESCEL.TMP_NUCMP','V V I',NCMPMX,JNUCMP)
        CALL WKVECT('&&CESCEL.TMP_NUCM1','V V I',NCMP1,JNUCM1)

        DO 10,ICMP1 = 1,NCMP1
          NOMCMP = ZK8(JCESC-1+ICMP1)
          ICMP = INDIK8(ZK8(JCMPGD),NOMCMP,1,NCMPMX)
          IF (ICMP.EQ.0) CALL UTMESS('F','CESCEL','LA CMP:'//NOMCMP//
     &                               ' N''APPARTIENT PAS A LA GRANDEUR:'
     &                               //NOMGD)
          ZI(JNUCMP-1+ICMP) = ICMP1
          ZI(JNUCM1-1+ICMP1) = ICMP
   10   CONTINUE
      END IF



C     2- ON ALLOUE LE CHAM_ELEM CEL "VIERGE"
C     =========================================

C     2.1 DETERMINATION DE OPTION SI NECESSAIRE :
C     -------------------------------------------
      IF (OPTION.EQ.' ') THEN
        IF (TYPCES.EQ.'ELNO') THEN
          OPTION = 'TOU_INI_ELNO'
        ELSE IF (TYPCES.EQ.'ELGA') THEN
          OPTION = 'TOU_INI_ELGA'
        ELSE IF (TYPCES.EQ.'ELEM') THEN
          OPTION = 'TOU_INI_ELEM'
        ELSE
          CALL UTMESS('F','CESCEL','STOP')
        END IF
      END IF
      CALL JENONU(JEXNOM('&CATA.OP.NOMOPT',OPTION),IOPT)


      IF (IOPT.EQ.0) CALL UTMESS('F','CESCEL','OPTION :'//OPTION//
     &                           ' INEXISTANTE DANS LES CATALOGUES.')


C     2.2 DETERMINATION DE NOMPAR SI NECESSAIRE :
C     -------------------------------------------
      IF (NOMPAR.EQ.' ')  NOMPAR=NOPAR2(OPTION,NOMGD,'INOUT')


C     2.3 CREATION DE DCEL :
C     ----------------------------------------------
      LICMP(1) = 'NPG_DYN'
      LICMP(2) = 'NCMP_DYN'
      DCEL = '&&CESCEL.DCEL'
      CALL CESCRE('V',DCEL,'ELEM',MA,'DCEL_I',2,LICMP,-1,-1,-2)
      CALL JEVEUO(DCEL//'.CESD','L',JDCELD)
      CALL JEVEUO(DCEL//'.CESV','E',JDCELV)
      CALL JEVEUO(DCEL//'.CESL','E',JDCELL)
      DO 60,IMA = 1,NBMA
        CALL CESEXI('C',JDCELD,JDCELL,IMA,1,1,1,IAD)
        IF (IAD.GE.0) CALL UTMESS('F','CESCEL','1')
        ZL(JDCELL-1-IAD) = .TRUE.
        ZI(JDCELV-1-IAD) = ZI(JCESD-1+5+4* (IMA-1)+2)

        CALL CESEXI('C',JDCELD,JDCELL,IMA,1,1,2,IAD)
        IF (IAD.GE.0) CALL UTMESS('F','CESCEL','2')
        ZL(JDCELL-1-IAD) = .TRUE.
        IF (NOMGD.EQ.'VARI_R') THEN
          NBPT = ZI(JCESD-1+5+4* (IMA-1)+1)
          NBSP = ZI(JCESD-1+5+4* (IMA-1)+2)
          NBCMP = ZI(JCESD-1+5+4* (IMA-1)+3)
          ICMPMX = 0
          DO 50,IPT = 1,NBPT
            DO 40,ISP = 1,NBSP
              DO 30,ICMP = 1,NBCMP
                CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD2)
                IF (IAD2.GT.0) ICMPMX = ICMP
   30         CONTINUE
   40       CONTINUE
   50     CONTINUE
          ZI(JDCELV-1-IAD) = ICMPMX
        ELSE
          ZI(JDCELV-1-IAD) = 0
        END IF
   60 CONTINUE


C     2.4 ALLOCATION DU CHAM_ELEM :
C     ----------------------------------------------
      CALL ALCHML(LIGREL,OPTION,NOMPAR,BASE,CEL,IRET,DCEL)
      IF (IRET.EQ.1) CALL UTMESS('F','CESCEL','LE PARAMETRE: '//NOMPAR//
     &                           ' DE L''OPTION: '//OPTION//
     &                           ' N''EST PAS CONNU '//
     &                           'DES TYPE_ELEM DU LIGREL: '//LIGREL)


C     3- ON REMPLIT LE .CELV :
C     ===================================================
      CALL JEVEUO(CEL//'.CELV','E',JCELV)
      CALL JELIRA(CEL//'.CELV','LONMAX',NEQ,KBID)
      CALL JEVEUO(CEL//'.CELD','L',JCELD)
      NBGR = ZI(JCELD-1+2)
      CALL JEVEUO(LIGREL//'.LIEL','L',IALIEL)
      CALL JEVEUO(JEXATR(LIGREL//'.LIEL','LONCUM'),'L',ILLIEL)


C     3.1 CAS NOMGD /= 'VARI_R' :
C     ---------------------------------------------------
      IF (NOMGD.NE.'VARI_R') THEN


C       3.1.1 ALLOCATION DE 2 VECTEURS DE TRAVAIL :
        NPTMX = ZI(JCESD-1+3)
        DO 70,IGR = 1,NBGR
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GO TO 70
          CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
          NBPT = MOD(ZI(JMOLO-1+4),10000)
          NPTMX = MAX(NPTMX,NBPT)
   70   CONTINUE

        CALL WKVECT('&&CESCEL.LONG_PT','V V I',NPTMX,JLPT)
        CALL WKVECT('&&CESCEL.LONG_PT_CUMU','V V I',NPTMX,JLCUPT)

C       3.1.2 BOUCLE SUR LES ELEMENTS DU LIGREL
        DO 150,IGR = 1,NBGR
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GO TO 150

          CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
          DIFF = (ZI(JMOLO-1+4).GT.10000)
          NBPT = MOD(ZI(JMOLO-1+4),10000)
          NBEL = NBELEM(LIGREL,IGR)

C         -- CALCUL DU NOMBRE DE CMPS POUR CHAQUE POINT
C            ET DU CUMUL SUR LES POINTS PRECEDENTS :
          DO 90,IPT = 1,NBPT
            ICO = 0
            K = 1
            IF (DIFF) K = IPT
            IADG = JMOLO - 1 + 4 + (K-1)*NEC + 1
            DO 80,ICMP = 1,NCMPMX
              IF (EXISDG(ZI(IADG),ICMP)) ICO = ICO + 1
   80       CONTINUE
            ZI(JLPT-1+IPT) = ICO
   90     CONTINUE

          CUMU = 0
          DO 100,IPT = 1,NBPT
            ZI(JLCUPT-1+IPT) = CUMU
            CUMU = CUMU + ZI(JLPT-1+IPT)
  100     CONTINUE

          DO 140,IPT = 1,NBPT
            ICO = 0
            K = 1
            IF (DIFF) K = IPT
            IADG = JMOLO - 1 + 4 + (K-1)*NEC + 1
            DO 130,ICMP = 1,NCMPMX
              IF (EXISDG(ZI(IADG),ICMP)) THEN
                ICO = ICO + 1
                ICMP1 = ZI(JNUCMP-1+ICMP)
                IF (ICMP1.EQ.0) THEN
                  IF (PROL) THEN
                    GO TO 130
                  ELSE
                    NOMCMP = ZK8(JCMPGD-1+ICMP)
                    CALL UTMESS('F','CESCEL',
     &                          'IL MANQUE LA CMP:'//NOMCMP)
                  END IF
                END IF

                DO 120,IEL = 1,NBEL
                  NUMA = NUMAIL(IGR,IEL)

C                 -- QUE FAIRE SI LA MAILLE EST TARDIVE ?
                  IF (NUMA.LT.0) THEN
                    IF (PROL2) THEN
                      GO TO 120
                    ELSE
                      CALL UTMESS('F','CESCEL',
     &                        'LE LIGREL CONTIENT DES MAILLES TARDIVES,'
     &                            )
                    END IF
                  END IF

                  NBPT2 = ZI(JCESD-1+5+4* (NUMA-1)+1)
                  IF (NBPT.NE.NBPT2) THEN
                    IF ((NBPT2.EQ.0) .AND. PROL2) THEN
                      GO TO 120
                    ELSE
                      CALL JENUNO(JEXNUM(MA//'.NOMMAI',NUMA),NOMMA)
                      CALL UTMESS('F','CESCEL','NOMBRES DE POINTS'//
     &                      ' DIFFERENTS POUR LA MAILLE: '//NOMMA//
     &                      ' CHAM_ELEM DE: '//NOMGD)
                    END IF
                  END IF


                  NBSPT = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+1)
                  NBSPT = MAX(NBSPT,1)
                  ADIEL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+4)
                  DO 110,ISPT = 1,NBSPT


                    CALL CESEXI('C',JCESD,JCESL,NUMA,IPT,ISPT,ICMP1,IAD)
                    IF (IAD.LE.0) THEN
                      IF (PROL) THEN
                        GO TO 110
                      ELSE
                        NOMCMP = ZK8(JCMPGD-1+ICMP)
                        CALL JENUNO(JEXNUM(MA//'.NOMMAI',NUMA),NOMMA)
                        CALL UTMESS('F','CESCEL',
     &                              'IL MANQUE LA CMP:'//NOMCMP//
     &                              ' SUR LA MAILLE:'//NOMMA)
                      END IF
                    END IF

                    IEQ = ADIEL - 1 + NBSPT*ZI(JLCUPT-1+IPT) +
     &                    (ISPT-1)*ZI(JLPT-1+IPT) + ICO
                    IF (TSCA.EQ.'R') THEN
                      ZR(JCELV-1+IEQ) = ZR(JCESV-1+IAD)
                    ELSE IF (TSCA.EQ.'I') THEN
                      ZI(JCELV-1+IEQ) = ZI(JCESV-1+IAD)
                    ELSE IF (TSCA.EQ.'C') THEN
                      ZC(JCELV-1+IEQ) = ZC(JCESV-1+IAD)
                    ELSE IF (TSCA.EQ.'L') THEN
                      ZL(JCELV-1+IEQ) = ZL(JCESV-1+IAD)
                    ELSE IF (TSCA.EQ.'K8') THEN
                      ZK8(JCELV-1+IEQ) = ZK8(JCESV-1+IAD)
                    ELSE
                      CALL UTMESS('F','CESCEL','STOP 1')
                    END IF
                    ZI(JCOPI-1+IAD)=1
  110             CONTINUE
  120           CONTINUE
              END IF
  130       CONTINUE
  140     CONTINUE
  150   CONTINUE


C     3.2 CAS NOMGD = 'VARI_R' :
C     ---------------------------------------------------
      ELSE
        DO 200,IGR = 1,NBGR
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GO TO 200

          CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
          DIFF = (ZI(JMOLO-1+4).GT.10000)
          IF (DIFF) CALL UTMESS('F','CESCEL','A FAIRE...')
          NBPT = MOD(ZI(JMOLO-1+4),10000)
          LGCATA = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+3)
          IF (NBPT.NE.LGCATA) CALL UTMESS('F','CESCEL','STOP 1')
          NBEL = NBELEM(LIGREL,IGR)


          DO 190,IEL = 1,NBEL
            NUMA = NUMAIL(IGR,IEL)

C           -- QUE FAIRE SI LA MAILLE EST TARDIVE ?
            IF (NUMA.LT.0) THEN
              IF (PROL2) THEN
                GO TO 190
              ELSE
                CALL UTMESS('F','CESCEL',
     &                      'LE LIGREL CONTIENT DES MAILLES TARDIVES,')
              END IF
            END IF

            NBPT2 = ZI(JCESD-1+5+4* (NUMA-1)+1)
            IF (NBPT.NE.NBPT2) THEN
              IF ((NBPT2.EQ.0) .AND. PROL2) THEN
                GO TO 190
              ELSE
                CALL JENUNO(JEXNUM(MA//'.NOMMAI',NUMA),NOMMA)
                CALL UTMESS('F','CESCEL','NOMBRES DE POINTS'//
     &                      ' DIFFERENTS POUR LA MAILLE: '//NOMMA//
     &                      ' CHAM_ELEM DE: '//NOMGD)
              END IF
            END IF

            NBSPT = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+1)
            NBSPT = MAX(NBSPT,1)
            NCDYN = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+2)
            ADIEL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+4)

            DO 180,IPT = 1,NBPT
              DO 170,ISPT = 1,NBSPT
                DO 160,ICMP = 1,NCDYN

                  CALL CESEXI('C',JCESD,JCESL,NUMA,IPT,ISPT,ICMP,IAD)
                  IF (IAD.LE.0) THEN
                    IF (PROL) THEN
                      GO TO 160
                    ELSE
                      NOMCMP = 'V'
                      CALL CODENT(ICMP,'G',NOMCMP(2:8))
                      CALL JENUNO(JEXNUM(MA//'.NOMMAI',NUMA),NOMMA)
                      CALL UTMESS('F','CESCEL',
     &                            'IL MANQUE LA CMP:'//NOMCMP//
     &                            ' SUR LA MAILLE:'//NOMMA)
                    END IF
                  END IF

                  IEQ = ADIEL - 1 + ((IPT-1)*NBSPT+ISPT-1)*NCDYN + ICMP
                  IF (TSCA.EQ.'R') THEN
                    ZR(JCELV-1+IEQ) = ZR(JCESV-1+IAD)
                  ELSE IF (TSCA.EQ.'I') THEN
                    ZI(JCELV-1+IEQ) = ZI(JCESV-1+IAD)
                  ELSE IF (TSCA.EQ.'C') THEN
                    ZC(JCELV-1+IEQ) = ZC(JCESV-1+IAD)
                  ELSE IF (TSCA.EQ.'L') THEN
                    ZL(JCELV-1+IEQ) = ZL(JCESV-1+IAD)
                  ELSE IF (TSCA.EQ.'K8') THEN
                    ZK8(JCELV-1+IEQ) = ZK8(JCESV-1+IAD)
                  ELSE
                    CALL UTMESS('F','CESCEL','STOP 2')
                  END IF
                  ZI(JCOPI-1+IAD)=1
  160           CONTINUE
  170         CONTINUE
  180       CONTINUE
  190     CONTINUE
  200   CONTINUE
      END IF


C     -- CALCUL DU NOMBRE DE CMPS NON RECOPIEES (NNCP):
C     ------------------------------------------------------
      NBVCOP=0
      DO 210, IAD=1,NBVCES
         IF (ZI(JCOPI-1+IAD).EQ.1) NBVCOP=NBVCOP+1
210   CONTINUE
      NNCP=NBVCES-NBVCOP


      CALL DETRSD('CHAM_ELEM_S',DCEL)
      CALL JEDETR('&&CESCEL.COPI')
      CALL JEDETR('&&CESCEL.TMP_NUCMP')
      CALL JEDETR('&&CESCEL.TMP_NUCM1')
      CALL JEDETR('&&CESCEL.LONG_PT')
      CALL JEDETR('&&CESCEL.LONG_PT_CUMU')

      CALL JEDEMA()
      END
