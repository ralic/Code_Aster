      SUBROUTINE CHRPEL(CHAMP1, REPERE, NBCMP, ICHAM, TYPE, NOMCH)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/08/2011   AUTEUR DESROCHE X.DESROCHES 
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
C TOLE CRP_20
      IMPLICIT      NONE
      INTEGER       NBCMP , ICHAM
      CHARACTER*(*) CHAMP1, REPERE, TYPE, NOMCH
C ----------------------------------------------------------------------
C
C     BUT : CHANGEMENT DE REPERE DANS LE CAS D'UN CHAM_ELEM
C ----------------------------------------------------------------------
C     ARGUMENTS :
C     CHAMP1   IN  K16  : NOM DU CHAMP A TRAITER
C     REPERE   IN  K8   : TYPE DE REPERE (UTILISATEUR OU CYLINDRIQUE)
C     NBCMP    IN  I    : NOMBRE DE COMPOSANTES A TRAITER
C     ICHAM    IN  I    : NUMERO D'OCCURRENCE
C     TYPE     IN  K8   : TYPE DU CHAMP : 'TENS' 'VECT' OU 'TORS'
C     NOMCH    IN  K16  : NOM DE CHAMP
C ----------------------------------------------------------------------
C ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------------
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
      CHARACTER*32      JEXNUM, JEXATR
C ----- FIN COMMUNS NORMALISES  JEVEUX  -------------------------------
C ---------------------------------------------------------------------
C
      INTEGER      I     , II    , INO   , IAD   , IPT   , ISP
      INTEGER      JCESD , JCESV , JCESL , NBPT  , AXYZM , NCMP
      INTEGER      JCONX1, JCONX2, NBSP  , INEL  , JCMP  , IPT2
      INTEGER      IBID  , NBMA  , JCESK , IRET  , INOT  , INBNO
      INTEGER      NDIM  , LICMPU(6), NBM, IDMAIL, NBMAIL, IMAI
      INTEGER      INOEU , IRET0  , IRET1 , NBGNO , IGNO,NNCP,I2
      INTEGER      IERK,  MNOGAK, MNOGAD, MNOGAL, MNOGAV, IADR
      INTEGER      IMAREF, NBNO,   NBPG,   NBNO2,   NBPG2, NUNO  , IPG
      LOGICAL      TEST
      REAL*8       ANGNOT(3), PGL(3,3), VALER(6), VALED(6),DDOT
      REAL*8       VALR , VALEI(6), XX, YY, ZZ
      REAL*8       VALET(6) , EPSI    , XNORMR  , PROSCA,  R8DGRD
      REAL*8       ORIG(3)  , AXEZ(3) , AXER(3) , AXET(3)
      REAL*8       VECTX(3), VECTY(3)
      REAL*8       X(27), Y(27), Z(27)
      REAL*8       XPG(27), YPG(27), ZPG(27)
      COMPLEX*16   VALETC(6)
      CHARACTER*1  K1B
      CHARACTER*3  TSCA
      CHARACTER*8  MA    , K8B, TYPMCL(2), NOMGD, TYCH, PARAM
      CHARACTER*16 OPTION,MOTCLE(2),NOMCH2
      CHARACTER*19 CHAMS1,CHAMS0,LIGREL,MANOGA
      CHARACTER*24 MESMAI
      CHARACTER*24 VALK(3)
C
      CALL JEMARQ()
      EPSI = 1.0D-6
      MOTCLE(1) = 'GROUP_MA'
      TYPMCL(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(2) = 'MAILLE'

      MESMAI = '&&CHRPEL.MES_MAILLES'
C
      IF (NBCMP.GT.0) THEN
         CALL WKVECT('&&CHRPEL.NOM_CMP','V V K8',NBCMP,JCMP)
         CALL GETVTX('MODI_CHAM','NOM_CMP',ICHAM,1,NBCMP,
     &                ZK8(JCMP),IBID)
      ELSE
           CALL U2MESS('F','ALGORITH2_6')
      ENDIF

      CALL DISMOI ( 'F', 'NOM_LIGREL', CHAMP1, 'CHAM_ELEM',
     &              IBID, LIGREL, IBID )

C
C ----- DEFINITION ET CREATION DU CHAM_NO SIMPLE CHAMS1
C ----- A PARTIR DU CHAM_NO CHAMP1
C
      CHAMS0='&&CHRPEL.CHAMS0'
      CHAMS1='&&CHRPEL.CHAMS1'
      CALL CELCES(CHAMP1,'V',CHAMS0)
      CALL CESRED(CHAMS0,0,0,NBCMP,ZK8(JCMP),'V',CHAMS1)
      CALL DETRSD('CHAM_ELEM_S',CHAMS0)
      CALL JEVEUO(CHAMS1//'.CESK','L',JCESK)
      CALL JEVEUO(CHAMS1//'.CESD','L',JCESD)
      MA     = ZK8(JCESK-1+1)
      NOMGD    = ZK8(JCESK-1+2)
      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)
C
C     ON EXCLUT LES MOT-CLES 'NOEUD' ET 'GROUP_NO'
      CALL JEVEUO(MA//'.DIME   ','L',INBNO)
      CALL WKVECT('&&CHRPEL.NOEUDS','V V K8',ZI(INBNO),INOEU)
      CALL GETVTX('MODI_CHAM','NOEUD',ICHAM,1,0,ZK8(INOEU),IRET0)
      CALL JEEXIN(MA//'.GROUPENO',IERK)
      IF (IERK.NE.0) THEN
        CALL JELIRA(MA//'.GROUPENO','NMAXOC',NBGNO,K1B)
        CALL WKVECT('&&CHRPEL.GROUP_NO','V V K8',NBGNO,IGNO)
        CALL GETVTX('MODI_CHAM','GROUP_NO',ICHAM,1,0,ZK8(IGNO),IRET1)
      ELSE
        IRET1=0
      ENDIF
      IF(IRET0.LT.0)THEN
           K8B='NOEUD   '
      ELSE IF(IRET1.LT.0)THEN
           K8B='GROUP_NO'
      ELSE
        GOTO 100
      ENDIF
      VALK (1) = K8B
      VALK (2) = NOMCH
      VALK (3) = ' '
      CALL U2MESG('F', 'ALGORITH12_42',3,VALK,0,0,0,0.D0)
 100  CONTINUE
      CALL JEDETR('&&CHRPEL.NOEUDS')
      CALL JEDETR('&&CHRPEL.GROUP_NO')
C
      NBMA   = ZI(JCESD-1+1)
      NCMP   = ZI(JCESD-1+2)
      CALL DISMOI ('F','Z_CST',MA,'MAILLAGE',NDIM,K8B,IRET)
      NDIM = 3
      IF (K8B.EQ.'OUI') NDIM = 2

      CALL RELIEM(' ',MA,'NU_MAILLE','MODI_CHAM',ICHAM,2,MOTCLE,TYPMCL,
     &                                                   MESMAI,NBM)
      IF (NBM.GT.0) THEN
        NBMAIL = NBM
        CALL JEVEUO(MESMAI,'L',IDMAIL)
      ELSE
        NBMAIL = NBMA
      ENDIF

      CALL JEEXIN(MA//'.CONNEX',IRET)
      CALL ASSERT(IRET.NE.0)
      CALL JEVEUO(MA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',JCONX2)
      CALL JEVEUO(CHAMS1//'.CESV','E',JCESV)
      CALL JEVEUO(CHAMS1//'.CESL','L',JCESL)
C
      DO 1 I = 1,6
         VALED(I) = 0.0D0
         VALER(I) = 0.0D0
         VALET(I) = 0.0D0
 1    CONTINUE
      DO 2 I = 1,3
         AXER(I)   = 0.0D0
         AXET(I)   = 0.0D0
         AXEZ(I)   = 0.0D0
         ORIG(I)   = 0.0D0
         ANGNOT(I) = 0.0D0
 2    CONTINUE
      LICMPU(1) = 1
      LICMPU(2) = 2
      LICMPU(3) = 3
      LICMPU(4) = 4
      LICMPU(5) = 5
      LICMPU(6) = 6
C
C ----- CHANGEMENT DE REPERE SUIVANT LE CHOIX UTILISATEUR
C
      IF (REPERE.EQ.'UTILISAT') THEN
C        SI LE NOUVEAU REPERE EST DONNE VIA DES VECTEURS
         CALL GETVR8('DEFI_REPERE','VECT_X',1,1,3,VECTX,IBID)
         IF ((IBID.EQ.3).AND.(NDIM.EQ.3)) THEN
            CALL GETVR8('DEFI_REPERE','VECT_Y',1,1,3,VECTY,IBID)
            CALL ANGVXY(VECTX,VECTY,ANGNOT)
         ELSE
           IF (NDIM.EQ.3) THEN
            CALL GETVR8('DEFI_REPERE','ANGL_NAUT',1,1,3,ANGNOT,IBID)
            IF (IBID.NE.3) THEN
               CALL U2MESS('F','ALGORITH2_7')
            ENDIF
           ELSE
            CALL GETVR8('DEFI_REPERE','ANGL_NAUT',1,1,1,ANGNOT(1),IBID)
            IF (IBID.NE.1) THEN
                           VALR = ANGNOT(1)
               CALL U2MESG('A', 'ALGORITH12_43',0,' ',0,0,1,VALR)
            ENDIF
           ENDIF
           ANGNOT(1) = ANGNOT(1)*R8DGRD()
           ANGNOT(2) = ANGNOT(2)*R8DGRD()
           ANGNOT(3) = ANGNOT(3)*R8DGRD()
         ENDIF
         CALL MATROT(ANGNOT,PGL)
         IF (TYPE(1:4).EQ.'TENS') THEN
C TENSEUR
            DO 10 INEL=1,NBMAIL
             IF (NBM.NE.0) THEN
               IMAI = ZI(IDMAIL+INEL-1)
             ELSE
               IMAI = INEL
             ENDIF
             NBPT = ZI(JCESD-1+5+4* (IMAI-1)+1)
             NBSP = ZI(JCESD-1+5+4* (IMAI-1)+2)
             IF (TSCA.EQ.'R') THEN
C CHAMP REEL
               DO 11,IPT = 1,NBPT
                  DO 12,ISP = 1,NBSP
                     DO 13 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALET(II)=ZR(JCESV-1+IAD)
                        ELSE
                           GOTO 10
                        ENDIF
 13                  CONTINUE
                     VALED(1) = VALET(1)
                     VALED(2) = VALET(4)
                     VALED(3) = VALET(2)
                     VALED(4) = VALET(5)
                     VALED(5) = VALET(6)
                     VALED(6) = VALET(3)
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALER(1) = VALET(1)
                     VALER(2) = VALET(3)
                     VALER(3) = VALET(6)
                     VALER(4) = VALET(2)
                     VALER(5) = VALET(4)
                     VALER(6) = VALET(5)
                     DO 14 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,
     &                              II,IAD)
                        IF (IAD.GT.0) THEN
                           ZR(JCESV-1+IAD) = VALER(II)
                        ELSE
                           GOTO 10
                        ENDIF
 14                  CONTINUE
 12               CONTINUE
 11            CONTINUE
              ELSE
C CHAMP COMPLEXE
               DO 111,IPT = 1,NBPT
                  DO 112,ISP = 1,NBSP
                     DO 113 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALETC(II)=ZC(JCESV-1+IAD)
                        ELSE
                           GOTO 10
                        ENDIF
 113                 CONTINUE
                     VALED(1) = DBLE(VALETC(1))
                     VALED(2) = DBLE(VALETC(4))
                     VALED(3) = DBLE(VALETC(2))
                     VALED(4) = DBLE(VALETC(5))
                     VALED(5) = DBLE(VALETC(6))
                     VALED(6) = DBLE(VALETC(3))
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALER(1) = VALET(1)
                     VALER(2) = VALET(3)
                     VALER(3) = VALET(6)
                     VALER(4) = VALET(2)
                     VALER(5) = VALET(4)
                     VALER(6) = VALET(5)
C
                     VALED(1) = DIMAG(VALETC(1))
                     VALED(2) = DIMAG(VALETC(4))
                     VALED(3) = DIMAG(VALETC(2))
                     VALED(4) = DIMAG(VALETC(5))
                     VALED(5) = DIMAG(VALETC(6))
                     VALED(6) = DIMAG(VALETC(3))
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALEI(1) = VALET(1)
                     VALEI(2) = VALET(3)
                     VALEI(3) = VALET(6)
                     VALEI(4) = VALET(2)
                     VALEI(5) = VALET(4)
                     VALEI(6) = VALET(5)
C
                     DO 114 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,
     &                              II,IAD)
                        IF (IAD.GT.0) THEN
                           ZC(JCESV-1+IAD) = DCMPLX(VALER(II),VALEI(II))
                        ELSE
                           GOTO 10
                        ENDIF
 114                 CONTINUE
 112              CONTINUE
 111           CONTINUE
             ENDIF
 10         CONTINUE
         ELSE
C  VECTEUR
            DO 15 INEL=1,NBMAIL
               IF (NBM.NE.0) THEN
                 IMAI = ZI(IDMAIL+INEL-1)
               ELSE
                 IMAI = INEL
               ENDIF
               NBPT = ZI(JCESD-1+5+4* (IMAI-1)+1)
               NBSP = ZI(JCESD-1+5+4* (IMAI-1)+2)
             IF (TSCA.EQ.'R') THEN
C CHAMP REEL
               DO 16,IPT = 1,NBPT
                  DO 17,ISP = 1,NBSP
                     DO 18 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALED(II) = ZR(JCESV-1+IAD)
                        ELSE
                           GOTO 15
                        ENDIF
 18                  CONTINUE
                     IF (NDIM.EQ.3) THEN
                        CALL UTPVGL(1,NCMP,PGL,VALED,VALER)
                     ELSE
                        CALL UT2VGL(1,NCMP,PGL,VALED,VALER)
                     ENDIF
                     DO 19 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           ZR(JCESV-1+IAD) = VALER(II)
                        ELSE
                           GOTO 15
                        ENDIF
 19                  CONTINUE
 17               CONTINUE
 16            CONTINUE
           ELSE
C CHAMP COMPLEXE
               DO 116,IPT = 1,NBPT
                  DO 117,ISP = 1,NBSP
                     DO 118 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALETC(II) = ZC(JCESV-1+IAD)
                           VALED(II) = DBLE(VALETC(II))
                           VALET(II) = DIMAG(VALETC(II))
                        ELSE
                           GOTO 15
                        ENDIF
 118                  CONTINUE
                     IF (NDIM.EQ.3) THEN
                        CALL UTPVGL(1,NCMP,PGL,VALED,VALER)
                        CALL UTPVGL(1,NCMP,PGL,VALET,VALEI)
                     ELSE
                        CALL UT2VGL(1,NCMP,PGL,VALED,VALER)
                        CALL UT2VGL(1,NCMP,PGL,VALET,VALEI)
                     ENDIF
                     DO 119 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           ZC(JCESV-1+IAD) = DCMPLX(VALER(II),VALEI(II))
                        ELSE
                           GOTO 15
                        ENDIF
 119                  CONTINUE
 117               CONTINUE
 116            CONTINUE
           ENDIF
 15         CONTINUE
         ENDIF
      ELSE
C REPERE CYLINDRIQUE
         CALL DISMOI('C','TYPE_CHAMP',CHAMP1,'CHAMP',IBID,
     &                TYCH, IRET)
         IF (NDIM.EQ.3) THEN
            CALL GETVR8('DEFI_REPERE','ORIGINE',1,1,3,ORIG,IBID)
            IF (IBID.NE.3) THEN
               CALL U2MESS('F','ALGORITH2_8')
            ENDIF
            CALL GETVR8('DEFI_REPERE','AXE_Z',1,1,3,AXEZ,IBID)
            IF (IBID.EQ.0) THEN
               CALL U2MESS('F','ALGORITH2_9')
            ENDIF
         ELSE
            CALL GETVR8('DEFI_REPERE','ORIGINE',1,1,2,ORIG,IBID)
            IF (IBID.NE.2) THEN
               CALL U2MESS('A','ALGORITH2_10')
            ENDIF
            CALL GETVR8('DEFI_REPERE','AXE_Z',1,1,0,AXEZ,IBID)
            IF (IBID.NE.0) THEN
               CALL U2MESS('A','ALGORITH2_1')
            ENDIF
            AXEZ(1) = 0.0D0
            AXEZ(2) = 0.0D0
            AXEZ(3) = 1.0D0
         ENDIF
         XNORMR = 0.0D0
         CALL NORMEV(AXEZ,XNORMR)
         CALL JEVEUO ( MA//'.COORDO    .VALE', 'L', AXYZM )
C
         MANOGA='&&CHRPEL.MANOGA'
C
         IF(NOMCH(1:4).EQ.'SIEF'.OR.NOMCH(1:4).EQ.'SIGM') THEN
           PARAM='PCONTRR'
         ELSE IF(NOMCH(1:4).EQ.'EPSI'.OR.NOMCH(1:4).EQ.'EPME' 
     &       .OR.NOMCH(1:4).EQ.'EPFD'.OR.NOMCH(1:4).EQ.'EPFP'
     &       .OR.NOMCH(1:4).EQ.'EPMG'.OR.NOMCH(1:4).EQ.'EPSG') THEN
           PARAM='PDEFORR'
         ELSE IF(NOMCH(1:4).EQ.'EPSP') THEN
           PARAM='PDEFOPL'
         ELSE IF(NOMCH(1:4).EQ.'EPVC') THEN
           PARAM='PDEFOVC'
         ELSE IF(NOMCH(1:4).EQ.'EPMQ'.OR.NOMCH(1:4).EQ.'EPEQ') THEN
           PARAM='PDEFOEQ'
         ELSE IF(NOMCH(1:4).EQ.'VARI') THEN
           PARAM='PVARIGR'
         ELSE
           CALL U2MESG('F','ALGORITH2_14',1,NOMCH,0,0,0,0.D0)
         ENDIF
C  POUR AVOIR DES OPTIONS FIGURANT DANS LES CATALOGUES D ELEMENTS
         NOMCH2 = NOMCH
         IF(NOMCH2.EQ.'VARI_ELGA') NOMCH2='VARI_ELGA_ELNO'
C
         CALL MANOPG(LIGREL,NOMCH2,PARAM,MANOGA)
C ----- TYPE DE COMPOSANTES
C
         IF (TYPE(1:4).EQ.'TENS') THEN
            IF (NDIM.EQ.2) THEN
               LICMPU(1)=1
               LICMPU(2)=2
               LICMPU(3)=3
               LICMPU(4)=5
            ENDIF
C
C  TRAITEMENT DIFFERENT SI CHAMP ELNO OU ELGA
C
            IF (TYCH(1:4).EQ.'ELNO') THEN
C
            DO 20 INEL = 1, NBMAIL
               IF (NBM.NE.0) THEN
                 IMAI = ZI(IDMAIL+INEL-1)
               ELSE
                 IMAI = INEL
               ENDIF
               NBPT = ZI(JCESD-1+5+4* (IMAI-1)+1)
               NBSP = ZI(JCESD-1+5+4* (IMAI-1)+2)
               NCMP = ZI(JCESD-1+5+4* (IMAI-1)+3)
               DO 21,IPT = 1,NBPT
                  DO 22,ISP = 1,NBSP
                     TEST = .TRUE.
                     DO 23 II=1,NCMP
                        CALL CESEXI('S',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           TEST = .FALSE.
                        ENDIF
 23                  CONTINUE
                     IF(TEST) GOTO 20
                     INO = ZI(JCONX1-1+ZI(JCONX2+IMAI-1)+IPT-1)
                     AXER(1) = ZR(AXYZM+3*(INO-1)  ) - ORIG(1)
                     AXER(2) = ZR(AXYZM+3*(INO-1)+1) - ORIG(2)
                     IF (NDIM.EQ.3) THEN
                        AXER(3) = ZR(AXYZM+3*(INO-1)+2) - ORIG(3)
                     ELSE
                        AXER(3) = 0.0D0
                     ENDIF
                     PROSCA=DDOT(3,AXER,1,AXEZ,1)
                     AXER(1) = AXER(1) - PROSCA*AXEZ(1)
                     AXER(2) = AXER(2) - PROSCA*AXEZ(2)
                     IF (NDIM.EQ.3) THEN
                        AXER(3) = AXER(3) - PROSCA*AXEZ(3)
                     ELSE
                        AXER(3) = 0.0D0
                     ENDIF
                     XNORMR = 0.0D0
                     CALL NORMEV(AXER,XNORMR)
                     IF (XNORMR .LT. EPSI) THEN
                        CALL JENUNO(JEXNUM(MA//'.NOMNOE',INO),K8B)
                        CALL U2MESS('A','ALGORITH2_12')
                        AXER(1) = 0.0D0
                        AXER(2) = 0.0D0
                        AXER(3) = 0.0D0
                        DO 24 IPT2 = 1,NBPT
                           INOT = ZI(JCONX1-1+ZI(JCONX2+IMAI-1)+IPT2-1)
                           AXER(1) = AXER(1) + ZR(AXYZM+3*(INOT-1)  )
                           AXER(2) = AXER(2) + ZR(AXYZM+3*(INOT-1)+1)
                           IF (NDIM.EQ.3) THEN
                              AXER(3) = AXER(3) +
     &                                  ZR(AXYZM+3*(INOT-1)+2)
                           ENDIF
 24                     CONTINUE
                        AXER(1) = AXER(1)/NBPT - ORIG(1)
                        AXER(2) = AXER(2)/NBPT - ORIG(2)
                        AXER(3) = AXER(3)/NBPT - ORIG(3)
                        PROSCA=DDOT(3,AXER,1,AXEZ,1)
                        AXER(1) = AXER(1) - PROSCA*AXEZ(1)
                        AXER(2) = AXER(2) - PROSCA*AXEZ(2)
                        IF (NDIM.EQ.3) THEN
                           AXER(3) = AXER(3) - PROSCA*AXEZ(3)
                        ELSE
                           AXER(3) = 0.0D0
                        ENDIF
                        XNORMR = 0.0D0
                        CALL NORMEV(AXER,XNORMR)
                        IF (XNORMR .LT. EPSI) THEN
                           CALL JENUNO(JEXNUM(MA//'.NOMNOE',INO),K8B)
                           VALK (1) = K8B
                     CALL U2MESG('F', 'ALGORITH12_44',1,VALK,0,0,0,0.D0)
                        ENDIF
                     ENDIF
                     CALL PROVEC(AXEZ,AXER,AXET)
                     XNORMR = 0.0D0
                     CALL NORMEV(AXET,XNORMR)
                     DO 26 I = 1,3
                        PGL(1,I) = AXER(I)
                        PGL(2,I) = AXEZ(I)
                        PGL(3,I) = AXET(I)
 26                  CONTINUE
                    IF (TSCA.EQ.'R') THEN
C CHAMP REEL
                     DO 27 II=1,NCMP
                        CALL CESEXI('S',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALET(II)=ZR(JCESV-1+IAD)
                        ELSE
                           GO TO 20
                        ENDIF
 27                  CONTINUE
                     VALED(1) = VALET(1)
                     VALED(2) = VALET(4)
                     VALED(3) = VALET(2)
                     VALED(4) = VALET(5)
                     VALED(5) = VALET(6)
                     VALED(6) = VALET(3)
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALER(1) = VALET(1)
                     VALER(2) = VALET(3)
                     VALER(3) = VALET(6)
                     VALER(4) = VALET(2)
                     VALER(5) = VALET(4)
                     VALER(6) = VALET(5)
                     DO 28 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,
     &                              II,IAD)
                        IF (IAD.GT.0) THEN
                           ZR(JCESV-1+IAD) = VALER(LICMPU(II))
                        ELSE
                           GOTO 20
                        ENDIF
 28                  CONTINUE
                    ELSE
C CHAMP COMPLEXE
                     DO 213 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALETC(II)=ZC(JCESV-1+IAD)
                        ELSE
                           GOTO 20
                        ENDIF
 213                 CONTINUE
                     VALED(1) = DBLE(VALETC(1))
                     VALED(2) = DBLE(VALETC(4))
                     VALED(3) = DBLE(VALETC(2))
                     VALED(4) = DBLE(VALETC(5))
                     VALED(5) = DBLE(VALETC(6))
                     VALED(6) = DBLE(VALETC(3))
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALER(1) = VALET(1)
                     VALER(2) = VALET(3)
                     VALER(3) = VALET(6)
                     VALER(4) = VALET(2)
                     VALER(5) = VALET(4)
                     VALER(6) = VALET(5)
C
                     VALED(1) = DIMAG(VALETC(1))
                     VALED(2) = DIMAG(VALETC(4))
                     VALED(3) = DIMAG(VALETC(2))
                     VALED(4) = DIMAG(VALETC(5))
                     VALED(5) = DIMAG(VALETC(6))
                     VALED(6) = DIMAG(VALETC(3))
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALEI(1) = VALET(1)
                     VALEI(2) = VALET(3)
                     VALEI(3) = VALET(6)
                     VALEI(4) = VALET(2)
                     VALEI(5) = VALET(4)
                     VALEI(6) = VALET(5)
C
                     DO 214 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,
     &                              II,IAD)
                        IF (IAD.GT.0) THEN
                           ZC(JCESV-1+IAD) = DCMPLX(VALER(II),VALEI(II))
                        ELSE
                           GOTO 20
                        ENDIF
 214                 CONTINUE
                    ENDIF
 22               CONTINUE
 21            CONTINUE
 20         CONTINUE
C
         ELSE IF (TYCH(1:4).EQ.'ELGA') THEN
C        ----------------------------
            CALL JEVEUO(MANOGA//'.CESK','L',MNOGAK)
            CALL JEVEUO(MANOGA//'.CESD','L',MNOGAD)
            CALL JEVEUO(MANOGA//'.CESL','L',MNOGAL)
            CALL JEVEUO(MANOGA//'.CESV','L',MNOGAV)
            CALL ASSERT(ZK8(MNOGAK).EQ.MA)
C
            DO 120 INEL = 1, NBMAIL
               IF (NBM.NE.0) THEN
                 IMAI = ZI(IDMAIL+INEL-1)
               ELSE
                 IMAI = INEL
               ENDIF
C
C  RECUP DE L'ADRESSE DE LA MATRICE DE PASSAGE NOEUDS ==> GAUSS
C
               CALL CESEXI('C',MNOGAD,MNOGAL,IMAI,1,1,1,IAD)
               IF (IAD.LE.0) GO TO 120
               IF (NINT(ZR(MNOGAV-1+IAD)).GT.0) THEN
                  IMAREF=IMAI
               ELSE
                  IMAREF=-NINT(ZR(MNOGAV-1+IAD))
               ENDIF
               CALL CESEXI('C',MNOGAD,MNOGAL,IMAREF,1,1,1,IAD)
               IF (IAD.LE.0) GO TO 120

               NBNO2 = NINT(ZR(MNOGAV-1+IAD))
               NBPG2 = NINT(ZR(MNOGAV-1+IAD+1))

               NBNO = ZI(JCONX2+IMAI) - ZI(JCONX2-1+IMAI)
               NBPG = ZI(JCESD-1+5+4* (IMAI-1)+1)
               NBSP = ZI(JCESD-1+5+4* (IMAI-1)+2)
               NCMP = ZI(JCESD-1+5+4* (IMAI-1)+3)
               CALL ASSERT(NBNO.EQ.NBNO2)
               CALL ASSERT(NBPG.EQ.NBPG2)
C
C  RECUP DES COORDONNEES DES NOEUDS
C
               DO 130 INO=1,NBNO
                  NUNO = ZI(JCONX1-1+ZI(JCONX2+IMAI-1)+INO-1)
                  X(INO) = ZR(AXYZM+3*(NUNO-1)  )
                  Y(INO) = ZR(AXYZM+3*(NUNO-1)+1)
                  Z(INO) = ZR(AXYZM+3*(NUNO-1)+2)
 130           CONTINUE
C
C  CALCUL DES COORDONNEES DES POINTS DE GAUSS DE LA MAILLE COURANTE
C
               DO 140 IPG=1,NBPG
                  XX=0.D0
                  YY=0.D0
                  ZZ=0.D0
                  IADR=MNOGAV-1+IAD+1+NBNO*(IPG-1)
                  DO 141 INO=1,NBNO
                     XX = XX + X(INO)*ZR(IADR+INO)
                     YY = YY + Y(INO)*ZR(IADR+INO)
                     ZZ = ZZ + Z(INO)*ZR(IADR+INO)
141               CONTINUE
                  XPG(IPG) = XX
                  YPG(IPG) = YY
                  ZPG(IPG) = ZZ
140            CONTINUE

               DO 121,IPG = 1,NBPG
                  DO 122,ISP = 1,NBSP
                     TEST = .TRUE.
                     DO 123 II=1,NCMP
                        CALL CESEXI('S',JCESD,JCESL,IMAI,IPG,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           TEST = .FALSE.
                        ENDIF
 123                  CONTINUE
                     IF(TEST) GOTO 120
                     AXER(1) = XPG(IPG) - ORIG(1)
                     AXER(2) = YPG(IPG) - ORIG(2)
                     IF (NDIM.EQ.3) THEN
                        AXER(3) = ZPG(IPG) - ORIG(3)
                     ELSE
                        AXER(3) = 0.0D0
                     ENDIF
                     PROSCA=DDOT(3,AXER,1,AXEZ,1)
                     AXER(1) = AXER(1) - PROSCA*AXEZ(1)
                     AXER(2) = AXER(2) - PROSCA*AXEZ(2)
                     IF (NDIM.EQ.3) THEN
                        AXER(3) = AXER(3) - PROSCA*AXEZ(3)
                     ELSE
                        AXER(3) = 0.0D0
                     ENDIF
                     XNORMR = 0.0D0
                     CALL NORMEV(AXER,XNORMR)
                     IF (XNORMR .LT. EPSI) THEN
                        CALL ASSERT (.FALSE.)
                     ENDIF
                     CALL PROVEC(AXEZ,AXER,AXET)
                     XNORMR = 0.0D0
                     CALL NORMEV(AXET,XNORMR)
                     DO 126 I = 1,3
                        PGL(1,I) = AXER(I)
                        PGL(2,I) = AXEZ(I)
                        PGL(3,I) = AXET(I)
 126                  CONTINUE
                    IF (TSCA.EQ.'R') THEN
C CHAMP REEL
                     DO 127 II=1,NCMP
                        CALL CESEXI('S',JCESD,JCESL,IMAI,IPG,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALET(II)=ZR(JCESV-1+IAD)
                        ELSE
                           GO TO 120
                        ENDIF
 127                  CONTINUE
                     VALED(1) = VALET(1)
                     VALED(2) = VALET(4)
                     VALED(3) = VALET(2)
                     VALED(4) = VALET(5)
                     VALED(5) = VALET(6)
                     VALED(6) = VALET(3)
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALER(1) = VALET(1)
                     VALER(2) = VALET(3)
                     VALER(3) = VALET(6)
                     VALER(4) = VALET(2)
                     VALER(5) = VALET(4)
                     VALER(6) = VALET(5)
                     DO 128 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPG,ISP,
     &                              II,IAD)
                        IF (IAD.GT.0) THEN
                           ZR(JCESV-1+IAD) = VALER(LICMPU(II))
                        ELSE
                           GOTO 120
                        ENDIF
 128                  CONTINUE
                    ELSE
C CHAMP COMPLEXE
                     DO 313 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPG,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALETC(II)=ZC(JCESV-1+IAD)
                        ELSE
                           GOTO 120
                        ENDIF
 313                 CONTINUE
                     VALED(1) = DBLE(VALETC(1))
                     VALED(2) = DBLE(VALETC(4))
                     VALED(3) = DBLE(VALETC(2))
                     VALED(4) = DBLE(VALETC(5))
                     VALED(5) = DBLE(VALETC(6))
                     VALED(6) = DBLE(VALETC(3))
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALER(1) = VALET(1)
                     VALER(2) = VALET(3)
                     VALER(3) = VALET(6)
                     VALER(4) = VALET(2)
                     VALER(5) = VALET(4)
                     VALER(6) = VALET(5)
C
                     VALED(1) = DIMAG(VALETC(1))
                     VALED(2) = DIMAG(VALETC(4))
                     VALED(3) = DIMAG(VALETC(2))
                     VALED(4) = DIMAG(VALETC(5))
                     VALED(5) = DIMAG(VALETC(6))
                     VALED(6) = DIMAG(VALETC(3))
                     CALL UTPSGL(1,3,PGL,VALED,VALET)
                     VALEI(1) = VALET(1)
                     VALEI(2) = VALET(3)
                     VALEI(3) = VALET(6)
                     VALEI(4) = VALET(2)
                     VALEI(5) = VALET(4)
                     VALEI(6) = VALET(5)
C
                     DO 314 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPG,ISP,
     &                              II,IAD)
                        IF (IAD.GT.0) THEN
                           ZC(JCESV-1+IAD) = DCMPLX(VALER(II),VALEI(II))
                        ELSE
                           GOTO 120
                        ENDIF
 314                 CONTINUE
                    ENDIF
 122              CONTINUE
 121           CONTINUE
 120        CONTINUE
         ENDIF
         ELSE
C VECTEUR
            IF (NDIM.EQ.2) THEN
               LICMPU(1)=1
               LICMPU(2)=3
               LICMPU(3)=2
            ENDIF
            DO 29 INEL=1,NBMAIL
               IF (NBM.NE.0) THEN
                 IMAI = ZI(IDMAIL+INEL-1)
               ELSE
                 IMAI = INEL
               ENDIF
               NBPT = ZI(JCESD-1+5+4* (IMAI-1)+1)
               NBSP = ZI(JCESD-1+5+4* (IMAI-1)+2)
               DO 30,IPT = 1,NBPT
                  DO 31,ISP = 1,NBSP
                     TEST = .TRUE.
                     DO 32 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           TEST = .FALSE.
                        ENDIF
 32                  CONTINUE
                     IF(TEST) GOTO 29
                     INO = ZI(JCONX1-1+ZI(JCONX2+IMAI-1)+IPT-1)
                     AXER(1) = ZR(AXYZM+3*(INO-1)  ) - ORIG(1)
                     AXER(2) = ZR(AXYZM+3*(INO-1)+1) - ORIG(2)
                     IF (NDIM.EQ.3) THEN
                        AXER(3) = ZR(AXYZM+3*(INO-1)+2) - ORIG(3)
                     ELSE
                        AXER(3) = 0.0D0
                     ENDIF
                     PROSCA=DDOT(3,AXER,1,AXEZ,1)
                     AXER(1) = AXER(1) - PROSCA*AXEZ(1)
                     AXER(2) = AXER(2) - PROSCA*AXEZ(2)
                     IF (NDIM.EQ.3) THEN
                        AXER(3) = AXER(3) - PROSCA*AXEZ(3)
                     ELSE
                        AXER(3) = 0.0D0
                     ENDIF
                     XNORMR = 0.0D0
                     CALL NORMEV(AXER,XNORMR)
                     IF (XNORMR .LT. EPSI) THEN
                        CALL JENUNO(JEXNUM(MA//'.NOMNOE',INO),K8B)
                        CALL U2MESS('A','ALGORITH2_12')
                        AXER(1) = 0.0D0
                        AXER(2) = 0.0D0
                        AXER(3) = 0.0D0
                        DO 33 IPT2 = 1,NBPT
                           INOT = ZI(JCONX1-1+ZI(JCONX2+IMAI-1)+IPT2-1)
                           AXER(1) = AXER(1) + ZR(AXYZM+3*(INOT-1)  )
                           AXER(2) = AXER(2) + ZR(AXYZM+3*(INOT-1)+1)
                           IF (NDIM.EQ.3) THEN
                              AXER(3) = AXER(3) +
     &                                  ZR(AXYZM+3*(INOT-1)+2)
                           ENDIF
 33                     CONTINUE
                        AXER(1) = AXER(1)/NBPT - ORIG(1)
                        AXER(2) = AXER(2)/NBPT - ORIG(2)
                        AXER(3) = AXER(3)/NBPT - ORIG(3)
                        PROSCA=DDOT(3,AXER,1,AXEZ,1)
                        AXER(1) = AXER(1) - PROSCA*AXEZ(1)
                        AXER(2) = AXER(2) - PROSCA*AXEZ(2)
                        IF (NDIM.EQ.3) THEN
                           AXER(3) = AXER(3) - PROSCA*AXEZ(3)
                        ELSE
                           AXER(3) = 0.0D0
                        ENDIF
                        XNORMR = 0.0D0
                        CALL NORMEV(AXER,XNORMR)
                        IF (XNORMR .LT. EPSI) THEN
                           CALL JENUNO(JEXNUM(MA//'.NOMNOE',INO),K8B)
                           VALK (1) = K8B
                    CALL U2MESG('F', 'ALGORITH12_44',1,VALK,0,0,0,0.D0)
                        ENDIF
                     ENDIF
                     CALL PROVEC(AXEZ,AXER,AXET)
                     XNORMR = 0.0D0
                     CALL NORMEV(AXET,XNORMR)
                     DO 35 I = 1,3
                        PGL(1,I) = AXER(I)
                        PGL(2,I) = AXEZ(I)
                        PGL(3,I) = AXET(I)
 35                  CONTINUE
             IF (TSCA.EQ.'R') THEN
C CHAMP REEL
                     DO 36 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALED(II)=ZR(JCESV-1+IAD)
                        ELSE
                           GOTO 29
                        ENDIF
 36                  CONTINUE
                     IF (NDIM.EQ.3) THEN
                        CALL UTPVGL(1,3,PGL,VALED,VALER)
                     ELSE
                        CALL UT2VGL(1,3,PGL,VALED,VALER)
                     ENDIF
                     DO 37 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,
     &                              II,IAD)
                        IF (IAD.GT.0) THEN
                           ZR(JCESV-1+IAD) = VALER(LICMPU(II))
                        ELSE
                           GOTO 29
                        ENDIF
 37                  CONTINUE
            ELSE
C CHAMP COMPLEXE
                     DO 136 II=1,NCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,II,IAD)
                        IF (IAD.GT.0) THEN
                           VALETC(II) = ZC(JCESV-1+IAD)
                           VALED(II) = DBLE(VALETC(II))
                           VALET(II) = DIMAG(VALETC(II))
                        ELSE
                           GOTO 29
                        ENDIF
 136                  CONTINUE
                     IF (NDIM.EQ.3) THEN
                        CALL UTPVGL(1,3,PGL,VALED,VALER)
                        CALL UTPVGL(1,3,PGL,VALET,VALEI)
                     ELSE
                        CALL UT2VGL(1,3,PGL,VALED,VALER)
                        CALL UT2VGL(1,3,PGL,VALET,VALEI)
                     ENDIF
                     DO 137 II=1,NBCMP
                        CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,
     &                              II,IAD)
                        IF (IAD.GT.0) THEN
                           I2=LICMPU(II)
                           ZC(JCESV-1+IAD) = DCMPLX(VALER(I2),VALEI(I2))
                        ELSE
                           GOTO 29
                        ENDIF
 137                  CONTINUE
            ENDIF

 31               CONTINUE
 30            CONTINUE
 29         CONTINUE
         ENDIF
      ENDIF
      CALL DISMOI ( 'F', 'NOM_OPTION', CHAMP1, 'CHAM_ELEM',
     &              IBID, OPTION, IBID )
      CALL CESCEL(CHAMS1,LIGREL,OPTION,' ','OUI',NNCP,'G',CHAMP1,'F',
     &            IBID)
      CALL DETRSD('CHAM_ELEM_S',CHAMS1)
      CALL JEDETR('&&CHRPEL.NOM_CMP')
      CALL JEDETR(MESMAI)
      CALL JEDEMA( )
C
      END
