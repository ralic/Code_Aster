      SUBROUTINE ACEAMR(NOMA,NOMO,LMAX,NOEMAF,NBOCC,IVR,IFM)
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       IFM,LMAX,NOEMAF,NBOCC,IVR(*)
      CHARACTER*8   NOMA,NOMO
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/10/2012   AUTEUR DEVESA G.DEVESA 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET PAR
C     MASSE REPARTIE
C ----------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : NOMO   : NOM DU MODELE
C IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
C IN  : NBOCC  : NOMBRE D'OCCURRENCES DU MOT CLE MASS_AJOU
C IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
C ----------------------------------------------------------------------
C
      INTEGER      NBCAR,NBVAL,NRD
      PARAMETER    ( NBCAR = 100 , NBVAL = 6 , NRD = 2 )
      INTEGER      JDC(3), JDV(3), IBID,NIV,IUNITE
      INTEGER      JDCINF,  JDVINF
      INTEGER      I,IAMTO,IER,II,IN,INBN,INO,INOE,IOC,IREP
      INTEGER      IRGNO,IRGTO,ISYM,ITBMP,ITBNO,ITROU,IUNIFI,IV
      INTEGER      IREPN,IREPV,IAEPN,IAEPV
      INTEGER      IXCI,IXCKMA,IXNW,J,JD,JDDI,JDLS,JDNW,JJ,JN,K,KK
      INTEGER      L,LDGM,LDNM,LOKM,LOREP,NBNMA
      INTEGER      NBNO,NBNOEU,NBORM,NC,NCAR,NCARAC,NCMP,NBORP
      INTEGER      NDIM,NG,NGP,NMA,NREP,NUMNOE,NVAL,DIMCAR
      INTEGER      VALI(2),NBVAL2

      REAL*8       VAL(NBVAL), ETA, VALE(NBVAL),R8BID
      CHARACTER*1  KMA(3), K1BID
      CHARACTER*8  NOMNOE,NOGP,NOMMAI,K8BID,NOMU,CAR(NBCAR),LAMASS
      CHARACTER*16 REP,REPDIS(NRD),CONCEP,CMD
      CHARACTER*19 CART(3),CARTDI
      CHARACTER*19 VREPXV,VREPXN,VAEPXV,VAEPXN
      CHARACTER*24 TMPND(3),TMPVD(3)
      CHARACTER*24 MLGNNO, MLGNMA, TMCINF, TMVINF


      LOGICAL      TRANSL,LBID,LVALE
      INTEGER      IARG
      DATA REPDIS  /'GLOBAL          ','LOCAL           '/
      DATA KMA     /'K','M','A'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
      NBVAL2 = 3

      MLGNNO = NOMA//'.NOMNOE'
      MLGNMA = NOMA//'.NOMMAI'
      CALL WKVECT('&&TMPDISCRET','V V K8',LMAX,JDLS)
      CALL WKVECT('&&TMPTABNO','V V K8',LMAX,ITBNO)
      CALL WKVECT('&&TMPRIGNO','V V R',6*LMAX,IRGNO)
      CALL WKVECT('&&TMPRIGTO','V V R',6*NOEMAF,IRGTO)
      CALL WKVECT('&&TMPTABMP','V V K8',LMAX,ITBMP)

C
      IFM = IUNIFI('MESSAGE')
C
C --- RECUPERATION DE LA DIMENSION GEOMETRIQUE DU MODELE
      CALL DISMOI('F','DIM_GEOM',NOMO,'MODELE',IBID,K8BID,IER)
      NDIM=IBID
      IF (IBID.GE.100) THEN
         IBID = IBID - 100
         NDIM=1
      ENDIF
      IF (IBID.GE.20) THEN
         IBID = IBID - 20
         NDIM=2
      ENDIF
      IF (IBID.EQ.3) NDIM=3
C     POUR LES DISCRETS C'EST OBLIGATOIREMENT DU 2D OU 3D
      CALL ASSERT( (NDIM.EQ.2).OR.(NDIM.EQ.3) )
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
      CARTDI = NOMU//'.CARDINFO'
      TMCINF = CARTDI//'.NCMP'
      TMVINF = CARTDI//'.VALV'
C     SI LA CARTE N'EXISTE PAS ON LA CREE
      CALL JEEXIN(TMCINF,IXCI)
      IF (IXCI.EQ.0) CALL ALCART('G',CARTDI,NOMA,'CINFDI')
      CALL JEVEUO(TMCINF,'E',JDCINF)
      CALL JEVEUO(TMVINF,'E',JDVINF)
C     PAR DEFAUT POUR M, A, K :
C        REPERE GLOBAL, MATRICE SYMETRIQUE, PAS AFFECTEE
      CALL INFDIS('DIMC',DIMCAR,R8BID,K8BID)
      DO 200 I = 1 , 3
         ZK8(JDCINF+I-1) = 'REP'//KMA(I)//'    '
         CALL INFDIS('INIT',IBID,ZR(JDVINF+I-1),ZK8(JDCINF+I-1))
         ZK8(JDCINF+I+2) = 'SYM'//KMA(I)//'    '
         CALL INFDIS('INIT',IBID,ZR(JDVINF+I+2),ZK8(JDCINF+I+2))
         ZK8(JDCINF+I+5) = 'DIS'//KMA(I)//'    '
         CALL INFDIS('INIT',IBID,ZR(JDVINF+I+5),ZK8(JDCINF+I+5))
200   CONTINUE
      ZK8(JDCINF+9)  = 'ETAK    '
      CALL INFDIS('INIT',IBID,ZR(JDVINF+9),ZK8(JDCINF+9))
      ZK8(JDCINF+10) = 'TYDI    '
      CALL INFDIS('INIT',IBID,ZR(JDVINF+10),ZK8(JDCINF+10))
C --- CREATION DES CARTES
      DO 220 I = 1 , 3
         CART(I)  = NOMU//'.CARDISC'//KMA(I)
         TMPND(I) = CART(I)//'.NCMP'
         TMPVD(I) = CART(I)//'.VALV'
C        SI LES CARTES N'EXISTENT PAS ON LES CREE
         CALL JEEXIN(TMPND(I),IXCKMA)
         IF (IXCKMA .EQ. 0) THEN
            CALL ALCART('G',CART(I),NOMA,'CADIS'//KMA(I))
         ENDIF
         CALL JEVEUO(TMPND(I),'E',JDC(I))
         CALL JEVEUO(TMPVD(I),'E',JDV(I))
220   CONTINUE
C
C     RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IBID,NIV)

C --- BOUCLE SUR LES OCCURRENCES DE MASS_AJOU
      DO 30 IOC = 1 , NBOCC
         ETA = 0.0D0
C        PAR DEFAUT ON EST DANS LE REPERE GLOBAL, MATRICES SYMETRIQUES
         IREP = 1
         ISYM = 1
         REP = REPDIS(1)
         LVALE=.FALSE.
C
         CALL GETVEM(NOMA,'GROUP_MA','MASS_AJOU','GROUP_MA',
     &               IOC,IARG,LMAX,ZK8(JDLS),NG)
C         CALL GETVR8('MASS_AJOU','VALE'    ,IOC,IARG,NBVAL,VALE,NVAL)
         CALL R8INIR(NBVAL,0.0D0,VALE,1)
C         CALL GETVTX('MASS_AJOU','REPERE'  ,IOC,IARG,1,REP,NREP)
         CALL GETVTX('MASS_AJOU','GROUP_MA_POI1',IOC,IARG,1,NOGP,NGP)
         
         DO 32 I = 1 , NRD
            IF (REP.EQ.REPDIS(I)) IREP = I
 32      CONTINUE
C        UNITE POUR IMPRIMER LES VALEUR DES DISCRETS
C         CALL GETVIS('MASS_AJOU','UNITE',IOC,IARG,1,IBID,IER)
         IUNITE = 6
C
         NCARAC=1
         IF ( IUNITE .GT. 0) THEN
            WRITE(IUNITE,1000) REP,IOC
         ENDIF
C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
         IF (NG.LE.0) GOTO 30
         CAR(1)='M_T_N'

C         II = 0
         DO 34 NC = 1,NCARAC
            TRANSL=.TRUE.
C
            IF ( TRANSL ) THEN
               LAMASS = 'K_T_D_N'
               CALL MASREP(NOMA,IOC,VALE,LVALE,NG,ZK8(JDLS),NBNO,
     &               ZK8(ITBNO),ZR(IRGNO),ZR(IRGTO),NDIM)
            ELSE
               CALL ASSERT( .FALSE. )
            ENDIF

            DO 255 INO = 1,NBNO
                ZK8(ITBMP + INO - 1) = ' '
 255        CONTINUE

C

            IF (NGP.NE.0) THEN
               NBNOEU = 1
               LOKM   = 5

               CALL JELIRA(JEXNOM(NOMA//'.GROUPEMA',NOGP),'LONMAX',
     &                     NMA,K8BID)
               CALL JEVEUO(JEXNOM(NOMA//'.GROUPEMA',NOGP),'L',LDGM)

               IF (NMA.NE.NBNO) THEN
                  VALI(1) = NBNO
                  VALI(2) = NMA
                  CALL U2MESG('F','MODELISA2_10',1,NOGP,2,VALI,0,R8BID)
               ENDIF


               DO 22 IN = 0,NMA-1
C                 RECUPERE LE NOMBRE DE NOEUD DE LA MAILLE
                  CALL JELIRA(JEXNUM(NOMA//'.CONNEX',ZI(LDGM+IN)),
     &                        'LONMAX',NBNMA,K8BID)
                  CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',ZI(LDGM+IN)),
     &                        'L',LDNM)
                  CALL JENUNO(JEXNUM(MLGNMA,ZI(LDGM+IN)),NOMMAI)
C                 BOUCLE SUR LE NB DE NOEUD DE LA MAILLE
                  IF ( NBNMA .NE. NBNOEU) THEN
                     CALL U2MESK('F','MODELISA_20',1,NOMMAI)
                  ENDIF
                  DO 25 INBN = 1 , NBNMA
                     INOE = ZI(LDNM+INBN-1)
                     CALL JENUNO(JEXNUM(MLGNNO,INOE),NOMNOE)
                     DO 24 INO = 1, NBNO
                        IF (ZK8(ITBNO+INO-1) .EQ. NOMNOE) THEN
                           ZK8(ITBMP+INO-1) = NOMMAI

                           GOTO 22
                        ENDIF
 24                  CONTINUE
 25               CONTINUE
C                 SI ON PASSE ICI AUCUN DES NOEUDS DU DISCRET APPARTIENT
C                 A LA SURFACE, ET CE N'EST PAS NORMAL
                  WRITE(IFM,*)'GROUP_MA :',(' '//ZK8(JDLS+II-1),II=1,NG)
                  CALL U2MESK('F','MODELISA_21',1,NOMNOE)
 22            CONTINUE
C              PREPARATION DES IMPRESSIONS DANS LE FICHIER MESSAGE
               LOREP  = 5
               IF ( IREP .EQ. 1) LOREP  = 6

C
C            VERIF QU'UN DISCRET EST FIXE A CHACUN DES NOEUDS DU RADIER
C            (UNE SEULE FOIS PAR OCCURRENCE DE MASS_AJOU)
               IF (NC .EQ. 1) THEN
                  DO 227 INO = 1, NBNO
                    IF(ZK8(ITBMP + INO - 1).EQ.' ')THEN
                       CALL JENUNO(JEXNUM(MLGNNO,INO),NOMNOE)
                       CALL U2MESK('F','MODELISA2_8',1,NOMNOE)
                    ENDIF
 227              CONTINUE
               ENDIF

               IF ( IUNITE.GT.0 .AND. NIV.EQ.2) THEN
                  DO 27 I = 1,NBNO
                     IV = 1
                     JD = ITBMP + I - 1
                     JN = ITBNO + I - 1
                     IF ( NBNOEU .EQ. 1 ) THEN
                        IF ( TRANSL ) THEN
                           WRITE(IUNITE,1011) 'MAILLE',ZK8(JN),
     &                        CAR(NC)(1:LOKM),
     &                        (ZR(IRGNO+6*I-6+JJ),JJ=0,5),
     &                        REPDIS(IREP)(1:LOREP)
                        ENDIF
                     ENDIF
 27               CONTINUE
               ENDIF

               DO 28 I = 1,NBNO
                  IV = 1
                  JD = ITBMP + I - 1
                  JN = ITBNO + I - 1

                  CALL AFFDIS(NDIM,IREP,ETA,CAR(NC),ZR(IRGNO+6*I-6),
     &                        JDC,JDV,IVR,IV,KMA,NCMP,L,
     &                        JDCINF,JDVINF,ISYM,IFM)
                  CALL NOCART(CARTDI, 3,' ','NOM',1,ZK8(JD),0,' ',
     &                        DIMCAR)
                  CALL NOCART(CART(L),3,' ','NOM',1,ZK8(JD),0,' ',
     &                        NCMP)
C                 AFFECTATION DE MATRICE RIGIDITE NULLE
                  IV = 1

                  CALL R8INIR(NBVAL2,0.0D0,VALE,1)
                  CALL AFFDIS(NDIM,IREP,ETA,LAMASS,VALE,
     &                        JDC,JDV,IVR,IV,KMA,NCMP,L,
     &                        JDCINF,JDVINF,ISYM,IFM)
                  CALL NOCART(CARTDI, 3,' ','NOM',1,ZK8(JD),0,' ',
     &                        DIMCAR)
                  CALL NOCART(CART(L),3,' ','NOM',1,ZK8(JD),0,' ',
     &                        NCMP)
 28            CONTINUE
            ENDIF
 34      CONTINUE
 
 30   CONTINUE

      CALL JEDETR('&&TMPDISCRET')
      CALL JEDETR('&&TMPTABNO')
      CALL JEDETR('&&TMPRIGNO')
      CALL JEDETR('&&TMPRIGTO')
      CALL JEDETR('&&TMPTABMP')
      CALL GETFAC('RIGI_PARASOL',NBORP)
      CALL GETFAC('RIGI_MISS_3D',NBORM)
      IF (NBORP.EQ.0.AND.NBORM.EQ.0) THEN
         DO 240 I = 1 , 3
            CALL JEDETR(TMPND(I))
            CALL JEDETR(TMPVD(I))
240      CONTINUE
         CALL JEDETR(TMCINF)
         CALL JEDETR(TMVINF)
      ENDIF
C
      CALL JEDEMA()
C
 1000 FORMAT(/,
     &    ' <DISCRET> MATRICES AFFECTEES AUX ELEMENTS DISCRET ',
     &    '(REPERE ',A6,'), OCCURRENCE ',I4)

 1010 FORMAT(' _F(',A,'=''',A8,''', CARA=''',A,''',',/,
     &       '   VALE=(',1X,1PE12.5,','1X,1PE12.5,','1X,1PE12.5,','/,
     &       '         ',1X,1PE12.5,','1X,1PE12.5,','1X,1PE12.5,',),',/,
     &       '   REPERE=''',A,'''),')
 1011 FORMAT(' _F(',A,'=''',A8,''', CARA=''',A,''',',/,
     &       '   VALE=(',3(1X,1PE12.5,','),/,
     &       '         ',3(1X,1PE12.5,','),'),',/,
     &       '   REPERE=''',A,'''),')
      END
