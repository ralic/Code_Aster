      SUBROUTINE FETCRF(SDFET1)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/11/2004   AUTEUR ASSIRE A.ASSIRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C RESPONSABLE ASSIRE A.ASSIRE
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CREATION DE LA STRUCTURE DE DONNEES FETI.
C      ELLE EST CONTENUE DANS UN CONCEPT PRODUIT DE TYPE SD_FETI
C
C IN SDFETI   : NOM DU CONCEPT PRODUIT
C OUT SDFETI  : LE CONCEPT EST CREE ET INSTANCIE
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       MESSAGE:INFNIV.
C       JEVEUX:JEMARQ,JEDEMA,JECROC,JEECRA,JEVEUO,WKVECT.
C
C     FONCTIONS INTRINSEQUES:
C       NONE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       02/11/03 (OB): CREATION.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*8 SDFET1

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
C DECLARATION VARIABLES LOCALES
      INTEGER     NBMA(10),LISTMA(10,2),NBNO(10),LISTNO(12,2),
     &   NBVO(10),LISTCO(3,2),NBFETE,LISTFE(10),LISTNI(10),NBPR(10),
     &   LISTPR(10,2),NBDDL(10)
      INTEGER      JADR,I,J,NBSD,INTBUF,NBNOIN,IFM,NIV,NBMAI,
     &             NBNOI3,NBNOG,NBCHAR,MULTC
      CHARACTER*8  K8BID,K8BUFF,NOMSD(10)
      CHARACTER*19 SDFETI
      CHARACTER*24 NOMSDA,NOMSDB,NOMSDI,NOMSDG,NOMSDM,NOMSDH,NOMSDJ,
     &             NOMSLN,NOMSLI,NOMSLM
      CHARACTER*32 JEXNOM
      
      INTEGER      NBID,ITMP,JVALE,NBGRMN,IALIK8,IALII1,IALII2,
     &             NBK8,NBIS,IRET,IER,NBNOTO,IERD,IALINO,JTRAV,IANBNO,
     &             NBGMA,NBMAIL,IALIMA,II,JJ,NBNOSD,NB,IANBMA,IALIBD,
     &             IS9,INCRS,L,XT,YT,ZT,VALE(12),IALSNO,IALSMU,K,KK,NN,
     &             LINOMA,IALSMA,IND,JNOMA,JPRNM,NEC,N,INO,DDLC,
     &             IALSK,NUMSD,IALSPO,IPOS,IAJADR,JTMP,IALSTR,IALSTB,
     &             JADRI,JADRJ,IALSFG,NBMATO,NBVM,NGMA(1000),NMA(1000),
     &             NBRD(1000),NBMABD(1000),IALSML,IALSMD,IALSCR,NBMAMA
      CHARACTER*1  K1BID
      CHARACTER*4  K4TMP
      CHARACTER*8  LSTGMA(1000),LSTMA(1000),KTMP,NOM,MA,K8B,NOMGMA,NOMO,
     &             LSTBRD(1000),NOMCHA(1000),NOMN
      CHARACTER*16 CONCEP,CMD,OPTION, MOTCLE, TYPMCL, MOTFAC
      CHARACTER*19 LIGRMO,LIGRCH
      CHARACTER*24 NOMNOE,GRPNOE,COOVAL,LISNO,LISNOM,NOMREF
      CHARACTER*24 GRPMA,GRPNO
      CHARACTER*32 JEXNUM
      LOGICAL      EXISDG
      
      CHARACTER*8  MODELE, CHARGE
      INTEGER      IBID,ND,IADR,N1,N2,N3,N4,NBOBJ1,LONLI1,IDLIG1,NVAL,
     &             ICSDMA,ICHNBM,ISDLSM,NBMATA,ILMAT,NDTAR,
     &             ISDLCH,ISDCHA,ISDMAT,INBMCH,IFLIM,IDFLIN,IFLII,DEC,
     &             ICH,ISD,IMAT,ILSCHA,IDFLII,IDFLIM,IDFLM,IDFLN,IFNT,
     &             NB1,NB2,NB3,BIFLII,BIFLIM,BIFLN,JADRH,NBNOTA,
     &             NOINCH,NBNOEU

C CORPS DU PROGRAMME
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C INITIALISATIONS
      SDFETI=SDFET1
      NOMREF=SDFETI//'.FREF'            
      NOMSDM=SDFETI//'.FDIM'      
      NOMSDA=SDFETI//'.FETA'
      NOMSDB=SDFETI//'.FETB'
C      NOMSDD=SDFETI//'.FETD'
C      NOMSDE=SDFETI//'.FETE'
      NOMSDI=SDFETI//'.FETI'
      NOMSDG=SDFETI//'.FETG'
      NOMSDH=SDFETI//'.FETH'
      NOMSDJ=SDFETI//'.FETJ'

      NOMSLN=SDFETI//'.FLIN'
      NOMSLI=SDFETI//'.FLII'
      NOMSLM=SDFETI//'.FLIM'

C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
      CALL GETVID(' ','MODELE',1,1,1,NOMO,NBVM)

C --- LIGREL DU MODELE
      LIGRMO = NOMO(1:8)//'.MODELE'


C CREATION DES DIFFERENTS ATTRIBUTS DE LA S.D. SD_FETI
C .REFE

C NBRE DE CHARGE
      CALL GETFAC('EXCIT',NBCHAR)
      IF (NIV.GE.4) WRITE(6,*) 'NBCHAR=',NBCHAR

C LISTE DES CHARGES
      IF (NBCHAR.GE.1) THEN
        DO 800 I = 1,NBCHAR
          CALL GETVID('EXCIT','CHARGE',I,1,1,K8B,ND)
          IF (NIV.GE.4) WRITE(6,*) 'CHARGE=',K8B
          NOMCHA(I)=K8B
  800   CONTINUE
      ENDIF 

C     CREATION .FREF
      INTBUF=NBCHAR+1
      CALL WKVECT(NOMREF,'G V K8',INTBUF,JADR)
      ZK8(JADR)=NOMO
      DO 50 I=1,NBCHAR
        ZK8(JADR+I)=NOMCHA(I)
   50 CONTINUE

      IF (NIV.GE.3) THEN 
        WRITE(IFM,*)
        WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMREF 
      ENDIF 




C --- MAILLAGE ASSOCIE AU MODELE
      CALL JEVEUO(LIGRMO//'.NOMA','L',JNOMA)
      MA = ZK8(JNOMA)

C DETERMINATION DU NB TOTAL DE NOEUDS ET DE MAILLES DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNOTO,K8B,IERD)
      CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMATO,K8B,IERD)
      IF (NIV.GE.4) WRITE(6,*) 'NBNOTO ET NBMATO=',NBNOTO,NBMATO

      GRPMA = MA//'.GROUPEMA       '

C
C --- ACCES A L'OBJET .PRNM
C
      CALL JEVEUO(LIGRMO//'.PRNM','L',JPRNM)
      CALL JELIRA(LIGRMO//'.PRNM','LONMAX',N,K8B)
      NEC = N/NBNOTO



C
C --- LECTURE DU CONTENU DES MOT-CLES DEFI
C
      CALL GETFAC('DEFI',NBSD)
      NBSD = ABS(NBSD)

      DO 1 I = 1, NBSD
        CALL GETVID('DEFI','GROUP_MA',I,1,1,LSTGMA(I),NGMA(I))
C        WRITE(6,*) 'NGMA=',NGMA,LSTGMA(I)
        CALL GETVID('DEFI','GROUP_MA_BORD',I,1,1,LSTBRD(I),NBRD(I))
C        WRITE(6,*) 'NBRD=',NBRD,LSTBRD(I)
C        CALL GETVID('DEFI','MAILLE',  I,1,1,LSTMA(I),NMA(I))
C        WRITE(6,*) 'NMA=',NMA,LSTMA(I)
  1   CONTINUE


C NOMS DES SOUS-DOMAINES : NOMSD
C  ON PREND LE MOT-CLE NOM TRONQUE A 4 CARACTERES ET ON AJOUTE LE NUM
      CALL GETVTX(' ','NOM',1,1,1,NOM,NBID)
      K4TMP=NOM
      ITMP=0
      DO 3 I = 1, LEN(K4TMP)
        IF ( K4TMP(I:I) .NE. ' ' ) ITMP=ITMP+1
  3   CONTINUE

      DO 5 I = 1, NBSD
        WRITE(KTMP,'(I4)') I
        CALL LXCADR(KTMP)
        NOMSD(I) = K4TMP(1:ITMP)//KTMP
C         CALL JEEXIN(JEXNOM(GRPMA,NOMSD(I)),IRET)
C         IF (IRET.GT.0) THEN
C           CALL UTMESS('F','FETCRF','LE GROUP_MA : '//NOMSD(I)//
C      &                ' EXISTE DEJA, CHANGER ''NOM''.')
C         ENDIF
  5   CONTINUE


C --------------------------------------------------------------
C .FETA : LISTE DES MAILLES DE CHAQUE SD
C .FETB : LISTE DES NOEUDS ET DDL CUMULES DE CHAQUE SD
C --------------------------------------------------------------

C CREATION DE LA LISTE DES NOEUDS DE TOUS LES SD : 
C   ZI(IALINO-1+J), J = 1,NBNOT

      IER = 0
      DO 21 I = 1,NBSD
        IF (NGMA(I).EQ.1) THEN
          NOMGMA=LSTGMA(I)
          CALL JEEXIN(JEXNOM(GRPMA,NOMGMA),IRET)
            IF (IRET.EQ.0) THEN
              IER = IER + 1
              CALL UTMESS('E','FETCRF','GROUP_MA : '//NOMGMA//
     &                  ' INCONNU DANS LE MAILLAGE')
            END IF
        ENDIF
   21 CONTINUE
      IF (IER.NE.0) CALL UTMESS('F','FETCRF','ERREURS GROUP_MA')


C     BOUCLE SUR CHAQUE SOUS-DOMAINES POUR DETERMINER :
C      - SES MAILLES  : ZI(IALIMA-1+J), J = 1,NBMAIL
C      - SES NOEUDS   : ZI(IALINO-1+J), J = 1,NBNOT
C      - LES DDL CUMULES DE CES NOEUDS

C     CREATION SDFETI / .FETB
      CALL JECREC(NOMSDB,'G V I','NO','DISPERSE','VARIABLE',NBSD)

      CALL WKVECT('&&FETCRF.LISTE_NO ','V V I',3*NBNOTO,IALINO)
      CALL WKVECT('&&FETCRF.L_NO_GMA ','V V I',3*NBNOTO,LINOMA)
      CALL WKVECT('&&FETCRF.L_NO_POS ','V V I',3*NBNOTO,IALSPO)
      CALL WKVECT('&&FETCRF.NB_NO    ','V V I',NBSD,IANBNO)
      CALL WKVECT('&&FETCRF.NB_MA    ','V V I',NBSD,IANBMA)
      CALL WKVECT('&&FETCRF.L_NO_JADR','V V I',NBSD,IAJADR)


C     SOMME DES DDL CUMULES DE CHAQUE SD
      CALL WKVECT('&&FETCRF.L_K      ','V V I',NBSD,IALSK)

C     NB MAILLES TOTALES
      NBMATO=0

      NB=0
      CALL WKVECT('&&FETCRF.TRAV ','V V I',2*NBNOTO,JTRAV)

C     BOUCLE SUR LES SOUS-DOMAINES
      DO 22 I = 1,NBSD

        CALL WKVECT('&&FETCRF.TMP  ','V V I',2*NBNOTO,JTMP)

        IF (NGMA(I).EQ.1) THEN
          NOMGMA=LSTGMA(I)
          IF (NIV.GE.4) WRITE(6,*) 'NOMGMA=',NOMGMA

C         MAILLES DU GROUP_MA (POUR LE CALCUL DE NBMAIL)
          CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',NBMAIL,K8B)
          CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',IALIMA)

C         NB TOTAL DE MAILLE DANS LE SD
          ZI(IANBMA-1+I)=NBMAIL
          NBMATO=NBMATO+NBMAIL

        ENDIF

        IF (NIV.GE.4) THEN
          DO 221 J = 1,NBMAIL
            WRITE(6,*) 'ZI(IALIMA)=',ZI(IALIMA-1+J)
  221     CONTINUE
        ENDIF

C       GMGNRE : donne la liste des noeuds ZI(JTMP) d'une liste de
C                mailles ZI(IALIMA)
C                sortie : liste des noeuds = ZI(JTMP)
C                         nombre de noeuds = ZI(IANBNO-1+I)

        CALL GMGNRE(MA,NBNOTO,ZI(JTRAV),ZI(IALIMA),NBMAIL,ZI(JTMP),
     &              ZI(IANBNO-1+I),'TOUS')

        IF (NIV.GE.4) THEN
          DO 222 J = 1,ZI(IANBNO-1+I)
            WRITE(6,*) 'ZI(JTMP)=',ZI(JTMP-1+J)
  222     CONTINUE
        ENDIF


C       REMPLISSAGE DE SDFETI / .FETB
        K8BUFF=NOMSD(I)
        CALL JECROC(JEXNOM(NOMSDB,K8BUFF))
        INTBUF=2*ZI(IANBNO-1+I)
        CALL JEECRA(JEXNOM(NOMSDB,K8BUFF),'LONMAX',INTBUF,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDB,K8BUFF),'E',JADR)
        K=0
        ZI(IAJADR-1+I)=JADR
        DO 210 J=1,ZI(IANBNO-1+I)
C         - NOEUD
          INO = ZI(JTMP-1+J)
          ZI(JADR+2*J-1-1)=INO
C          ZI(JADR+2*J-1-1)=ZI(JTMP-1+J)

C         - NB DDL CUMULES
          DO 7 L=1,30*NEC
C          INO = ZI(JTMP-1+J)
            IF (EXISDG( ZI(JPRNM-1+NEC*(INO-1)+1) ,L )) THEN
              K=K+1
            ENDIF
  7       CONTINUE
          ZI(JADR+2*J-1  )=K
          IF (NIV.GE.4) WRITE(6,*) 'DDL___=',K
  210     CONTINUE                      
  200   CONTINUE

C       NB DDL TOTAL DU SOUS-DOMAINE I
        ZI(IALSK-1+I)=K

C       NB NOEUD DANS LE SD COURANT
        NBNO(I)=ZI(IANBNO-1+I)

C       CREATION DU VECTEUR DES NOEUDS :  ZI(IALINO-1+...+3*NBNOTO)
C                DU VECTEUR DE CORRESPONDANCE NOEUD -> SD 
C                                      :  ZI(LINOMA-1+...+3*NBNOTO)
C                DU VECTEUR DE CORRESPONDANCE NOEUD -> POSITION DANS SD
        IPOS=1
        DO 25 J = 1,NBNO(I)
          ZI(IALINO+NB-1+J)= ZI(JTMP-1+J)
          ZI(LINOMA+NB-1+J)= I
          ZI(IALSPO+NB-1+J)= IPOS
          IPOS=IPOS+1
          IF (NIV.GE.4) WRITE(6,*) 'NOEUD=',ZI(JTMP-1+J)
          IF (NIV.GE.4) WRITE(6,*) 'GMA=',ZI(LINOMA+NB-1+J)
          IF (NIV.GE.4) WRITE(6,*) 'POS=',ZI(IALSPO+NB-1+J)
   25   CONTINUE

C       ON INCREMENTE LE NOMBRE DE NOEUDS TOTAL DES LISTES
        NB=NB+NBNO(I)


        CALL JEDETR('&&FETCRF.TMP')

   22 CONTINUE

      CALL JEDETR('&&FETCRF.TRAV')



C      REMPLISSAGE DE SDFETI / .FETA
      CALL JECREC(NOMSDA,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      DO 100 I=1,NBSD
        K8BUFF=NOMSD(I)
        CALL JECROC(JEXNOM(NOMSDA,K8BUFF))

C       NB MAILLES
        INTBUF=ZI(IANBMA-1+I)

C       NB MAILLES DE BORDS SI BESOIN...
        NBMABD(I)=0
        IF (NBRD(I).EQ.1) THEN
          CALL JELIRA(JEXNOM(GRPMA,LSTBRD(I)),'LONMAX',NBMABD(I),K8B)
C          WRITE(6,*) 'NBID=',NBID
        ENDIF
        
C       NB MAILLES TOTALES
        NBID=INTBUF+NBMABD(I)

        CALL JEECRA(JEXNOM(NOMSDA,K8BUFF),'LONMAX',NBID,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDA,K8BUFF),'E',JADR)

C       MAILLES DU GROUP_MA (POUR .FETA)
        IF (NGMA(I).EQ.1) THEN
          NOMGMA=LSTGMA(I)
          CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',ZI(IANBMA-1+I),K8B)
          CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',IALIMA)
        ENDIF

        DO 90 J=1,INTBUF
          ZI(JADR+J-1)=ZI(IALIMA-1+J)
C          WRITE(6,*) JADR+J-1
   90   CONTINUE                        

C       MAILLES DE BORD (POUR .FETA)
        IF (NBRD(I).EQ.1) THEN
          NOMGMA=LSTBRD(I)

C         MAILLES DU GROUP_MA (POUR LE CALCUL DE NBMAIL)
          CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',NBMABD(I),K8B)
          CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',IALIBD)

          DO 91 J=1,NBMABD(I)
            ZI(JADR+J-1+INTBUF)=ZI(IALIBD-1+J)
C            WRITE(6,*) JADR+J-1+INTBUF
C            WRITE(6,*) 'ZI(IALIBD-1+J)=',ZI(IALIBD-1+J)
   91     CONTINUE                        

        ENDIF


  100 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDA   


C FIN .FETA ET .FETB
C --------------------------------------------------------------

      IF (NIV.GE.4) THEN
        WRITE(6,*) 'NB=',NB
        DO 94 I = 1,NBSD
          WRITE(6,*) 'SD=',I
          WRITE(6,*) 'DDL TOTAL=',ZI(IALSK-1+I)
          WRITE(6,*) 'MA TOTAL =',ZI(IANBMA-1+I)
          WRITE(6,*) 'NO TOTAL =',ZI(IANBNO-1+I)
   94   CONTINUE
        WRITE(6,*) '++++++++++++++++++++++++++++++++++++++'
        DO 24 J = 1,NB
          WRITE(6,*) 'NOEUD=',ZI(IALINO-1+J)
          WRITE(6,*) 'GMA=',ZI(LINOMA-1+J)
   24   CONTINUE
        WRITE(6,*)  'NB=',NB 
        WRITE(6,*) '++++++++++++++++++++++++++++++++++++++'
        CALL JXVERI('MESSAGE',' ')
      ENDIF


C --------------------------------------------------------------
C ON REORDONNE LA LISTE DE TOUS LES NOEUDS DES SD (ET LEUR SD)
C   (INSPIRE DU TRI A BULLE UTTRIF.F) 
C   PARTIE A OPTIMISER (VOIR TRI PAR INSERTION)
C --------------------------------------------------------------

C        --- TRI BULLE ---
      IF ( NB .GT. 1 ) THEN
C         --- CHOIX DE L'INCREMENT ---
        INCRS = 1
        IS9   = NB / 9
 11     CONTINUE
        IF (INCRS .LT. IS9) THEN
          INCRS = 3*INCRS+1
          GOTO 11
        ENDIF
C         --- REMONTEE DES BULLES ---
12      CONTINUE
        DO 15 J=INCRS+1,NB
          L = J-INCRS
13        CONTINUE
          IF ( L.GT.0) THEN
            IF ( ZI(IALINO-1+L) .GT. ZI(IALINO-1+L+INCRS) ) THEN
C            --- PERMUTATION DES VALEURS ---
              XT                   = ZI(IALINO-1+L)
              ZI(IALINO-1+L)       = ZI(IALINO-1+L+INCRS)
              ZI(IALINO-1+L+INCRS) = XT
              YT                   = ZI(LINOMA-1+L)
              ZI(LINOMA-1+L)       = ZI(LINOMA-1+L+INCRS)
              ZI(LINOMA-1+L+INCRS) = YT
              ZT                   = ZI(IALSPO-1+L)
              ZI(IALSPO-1+L)       = ZI(IALSPO-1+L+INCRS)
              ZI(IALSPO-1+L+INCRS) = ZT
              L = L - INCRS
              GOTO 13
            ENDIF
          ENDIF
15      CONTINUE
        INCRS = INCRS/3
        IF (INCRS.GE.1) GOTO 12
      ENDIF

      CALL JXVERI('MESSAGE',' ')


C ----------------------------------------------------------------------
C CONSTRUCTION DES LISTES DES NOEUDS D'INTERFACE, DE LEUR MULTIPLICITE
C   (IE LES NOEUDS PRESENTS AU MOINS DEUX FOIS DANS LA LISTE PRECEDENTE)
C   ET DE LA LISTE DES SD AUXQUELS ILS APPARTIENNENT :
C    ZI(IALSNO-1+K)  , K=1,NBFETE  : LISTE DES NOEUDS D'INTERFACE
C    ZI(IALSMU-1+K)  , K=1,NBFETE  : LISTE DE LEUR MULTIPLICITE
C    ZI(IALSMA-1+KK) , KK=1,(SOMME DES PRECEDENTS) : LISTE DES SD
C ----------------------------------------------------------------------

C     TABLEAU NBSDxNBSD CONTENANT LE NB DE NOEUD D'INTERFACE ENTRE 2 SD
      CALL WKVECT('&&FETCRF.LST_TBL  ','V V I',NBSD*NBSD,IALSTB)
      DO 49 I = 1,NBSD
        DO 48 J = 1,NBSD
          ZI(IALSTB-1+(I-1)*NBSD+J)=0
   48   CONTINUE
   49 CONTINUE

      CALL JXVERI('MESSAGE',' ')


      CALL WKVECT('&&FETCRF.LST_ORDO ','V V I',NB,IALSNO)
      CALL WKVECT('&&FETCRF.LST_MULT ','V V I',NB,IALSMU)
      CALL WKVECT('&&FETCRF.LST_GMA  ','V V I',2*NB,IALSMA)

      CALL WKVECT('&&FETCRF.LST_TRI  ','V V I',3*NB+3,IALSTR)

      CALL JXVERI('MESSAGE',' ')

      IF (NIV.GE.4) WRITE(6,*) '**************************',NB

C       NN=1
      K=1
      DO 28 I = 1,NB
        DO 29 J = I+1,NB

C         SI ON A TROUVE UN NOEUD D'INTERFACE (CAD COMMUN A 2 SD)
          IF ( ZI(IALINO-1+I).EQ.ZI(IALINO-1+J) ) THEN

            IF (NIV.GE.4) WRITE(6,*) I,J,K
            IF (NIV.GE.4) WRITE(6,*) '--- ',ZI(IALINO-1+I),
     &                       ZI(LINOMA-1+I),ZI(LINOMA-1+J)

C            NN=NN+1
C            WRITE(6,*) 'NN=',NN

C           ON COMPLETE LE TABLEAU DES NOEUDS D'INTERFACE ENTRE 2 SD
            ITMP=ZI(LINOMA-1+I)
            JTMP=ZI(LINOMA-1+J)
            IF (JTMP.LT.ITMP) THEN
              ITMP=ZI(LINOMA-1+J)
              JTMP=ZI(LINOMA-1+I)
            ENDIF

            IF (NIV.GE.4) WRITE(6,*) 'ITMP ET JTMP :',ITMP,JTMP

            ZI(IALSTB-1+(ITMP-1)*NBSD+JTMP)=
     &                          ZI(IALSTB-1+(ITMP-1)*NBSD+JTMP)+1

C           ON MODIFIE LE SIGNE DU NOEUD DANS .FETB POUR LES 2 SD
            JADR=ZI(IAJADR-1+ZI(LINOMA-1+I))
            ZI(JADR+2*ZI(IALSPO-1+I)-1-1)=
     &                            -1*ABS(ZI(JADR+2*ZI(IALSPO-1+I)-1-1))
            JADR=ZI(IAJADR-1+ZI(LINOMA-1+J))
            ZI(JADR+2*ZI(IALSPO-1+J)-1-1)=
     &                            -1*ABS(ZI(JADR+2*ZI(IALSPO-1+J)-1-1))

C           ON REMPLI LA LISTE DES TRIPLETS (NOEUD, SD1, SD2)
            ZI(IALSTR+3*(K-1))  =ZI(IALINO-1+I)
            IF ( ZI(LINOMA-1+I).LT.ZI(LINOMA-1+J) ) THEN
              ZI(IALSTR+3*(K-1)+1)=ZI(LINOMA-1+I)
              ZI(IALSTR+3*(K-1)+2)=ZI(LINOMA-1+J)
            ELSE
              ZI(IALSTR+3*(K-1)+1)=ZI(LINOMA-1+J)
              ZI(IALSTR+3*(K-1)+2)=ZI(LINOMA-1+I)
            ENDIF

C           ON INCREMENTE LE NOMBRE DE NOEUDS TROUVES
            K=K+1

          ENDIF
C           ELSE
C             WRITE(6,*) 'NNNN=',NN
C             NN=1
C           ENDIF

   29   CONTINUE
   28 CONTINUE


C     CALCUL DE LA MULTIPLICITE DES NOEUDS D'INTERFACE
      CALL WKVECT('&&FETCRF.LST_MLT  ','V V I',K-1,IALSML)
      CALL WKVECT('&&FETCRF.LST_COR  ','V V I',K-1,IALSCR)
      CALL WKVECT('&&FETCRF.LST_MSD  ','V V I',NBSD,IALSMD)

      DO 34 L = 1,K-1

        ITMP = ZI(IALSTR+3*(L-1))
        ZI(IALSCR+L-1)=ITMP

        DO 31 J = 1,NBSD
          ZI(IALSMD+J-1)=0
   31   CONTINUE

        IF (NIV.GE.4) WRITE(6,*) 'ITMP=',ITMP

        DO 30 I = 1,K-1
          IF ( I.NE.L+1 ) THEN
            IF ( ZI(IALSTR+3*(I-1)).EQ.ZI(IALSTR+3*(L-1)) ) THEN
C              WRITE(6,*) 'I=',ZI(IALSTR+3*(I-1))
               JTMP=ZI(IALSTR+3*(I-1)+1)
               ZI(IALSMD+JTMP-1)=1
C               WRITE(6,*) 'JJTMP=',JTMP
               JTMP=ZI(IALSTR+3*(I-1)+2)
               ZI(IALSMD+JTMP-1)=1
C               WRITE(6,*) 'JJTMP=',JTMP
            ENDIF
          ENDIF

   30   CONTINUE

        NN=0
        DO 35 J = 1,NBSD
          IF (NIV.GE.4) WRITE(6,*) 'IALSMD=',ZI(IALSMD+J-1)
          IF ( ZI(IALSMD+J-1).EQ.1 ) NN=NN+1
   35   CONTINUE
        ZI(IALSML+L-1)=NN
C        WRITE(6,*) 'NN2=',NN

   34 CONTINUE


      DO 38 J = 1,K-1
        IF (NIV.GE.4) WRITE(6,*) 'MLT=',ZI(IALSML+J-1)
   38 CONTINUE


      CALL JXVERI('MESSAGE',' ')

      IF (NIV.GE.4) THEN 
        DO 59 I = 1,NBSD
          DO 58 J = 1,NBSD
            WRITE(6,*) I,J,ZI(IALSTB-1+(I-1)*NBSD+J)
   58     CONTINUE
   59   CONTINUE
        WRITE(6,*) '---------------'  
      ENDIF


      KK=1
      DO 37 I = 1,K-1
        ITMP=ZI(IALSTR+3*(I-1)+1)
        JTMP=ZI(IALSTR+3*(I-1)+2)

        IF (NIV.GE.4) WRITE(6,*) 'NOEUD =',ZI(IALSTR+3*(I-1))
        IF (NIV.GE.4) WRITE(6,*) 'SD1   =',ZI(IALSTR+3*(I-1)+1)
        IF (NIV.GE.4) WRITE(6,*) 'SD2   =',ZI(IALSTR+3*(I-1)+2)
        IF (NIV.GE.4) WRITE(6,*) 'NB=',ZI(IALSTB-1+(ITMP-1)*NBSD+JTMP)
        IF (NIV.GE.4) WRITE(6,*) ' '  

C       ON NE CONSERVE QUE LES SD QUI ONT AU MOINS 2 NOEUDS COMMUNS
        IF ( ZI(IALSTB-1+(ITMP-1)*NBSD+JTMP).GE.2 ) THEN

          NBID=0
          DO 36 J = 1,K-1
            IF ( (ZI(IALSTR+3*(J-1)).EQ.ZI(IALSTR+3*(I-1))).AND.
     &           (ZI(IALSTB-1+(ZI(IALSTR+3*(J-1)+1)-1)*NBSD+
     &               ZI(IALSTR+3*(J-1)+2)).GE.2) ) THEN
              NBID=NBID+1
            ENDIF
   36     CONTINUE

          ZI(IALSNO-1+KK)      = ZI(IALSTR+3*(I-1))
C          ZI(IALSMU-1+KK)      = ZI(IALSTB-1+(ITMP-1)*NBSD+JTMP)
          ZI(IALSMU-1+KK)      = NBID
          ZI(IALSMA+2*(KK-1))  = ITMP
          ZI(IALSMA+2*(KK-1)+1)= JTMP
          KK=KK+1

        ENDIF

   37 CONTINUE

C     NB NOEUDS INTERFACE
      NBFETE=KK-1

C     TAILLE DE LA LISTE RELIANT LES NOEUDS A LEURS SD
      NBNOSD=2*(KK-1)

      IF (NIV.GE.4) THEN
        DO 32 J = 1,NBFETE
          WRITE(6,*) 'NOEUD =',ZI(IALSNO-1+J)
          WRITE(6,*) 'MULT     =',ZI(IALSMU-1+J)
   32   CONTINUE
        DO 33 J = 1,NBNOSD
          WRITE(6,*) 'SD_=',ZI(IALSMA-1+J)
   33   CONTINUE
      ENDIF


C .FETI : LISTE DES NOEUDS D'INTERFACE + MULTIPLICITE + DDL 
C         + POINTEUR VERS LA LISTE DES SD (FETJ)
      CALL WKVECT(NOMSDI,'G V I',4*NBFETE,JADR)
      JADRI=JADR
      MULTC = 1
      K=0
      DO 500 I=1,NBFETE

C      - NUMERO DU NOEUD D'INTERFACE
        ZI(JADR+4*(I-1))=ZI(IALSNO-1+I)

C      - MULTIPLICITE
C        ZI(JADR+4*(I-1)+1)=ZI(IALSMU-1+I)
         IF (NIV.GE.4) WRITE(6,*) 'NOEUD=', ZI(IALSNO-1+I)
         DO 40 J=1,NBFETE+1
           IF ( ZI(IALSCR+J-1).EQ.ZI(IALSNO-1+I) ) THEN
             GOTO 41
           ENDIF
   40    CONTINUE
   41    CONTINUE
         ZI(JADR+4*(I-1)+1)=ZI(IALSML+J-1)
C         ZI(JADR+4*(I-1)+1)=ZI(IALSML+ZI(IALSNO-1+I)-1)
         IF (NIV.GE.4) WRITE(6,*) 'J=',J
         IF (NIV.GE.4) WRITE(6,*) 'MUL=', ZI(IALSML+J-1),ZI(IALSCR+J-1)

C      - NB DDL CUMULES
C        K=0
        DO 2 J=1,30*NEC
          INO = ZI(IALSNO-1+I)
          IF (EXISDG( ZI(JPRNM-1+NEC*(INO-1)+1) ,J )) THEN
            K=K+1
          ENDIF
  2     CONTINUE
        IF (NIV.GE.4) WRITE(6,*) 'DDL=',K
        ZI(JADR+4*(I-1)+2)=K

C      - LISTE DES SD D'APPARTENANCE
        ZI(JADR+4*(I-1)+3)=MULTC
C        MULTC=MULTC+ZI(IALSMU-1+I)
        MULTC=MULTC+2

  500 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDI   


C .FETJ : LISTE DES SD PARTAGEANT UN NOEUD D'INTERFACE
      CALL WKVECT(NOMSDJ,'G V I',NBNOSD,JADR)
      JADRJ=JADR
      DO 501 J=1,NBNOSD
        ZI(JADR+J-1)=ZI(IALSMA-1+J)
        IF (NIV.GE.4) WRITE(6,*) 'SD__=',ZI(IALSMA-1+J)
  501 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDJ   

      CALL JEDETR('&&FETCRF.NB_NO')

      CALL JXVERI('MESSAGE',' ')







C ----------------------------------------------------------------------
C .FETG
C CONSTRUCTION DE LA COLLECTION FETG POUR SIMULER L'ACTION DES
C   RESTRICTION/PREDICTION
C ----------------------------------------------------------------------

C .FETG
      CALL JECREC(NOMSDG,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDG


      CALL WKVECT('&&FETCRF.LST_FETG ','V V I',NB,IALSFG)

C     BOUCLE SUR LES SOUS-DOMAINES
      DO 51 I = 1,NBSD

        IF (NIV.GE.4) WRITE(6,*) 'NUMERO DU SD (VARIABLE I) : ',I

C       NB D'OCCURENCE DANS FETG
        NN=0

C       ADRESSE DE DEBUT DE FETB POUR LE SD I
        JADR=ZI(IAJADR-1+I)

C       BOUCLE SUR LES NOEUDS DANS FETB
        DO 52 J = 1,ZI(IANBNO-1+I)

         IF (NIV.GE.4) WRITE(6,*) ' '
         IF (NIV.GE.4) WRITE(6,*) ' '
         IF (NIV.GE.4) WRITE(6,*) '--- FETB / NUMERO DU COUPLE : ',J
         IF (NIV.GE.4) WRITE(6,*) '--- FETB / NOEUD , DDL : ',
     &         ZI(JADR+2*J-1-1),ZI(JADR+2*J-1)

C         SI LE NOEUD DANS FETB EST NEGATIF (IE NOEUD D'INTERFACE)
          IF ( ZI(JADR+2*J-1-1).LT.0 ) THEN
C           ON RECHERCHE DANS FETI LE NOEUD CORRESPONDANT
            DO 53 K = 1,NBFETE
C             SI LES NOEUDS CORRESPONDENT
              IF ( ZI(JADRI+4*(K-1)).EQ.ABS(ZI(JADR+2*J-1-1)) ) THEN

                IF (NIV.GE.4) THEN
                  IF (NIV.GE.4) WRITE(6,*) ' '
                  WRITE(6,*) '  FETI : POSITION : ',K
                  WRITE(6,*) '  FETI : NOEUD CORRESPONDANT : ',
     &                               ZI(JADRI+4*(K-1))
                  WRITE(6,*) '  FETI : POSITION 1ER SD : ',
     &                               ZI(JADRI+4*(K-1)+3)
                  WRITE(6,*) '  FETI : SD :',
     &                               ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3)),
     &                               ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3)+1)
                ENDIF

C               SI LE SD I EST BIEN UN DES DEUX SD DU NOEUD COURANT
C                IF ( (I.EQ.ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3))).OR.
C     &               (I.EQ.ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3)+1)) ) THEN

C               SI LE SD I EST LE 1ER SD DU NOEUD COURANT
                IF (I.EQ.ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3))) THEN
C                   SI LE SD COURANT EST PLUS FAIBLE QUE LE 1ER SD
                    IF (I.LT.ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3)+1)) THEN
                      NBID=-1*K
                    ELSE
                      NBID=K
                    ENDIF

C               OU SI LE SD I EST LE DEUXIEME SD DU NOEUD COURANT
                ELSEIF (I.EQ.ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3)+1)) THEN
C                  ELSE
C                   SI LE SD COURANT EST PLUS FAIBLE QUE LE 2IEME SD
                    IF (I.LT.ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3))) THEN
                      NBID=-1*K
                    ELSE
                      NBID=K
                    ENDIF
                ENDIF

                IF ( (I.EQ.ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3))).OR.
     &               (I.EQ.ZI(JADRJ-1+ZI(JADRI+4*(K-1)+3)+1)) ) THEN
                  NN=NN+1
                  ZI(IALSFG+2*(NN-1))  = NBID
                  ZI(IALSFG+2*(NN-1)+1)= J
                  IF (NIV.GE.4) WRITE(6,*) '-- NOEUD,FETB,FETG',J,NBID,K
                ENDIF

              ENDIF
  53        CONTINUE
          ENDIF

  52    CONTINUE

        K8BUFF=NOMSD(I)      
        CALL JECROC(JEXNOM(NOMSDG,K8BUFF))
        INTBUF=2*NN
        CALL JEECRA(JEXNOM(NOMSDG,K8BUFF),'LONMAX',INTBUF,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDG,K8BUFF),'E',JADR)
        DO 610 J=1,NN
          ZI(JADR+2*(J-1))  =ZI(IALSFG+2*(J-1))
          ZI(JADR+2*(J-1)+1)=ZI(IALSFG+2*(J-1)+1)
  610   CONTINUE                      

C FIN BOUCLE SUR LES SD
  51  CONTINUE



C ----------------------------------------------------------------------
C
C       T R A I T E M E N T   D U   C A S   D E S   C H A R G E S
C
C ----------------------------------------------------------------------

C     NB NOMBRE DE MAILLES TARDIVES ET DE NOEUDS TARDIFS
      NBMATA=0
      NBNOTA=0
      NOMNOE = MA//'.NOMNOE'

      IF (NBCHAR.GE.1) THEN

C       TABLE TEMPORAIRE : 2xNB CHARGE PAR SD
        CALL WKVECT('&&FETCRF.L_2CHA_SD','V V I',NBSD*2*NBCHAR,ISDLCH)

C       LISTE TEMPORAIRE : NB MAILLES TARDIVES PAR CHARGE
        CALL WKVECT('&&FETCRF.L_NBMT_CH','V V I',NBSD*2*NBCHAR,INBMCH)

C       LISTE TEMPORAIRE : NOMS DES LIGREL DE CHARGE
        CALL WKVECT('&&FETCRF.LST_CHA  ','V V K24',NBCHAR,ILSCHA)

C       PREMIERE PASSE : CALCUL DU NB TOTAL DE MAILLES TARDIVES
        DO 811 I = 1,NBCHAR
          LIGRCH = NOMCHA(I)//'.CHME.LIGRE'
          ZK24(ILSCHA-1+I)= LIGRCH
          CALL JEEXIN(LIGRCH//'.NEMA',N2)
          IF (N2.NE.0) THEN
            CALL JELIRA(LIGRCH//'.NEMA','NUTIOC',NBOBJ1,K8BID)
            NBMATA=NBMATA+NBOBJ1
            ZI(INBMCH-1+I)=NBOBJ1
          ENDIF
  811   CONTINUE

        IF (NIV.GE.4) WRITE(6,*) 'NBMATA:', NBMATA

      ENDIF


      IF ( (NBCHAR.GE.1).AND.(NBMATA.GT.0) ) THEN

C       CREATION DES OBJETS TEMPORAIRES
C
C IFLIM  :  MT  1          2          3          4          5 (4+1) ...
C           SD  1 2 3 4    1 2 3 4    1 2 3 4    1 2 3 4    1 2 3 4 ...
C      oui/non  1 0 0 0    1 0 0 0    0 0 1 0    0 0 1 0    1 0 0 0 ...
C
C IDFLIM :  SD  1 2 3 4
C        nb MT  2 0 2 0
C
        CALL WKVECT('&&FETCRF.FLIM     ','V V I',NBSD*NBMATA,IFLIM)
        CALL WKVECT('&&FETCRF.IDFLIM   ','V V I',NBSD,IDFLIM)

C
C       IFNT   :  MT  1 2 3 4
C             nb NoT  2 2 2 2
C
        CALL WKVECT('&&FETCRF.IFNT     ','V V I',NBMATA,IFNT)

C
C       IFLII  :  CH  1          2          3       ...
C                 SD  1 2 3 4    1 2 3 4    1 2 3 4 ...
C              nb MT  0 0 0 0    2 0 2 0            ...
C
        CALL WKVECT('&&FETCRF.FLII     ','V V I',NBSD*NBCHAR,IFLII)


C       INITIALISATION DES VARIABLES TEMPORAIRES
        DO 820 K = 1, NBSD*NBMATA
          ZI(IFLIM-1+K)=0
  820   CONTINUE

        DO 821 K = 1, NBSD
          ZI(IDFLIM-1+K)=0
  821   CONTINUE

        DO 822 K = 1, NBSD*NBCHAR
          ZI(IFLII-1+K)=0
  822   CONTINUE

        DO 823 K = 1, NBMATA
          ZI(IFNT-1+K)=0
  823   CONTINUE



C       DECALAGE POUR PRENDRE EN COMPTE PLUSIEURS CHARGES
        NB2=0
C       NB DE NOEUDS DE L'INTERFACE QUI SONT SUR DES CHARGEMENTS
        NOINCH=0

        DO 801 ICH = 1,NBCHAR

          LIGRCH = NOMCHA(ICH)//'.CHME.LIGRE'

          IF (NIV.GE.4) THEN
            WRITE(6,*) '----------------------------------------------'
            WRITE(6,*) '        CHARGE : ', ICH
            WRITE(6,*) '----------------------------------------------'
            WRITE(6,*) 'LIGRCH:', LIGRCH
          ENDIF

C         SI ON TROUVE DES MAILLES TARDIVES DANS LA CHARGE
          CALL JEEXIN(LIGRCH//'.NBNO',N1)
          IF (N1.NE.0) THEN
            CALL JEVEUO(LIGRCH//'.NBNO','L',IADR)
            CALL JELIRA(LIGRCH//'.NBNO','LONMAX',N1,K8B)
C             write(6,*) 'LIGRCH:', LIGRCH, IADR, N1
C             write(6,*) 'ZI:', ZI(IADR-1+1)
          ENDIF

C         SI ON TROUVE DES MAILLES TARDIVES DANS LA CHARGE
          CALL JEEXIN(LIGRCH//'.NEMA',N2)
          IF (N2.NE.0) THEN
C           NB MAILLES TARDIVES
            CALL JELIRA(LIGRCH//'.NEMA','NUTIOC',NBOBJ1,K8BID)
C           LONGUEUR TOTALE DE LA COLLECTION
            CALL JELIRA(LIGRCH//'.NEMA','LONT',LONLI1,K8BID)
C           ADRESSE DE DEBUT DE LA COLLECTION
            CALL JEVEUO(LIGRCH//'.NEMA','L',IDLIG1)
C            NBMATA=NBOBJ1

C           DIMENSIONNEMENT DES OBJETS TEMPORAIRES
C           LISTE DES MAILLES TARDIVES DE CHAQUE SD
            CALL WKVECT('&&FETCRF.L_MAT_SD ','V V I',NBSD*NBOBJ1,ISDMAT)
C
            DO 809 K = 1, NBSD*NBOBJ1
              ZI(ISDMAT-1+K)=0
  809       CONTINUE

C             write(6,*) '_Adr0_Mat_=',ISDMAT
C             write(6,*) 'LIGRCH:', LIGRCH, IDLIG1, N2, NBOBJ1, LONLI1
C             write(6,*) 'ZI:', ZI(IDLIG1-1+1)

            CALL JXVERI('MESSAGE',' ')

C           POINTEUR VERS LA POSITION DANS LA COLLECTION
            IADR=IDLIG1
            DO 802 J = 1, NBOBJ1
C             NB NOEUDS DE CHAQUE MAILLE
              CALL JELIRA(JEXNUM(LIGRCH//'.NEMA',J),'LONMAX',N3,K8B)
C               write(6,*) '\n\nMaille Tar:', J

C             ON PARCOURS LES NOEUDS DE LA MAILLE TARDIVE
              DO 803 K = 1, N3-1
C                 write(6,*) ' '
C                 write(6,*) 'Nd:', ZI(IADR-1+K)
                NDTAR=ZI(IADR-1+K)

C               SI C'EST UN NOEUD PHYSIQUE, ON CHERCHE SON(SES) SD
                IF ( NDTAR.GT.0 ) THEN
                  NB1=0
                  DO 804 L = 1, NB
                    IF ( NDTAR.EQ.ZI(IALINO-1+L) ) THEN

                      NUMSD=ZI(LINOMA-1+L)
                      DEC=(J-1)*NBSD+NUMSD
                      NB1=NB1+1

C                       WRITE(6,*) 'Nd_=',ZI(IALINO-1+L)
C                       WRITE(6,*) 'Sd_=',NUMSD
C                       write(6,*) '_Ad_=',ISDMAT-1+(J-1)*NBSD+NUMSD
C                       write(6,*) '_Ad_=',ISDMAT-1+DEC
C                       write(6,*) '_Val_=',ZI( ISDMAT-1+DEC )

                      IF (ZI(ISDMAT-1+DEC).EQ.0) THEN
                          ZI(ISDMAT-1+DEC) = 1
                      ENDIF

                    ENDIF
  804             CONTINUE

C                 .AJOUT DU NOEUD CHARGE SUR L'INTERFACE DANS LA LISTE
                  IF (NB1.GT.1) THEN
                    NOINCH=NOINCH+1
                    CALL JENUNO(JEXNUM(NOMNOE,NDTAR),NOMN)
C                     CALL UTDEBM('A','FETCRF','LE NOEUD ')
C                     CALL UTIMPK('S','',1,NOMN//' DU CHARGEMENT 
C      &                     '//NOMCHA(ICH)//' EST'//
C      &                     ' SUR L''INTERFACE.')
C                     CALL UTFINM

                    CALL UTMESS('A','FETCRF','LE NOEUD '//NOMN//' DU '//
     &             'CHARGEMENT '//NOMCHA(ICH)//' EST SUR L''INTERFACE.')

                  ENDIF

C               SINON C'EST UN NOEUD TARDIF
                ELSE
C                  ZI(IFNT-1+J)=ZI(IFNT-1+J)+1
                  ZI(IFNT-1+J+NB2)=ZI(IFNT-1+J+NB2)+1
                  NBNOTA=NBNOTA+1
                ENDIF

  803         CONTINUE

C             POSITION DE LA MAILLE TARDIVE SUIVANTE DANS LA COLLECTION
              IADR=IADR+N3

  802       CONTINUE


C           VERIFICATION DES OBJETS TEMPORAIRES
            IF (NIV.GE.4) THEN
              WRITE(6,*) '----------'
              DO 810 J = 1, NBOBJ1
                DO 812 NUMSD = 1, NBSD
                  DEC=(J-1)*NBSD+NUMSD
                WRITE(6,*) 'SD:',NUMSD,' MT:',J,' VAL:',ZI(ISDMAT-1+DEC)
  812           CONTINUE
  810         CONTINUE
              WRITE(6,*) '----------'
              DO 815 NUMSD = 1, NBSD
                DO 814 J = 1, NBOBJ1
                  DEC=(J-1)*NBSD+NUMSD
                WRITE(6,*) 'MT:',J,' SD:',NUMSD,' VAL:',ZI(ISDMAT-1+DEC)
  814           CONTINUE
  815         CONTINUE
              WRITE(6,*) '----------', NB2
            ENDIF


C           MISE A JOUR DE L'OBJET TEMPORAIRE POUR FLIM
            DO 900 J = 1, NBOBJ1
              DO 901 NUMSD = 1, NBSD
                DEC=(J-1)*NBSD+NUMSD
                NVAL=ZI(ISDMAT-1+DEC)
                IF (NVAL.GT.0) THEN

C                 MAJ de IDFLIM (NB DE MAILLES TARDIVES TROUVEES PAR SD)
                  ZI(IDFLIM-1+NUMSD)=ZI(IDFLIM-1+NUMSD)+1
C          write(6,*) ' SD:',NUMSD,' IdF:',ZI(IDFLIM-1+NUMSD)

C                 MAJ DE IFLIM (MAILLES TARDIVES TROUVEES)
                  DEC=(J-1 + NB2)*NBSD+ NUMSD
                  ZI(IFLIM-1+DEC) = J
C          write(6,*) '_DEC=',DEC,' SD ',NUMSD,' MaT ',J,' CH ',ICH,NB2
C          write(6,*) J, ZI(IDFLIM-1+NUMSD)

C                 MAJ DE IFLII
                  DEC=(ICH-1)*NBSD+NUMSD
                  ZI(IFLII-1+ DEC) = ZI(IFLII-1+ DEC) + 1

                ENDIF
C                write(6,*) 'SD ',NUMSD,' MT ',J,' _VAL_ ',NVAL
  901         CONTINUE
C              write(6,*) '---'
  900       CONTINUE

C           DECALAGE POUR IFLIM (LES NOUVELLES MaT COMMENCENT A LA FIN)
            NB2=NB2+NBOBJ1

C           DESTRUCTION DES OBJETS TEMPORAIRES
            CALL JEDETR('&&FETCRF.L_MAT_SD')

C         FIN SI ON A TROUVE DES MAILLES TARDIVES DANS CETTE CHARGE...
          ENDIF 

C       FIN BOUCLE DE 1 A NBCHAR
  801   CONTINUE


C       VERIFICATION QU'UN NOEUD CHARGE N'EST PAS SUR L'INTERFACE
        IF (NOINCH.GT.0) THEN
          NOMNOE = MA//'.NOMNOE'
C          CALL JELIRA ( NOMNOE, 'NOMMAX', NBNOEU, K1BID )
C          CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',GRAN),'L',IACMP)


          IF (NOINCH.EQ.1) THEN
            CALL UTMESS('F','FETCRF','ON ARRETE : UN NOEUD QUI '//
     &          'APPARTIENT AU CHARGEMENT EST SUR L''INTERFACE.'//
     &          ' CE CAS N''EST PAS ENCORE TRAITE.')
          ENDIF
          IF (NOINCH.GT.1) THEN
            CALL UTMESS('F','FETCRF','ON ARRETE : DES NOEUDS QUI '//
     &          'APPARTIENNENT AU CHARGEMENT SONT SUR L''INTERFACE.'//
     &          ' CE CAS N''EST PAS ENCORE TRAITE.')
          ENDIF

        ENDIF




C       VERIFICATION DES OBJETS TEMPORAIRES
        IF (NIV.GE.4) THEN
          WRITE(6,*) ' '  
          WRITE(6,*) '----------------------------------------------'
          WRITE(6,*) '         FIN DES CHARGES'
          WRITE(6,*) '----------------------------------------------'
          WRITE(6,*) ' '  
          WRITE(6,*) 'NB1=',NB1
          WRITE(6,*) 'NB2=',NB2
          WRITE(6,*) 'NBMATA=',NBMATA
          WRITE(6,*) 'NBSD=',NBSD
          WRITE(6,*) 'NBCHAR=',NBCHAR

          WRITE(6,*) '--------------------------'
          DO 902 J = 1, NBMATA
            WRITE(6,*) ' '
            DO 903 NUMSD = 1, NBSD
              DEC=(J-1)*NBSD+NUMSD
              WRITE(6,*) 'SD:',NUMSD,' MT:',J,' IFLIM:',ZI(IFLIM-1+DEC)
  903       CONTINUE
  902     CONTINUE
          WRITE(6,*) '--------------------------'
          DO 905 K = 1, NBSD*NBMATA
            WRITE(6,*) 'IFLIM:',ZI(IFLIM-1+K)
  905     CONTINUE
          WRITE(6,*) '--------------------------'
          DO 904 ICH = 1, NBCHAR
            DO 908 ISD = 1, NBSD
              DEC=(ICH-1)*NBSD+ISD
              WRITE(6,*) 'CH:',ICH,' SD:',ISD,' IFLII:',ZI(IFLII-1+DEC)
  908       CONTINUE
  904     CONTINUE
          WRITE(6,*) '--------------------------'
          DO 907 K = 1, NBSD
            WRITE(6,*) 'SD:',K,' IDFLIM:',ZI(IDFLIM-1+ K)
  907     CONTINUE
          WRITE(6,*) '--------------------------'
          DO 906 K = 1, NBCHAR
            WRITE(6,*) 'CHA:',ZK24(ILSCHA-1+ K)
  906     CONTINUE
          WRITE(6,*) '--------------------------'
          DO 824 K = 1, NBMATA
            WRITE(6,*) 'MAT',K,' NBNOT:',ZI(IFNT-1+K)
  824     CONTINUE
          WRITE(6,*) '--------------------------'
        ENDIF 


C     FIN SI IL Y A DES CHARGES
      ENDIF 





C -------------------------------------------------------------------
C           C R E A T I O N   D E   L A   S D  F E T I
C -------------------------------------------------------------------



C CREATION DES DIFFERENTS ATTRIBUTS DE LA S.D. SD_FETI

C .FDIM                 
C     ON COMPTE LES MAILLES DE BORDS DANS LE TOTAL DES MAILLES
      DO 93 I=1,NBSD
        NBMATO=NBMATO+NBMABD(I)
  93  CONTINUE

      CALL JELIRA(NOMO(1:8)//'.MAILLE','LONMAX',NBMAMA,K8B)
      CALL WKVECT(NOMSDM,'G V I',5,JADR)
      ZI(JADR)=NBSD
      ZI(JADR+1)=NBFETE
      ZI(JADR+2)=NBMAMA
      ZI(JADR+3)=NBFETE*2
      ZI(JADR+4)=NBNOTO
      IF (NIV.GE.3)
     &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDM


C.FETH
      CALL WKVECT(NOMSDH,'G V I',NBSD,JADRH)
      DO 700 ISD=1,NBSD
        ZI(JADRH+ISD-1)=ZI(IALSK-1+ISD)
  700 CONTINUE

C     MISE A JOUR DE FETH (AJOUT DES NOEUDS TARDIFS)
      IF (NBNOTA.GT.0) THEN
        DO 701 J = 1, NBMATA
C          write(6,*) 'HH MAT',J,' NBNoT:',ZI(IFNT-1+J)
          DO 702 NUMSD = 1, NBSD
            DEC=(J-1)*NBSD+NUMSD
            IF ( ZI(IFLIM-1+DEC).GT.0 ) THEN
              ZI(JADRH-1+NUMSD)=ZI(JADRH-1+NUMSD) + ZI(IFNT-1+J)
            ENDIF
C            write(6,*) 'HH IFLIM:',ZI(IFLIM-1+DEC),' NUMSD:',NUMSD
  702     CONTINUE
  701   CONTINUE
      ENDIF

      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDH


C .FLII, .FLIM, .FLIN

C     CREATION DES TROIS COLLECTIONS
      CALL JECREC(NOMSLN,'G V K24','NO','DISPERSE','VARIABLE',NBSD)
      CALL JECREC(NOMSLI,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      CALL JECREC(NOMSLM,'G V I','NO','DISPERSE','VARIABLE',NBSD)

C     ON NE REMPLI CES COLLECTIONS QUE SI ON A DES NOEUDS TARDIFS
      IF (NBNOTA.GT.0) THEN

C       VECTEURS TEMPORAIRES CONTENANT LES TAILLES DES COLLECTIONS FLIx
        CALL WKVECT('&&FETCRF.DIM_FLII ','V V I',NBSD,IDFLII)
        CALL WKVECT('&&FETCRF.DIM_FLIM ','V V I',NBSD,IDFLM)
        CALL WKVECT('&&FETCRF.DIM_FLIN ','V V I',NBSD,IDFLN)
        CALL WKVECT('&&FETCRF.TEMP     ','V V I',NBCHAR,ITMP)

        DO 915 NUMSD = 1, NBSD
          ZI(IDFLII-1+NUMSD)=0
          ZI(IDFLM-1+NUMSD)=0
          ZI(IDFLN-1+NUMSD) =0
  915   CONTINUE

        IF (NIV.GE.4) WRITE(6,*) '---- PREMIER PASSAGE ----'

C       PREMIER PASSAGE : RECUPERATION DE LA TAILLE DES COLLECTIONS
        DO 910 ICH = 1, NBCHAR
          NN = 0
          DO 911 ISD = 1, NBSD
            DEC=(ICH-1)*NBSD+ISD
            NN=NN+ZI(IFLII-1+ DEC)
C            write(6,*) 'SD:',ISD,' CHA:',ICH,' nb:',ZI(IFLII-1+ DEC)
  911     CONTINUE
C          write(6,*) 'NN:',NN

          IF (NN.GT.0) THEN
            DO 913 ISD = 1, NBSD
              DEC=(ICH-1)*NBSD+ISD
C             write(6,*) '  SD:',ISD,' CHA:',ICH,' nb:',ZI(IFLII-1+ DEC)

C             SI LA CHARGE COURANTE A DES MAILLES TARDIVES DANS LE SD
C             ON INCREMENTE LES TAILLES DES 3 COLLECTIONS POUR LE SD
              IF (ZI(IFLII-1+ DEC).GT.0) THEN
                ZI(IDFLN-1+ISD)=ZI(IDFLN-1+ISD)+1
                ZI(IDFLII-1+ISD)=ZI(IDFLII-1+ISD)+2
                ZI(IDFLM-1+ISD)=ZI(IDFLM-1+ISD)+ZI(IFLII-1+ DEC)
              ENDIF

  913       CONTINUE
          ENDIF

  910   CONTINUE

        IF (NIV.GE.4) THEN
          DO 916 ISD = 1, NBSD
            WRITE(6,*) 'SD',ISD,' DIM IDFLII:',ZI(IDFLII-1+ISD)
            WRITE(6,*) 'SD',ISD,' DIM IDFLM:',ZI(IDFLM-1+ISD)
            WRITE(6,*) 'SD',ISD,' DIM IDFLN:',ZI(IDFLN-1+ISD)
            WRITE(6,*) ' '
  916     CONTINUE
        ENDIF

C       VECTEURS DE POINTEUR VERS L'ADDRESSE JEVEUX DES OBJETS DE COLL.:
C       CA VAUT 0 SI L'OBJET N'A PAS ENCORE ETE CREE
C       SINON CA VAUT L'ADRESSE MEMOIRE COURANTE POUR CHAQUE SD
        CALL WKVECT('&&FETCRF.BIM_FLII ','V V I',NBSD,BIFLII)
        CALL WKVECT('&&FETCRF.BIM_FLIM ','V V I',NBSD,BIFLIM)
        CALL WKVECT('&&FETCRF.BIM_FLIN ','V V I',NBSD,BIFLN)
        DO 925 ISD = 1, NBSD
          ZI(BIFLII-1+ISD)=0
          ZI(BIFLIM-1+ISD)=0
          ZI(BIFLN-1+ISD)=0
  925   CONTINUE

        IF (NIV.GE.4) WRITE(6,*) '---- DEUXIEME PASSAGE ----'

C       DEUXIEME PASSAGE : REMPLISSAGE DES COLLECTIONS FLII ET FLIN
        DO 920 ICH = 1, NBCHAR
C         NN = NB DE MAILLES TARDIVE DE LA CHARGE
          NN = 0
          DO 921 ISD = 1, NBSD
            DEC=(ICH-1)*NBSD+ISD
            NN=NN+ZI(IFLII-1+ DEC)
C            write(6,*) 'SD:',ISD,' CHA:',ICH,' nb:',ZI(IFLII-1+ DEC)
  921     CONTINUE
C          WRITE(6,*) 'CH:',ICH,' NB MaT:',NN

C         SI LA CHARGE COURANTE A DES MAILLES TARDIVES
          IF (NN.GT.0) THEN

            DO 923 ISD = 1, NBSD
              DEC=(ICH-1)*NBSD+ISD
C              write(6,*) '  SD:',ISD,' CHA:',ICH,' nb:',ZI(IFLII-1+DEC)

C             SI LA CHARGE COURANTE A DES MAILLES TARDIVES DANS LE SD
              IF (ZI(IFLII-1+DEC).GT.0) THEN

C               CREATION DE .FLIN (NOM DE LA CHARGE)
                N1 = ZI(IDFLN-1+ISD)
C                write(6,*) 'N1:',N1
                IF ( N1.GT.0 ) THEN
                  IF ( ZI(BIFLN-1+ISD).EQ.0 ) THEN
                    CALL JECROC(JEXNOM(NOMSLN,NOMSD(ISD)))
                    CALL JEECRA(JEXNOM(NOMSLN,NOMSD(ISD)),'LONMAX',N1,
     &                          K8B)
                    CALL JEVEUO(JEXNOM(NOMSLN,NOMSD(ISD)),'E',JADR)
C                   ADDR DE DEPART POUR FLIN/ISD
                    ZI(BIFLN-1+ISD)=JADR
                  ENDIF
C                 ADDR COURANTE POUR FLIN/ISD
                  JADR=ZI(BIFLN-1+ISD)
C                 REMPLISSAGE DE FLIN/ISD
                  ZK24(JADR)=ZK24(ILSCHA-1+ICH)
C                 ADDR COURANTE POUR FLIN/ISD INCREMENTEE DE 1
                  ZI(BIFLN-1+ISD)=ZI(BIFLN-1+ISD)+1
                ENDIF

C               CREATION DE .FLII (NB MAT POUR LE SD ET POUR LA CHARGE)
                N1 = ZI(IDFLII-1+ISD)
C                write(6,*) 'N1:',N1
                IF ( N1.GT.0 ) THEN
                  IF ( ZI(BIFLII-1+ISD).EQ.0 ) THEN
                    CALL JECROC(JEXNOM(NOMSLI,NOMSD(ISD)))
                    CALL JEECRA(JEXNOM(NOMSLI,NOMSD(ISD)),'LONMAX',N1,
     &                          K8B)
                    CALL JEVEUO(JEXNOM(NOMSLI,NOMSD(ISD)),'E',JADR)
C                   ADDR DE DEPART POUR FLII/ISD
                    ZI(BIFLII-1+ISD)=JADR
                  ENDIF
C                 ADDR COURANTE POUR FLII/ISD
                  JADR=ZI(BIFLII-1+ISD)
C                 REMPLISSAGE DE FLII/ISD
                  ZI(JADR)=NN
                  ZI(JADR+1)=ZI(IFLII-1+DEC)
C                 ADDR COURANTE POUR FLII/ISD INCREMENTEE DE 2
                  ZI(BIFLII-1+ISD)=ZI(BIFLII-1+ISD)+2
C                  write(6,*) 'NBOBJ1:',NBOBJ1, ZI(JADR),ZI(JADR+1)
                ENDIF

C               CREATION DE .FLIM (- NUMERO DES LIGRE.LIEL)
                N1 = ZI(IDFLM-1+ISD)
                IF ( N1.GT.0 ) THEN
                  IF ( ZI(BIFLIM-1+ISD).EQ.0 ) THEN
                    CALL JECROC(JEXNOM(NOMSLM,NOMSD(ISD)))
                    CALL JEECRA(JEXNOM(NOMSLM,NOMSD(ISD)),'LONMAX',N1,
     &                          K8B)
                    CALL JEVEUO(JEXNOM(NOMSLM,NOMSD(ISD)),'E',JADR)
C                   ADDR DE DEPART POUR FLIM/ISD
                    ZI(BIFLIM-1+ISD)=JADR
                  ENDIF
                ENDIF

              ENDIF
  923       CONTINUE

C         FIN SI LA CHARGE COURANTE A DES MAILLES TARDIVES
          ENDIF

C       FIN BOUCLE SUR LES CHARGES
  920   CONTINUE

        IF (NIV.GE.4) WRITE(6,*) '---- TROISIEME PASSAGE ----'

C       TROISIEME PASSAGE : REMPLISSAGE DE LA COLLECTION FLIM
        DO 930 J = 1, NBMATA
          DO 932 NUMSD = 1, NBSD
            DEC=(J-1)*NBSD+NUMSD
C        write(6,*) 'SD:',NUMSD,' MT:',J,' IFLIM=',ZI(IFLIM-1+DEC)
            IF (ZI(IFLIM-1+DEC).GT.0) THEN
C             ADDR COURANTE POUR FLIM/ISD
              JADR=ZI(BIFLIM-1+NUMSD)
C             REMPLISSAGE DE FLIM/ISD
              ZI(JADR)=ZI(IFLIM-1+DEC)
C             ADDR COURANTE POUR FLIM/ISD INCREMENTEE DE 1
              ZI(BIFLIM-1+NUMSD)=ZI(BIFLIM-1+NUMSD)+1
            ENDIF
  932     CONTINUE
  930   CONTINUE

        IF (NIV.GE.3) THEN
          WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSLI   
          WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSLM   
          WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSLN   
        ENDIF

C     FIN DE CREATION DE FLII, FLIM ET FLIN
      ENDIF


      CALL JXVERI('MESSAGE','FIN')
      CALL JEDEMA()
      END
