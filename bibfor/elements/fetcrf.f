      SUBROUTINE FETCRF(SDFET1)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/08/2004   AUTEUR DURAND C.DURAND 
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
C      CHARACTER*24 NOMSDD,NOMSDE
      CHARACTER*24 NOMSDA,NOMSDB,NOMSDI,NOMSDG,NOMSDM,NOMSDH,NOMSDJ
      CHARACTER*32 JEXNOM
      
      INTEGER      NBID,ITMP,JVALE,NBGRMN,IALIK8,IALII1,IALII2,
     &             NBK8,NBIS,IRET,IER,NBNOTO,IERD,IALINO,JTRAV,IANBNO,
     &             NBGMA,NBMAIL,IALIMA,II,JJ,NBNOSD,NB,IANBMA,IALIBD,
     &             IS9,INCRS,L,XT,YT,ZT,VALE(12),IALSNO,IALSMU,K,KK,NN,
     &             LINOMA,IALSMA,IND,JNOMA,JPRNM,NEC,N,INO,DDLC,
     &             IALSK,NUMSD,IALSPO,IPOS,IAJADR,JTMP,IALSTR,IALSTB,
     &             JADRI,JADRJ,IALSFG,NBMATO,NBVM,NGMA(100),NMA(100),
     &             NBRD(100),NBMABD(100),IALSML,IALSMD,IALSCR
      CHARACTER*4  K4TMP
      CHARACTER*8  LSTGMA(100),LSTMA(100),KTMP,NOM,MA,K8B,NOMGMA,NOMO,
     &             LSTBRD(100),NOMCHA(01)
      CHARACTER*16 CONCEP,CMD,OPTION, MOTCLE, TYPMCL, MOTFAC
      CHARACTER*19 LIGRMO
      CHARACTER*24 NOMNOE,GRPNOE,COOVAL,LISNO,LISNOM,NOMREF
      CHARACTER*24 GRPMA,GRPNO
      LOGICAL      EXISDG
      
      
C CORPS DU PROGRAMME
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C INITIALISATIONS
      SDFETI=SDFET1
      NOMREF=SDFETI//'.REFE'            
      NOMSDM=SDFETI//'.DIME'      
      NOMSDA=SDFETI//'.FETA'
      NOMSDB=SDFETI//'.FETB'
C      NOMSDD=SDFETI//'.FETD'
C      NOMSDE=SDFETI//'.FETE'
      NOMSDI=SDFETI//'.FETI'
      NOMSDG=SDFETI//'.FETG'
      NOMSDH=SDFETI//'.FETH'
      NOMSDJ=SDFETI//'.FETJ'

C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
      CALL GETVID(' ','MODELE',1,1,1,NOMO,NBVM)

C --- LIGREL DU MODELE
      LIGRMO = NOMO(1:8)//'.MODELE'


C CREATION DES DIFFERENTS ATTRIBUTS DE LA S.D. SD_FETI
C .REFE

C NBRE DE CHARGE
      NBCHAR=1

C LISTE DES CHARGES
      NOMCHA(1)='CH1'

      INTBUF=NBCHAR+1
      CALL WKVECT(NOMREF,'G V K8',INTBUF,JADR)
      ZK8(JADR)=NOMO
      DO 50 I=1,NBCHAR
        ZK8(JADR+I)=NOMCHA(I)
   50 CONTINUE

      IF (NIV.GE.3) THEN 
        WRITE(IFM,*)
        WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'      
        WRITE (IFM,*)'<FETI/OBTEMP> CREATION OBJET JEVEUX ',NOMREF 
      ENDIF 

C A FINIR.....



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
      CALL JEVEUO (LIGRMO//'.PRNM','L',JPRNM)
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


C C C NOMS DES GROUP_MA CONTENANT LES PEAUX DES SOUS-DOMAINES : LSTBRD
C       CALL GETVTX(' ','LIST_BRD',1,1,NSDMAX,LSTBRD,NBSD)
C       WRITE(6,*) 'LSTBRD=',LSTBRD


C C NOMS DES GROUP_MA CONTENANT LES SOUS-DOMAINES : LSTGMA
C       CALL GETVTX(' ','LIST_GMA',1,1,NSDMAX,LSTGMA,NBSD)
C       IF (NBSD.GT.NSDMAX) THEN
C         CALL UTMESS('F','FETCRF','ON NE PEUT DEFINIR PLUS DE :
C      &                 '//NOMSD(I)//' SOUS-DOMAINES.')
C       END IF
C       IF (NIV.GE.4) WRITE(6,*) 'NBSD=',NBSD


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
        CALL JEEXIN(JEXNOM(GRPMA,NOMSD(I)),IRET)
        IF (IRET.GT.0) THEN
          CALL UTMESS('F','FETCRF','LE GROUP_MA : '//NOMSD(I)//
     &                ' EXISTE DEJA, CHANGER ''NOM''.')
        ENDIF
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
          CALL JXVERI('MESSAGE',' ')
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

        CALL JXVERI('MESSAGE',' ')


C       CREATION DU VECTEUR DES NOEUDS
C                DU VECTEUR DE CORRESPONDANCE NOEUD -> SD
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

        CALL JXVERI('MESSAGE',' ')

        CALL JEDETR('&&FETCRF.TMP')

   22 CONTINUE

      CALL JEDETR('&&FETCRF.TRAV')

      CALL JXVERI('MESSAGE',' ')


C      REMPLISSAGE DE SDFETI / .FETA
C voir comment ajouter les mailles de peau ??
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

C Note AA : Ancienne version
C       CALL WKVECT('&&FETCRF.LST_ORDO ','V V I',NB,IALSNO)
C       CALL WKVECT('&&FETCRF.LST_MULT ','V V I',NB,IALSMU)
C       CALL WKVECT('&&FETCRF.LST_GMA  ','V V I',NB,IALSMA)
C 
C       WRITE(6,*) '**************************'
C       K=1
C       KK=1
C       DO 28 I = 1,NB
C         JJ=1
C         DO 29 J = KK,NB
C           IF ( ZI(IALINO-1+I).EQ.ZI(IALINO-1+J) ) THEN
C             IF (I.NE.J) THEN
C               WRITE(6,*) '-------------'
C               WRITE(6,*) 'IDEM=',ZI(IALINO-1+I),ZI(LINOMA-1+I),I
C               WRITE(6,*) 'IDEM=',ZI(IALINO-1+J),ZI(LINOMA-1+J),J
C C             AJOUT DU GMA OU DES GMA
C              1. 1ER NOEUD DE LA LISTE : ON AJOUTE LES DEUX PREMIERS SD
C               IF (KK.EQ.1) THEN
C                 ZI(IALSMA-1+KK)=ZI(LINOMA-1+I)
C                 ZI(IALSMA-1+KK+1)=ZI(LINOMA-1+J)
C 
C                 WRITE(6,*) 'bla1',KK
C 
C             WRITE(6,*) 'ZI(IALINO-1+I)=  ',ZI(IALINO-1+I)
C             WRITE(6,*) 'ZI(LINOMA-1+I)= ',ZI(LINOMA-1+I)
C             WRITE(6,*) 'ZI(IALSPO-1+I)=',ZI(IALSPO-1+I)
C                 JADR=ZI(IAJADR-1+ZI(LINOMA-1+I))
C             WRITE(6,*) 'JADR=',JADR
C             WRITE(6,*) 'NOEUD=',-1*ABS(ZI(JADR+2*ZI(IALSPO-1+I)-1-1))
C                 ZI(JADR+2*ZI(IALSPO-1+I)-1-1)=
C      &            -1*ABS(ZI(JADR+2*ZI(IALSPO-1+I)-1-1))
C             WRITE(6,*) 'ZI(IALINO-1+J)=  ',ZI(IALINO-1+J)
C             WRITE(6,*) 'ZI(LINOMA-1+J)= ',ZI(LINOMA-1+J)
C             WRITE(6,*) 'ZI(IALSPO-1+J)=',ZI(IALSPO-1+J)
C                 JADR=ZI(IAJADR-1+ZI(LINOMA-1+J))
C             WRITE(6,*) 'JADR=',JADR
C             WRITE(6,*) 'NOEUD=',-1*ABS(ZI(JADR+2*ZI(IALSPO-1+J)-1-1))
C                 ZI(JADR+2*ZI(IALSPO-1+J)-1-1)=
C      &            -1*ABS(ZI(JADR+2*ZI(IALSPO-1+J)-1-1))
C                 KK=KK+2
C                 JJ=JJ+1
C              2. N-IEME APPARITION : ON COMPLETE LA LISTE DES SD (FETJ)
C               ELSE
C                 WRITE(6,*) '--- ',ZI(IALSNO-1+K-1),ZI(IALINO-1+I)
C C               2.1 LE NOEUD EST DEJA PRIS EN COMPTE
C                 IF ( ZI(IALSNO-1+K-1).EQ.ZI(IALINO-1+I) ) THEN
C                   ZI(IALSMA-1+KK)=ZI(LINOMA-1+J)
C                   WRITE(6,*) 'bla2',KK
C 
C             WRITE(6,*) 'ZI(IALINO-1+I)=  ',ZI(IALINO-1+I)
C             WRITE(6,*) 'ZI(LINOMA-1+I)= ',ZI(LINOMA-1+I)
C             WRITE(6,*) 'ZI(IALSPO-1+I)=',ZI(IALSPO-1+I)
C                   JADR=ZI(IAJADR-1+ZI(LINOMA-1+I))
C             WRITE(6,*) 'JADR=',JADR
C             WRITE(6,*) 'NOEUD=',-1*ABS(ZI(JADR+2*ZI(IALSPO-1+I)-1-1))
C                   ZI(JADR+2*ZI(IALSPO-1+I)-1-1)=
C      &              -1*ABS(ZI(JADR+2*ZI(IALSPO-1+I)-1-1))
C 
C             WRITE(6,*) 'ZI(IALINO-1+J)=  ',ZI(IALINO-1+J)
C             WRITE(6,*) 'ZI(LINOMA-1+J)= ',ZI(LINOMA-1+J)
C             WRITE(6,*) 'ZI(IALSPO-1+J)=',ZI(IALSPO-1+J)
C                   JADR=ZI(IAJADR-1+ZI(LINOMA-1+J))
C             WRITE(6,*) 'JADR=',JADR
C             WRITE(6,*) 'NOEUD=',-1*ABS(ZI(JADR+2*ZI(IALSPO-1+J)-1-1))
C                   ZI(JADR+2*ZI(IALSPO-1+J)-1-1)=
C      &              -1*ABS(ZI(JADR+2*ZI(IALSPO-1+J)-1-1))
C 
C                   KK=KK+1
C C               2.2 LE NOEUD N'EST PAS DEJA PRIS EN COMPTE
C                 ELSE
C                   ZI(IALSMA-1+KK)=ZI(LINOMA-1+I)
C                   ZI(IALSMA-1+KK+1)=ZI(LINOMA-1+J)
C                   WRITE(6,*) 'bla3',KK
C 
C             WRITE(6,*) 'ZI(IALINO-1+I)=  ',ZI(IALINO-1+I)
C             WRITE(6,*) 'ZI(LINOMA-1+I)= ',ZI(LINOMA-1+I)
C             WRITE(6,*) 'ZI(IALSPO-1+I)=',ZI(IALSPO-1+I)
C                   JADR=ZI(IAJADR-1+ZI(LINOMA-1+I))
C             WRITE(6,*) 'JADR=',JADR
C             WRITE(6,*) 'NOEUD=',-1*ABS(ZI(JADR+2*ZI(IALSPO-1+I)-1-1))
C                   ZI(JADR+2*ZI(IALSPO-1+I)-1-1)=
C      &              -1*ABS(ZI(JADR+2*ZI(IALSPO-1+I)-1-1))
C 
C             WRITE(6,*) 'ZI(IALINO-1+J)=  ',ZI(IALINO-1+J)
C             WRITE(6,*) 'ZI(LINOMA-1+J)= ',ZI(LINOMA-1+J)
C             WRITE(6,*) 'ZI(IALSPO-1+J)=',ZI(IALSPO-1+J)
C                   JADR=ZI(IAJADR-1+ZI(LINOMA-1+J))
C             WRITE(6,*) 'JADR=',JADR
C             WRITE(6,*) 'NOEUD=',-1*ABS(ZI(JADR+2*ZI(IALSPO-1+J)-1-1))
C                   ZI(JADR+2*ZI(IALSPO-1+J)-1-1)=
C      &              -1*ABS(ZI(JADR+2*ZI(IALSPO-1+J)-1-1))
C 
C                   KK=KK+2
C                   JJ=JJ+1
C                 ENDIF
C               ENDIF
C C              JJ=JJ+1
C               WRITE(6,*) '+++++++++++++'
C             ENDIF
C           ELSEIF ( (JJ.GT.1).AND.
C      &           ( ZI(IALINO-1+K-1).NE.ZI(IALINO-1+I) ) ) THEN
C             ZI(IALSNO-1+K)=ZI(IALINO-1+I)
C             ZI(IALSMU-1+K)=JJ
C 
C             IF (JJ.GT.2) THEN
C               DO 39 II=1,JJ
C                 WRITE(6,*) 'OK=',ZI(IALSMA-1+KK-JJ+II-1)
C             
C    39         CONTINUE
C             
C             ENDIF
C 
C             WRITE(6,*) 'ZI(IALSNO-1+K)=',ZI(IALSNO-1+K)
C             WRITE(6,*) 'JJ=',JJ
C             WRITE(6,*) 'I=',I
C             WRITE(6,*) 'J=',J
C 
C 
C             K=K+1
C             JJ=1
C 
C             GOTO 28
C           ENDIF
C    29   CONTINUE
C    28 CONTINUE






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
C C          ZI(JADR+J-1)=ZI(IALSFG-1+J)
          ZI(JADR+2*(J-1))  =ZI(IALSFG+2*(J-1))
          ZI(JADR+2*(J-1)+1)=ZI(IALSFG+2*(J-1)+1)
  610   CONTINUE                      

C FIN BOUCLE SUR LES SD
  51  CONTINUE





C CREATION DES DIFFERENTS ATTRIBUTS DE LA S.D. SD_FETI
C .DIME                 

C     ON COMPTE LES MAILLES DE BORDS DANS LE TOTAL DES MAILLES
      DO 93 I=1,NBSD
        NBMATO=NBMATO+NBMABD(I)
  93  CONTINUE

      CALL WKVECT(NOMSDM,'G V I',5,JADR)
      ZI(JADR)=NBSD
      ZI(JADR+1)=NBFETE
      ZI(JADR+2)=NBMATO
      ZI(JADR+3)=NBFETE*2
      ZI(JADR+4)=NBNOTO
            
      IF (NIV.GE.3) THEN 
             WRITE(IFM,*)
        WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'      
        WRITE (IFM,*)'<FETCRF.F> CREATION OBJET JEVEUX ',NOMSDM 
      ENDIF 

C C .FETA
C C voir comment ajouter les mailles de peau
C       CALL JECREC(NOMSDA,'G V I','NO','DISPERSE','VARIABLE',NBSD)
C       DO 100 I=1,NBSD
C         K8BUFF=NOMSD(I)
C         CALL JECROC(JEXNOM(NOMSDA,K8BUFF))
C C        INTBUF=NBMA(I)
C         INTBUF=ZI(IANBMA-1+I)
C         CALL JEECRA(JEXNOM(NOMSDA,K8BUFF),'LONMAX',INTBUF,K8BID)
C         CALL JEVEUO(JEXNOM(NOMSDA,K8BUFF),'E',JADR)
C         DO 90 J=1,INTBUF
C C        ZI(JADR+J-1)=LISTMA(J,I)
C           ZI(JADR+J-1)=ZI(IALIMA-1+J)
C    90   CONTINUE                        
C   100 CONTINUE
C       IF (NIV.GE.3) 
C      &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDA   
  
C C .FETB
C       CALL JECREC(NOMSDB,'G V I','NO','DISPERSE','VARIABLE',NBSD)
C       DO 200 I=1,NBSD
C         K8BUFF=NOMSD(I)      
C         CALL JECROC(JEXNOM(NOMSDB,K8BUFF))
C            INTBUF=2*NBNO(I)
C            CALL JEECRA(JEXNOM(NOMSDB,K8BUFF),'LONMAX',INTBUF,K8BID)
C         CALL JEVEUO(JEXNOM(NOMSDB,K8BUFF),'E',JADR)
C            DO 210 J=1,INTBUF
C              ZI(JADR+J-1)=LISTNO(J,I)
C   210   CONTINUE                      
C   200 CONTINUE
C       IF (NIV.GE.3) 
C      &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDB   

C C .FETD
C       CALL JECREC(NOMSDD,'G V I','NO','DISPERSE','VARIABLE',NBSD)
C       DO 300 I=1,NBSD
C         K8BUFF=NOMSD(I)      
C         CALL JECROC(JEXNOM(NOMSDD,K8BUFF))
C       INTBUF=3*NBVO(I)
C       CALL JEECRA(JEXNOM(NOMSDD,K8BUFF),'LONMAX',INTBUF,K8BID)
C         CALL JEVEUO(JEXNOM(NOMSDD,K8BUFF),'E',JADR)
C       DO 310 J=1,INTBUF
C         ZI(JADR+J-1)=LISTCO(J,I)
C   310   CONTINUE                      
C   300 CONTINUE
C       IF (NIV.GE.3) 
C      &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDD   

C C .FETE                 
C       CALL WKVECT(NOMSDE,'G V I',NBFETE,JADR)
C       DO 400 I=1,NBFETE
C         ZI(JADR+I-1)=LISTFE(I)
C   400 CONTINUE
C       IF (NIV.GE.3) 
C      &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDE   
     
C C .FETI
C       NBNOI3=3*NBNOIN                
C       CALL WKVECT(NOMSDI,'G V I',NBNOI3,JADR)
C       DO 500 I=1,NBNOI3
C         ZI(JADR+I-1)=LISTNI(I)
C   500 CONTINUE
C       IF (NIV.GE.3) 
C      &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDI   


C C .FETG
C       CALL JECREC(NOMSDG,'G V I','NO','DISPERSE','VARIABLE',NBSD)
C       DO 600 I=1,NBSD
C         K8BUFF=NOMSD(I)      
C         CALL JECROC(JEXNOM(NOMSDG,K8BUFF))
C       INTBUF=2*NBPR(I)
C       CALL JEECRA(JEXNOM(NOMSDG,K8BUFF),'LONMAX',INTBUF,K8BID)
C         CALL JEVEUO(JEXNOM(NOMSDG,K8BUFF),'E',JADR)
C       DO 610 J=1,INTBUF
C         ZI(JADR+J-1)=LISTPR(J,I)
C   610   CONTINUE                      
C   600 CONTINUE
C       IF (NIV.GE.3) 
C      &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDG

C.FETH
      CALL WKVECT(NOMSDH,'G V I',NBSD,JADR)
      DO 700 I=1,NBSD
C        ZI(JADR+I-1)=NBDDL(I)
        ZI(JADR+I-1)=ZI(IALSK-1+I)
  700 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDH
              
C       IF (NIV.GE.4)
C      &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,SDFETI(1:19),1,' ')
C       IF (NIV.GE.3) THEN        
C         WRITE(IFM,*)'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'
C              WRITE(IFM,*)
C       ENDIF

      CALL JXVERI('MESSAGE','FIN')
      CALL JEDEMA()
      END
