      SUBROUTINE  PRJLIS (MODA,MAA,MODB,MAB,NBNOA,NBNOB,MOTCLE,LINTA,
     &                    LINTB,INTFA,INTFB,FPLIAO,FPLIBO,IADA,IADB,
     &                    NUMLIS,MATPRJ,MODGEN,SSTA,SSTB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/08/2004   AUTEUR NICOLAS O.NICOLAS 
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
C***********************************************************************
      IMPLICIT NONE
C  O. NICOLAS     DATE 01/08/04
C-----------------------------------------------------------------------
C  BUT : < CALCUL DE LA MATRICE DE PROJECTION >
C
C  CALCULER LA MATRICE DE PROJECTION D'UNE INTERFACE ESCLAVE SUR UNE
C  MAITRE POUR LA SOUS STRUCTURATION DYNAMIQUE
C
C-----------------------------------------------------------------------
C
C MODA /I/ : NOM DU MODELE MAITRE
C MODB /I/ : NOM DU MODELE ESCLAVE
C MAA /I/ : NOM DU MAILLAGE MAITRE
C MAB /I/ : NOM DU MAILLAGE ESCLAVE
C NBNOA /I/ : NOMBRE DE NOEUDS D'INTERFACE DU MAILLAGE MAITRE
C NBNOB /I/ : NOMBRE DE NOEUDS D'INTERFACE DU MAILLAGE ESCLAVE
C MOTCLE /I/ : MOT CLE RENSEIGNANT LE GROUPE OU LA MAILLE MAITRE
C LINTA /I/ : NOM DE L'INTERFACE AMONT MAITRE
C LINTB /I/ : NOM DE L'INTERFACE AMONT ESCLAVE
C INTFA /I/ : NOM DE L'INTERFACE MAITRE
C INTFB /I/ : NOM DE L'INTERFACE ESCLAVE
C MATPRJ /O/ : NOM DE LA MATRICE D'OBSERVATION
C NUMLIS /I/ : NUMERO INTERFACE COURANTE
C FPLIAO /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON ORIENTEES SSTA
C FPLIBO /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON ORIENTEES SSTB
C IADA   /I/ : VECTEUR DES CARACTERISTIQUES LIAISON SSTA
C IADB   /I/ : VECTEUR DES CARACTERISTIQUES LIAISON SSTB
C MODGEN  /I/ : NOM K8 DU MODELE GENERALISE
C SSTA    /I/ : NOM K8 DE LA SOUS-STRUCTURE MAITRE
C SSTB    /I/ : NOM K8 DE LA SOUS-STRUCTURE ESCLAVE
C
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32 JEXNOM, JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*1    K1BID
      CHARACTER*4    ZCST,K4BID
      CHARACTER*8    K8BID,LINTA,LINTB,MODA,MODB,MAA,MAB,INTFA,INTFB,
     &               MATPRJ,NONOB,NONOA,NONOAI,NOMG,NONOB2,MODGEN,
     &               SSTA,SSTB
      CHARACTER*16   TYMOCL(2),MOTCLE(2),MOTFAC,CORRES,CORRED    
      CHARACTER*24   INTA,INTB,FPLIAO,FPLIBO,TOTO,GEOMA,GEOMB
      INTEGER        IBID,IER,NBNOA,NBNOB,LLINTA,LLINTB,NBMAA,IAGMAA,
     &               NDIM,IACONB,IACONU,IACOCF,IDMAX,IDNOMN,IDCOEF,
     &               NBNOB2,IDECAL,INOA,INOB,NBTERM,
     &               IREDNO,IREDNB,ITEMNU,ITEMCF,ITEMCM,ITEMTM,
     &               NNOA,NUNOA,NUNOB,NUNOA2,NUNOAI,NUNOBI,
     &               I,J,K,L,CPT,IINOB,IINOA,
     &               LLPLIA,LLPLIB,ICOMPA,ICOMPB,LDMAT,LDMAT2,
     &               IADOA,IADOB,NBEC,IERD,NUMLIS,
     &               IADA(3),IADB(3),
     &               NBNOEA,NBNOEB,NBCMPM,
     &               LDESCA,LDESCB,IFM,NIV
      PARAMETER      (NBCMPM=10)
      INTEGER        IDECOA(NBCMPM),IDECOB(NBCMPM)
      REAL*8         RBID,BETA,COEFA

C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C     -- IMPRESSION DE LA RELATION SI INFO:2:
C     ---------------------------------------
      CALL INFNIV(IFM,NIV)

      MOTFAC='LIAISON'
      TYMOCL(1) = 'MAILLE'
      TYMOCL(2) = 'GROUP_MA'
      
C--------------LES NOMBRES DES NOEUDS DES INTERFACES
      INTA=LINTA//'      .INTD.LINO'
      CALL JENONU(JEXNOM(INTA(1:19)//'.NOMS',INTFA),IBID)
      CALL JELIRA(JEXNUM(INTA,IBID),'LONMAX',NBNOA,K8BID)
C      
      INTB=LINTB//'      .INTD.LINO'
      CALL JENONU(JEXNOM(INTB(1:19)//'.NOMS',INTFB),IBID)
      CALL JELIRA(JEXNUM(INTB,IBID),'LONMAX',NBNOB,K8BID)

C--------------LES LISTES DES NUMEROS DES NOEUDS DES INTERFACES
      CALL JENONU(JEXNOM(LINTA //'      .INTD.NOMS',INTFA),IBID)
      CALL JEVEUO(JEXNUM(LINTA //'      .INTD.LINO',IBID),'L',LLINTA)
      CALL JEVEUO(LINTA//'      .INTD.DEFO','L',LDESCA)
      
      CALL JENONU(JEXNOM(LINTB //'      .INTD.NOMS',INTFB),IBID)
      CALL JEVEUO(JEXNUM(LINTB //'      .INTD.LINO',IBID),'L',LLINTB)
      CALL JEVEUO(LINTB//'      .INTD.DEFO','L',LDESCB)
  

C--------------LE NOMBRE DES MAILLES DE L'INTERFACE MAITRE
      CALL RELIEM(MODA,MAA,'NU_MAILLE',MOTFAC,1,2,MOTCLE,TYMOCL,
     &           '&&PRJLIS.LIMANUA',NBMAA)
     
C--------------LA LISTE DES NUMEROS DES MAILLES DE L'INTERFACE MAITRE
      CALL JEVEUO('&&PRJLIS.LIMANUA','L',IAGMAA)

C---ON FAIT LA PROJECTION
      NDIM = 3
      CALL DISMOI('F','Z_CST',MAA,'MAILLAGE',IBID,ZCST,IER)
      IF (ZCST.EQ.'OUI') NDIM = 2

      CORRES = '&&PRJLIS.CORRES'

C--------------TRANSFORMATION DE LA GEOMETRIE POUR LA PROJECTION
      GEOMA = '&&PRJLIS.GEOM_TRANSA'
      GEOMB = '&&PRJLIS.GEOM_TRANSB'
      CALL GEOLIS (MODGEN,SSTA,SSTB,INTFA,INTFB,GEOMA,GEOMB,
     &            '&&PRJLIS.LIMANUA',NBMAA)

C--------------CALCUL DE CORRES
C 
      IF (NDIM.EQ.2) THEN
        CALL PJ2DCO('PARTIE',MODA,MODB,NBMAA,ZI(IAGMAA),NBNOB,
     &        ZI(LDESCB),GEOMA,GEOMB,CORRES,.FALSE.,RBID)
      ELSE IF (NDIM.EQ.3) THEN
        CALL PJ3DCO('PARTIE',MODA,MODB,NBMAA,ZI(IAGMAA),NBNOB,
     &        ZI(LDESCB),GEOMA,GEOMB,CORRES,.FALSE.,RBID)
      END IF

      CALL JEVEUO(CORRES//'.PJEF_NB','L',IACONB)
      CALL JEVEUO(CORRES//'.PJEF_NU','L',IACONU)
      CALL JEVEUO(CORRES//'.PJEF_CF','L',IACOCF)
      CALL JELIRA(CORRES//'.PJEF_NB','LONMAX',NBNOB2,K8BID)
        
C      CALL UTIMSD(6,2,.TRUE.,.TRUE.,'&&PRJLIS.CORRES',1,' ')


C-------------ON RECUPERE LES COEFFICIENTS DE LA PROJECTION
      TOTO='TUTU'
      CALL WKVECT(TOTO,'V V R',NBNOB*NBNOA,ITEMTM)
      
C Initialisation de la matrice d'observation
      DO 445 INOB = 1,NBNOB
        DO 444 INOA = 1,NBNOA
          ZR(ITEMTM+(INOB-1)*NBNOA+INOA-1)=0.D0
 444    CONTINUE
 445  CONTINUE

C Remplissage et impression de la matrice d'observation
      IDECAL = 0
      BETA = 0.0D0
      NBTERM=0
      IINOB=1
C boucle sur l'ensemble des noeuds du modele maitre
      DO 10 INOB = 1,NBNOB2
C on recupere le nombre de noeuds maitre lie au noeud esclave courant
        NNOA = ZI(IACONB+INOB-1)
        NBTERM=NNOA+1
C si le nbre de noeud maitre lie au noeud esclave courant est > 0
        IF (NNOA.GT.0) THEN
          NUNOB=ZI(LLINTB+IINOB-1)
          NUNOBI=ZI(LDESCB+NUNOB-1)
          CALL JENUNO(JEXNUM(MAB//'.NOMNOE',NUNOBI),NONOB)
          IF (NIV.EQ.2) THEN
            WRITE (IFM,*) ' '
            WRITE (IFM,*) '_RELA IMPRESSION D''UNE RELATION 
     &            LINEAIRE ENTRE '
     &            ,NBTERM,' DDLS. (AVANT NORMALISATION DE LA RELATION)'
            WRITE (IFM,1001) -1.D0,NONOB
          ENDIF  
C boucle sur le nombre de noeud maitre lie au noeud esclave courant
          DO 30,INOA = 1,NNOA
            NUNOA = ZI(IACONU+IDECAL-1+INOA)
            COEFA = ZR(IACOCF+IDECAL-1+INOA)
            CALL JENUNO(JEXNUM(MAA//'.NOMNOE',NUNOA),NONOA)
C boucle sur le nombre de noeud maitre present dans l'interface
            DO 40 J = 1,NBNOA
C si le noeud maitre courant est present dans la liste des noeuds
C maitres d'interface on stocke la valeur du coefficient
              NUNOA2=ZI(LLINTA+J-1)
              NUNOAI=ZI(LDESCA+NUNOA2-1)
              IF (NUNOA.EQ.NUNOAI) THEN
C On stocke la valeur du coefficient dans la matrice d'observation
C le stockage est donc C(Nbre Noeud esclave,Nbre Noeud maitre)
C l'ordre est donc celui de l'interface esclave pour 
C les lignes de la matrice et celui de l'interface maitre pour les 
C colonnes
                 ZR(ITEMTM+(IINOB-1)*NBNOA+J-1)=COEFA
                 IF (NIV.EQ.2) THEN
                   WRITE (IFM,1001) COEFA,NONOA
                 ENDIF  
              END IF
 40         CONTINUE
 30       CONTINUE
          IF (NIV.EQ.2) THEN
            WRITE (IFM,*) '_RELA = ',BETA
          ENDIF  
          IDECAL = IDECAL+NNOA
          IINOB=IINOB+1
        ENDIF
 10   CONTINUE 

C ************************************************************
C Recuperation des donnees par composantes          
      NOMG = 'DEPL_R'
      CALL DISMOI('F','NB_EC',NOMG,'GRANDEUR',NBEC,K8BID,IERD)
      IF (NBEC.GT.10) THEN
         CALL UTMESS('F','ROTLIS',
     +                   'LE DESCRIPTEUR_GRANDEUR DES DEPLACEMENTS'//
     +                    ' NE TIENT PAS SUR DIX ENTIERS CODES')
      ENDIF

      CALL JEVEUO(JEXNUM(FPLIAO,NUMLIS),'L',LLPLIA)
      CALL JEVEUO(JEXNUM(FPLIBO,NUMLIS),'L',LLPLIB)

      CALL WKVECT(MATPRJ,'G V R',IADA(1)*IADB(1),ITEMCM)
C Initialisation de la matrice d'observation
      DO 446 INOB = 1,IADB(1)
        DO 447 INOA = 1,IADA(1)
          ZR(ITEMCM+(INOB-1)*IADA(1)+INOA-1)=0.D0
 447    CONTINUE 
 446  CONTINUE 
C
      DO 110 INOB = 1,NBNOB
        IADOB=ZI(LLPLIB+(INOB-1)*(1+NBEC))
        CALL ISDECO(ZI(LLPLIB+(INOB-1)*(1+NBEC)+1),
     &              IDECOB,NBCMPM)
        ICOMPB=IADOB-1
        DO 120 I=1,NBCMPM
          IF (IDECOB(I).GT.0) THEN
            ICOMPB=ICOMPB+1
            DO 130 INOA = 1,NBNOA
              IADOA=ZI(LLPLIA+(INOA-1)*(1+NBEC))
              CALL ISDECO(ZI(LLPLIA+(INOA-1)*(1+NBEC)+1),
     &                  IDECOA,NBCMPM)
              ICOMPA=IADOA-1
              DO 140 J=1,NBCMPM
                IF ((IDECOA(J).GT.0) 
     &           .AND.(I.EQ.J)) THEN
C On se limite au repere globaux     
                  ICOMPA=ICOMPA+I
                  ZR(ITEMCM+(ICOMPB-1)*IADA(1)+ICOMPA-1)=
     &            ZR(ITEMTM+(INOB-1)*NBNOA+INOA-1)
                ENDIF
 140          CONTINUE 
 130        CONTINUE
          ENDIF
 120    CONTINUE 
 110  CONTINUE 


C ************************************************************

C-------------FORMAT D'IMPRESSION
 1001 FORMAT (' _RELA ',E14.7,A10,A10)
 1002 FORMAT (' _RELA ',E14.7,A10,A10,3X,3 (1X,E14.7))
 1003 FORMAT (A8,A5,A26,I5,A8)

      CALL JEDETR(TOTO)
      CALL JEDETR(GEOMA)
      CALL JEDETR(GEOMB)
      CALL JEDETR(CORRES)
      CALL JEDETR('&&PRJLIS')
      
 9999 CONTINUE
      CALL JEDEMA()
      END
