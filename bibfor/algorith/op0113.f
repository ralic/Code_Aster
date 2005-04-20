      SUBROUTINE OP0113(IER)
      IMPLICIT NONE
      INTEGER           IER

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/04/2005   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE GENIAUT S.GENIAUT
C                       OPERATEUR MODI_MODELE_XFEM
C
C     ------------------------------------------------------------------
C     OUT : IER = 0 => TOUT S'EST BIEN PASSE
C     : IER > 0 => NOMBRE D'ERREURS RENCONTREES
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32    JEXATR,JEXNUM,JEXNOM


C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     
      INTEGER         IFM,NIV,IBID,IRET,IMAIL,I,J,JJ,IGR1,IEL,IMA,J2
      INTEGER         IGR2
      INTEGER         JNOUT,ICONX1,ICONX2,JDMA,ZZNBNE,ZZCONX
      INTEGER         JTAB,J1,JINDIC,JG,IADRMA,JMAIL2,JMOFIS
      INTEGER         NBNOEU,NBMAIL,NBNO,NGR1,NBMA,N1,NBELT,NELT
      INTEGER         NMAENR,ITYPEL,Q(6),NB1
      INTEGER         ITYXH8(3),ITYXP6(3),ITYXT4(3)
      CHARACTER*8     MOD2,MOD1,FISS,K8BID,NOMA
      CHARACTER*16    MOTFAC,K16BID,NOTYPE  
      CHARACTER*19    LIGR1,LIGR2
      CHARACTER*24    LIEL1,LIEL2,OBJMA,INDIC,GRP,MAIL2
      ZZCONX(IMAIL,J) = ZI(ICONX1-1+ZI(ICONX2+IMAIL-1)+J-1)
      ZZNBNE(IMAIL)   = ZI(ICONX2+IMAIL) - ZI(ICONX2+IMAIL-1)
C     ------------------------------------------------------------------
      CALL JEMARQ()

      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      
      WRITE(IFM,*)'OP0113 : MODI MODELE'

      MOTFAC=' '

      CALL GETRES(MOD2,K16BID,K16BID)     
      CALL GETVID( MOTFAC, 'MODELE_IN', 1,1,1, MOD1, IBID )
      CALL GETVID( MOTFAC, 'FISSURE', 1,1,1,FISS , IBID )
      
C     ON RAJOUTE D'UN '.FISS' AU MODELE DE SORTIE
      CALL WKVECT(MOD2//'.FISS','G V K8',1,JMOFIS) 
      ZK8(JMOFIS)=FISS

      LIGR1=MOD1//'.MODELE'
      LIEL1=LIGR1//'.LIEL'

      LIGR2=MOD2//'.MODELE'
      LIEL2=LIGR2//'.LIEL'
      
      OBJMA = MOD1//'.MODELE    .NOMA' 
      CALL JEVEUO(OBJMA,'L',IADRMA)
      NOMA = ZK8(IADRMA)
     
C-----------------------------------------------------------------------
C     1)  REMPLISSAGE DE TAB : NBMA X 5 : GR1 | GR2 | GR3 | GR0 | ITYP
C-----------------------------------------------------------------------
      
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IBID)
      CALL WKVECT('&&OP0113.TAB','V V I',NBMA*5,JTAB)

C     INITIALISATION DE TAB À 0
      DO 100 I=1,5*NBMA
        ZI(JTAB-1+I)=0
 100  CONTINUE

C     INITIALISATION DE LA COLONNE 4 (GR0) À 1
      DO 110 I=1,NBMA
        ZI(JTAB-1+5*(I-1)+4)=1
 110  CONTINUE

      INDIC=FISS//'.MAILFISS .INDIC'
      CALL JEVEUO(INDIC,'L',JINDIC)  

      IF (ZI(JINDIC-1+1).EQ.1) THEN
        GRP=FISS//'.MAILFISS  .HEAV'
        CALL JEVEUO(GRP,'L',JG)
        NMAENR=ZI(JINDIC-1+2)
C       POUR CHAQUE MAILLE DE CE GRP, ON MET À 1 LA CASE DE TAB 
C       COLONNE 1 ET À O LA CASE DE TAB COLONNE 4
        DO 120 I=1,NMAENR
          IMA=ZI(JG-1+I)
          ZI(JTAB-1+5*(IMA-1)+1)=1
          ZI(JTAB-1+5*(IMA-1)+4)=0
 120    CONTINUE
      ENDIF

      IF (ZI(JINDIC-1+3).EQ.1) THEN
        GRP=FISS//'.MAILFISS  .CTIP'
        CALL JEVEUO(GRP,'L',JG)
        NMAENR=ZI(JINDIC-1+4)
C       POUR CHAQUE MAILLE DE CE GRP, ON MET À 1 LA CASE DE TAB 
C       COLONNE 2 ET À O LA CASE DE TAB COLONNE 4
        DO 130 I=1,NMAENR
          IMA=ZI(JG-1+I)
          ZI(JTAB-1+5*(IMA-1)+2)=1
          ZI(JTAB-1+5*(IMA-1)+4)=0
 130    CONTINUE
      ENDIF

      IF (ZI(JINDIC-1+5).EQ.1) THEN
        GRP=FISS//'.MAILFISS  .HECT'
        CALL JEVEUO(GRP,'L',JG)
        NMAENR=ZI(JINDIC-1+6)
C       POUR CHAQUE MAILLE DE CE GRP, ON MET À 1 LA CASE DE TAB 
C       COLONNE 3 ET À O LA CASE DE TAB COLONNE 4
        DO 140 I=1,NMAENR
          IMA=ZI(JG-1+I)
          ZI(JTAB-1+5*(IMA-1)+3)=1
          ZI(JTAB-1+5*(IMA-1)+4)=0
 140    CONTINUE
      ENDIF 

      WRITE(IFM,*)'OP0113-1) : FIN DE CREATION DE TAB'

C-----------------------------------------------------------------------
C     2)  MODIFICATION DE TAB EN FONTION DE L'ENRICHISSEMENT
C-----------------------------------------------------------------------

      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XH_HEXA8'),ITYXH8(1))
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XT_HEXA8'),ITYXH8(2))
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XHT_HEXA8'),ITYXH8(3))

      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XH_PENTA6'),ITYXP6(1))
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XT_PENTA6'),ITYXP6(2))
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XHT_PENTA6'),ITYXP6(3))

      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XH_TETRA4'),ITYXT4(1))
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XT_TETRA4'),ITYXT4(2))
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','MECA_XHT_TETRA4'),ITYXT4(3))

      CALL JELIRA(LIEL1,'NMAXOC',NGR1,K8BID)
      DO 200 IGR1=1,NGR1
        CALL JEVEUO(JEXNUM(LIEL1,IGR1),'L',J1)
        CALL JELIRA(JEXNUM(LIEL1,IGR1),'LONMAX',N1,K8BID)
        NBELT=N1-1
        ITYPEL=ZI(J1-1+N1)
        CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOTYPE)

        DO 210 IEL=1,NBELT
          IMA=ZI(J1-1+IEL)
          JJ=JTAB-1+5*(IMA-1)
          
          IF (NOTYPE.EQ.'MECA_X_HEXA20') THEN
            IF (ZI(JJ+1).EQ.1)  ZI(JJ+5)=ITYXH8(1)
            IF (ZI(JJ+2).EQ.1)  ZI(JJ+5)=ITYXH8(2)
            IF (ZI(JJ+3).EQ.1)  ZI(JJ+5)=ITYXH8(3)
            IF (ZI(JJ+4).EQ.1)  ZI(JJ+5)=ITYPEL          
          ELSEIF (NOTYPE.EQ.'MECA_X_PENTA15') THEN
            IF (ZI(JJ+1).EQ.1)  ZI(JJ+5)=ITYXP6(1)
            IF (ZI(JJ+2).EQ.1)  ZI(JJ+5)=ITYXP6(2)
            IF (ZI(JJ+3).EQ.1)  ZI(JJ+5)=ITYXP6(3)
            IF (ZI(JJ+4).EQ.1)  ZI(JJ+5)=ITYPEL             
          ELSEIF (NOTYPE.EQ.'MECA_X_TETRA10') THEN
            IF (ZI(JJ+1).EQ.1)  ZI(JJ+5)=ITYXT4(1)
            IF (ZI(JJ+2).EQ.1)  ZI(JJ+5)=ITYXT4(2)
            IF (ZI(JJ+3).EQ.1)  ZI(JJ+5)=ITYXT4(3)
            IF (ZI(JJ+4).EQ.1)  ZI(JJ+5)=ITYPEL          
          ELSE
            ZI(JJ+5)=ITYPEL
          ENDIF

 210    CONTINUE
 200  CONTINUE

C     ON COMPTE LE NB DE MAILLES DU LIGREL1 (= NB DE GREL DE LIEL2)
      NELT=0
      DO 230 IMA=1,NBMA    
        IF (ZI(JTAB-1+5*(IMA-1)+5).NE.0) NELT=NELT+1
 230  CONTINUE
      WRITE(IFM,*)'NB D''ELT DU LIEL2 =',NELT 
      IF (NELT.EQ.0) CALL UTMESS('F','OP0113','AUCUNE MAILLE ENRICHIE')

C     VISU  DE TAB 
      IF (NIV.GT.1) THEN 
        WRITE(IFM,*)'IMPRESSION DE TAB :'
        WRITE(IFM,*)'-------------------'
        WRITE(IFM,222)
 222    FORMAT(8X,'GR1',2X,'GR2',2X,'GR3',2X,'GR0',2X,'ITYP')
        DO 223 I=1,NBMA
          Q(1)=I
          DO 224 J=1,5 
            Q(J+1)=ZI(JTAB-1+5*(I-1)+J)     
 224      CONTINUE
          WRITE(IFM,225)(Q(J),J=1,6)
 223    CONTINUE
 225    FORMAT(I5,4X,4(I1,4X),I3)
      ENDIF  
      WRITE(IFM,*)'OP0113-2) : FIN DE MODIFICATION DE TAB'

C-----------------------------------------------------------------------
C     3)  CONSTRUCTION DU .LIEL2
C-----------------------------------------------------------------------

      CALL JECREC(LIEL2,'G V I','NU','CONTIG','VARIABLE',NELT)
      CALL JEECRA(LIEL2,'LONT',2*NELT,K8BID) 

      IEL=0
      DO 300 IMA=1,NBMA
        IF (ZI(JTAB-1+5*(IMA-1)+5).EQ.0)  GOTO 300
        IEL=IEL+1
        CALL JECROC(JEXNUM(LIEL2,IEL))
        CALL JEECRA(JEXNUM(LIEL2,IEL),'LONMAX',2,K8BID)  
        CALL JEVEUO(JEXNUM(LIEL2,IEL),'E',J2)
        ZI(J2-1+1)=IMA
        ZI(J2-1+2)=ZI(JTAB-1+5*(IMA-1)+5)
 300  CONTINUE      

      CALL JELIRA(LIEL2,'NUTIOC',NB1,K8BID)
      CALL ASSERT(NB1.EQ.NELT)
      WRITE(IFM,*)'OP0113-3) : FIN DE CREATION DU LIEL2'

C-----------------------------------------------------------------------
C     4)  CONSTRUCTION DU .MAILLE2
C-----------------------------------------------------------------------

      MAIL2=MOD2//'.MAILLE'
      CALL WKVECT(MAIL2,'G V I',NBMA,JMAIL2)
      DO 400 IMA=1,NBMA
        ZI(JMAIL2-1+IMA)=ZI(JTAB-1+5*(IMA-1)+5)
 400  CONTINUE     
      
      WRITE(IFM,*)'OP0113-4) : FIN DE CREATION DU MAILLE2'
      
C-----------------------------------------------------------------------
C     5) DUPLICATION DU .NOMA, .NBNO
C                ET DES .NEMA, .SSSA, .NOEUD S'ILS EXISTENT
C        PUIS .NOEUD_UTIL
C        PUIS .REPE, .PRNM ET .PRNS AVEC CALL ADALIG CORMGI ET INITEL 
C-----------------------------------------------------------------------

      CALL JEDUPO(LIGR1//'.NOMA','G',LIGR2//'.NOMA',.FALSE.)
      CALL JEDUPO(LIGR1//'.NBNO','G',LIGR2//'.NBNO',.FALSE.)

      CALL JEEXIN(LIGR1//'.NEMA',IRET)      
      IF (IRET.EQ.1) THEN
        CALL JEDUPO(LIGR1//'.NEMA','G',LIGR2//'.NEMA',.FALSE.)        
      ENDIF
      CALL JEEXIN(MOD1//'.SSSA',IRET)
      IF (IRET.EQ.1) THEN
        CALL JEDUPO(MOD1//'.SSSA','G',MOD2//'.SSSA',.FALSE.)        
      ENDIF
      CALL JEEXIN(MOD1//'.NOEUD',IRET)
      IF (IRET.EQ.1) THEN
        CALL JEDUPO(MOD1//'.NOEUD','G',MOD2//'.NOEUD',.FALSE.)        
      ENDIF
      
      CALL ADALIG(LIGR2)
      CALL CORMGI('G',LIGR2)

C     --- CREATION DE L'OBJET .NOEUD_UTIL :     
      CALL DISMOI('F','NB_NO_MAILLA',MOD2,'MODELE',NBNOEU,K8BID,IBID)
      CALL WKVECT(MOD2//'.NOEUD_UTIL','G V I',NBNOEU,JNOUT)
      CALL DISMOI('F','NB_MA_MAILLA',MOD2,'MODELE',NBMAIL,K8BID,IBID)
      IF (NBMAIL .EQ. 0) GOTO 520 
      CALL JEVEUO(NOMA//'.CONNEX','L',ICONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ICONX2)
      CALL JEVEUO(MAIL2,'L',JDMA)
      DO 500 IMA = 1,NBMAIL
        IF (ZI(JDMA+IMA-1) .EQ. 0) GOTO 500
        NBNO = ZZNBNE(IMA)
        DO 510 J = 1,NBNO
          ZI(JNOUT-1+ZZCONX(IMA,J)) = 1
 510    CONTINUE
 500  CONTINUE
 520  CONTINUE

      CALL INITEL(LIGR2)

      WRITE(IFM,*)'OP0113-5) : FIN DE DUPLICATION'

C     CALCUL DES DONNÉS UTILES POUR L'INTÉGRATION (SOUS-TÉTRAS...)
      CALL XTOPOI(MOD2,FISS)  

C     CALCUL DE LA TOPOLOGIE DES FACETTES DE CONTACT
      CALL XTOPOC(MOD2,FISS)   

      CALL JEDEMA()
      END
