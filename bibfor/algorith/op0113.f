      SUBROUTINE OP0113(IER)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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
C
      IMPLICIT NONE
      INTEGER           IER
C      
C ----------------------------------------------------------------------
C
C OPERATEUR MODI_MODELE_XFEM
C
C
C ----------------------------------------------------------------------
C
C
C OUT IER   : CODE RETOUR ERREUR COMMANDE
C               IER = 0 => TOUT S'EST BIEN PASSE
C               IER > 0 => NOMBRE D'ERREURS RENCONTREES
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32    JEXATR,JEXNUM,JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      REAL*8          CRIMAX
      INTEGER         IBID,IRET,IMAIL,IGR1,IEL,IMA
      INTEGER         KK,I,IFISS,J,JJ,J1,J2
      INTEGER         JNOUT,JDMA
      INTEGER         IADRMA,JMOFIS
      INTEGER         NBMAIL,NGR1,NBMA,N1,NBELT,NELT
      INTEGER         NMAENR,ITYPEL,NB1
      INTEGER         ITYXH8(3),ITYXP6(3),ITYXT4(3),ITCPQ4(3),ITCPQ8(4)
      INTEGER         ITCPT3(3),ITDPQ4(3),ITDPT3(3),ITF4(3),ITF3(3)
      INTEGER         NFISS,JNFIS,NFISMX
      INTEGER         JDIME,JCONX1,JCONX2
      INTEGER         NDIM,NCTC,NDEGRE
      CHARACTER*16    MOTFAC,K16BID,NOTYPE,DEGRE
      CHARACTER*19    LIGR1,LIGR2
      CHARACTER*24    LIEL1,LIEL2
      CHARACTER*24    XINDIC,GRP(3),MAIL2
      INTEGER         JINDIC,JGRP,JMAIL2
      CHARACTER*24    TRAV,SDCONT
      INTEGER         JTAB
      INTEGER         JXC,JXSDC      
      PARAMETER       (NFISMX=100)
      CHARACTER*8     MOD2,MOD1,FISS(NFISMX),K8BID,NOMA,NOMC
      LOGICAL         LHEAV
      CHARACTER*19    PINTTO,CNSETO,HEAVTO,LONCHA
      CHARACTER*19    BASLOC,LTNO,LNNO,STNO 
      CHARACTER*19    PINTER,AINTER,CFACE ,FACLON,BASECO
C      
C --- FONCTION ACCESS MAILLAGE (POUR MAILLE NUMERO ABSOLU IMAIL)
C --- ZZCONX: CONNECTIVITE DE LA MAILLE IMAIL
C --- ZZNBNE: NOMBRE DE NOEUDS DE LA MAILLE IMAIL
C
      INTEGER         ZZNBNE,ZZCONX
            
      ZZCONX(IMAIL,J) = ZI(JCONX1-1+ZI(JCONX2+IMAIL-1)+J-1)
      ZZNBNE(IMAIL)   = ZI(JCONX2+IMAIL) - ZI(JCONX2+IMAIL-1)
C
      DATA MOTFAC /' '/      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- NOM DU MODELE MODIFIE
C
      CALL GETRES(MOD2,K16BID,K16BID)
      LIGR2  = MOD2(1:8)//'.MODELE'
      LIEL2  = LIGR2(1:19)//'.LIEL'
C      
C --- NOM DU MODELE INITIAL
C     
      CALL GETVID(MOTFAC,'MODELE_IN',1,1,1,MOD1,IBID )
      LIGR1  = MOD1(1:8)//'.MODELE'
      LIEL1  = LIGR1(1:19)//'.LIEL'
C
C --- ACCES AU MAILLAGE INITIAL
C      
      CALL JEVEUO(MOD1(1:8)//'.MODELE    .LGRF','L',IADRMA)
      NOMA   = ZK8(IADRMA)
      CALL JEVEUO(NOMA(1:8)//'.DIME','L',JDIME)
      NDIM   = ZI(JDIME-1+6)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IBID)
      CALL JEVEUO(NOMA(1:8)//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',JCONX2)

C     DEGRE D'APPROXIMATION DU DEPLACEMENT
      CALL GETVID(MOTFAC,'DEGRE_DEPL'  ,1,1,1,DEGRE,IBID)
      IF (DEGRE.EQ.'LINEAIRE') THEN
        NDEGRE = 1
      ELSEIF (DEGRE.EQ.'QUADRATIQUE') THEN
        NDEGRE = 2
      ENDIF
C      
C --- RECUPERER LE NOMBRE DE FISSURES
C      
      CALL GETVID(MOTFAC,'FISSURE'  ,1,1,0,FISS,NFISS)
C
C --- NOMBRE DE FISSURES
C
      NFISS = -NFISS
      IF (NFISS .GT. NFISMX) THEN
        CALL U2MESI ('F', 'XFEM_2', 1, NFISMX)
      ENDIF  
C      
C --- RECUPERER LES FISSURES
C       
      CALL GETVID(MOTFAC,'FISSURE',1,1,NFISS,FISS , IBID )
      CALL GETVR8(MOTFAC,'CRITERE',1,1,1,CRIMAX,IBID)
C      
C --- CREATION DES OBJETS POUR MULTIFISSURATION DANS MODELE MODIFIE
C
      CALL WKVECT(MOD2(1:8)//'.NFIS'  ,'G V I'  ,2    ,JNFIS)
      CALL WKVECT(MOD2(1:8)//'.FISS'  ,'G V K8' ,NFISS,JMOFIS) 
      CALL WKVECT(MOD2(1:8)//'.XFEM_CONT'  ,'G V I'  ,NFISS,JXC)
      CALL WKVECT(MOD2(1:8)//'.XFEM_SDCONT','G V K24',1    ,JXSDC)      
      ZI(JNFIS)  = NFISS
      ZI(JNFIS+1) = NDEGRE
      DO 10 IFISS = 1,NFISS
        ZK8(JMOFIS+IFISS-1) = FISS(IFISS)
 10   CONTINUE  
C
C --- PRISE EN COMPTE DU CONTACT
C
      CALL GETVID(MOTFAC,'CONTACT',1,1,1,NOMC,NCTC) 
      IF (NCTC.NE.0) THEN
        IF (NDEGRE.EQ.2) CALL U2MESS('F','XFEM_12')
        NCTC = ABS(NCTC)
        IF (NCTC.EQ.1) THEN
          SDCONT      = NOMC(1:8)//'.CONTACT        '  
          ZK24(JXSDC) = SDCONT
          CALL XCONTA(NOMA  ,MOD2  ,NDIM  ,NFISS,FISS  ,
     &                SDCONT)         
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ENDIF
C
C --- CREATION DU TABLEAU DE TRAVAIL
C
      TRAV  = '&&OP0113.TAB'
      CALL WKVECT(TRAV,'V V I',NBMA*5,JTAB)
C
      DO 110 I=1,NBMA
        ZI(JTAB-1+5*(I-1)+4) = 1
 110  CONTINUE
C
C --------------------------------------------------------------------- 
C     1)  REMPLISSAGE DE TAB : NBMA X 5 : GR1 | GR2 | GR3 | GR0 | ITYP
C --------------------------------------------------------------------- 
C
      LHEAV  = .FALSE.
C
C --- BOUCLE SUR NOMBRE OCCURRENCES FISSURES
C
      DO 220 IFISS = 1,NFISS
      
        GRP(1) = FISS(IFISS)//'.MAILFISS  .HEAV'
        GRP(2) = FISS(IFISS)//'.MAILFISS  .CTIP'
        GRP(3) = FISS(IFISS)//'.MAILFISS  .HECT'
        XINDIC = FISS(IFISS)//'.MAILFISS .INDIC'
        
        CALL JEVEUO(XINDIC,'L',JINDIC)  

        DO 1000 KK = 1,3
          IF (ZI(JINDIC-1+2*(KK-1)+1).EQ.1) THEN
            IF (KK .EQ. 1) THEN
              LHEAV = .TRUE.
            ENDIF  
            
            CALL JEVEUO(GRP(KK),'L',JGRP)
            NMAENR = ZI(JINDIC-1+2*KK)
C            
C --- POUR CHAQUE MAILLE DE CE GRP, ON MET À 1 LA CASE DE TAB 
C --- COLONNE 1 ET À O LA CASE DE TAB COLONNE 4
C
            DO 120 I = 1,NMAENR
              IMA                     = ZI(JGRP-1+I)
              ZI(JTAB-1+5*(IMA-1)+KK) = 1
              ZI(JTAB-1+5*(IMA-1)+4)  = 0
 120        CONTINUE
          ENDIF
 1000   CONTINUE
 220  CONTINUE 
C
C --------------------------------------------------------------------- 
C       2)  MODIFICATION DE TAB EN FONCTION DE L'ENRICHISSEMENT
C ---------------------------------------------------------------------
C
      CALL XMOLIG(LIEL1,NDEGRE,TRAV)
C      
C --- ON COMPTE LE NB DE MAILLES DU LIGREL1 (= NB DE GREL DE LIEL2)
C
      NELT   = 0
      DO 230 IMA = 1,NBMA
        IF (ZI(JTAB-1+5*(IMA-1)+5).NE.0) THEN
          NELT   = NELT+1
        ENDIF
 230  CONTINUE
      IF (NELT.EQ.0) THEN
        CALL U2MESS('A','XFEM2_51')
      ENDIF  

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

C-----------------------------------------------------------------------
C     4)  CONSTRUCTION DU .MAILLE 
C-----------------------------------------------------------------------

      MAIL2 = MOD2//'.MAILLE'
      CALL WKVECT(MAIL2,'G V I',NBMA,JMAIL2)
      DO 400 IMA = 1,NBMA
        ZI(JMAIL2-1+IMA)=ZI(JTAB-1+5*(IMA-1)+5)
 400  CONTINUE

C-----------------------------------------------------------------------
C     5) DUPLICATION DU .NOMA, .NBNO
C                ET DES .NEMA, .SSSA, .NOEUD S'ILS EXISTENT
C        PUIS .REPE, .PRNM ET .PRNS AVEC CALL ADALIG CORMGI ET INITEL
C-----------------------------------------------------------------------

      CALL JEDUPO(LIGR1//'.LGRF','G',LIGR2//'.LGRF',.FALSE.)
      CALL JEDUPO(LIGR1//'.NBNO','G',LIGR2//'.NBNO',.FALSE.)

      CALL JEDUP1(MOD1//'.NEMA' ,'G',MOD2//'.NEMA')
      CALL JEDUP1(MOD1//'.SSSA' ,'G',MOD2//'.SSSA')
      CALL JEDUP1(MOD1//'.NOEUD','G',MOD2//'.NOEUD')

      CALL ADALIG(LIGR2)
      CALL CORMGI('G',LIGR2)
      CALL INITEL(LIGR2)


C-----------------------------------------------------------------------
C     6)  CALCUL DU DÉCOUPAGE EN SOUS-TETRAS, DES FACETTES DE CONTACT
C         ET VERIFICATION DES CRITERES DE CONDITIONNEMENT
C-----------------------------------------------------------------------


      DO 600 IFISS=1,NFISS
C      
C --- CALCUL DES DONNEES UTILES POUR L'INTÉGRATION (SOUS-TETRAS...)
C
        CALL XTOPOI(MOD2,FISS(IFISS))  
C      
C --- ORIENTATION DES FACETTES DE PEAU X-FEM (COMME ORIE_PEAU)
C
        CALL XORIPE(MOD2,FISS(IFISS))
C      
C --- CALCUL DE LA TOPOLOGIE DES FACETTES DE CONTACT
C
        CALL XTOPOC(MOD2,FISS(IFISS))
C
C --- CREATION LISTE DE NOEUDS OU IL FAUDRA ANNULER LES DDLS HEAVISIDE
C 
        IF (NDIM .EQ.3) THEN
          CALL DISMOI('F','NB_MA_MAILLA',MOD2,'MODELE',NBMAIL,
     &                K8BID,IBID)
          CALL XSTAN2(CRIMAX,NOMA,NBMAIL,FISS(IFISS))
        ENDIF  
  600 CONTINUE
C
C --- CONCATENER LES CHAMPS ELEMENTAIRES ET NODAUX POUR LES
C --- FISSURES DU MODELE
C      
      PINTTO = MOD2(1:8)//'.TOPOSE.PIN'
      CNSETO = MOD2(1:8)//'.TOPOSE.CNS'
      HEAVTO = MOD2(1:8)//'.TOPOSE.HEA'
      LONCHA = MOD2(1:8)//'.TOPOSE.LON'
      BASLOC = MOD2(1:8)//'.BASLOC'
      LTNO   = MOD2(1:8)//'.LTNO'
      LNNO   = MOD2(1:8)//'.LNNO'
      STNO   = MOD2(1:8)//'.STNO'
      
      CALL XCONEL(MOD2,'.TOPOSE.PIN','G','TOPOSE','PPINTTO',PINTTO)
      CALL XCONEL(MOD2,'.TOPOSE.CNS','G','TOPOSE','PCNSETO',CNSETO)
      CALL XCONEL(MOD2,'.TOPOSE.HEA','G','TOPOSE','PHEAVTO',HEAVTO)
      CALL XCONEL(MOD2,'.TOPOSE.LON','G','TOPOSE','PLONCHA',LONCHA)


      CALL XCONNO(MOD2,'.BASLOC    ','G',BASLOC)
      CALL XCONNO(MOD2,'.LNNO      ','G',LNNO)  
      CALL XCONNO(MOD2,'.LTNO      ','G',LTNO)
      CALL XCONNO(MOD2,'.STNO      ','G',STNO)

      PINTER = MOD2(1:8)//'.TOPOFAC.PI'
      AINTER = MOD2(1:8)//'.TOPOFAC.AI'
      CFACE  = MOD2(1:8)//'.TOPOFAC.CF'
      FACLON = MOD2(1:8)//'.TOPOFAC.LO'
      BASECO = MOD2(1:8)//'.TOPOFAC.BA'
      
      
C     pour le moment, on zappe la concaténation des champs de contact
C     si on est en 2D et sans mailles HEAV (seules portant le contact)
C     bientot, toutes les mailles X-FEM 2D auront du contact, donc on 
C     pourra virer ce IF
      IF (LHEAV.OR.NDIM .EQ.3) THEN
        CALL XCONEL(MOD2,'.TOPOFAC.PI','G','RIGI_CONT','PPINTER',PINTER)
        CALL XCONEL(MOD2,'.TOPOFAC.AI','G','RIGI_CONT','PAINTER',AINTER)
        CALL XCONEL(MOD2,'.TOPOFAC.CF','G','RIGI_CONT','PCFACE' ,CFACE)
        CALL XCONEL(MOD2,'.TOPOFAC.LO','G','RIGI_CONT','PLONCHA',FACLON)
        CALL XCONEL(MOD2,'.TOPOFAC.BA','G','RIGI_CONT','PBASECO',BASECO)
      ENDIF
C
C --- MENAGE
C
      CALL JEDETR(TRAV)
C
      CALL JEDEMA()
      END
