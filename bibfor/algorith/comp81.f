      SUBROUTINE COMP81(NOMRES,BASMOD,RAIDF,MASSF,AMORF,NOMA)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8  NOMRES,NOMA,BASMOD
      CHARACTER*19 MASSF,RAIDF,AMORF
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/10/2010   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C  BUT : COMPATIBILITE MACR_ELEM_DYNA/MACR_ELEM_STAT
C-----------------------------------------------------------------------
C
C NOMRES /I/ : NOM UTILISATEUR DU RESULTAT
C BASMOD /I/ : NOM UT DE LA BASE MODALE DE PROJECTION
C RAIDF  /I/ : NOM UT DE LA MATRICE RAIDEUR A PROJETER
C MASSEF /I/ : NOM UT DE LA MATRICE DE MASSE A PROJETER
C AMORF  /I/ : NOM UT DE LA MATRICE D'AMORTISSEMENT A PROJETER
C MAILLA /I/ : NOM UT DU MAILLAGE EN AMONT
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
      CHARACTER*32 JEXNUM,JEXNOM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      IAREFM,IRET,IBID,NBNOE,LLDEF,IACONX,NTOT,
     +             IAK11,IAK1,IAM11,IAM1,NBMTOT,NBMDEF,IER,
     +             NBMDYN,NBNDYN,I,J,K,INEBID,NEC,GD,NBEC
      REAL*8       RBNDYN
C      CHARACTER*6  PGC
      CHARACTER*8  NOMO,BLANC,LINTF,K8BID,CHMAT,CHCAR,NOGDSI,NOGDS2
      CHARACTER*8  NOMCAS,VECTAS,GNEX
      CHARACTER*14 NUMDDL
      CHARACTER*19 NU
C      CHARACTER*3  TREDU
      LOGICAL      LREDU
C
C-----------------------------------------------------------------------
      DATA BLANC         /'        '/
C-----------------------------------------------------------------------
C --- RECUPERATION EVENTUELLE MATRICE RAIDEUR EN ARGUMENT
C
      CALL JEMARQ()
C
      NU = NOMRES
      NU = NU(1:14)//'.NUME'
      LREDU = .FALSE.

C **********************
C     RECUPERATION DES INFOS UTILES
C **********************
      CALL JEVEUO(BASMOD//'           .REFD','L',LLREFB)
      NUMDDL=ZK24(LLREFB+3)(1:14)
      LINTF=ZK24(LLREFB+4)(1:8)      

      CALL DISMOI('F','NOM_MODELE',NUMDDL,'NUME_DDL',IBID,NOMO,IRET)
      IF (RAIDF.NE.BLANC) THEN
        CALL DISMOI('F','CHAM_MATER',RAIDF,'MATR_ASSE',IBID,CHMAT,IRET)
        CALL DISMOI('F','CARA_ELEM',RAIDF,'MATR_ASSE',IBID,CHCAR,IRET)
      ELSE
        CHMAT = BLANC
        CHCAR = BLANC
      ENDIF
      
      IF (LINTF.NE.BLANC) THEN
C On recupere le nbre de noeuds presents dans interf_dyna
        CALL JELIRA(JEXNUM(LINTF//'.IDC_LINO',1),'LONMAX',
     &              NBNOE,K8BID)
C On recupere le liste des noeuds presents dans interf_dyna
        CALL JEVEUO(LINTF//'.IDC_DEFO','L',LLDEF)
      ELSE
        NBNOE=0
      ENDIF
      CALL JEVEUO(NOMRES//'.MAEL_MASS_DESC','L',IADESC)
      CALL DISMOI('F','NB_MODES_TOT',BASMOD,'RESULTAT',
     &                      NBMTOT,K8BID,IER)
      IF (NBMTOT.EQ.0) CALL ASSERT(.FALSE.)
      CALL DISMOI('F','NB_MODES_STA',BASMOD,'RESULTAT',
     &                      NBMDEF,K8BID,IER)
      NBMDYN=NBMTOT-NBMDEF
      IF (NBNDYN.LT.0) CALL ASSERT(.FALSE.)
      
      IF (NBMTOT.NE.ZI(IADESC+1)) THEN
        CALL U2MESS('I','ALGORITH_52')        
      ENDIF

C **********************
C     CREATION DU .NUME
C **********************
      CALL COPISD('NUME_DDL','G',NUMDDL,NU)

      CALL DISMOI('F','NOM_GD',NU(1:14),'NUME_DDL',IBID,NOGDSI,IERD)
      CALL DISMOI('F','NB_EC',NOGDSI,'GRANDEUR',NEC,K8BID,IERD)

C Il faut choisir NBNDYN qui ne soient pas sur l'interface et possedant 
C NCMPMX composantes.
      CALL JELIRA(JEXNUM(NU(1:19)//'.PRNO',1),'LONMAX',N1,K8BID)
      CALL JEVEUO(JEXNUM(NU(1:19)//'.PRNO',1),'L',IAPRNO)
      NBNO = N1/(NEC+2)
      K=1
      NCMPMX = 0
      DO 553 I=1,NBNO
        NUNOT=ZI(IAPRNO-1+ (I-1)* (NEC+2)+1)
        IF (NUNOT.NE.0) THEN
          NUEQ = ZI(IAPRNO-1+ (I-1)* (NEC+2)+2)
          NCMPMX = MAX(NCMPMX,NUEQ)
        ENDIF
  553 CONTINUE
C On va choisir plusieurs noeuds qui ne sont pas presents dans
C l'interface et tels que le nbre de ddl considere soit egal 
C au nbre de modes dynamiques
C On prend comme postulat que NBNDYN=PARTIE_ENTIERE de NBMDYN/NCMPMX
      CALL GETVTX(' ','SANS_GROUP_NO',1,1,1,GNEX,IGEX)
      IF (IGEX.NE.0) THEN
        CALL JELIRA(JEXNOM(NOMA//'.GROUPENO',GNEX),'LONMAX',NBNO2,K8BID)
        CALL JEVEUO(JEXNOM(NOMA//'.GROUPENO',GNEX),'L',LDGN0)
        CALL WKVECT('&&COMP81.NEUEXC','V V I',NBNO2,LDGN)
        DO 557 J=1,NBNO2
          ZI(LDGN+J-1)=ZI(LDGN0+J-1)
  557   CONTINUE
      ELSE
        NBNO2=NBNOE
        IF (NBNO2.NE.0) THEN
          CALL WKVECT('&&COMP81.NEUEXC','V V I',NBNO2,LDGN)
          DO 558 J=1,NBNO2
            ZI(LDGN+J-1)=ZI(LLDEF+J-1)
  558     CONTINUE
        ELSE
          CALL WKVECT('&&COMP81.NEUEXC','V V I',1,LDGN)
          ZI(LDGN) = 0
        ENDIF
      ENDIF
      NBNDYN=NBMDYN/NCMPMX
      RBNDYN=DBLE(NBMDYN)/DBLE(NCMPMX)
      IF (ABS(RBNDYN-DBLE(NBNDYN)).GT.0.D0) THEN
        CALL U2MESI('I','ALGORITH_53',1,NCMPMX)        
      ENDIF
      IF (NBNDYN.EQ.0) THEN
         CALL WKVECT(NOMRES//'.NEUBID','V V I',1,INEBID)
         ZI(INEBID) = 0
         GOTO 554
      ENDIF
      CALL WKVECT(NOMRES//'.NEUBID','V V I',NBNDYN,INEBID)
      DO 555 I=1,NBNO
        NUNOT=ZI(IAPRNO-1+ (I-1)* (NEC+2)+1)
        IF (NUNOT.NE.0) THEN
          NUEQ = ZI(IAPRNO-1+ (I-1)* (NEC+2)+2)
          IF (NUEQ.EQ.NCMPMX) THEN
            DO 556 J=1,NBNO2
              IF (I.EQ.ZI(LDGN+J-1)) GOTO 555
  556       CONTINUE
            ZI(INEBID+K-1)= I
            IF (K.EQ.NBNDYN) GOTO 554
            K=K+1
          ENDIF
        ENDIF
  555 CONTINUE

  554 CONTINUE
      IF (NBMDEF.NE.0) THEN
        CALL RSADPA(BASMOD,'L',1,'NOEUD_CMP',NBMDYN+1,0,LNOCMP,K8BID)
        IF (ZK16(LNOCMP).EQ.' ') LREDU=.TRUE.
      ENDIF
      IF (LREDU) THEN
        NBNDEF=NBMDEF/NCMPMX
        RBNDEF=DBLE(NBMDEF)/DBLE(NCMPMX)
        IF (ABS(RBNDEF-DBLE(NBNDEF)).GT.0.D0) THEN
          CALL U2MESI('I','ALGORITH_54',1,NCMPMX)        
        ENDIF
        IF (NBNDYN.NE.0) THEN
          NBNOT = NBNO2 + NBNDYN
          CALL JUVECA('&&COMP81.NEUEXC',NBNOT)
          CALL JEVEUO('&&COMP81.NEUEXC','E',LDGN)
          DO 651 J=NBNO2+1,NBNOT
            ZI(LDGN+J-1)=ZI(INEBID+J-1-NBNO2)
  651     CONTINUE
          NBNO2 = NBNOT        
        ENDIF
        CALL WKVECT('&&COMP81.NOSTDY','V V I',NBNDEF,INSTDY)
        K=1
        DO 655 I=1,NBNO
          NUNOT=ZI(IAPRNO-1+ (I-1)* (NEC+2)+1)
          IF (NUNOT.NE.0) THEN
            NUEQ = ZI(IAPRNO-1+ (I-1)* (NEC+2)+2)
            IF (NUEQ.EQ.NCMPMX) THEN
              DO 656 J=1,NBNO2
                IF (I.EQ.ZI(LDGN+J-1)) GOTO 655
  656         CONTINUE
              ZI(INSTDY+K-1)= I
              IF (K.EQ.NBNDEF) GOTO 654
              K=K+1
            ENDIF
          ENDIF
  655   CONTINUE

  654   CONTINUE
      ELSE
        IF (NBNOE.NE.0) THEN
          CALL WKVECT('&&COMP81.NOSTDY','V V I',NBNOE,INSTDY)
          DO 658 J=1,NBNOE
            ZI(INSTDY+J-1)=ZI(LLDEF+J-1)
  658     CONTINUE
        ELSE
          CALL WKVECT('&&COMP81.NOSTDY','V V I',1,INSTDY)
          ZI(INSTDY) = 0        
        ENDIF
        NBNDEF = NBNOE
      ENDIF
      
C **********************
C     CREATION DU .REFM
C **********************
      CALL WKVECT(NOMRES//'.REFM','G V K8',8,IAREFM)
C Stockage du nom du modele
      ZK8(IAREFM-1+1)= NOMO
C Stockage du nom du maillage
      ZK8(IAREFM-1+2)= NOMA
C Stockage du nom du champ de materiau
      ZK8(IAREFM-1+3)=CHMAT
C Stockage du nom du champ de caracteristiques elementaires
      ZK8(IAREFM-1+4)=CHCAR
C Stockage du nom de la numerotation
      ZK8(IAREFM-1+5)=NU
C Stockage du nom du champ de caracteristiques elementaires
      ZK8(IAREFM-1+6)= 'OUI_RIGI'
      ZK8(IAREFM-1+7)= 'OUI_MASS'
      ZK8(IAREFM-1+8)= 'NON_AMOR'

C **********************
C     CREATION DU .DESM
C **********************
      CALL WKVECT(NOMRES//'.DESM','G V I',10,IADESM)

C mettre ici le nbre de ?
      ZI(IADESM-1+1)= 0
C mettre ici le nbre de noeud exterieur non dupliques
      ZI(IADESM-1+2)=NBNDEF+NBNDYN
C mettre ici le nbre de noeuds internes
      ZI(IADESM-1+3)=NBNO
C mettre ici le nbre de ddl exterieur
      ZI(IADESM-1+4)=ZI(IADESC+1)
C mettre ici le nbre de ddl interieur (ou total)
      ZI(IADESM-1+5)=0
C mettre ici le nbre de chargement
      ZI(IADESM-1+6)=0
      ZI(IADESM-1+7)=0
C mettre ici le nbre de lagrange externe
      ZI(IADESM-1+8)=0
C mettre ici le nbre de lagrange liaison
      ZI(IADESM-1+9)=0
C mettre ici le nbre de lagrange interne
      ZI(IADESM-1+10)=0
C
      IF ((NBNDEF+NBNDYN).EQ.0) GOTO 669 
C
C **********************
C     CREATION DU .LINO
C **********************
      CALL WKVECT(NOMRES//'.LINO','G V I',NBNDEF+NBNDYN,IACONX)
      DO 665 I=1,NBNDYN
        ZI(IACONX+I-1)=ZI(INEBID+I-1)
 665  CONTINUE
      DO 666 I=NBNDYN+1,NBNDEF+NBNDYN
        ZI(IACONX+I-1)=ZI(INSTDY+I-NBNDYN-1)
 666  CONTINUE

C **********************
C     CREATION DU .CONX
C **********************
      CALL WKVECT(NOMRES//'.CONX','G V I',3*(NBNDEF+NBNDYN),IACON1)
      DO 667 I=1,NBNDYN
         ZI(IACON1+3*I-3)=1
         ZI(IACON1+3*I-2)=ZI(INEBID+I-1)
         ZI(IACON1+3*I-1)=0
 667  CONTINUE
      DO 668 I=NBNDYN+1,NBNDEF+NBNDYN
         ZI(IACON1+3*I-3)=1
         ZI(IACON1+3*I-2)=ZI(INSTDY+I-NBNDYN-1)
         ZI(IACON1+3*I-1)=0
 668  CONTINUE
 669  CONTINUE

C **********************
C     CREATION DU .KP_EE
C **********************
C      CALL WKVECT(NOMRES//'.KP_EE','G V R',NDDEXT,IAK1)
C      CALL WKVECT(NOMRES//'.MP_EE','G V R',NDDEXT,IAM1)
C      CALL JEVEUO(NOMRES//'.MAEL_RAID_VALE','L',IAK11)
C      CALL JEVEUO(NOMRES//'.MAEL_MASS_VALE','L',IAM11)
C      CALL DCOPY(NDDEXT,ZR(IAK11),1,ZR(IAK1),1)
C      CALL DCOPY(NDDEXT,ZR(IAM11),1,ZR(IAM1),1)
      CALL JEEXIN(NOMRES//'.MAEL_AMOR_VALE',IRET)
      IF (IRET.GT.0) THEN
C        CALL WKVECT(NOMRES//'.AP_EE','G V R',NDDEXT,IAA1)
C        CALL JEVEUO(NOMRES//'.MAEL_AMOR_VALE','L',IAA11)
C        CALL DCOPY(NDDEXT,ZR(IAA11),1,ZR(IAA1),1)
        ZK8(IAREFM-1+8)= 'OUI_AMOR'
      END IF
C
C     -- CREATION DES OBJETS .LICA ET .LICH:
C     --------------------------------------
      CALL GETFAC('CAS_CHARGE',NOCC)
      IF (NOCC.NE.0) THEN
        CALL JECREC(NOMRES//'.LICA','G V R','NO','DISPERSE','CONSTANT',
     +              NOCC)
        CALL JECREC(NOMRES//'.LICH','G V K8','NO','CONTIG','CONSTANT',
     +              NOCC)
        CALL JEECRA(NOMRES//'.LICA','LONMAX',2*NBMTOT,K8BID)
        CALL JEECRA(NOMRES//'.LICH','LONMAX',2,K8BID)
C
        DO 670, IOCC= 1,NOCC
          CALL GETVTX('CAS_CHARGE','NOM_CAS',IOCC,1,1,NOMCAS,N1)
          CALL GETVID('CAS_CHARGE','VECT_ASSE_GENE',IOCC,1,1,VECTAS,N1)
          CALL JECROC(JEXNOM(NOMRES//'.LICA',NOMCAS))
          CALL JECROC(JEXNOM(NOMRES//'.LICH',NOMCAS))
          CALL JENONU(JEXNOM(NOMRES//'.LICA',NOMCAS),ICAS)
          CALL JEVEUO(JEXNUM(NOMRES//'.LICA',ICAS),'E',IALICA)
          CALL JEVEUO(JEXNUM(NOMRES//'.LICH',ICAS),'E',IALICH)
          CALL JEVEUO(VECTAS//'           .VALE','L',IVAL)
          DO 671, IE= 1,NBMTOT
            ZR(IALICA+IE-1) = ZR(IVAL+IE-1)
            ZR(IALICA+NBMTOT+IE-1) = ZR(IVAL+IE-1)
 671      CONTINUE
          ZK8(IALICH)='NON_SUIV'
          ZK8(IALICH+1)=VECTAS
          ZI(IADESM-1+7)=ICAS
 670    CONTINUE
      ENDIF
      CALL JEDETC(' ','&&COMP81',1)
      
 9999 CONTINUE
      CALL JEDEMA()
      END
