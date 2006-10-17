      SUBROUTINE OP0074 (IER)
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/10/2006   AUTEUR REZETTE C.REZETTE 
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
C
C     OPERATEUR: DYNA_TRAN_MODAL
C
C ----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      CHARACTER*8  MATGEN, NOMRES, TRAN, RIGGEN, K8B
      CHARACTER*8  MODELE, CHMAT, CARAEL, NOBJS
      CHARACTER*14 NUMGEN
      CHARACTER*16 TYPREP, NOMCMD, TYPRES
      CHARACTER*32 JEXNUM
C
C     --- ETAPE DE VERIFICATIONS
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL MDVERI( )
C
C     --- RECUPERATION NOM DE LA COMMANDE ---
C
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
      CALL GETVID('ETAT_INIT','RESU_GENE',1,1,1,TRAN,NDT)
      IF (NDT.NE.0) THEN
C        --- TEST SI REPRISE AVEC NOM DE CONCEPT IDENTIQUE ---
         IF (TRAN.EQ.NOMRES) NOMRES='&&OP0074'
      ENDIF
C
C     --- DETERMINATION DU TYPE DE CALCUL ---
C
      CALL GETVID(' ','MASS_GENE',0,1,1,MATGEN,NM)
      CALL JEVEUO(MATGEN//'           .REFA','L',JREFE)
      NUMGEN = ZK24(JREFE+1)(1:14)
      CALL JEVEUO(NUMGEN//'.NUME.REFN','L',JREFE)
      CALL GETTCO(ZK24(JREFE),TYPREP)

C
C       --- CALCUL PAR SUPERPOSITION SUR BASE MODALE ---
C
      IF (TYPREP.EQ.'MODE_MECA       '.OR.
     &    TYPREP.EQ.'MODE_STAT       '.OR.
     &    TYPREP.EQ.'MODE_GENE       '.OR.
     &    TYPREP.EQ.'BASE_MODALE     ') THEN
          CALL MDTR74(NOMRES,NOMCMD)
      ENDIF
C
C       --- CALCUL PAR SOUS-STRUCTURATION DIRECTE ---
C
      IF (TYPREP.EQ.'MODELE_GENE     ') THEN
         CALL SSDT74(NOMRES,NOMCMD)
      ENDIF
C
C
C     --- CAS DE REPRISE AVEC LE MEME NOM DE CONCEPT ---
C
      IF (NOMRES.EQ.'&&OP0074') CALL RESU74(TRAN,NOMRES)
C
C     --- STOCKAGE ---
C
      IF (NOMRES.NE.'&&OP0074')THEN
        CALL JEVEUO(NOMRES//'           .ORDR','L',JORD)
        CALL JELIRA(NOMRES//'           .ORDR','LONUTI',NBORD,K8B)
        CALL JECREO(NOMRES//'           .MODL' ,'G V K8')
        CALL JEECRA(NOMRES//'           .MODL' ,'LONMAX',NBORD,K8B)
        CALL JEECRA(NOMRES//'           .MODL' ,'LONUTI',NBORD,K8B)
        CALL JECREO(NOMRES//'           .MATE' ,'G V K8')
        CALL JEECRA(NOMRES//'           .MATE' ,'LONMAX',NBORD,K8B)
        CALL JEECRA(NOMRES//'           .MATE' ,'LONUTI',NBORD,K8B)
        CALL JECREO(NOMRES//'           .CARA' ,'G V K8')
        CALL JEECRA(NOMRES//'           .CARA' ,'LONMAX',NBORD,K8B)
        CALL JEECRA(NOMRES//'           .CARA' ,'LONUTI',NBORD,K8B)
        CALL GETVID(' ','RIGI_GENE',0,1,1,RIGGEN,NM)
        CALL JEVEUO(RIGGEN//'           .LIME','L',JMODG)
        IF(ZK8(JMODG).EQ.'        ')THEN
C          ON EST PASSE PAR UN PROJ_MATR_BASE
           CALL JEVEUO(RIGGEN//'           .REFA','L',JMODG)
           CALL JEVEUO(ZK24(JMODG)(1:8)//'           .REFD','L',JRAID)
           IF(ZK24(JRAID)(1:8).EQ.'        ')THEN
             CALL JEVEUO(JEXNUM(ZK24(JMODG)(1:8)//'           .TACH',1),
     &                   'L',JBASM)
             CALL JEVEUO(ZK24(JBASM)(1:8)//'           .MODL','L',JMODL)
             CALL JEVEUO(ZK24(JBASM)(1:8)//'           .MATE','L',JMATE)
             CALL JEVEUO(ZK24(JBASM)(1:8)//'           .CARA','L',JCARA)
             MODELE=ZK8(JMODL)
             CHMAT =ZK8(JMATE)
             CARAEL=ZK8(JCARA)
             GOTO 44
           ENDIF
           CALL JEVEUO(ZK24(JRAID)(1:8)//'           .LIME','L',JMODG)
C          SI LA MATR DE RIGIDITE EST GENERALISEE:
           IF(ZK8(JMODG).NE.'        ')THEN
             CALL JEEXIN(ZK8(JMODG)//'.REFE_RESU',IRET)
             IF(IRET.NE.0)THEN
               CALL JEVEUO(ZK8(JMODG)//'.REFE_RESU','L',JRE)
               MODELE=ZK24(JRE)(1:8)
               CHMAT =ZK24(JRE+3)(1:8)
               CARAEL=ZK24(JRE+4)(1:8)
               GOTO 44
             ENDIF
             CALL JEVEUO(ZK8(JMODG)//'      .MODG.SSME','L',JMACR)
             CALL JEVEUO(ZK8(JMACR)//'.MAEL_INER_REFE','L',JBASM)
             CALL JEVEUO(ZK24(JBASM)(1:8)//'           .REFD','L',JRAID)
           ENDIF
        ELSE
C          ON EST PASSE PAR UN DEFI_MODELE_GENE
           CALL JEVEUO(ZK8(JMODG)//'      .MODG.SSME','L',JMACR)
           CALL JEVEUO(ZK8(JMACR)//'.MAEL_INER_REFE','L',JBASM)
           CALL JEVEUO(ZK24(JBASM)(1:8)//'           .REFD','L',JRAID)
        ENDIF
        CALL DISMOI('F','NOM_MODELE',ZK24(JRAID)(1:8),'MATR_ASSE',
     &     IBID,MODELE,IRET)
        CALL DISMOI('F','CHAM_MATER',ZK24(JRAID)(1:8),'MATR_ASSE',
     &     IBID, CHMAT,IRET)
        CALL DISMOI('F','CARA_ELEM',ZK24(JRAID)(1:8),'MATR_ASSE',
     &     IBID,CARAEL,IRET)
 44     CONTINUE
        CALL JEVEUO(NOMRES//'           .MODL','E',JMODL)
        CALL JEVEUO(NOMRES//'           .MATE','E',JMATE)
        CALL JEVEUO(NOMRES//'           .CARA','E',JCARA)
        DO  43 I=1,NBORD
          ZK8(JMODL+I-1)=MODELE
          ZK8(JMATE+I-1)=CHMAT
          ZK8(JCARA+I-1)=CARAEL
 43     CONTINUE
        
      ENDIF

      CALL JEDEMA()
      END
