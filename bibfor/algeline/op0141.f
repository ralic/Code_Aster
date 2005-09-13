      SUBROUTINE OP0141 ( IER )
      IMPLICIT NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/09/2005   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPERATEUR DE CALCUL DU MAC DE DEUX BASES MODALES
C     ------------------------------------------------------------------
C
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       N1,N2,N3,IBID,NBMOD1,NBMOD2,IADRI1,IADRI2,
     &              LLNEQU,NEQ,NBMODE,IDBAS1,IDBAS2,
     &              I,J,MIN,IDPIJ,NBPARA,INOM,ITYP,IND,IMATRA,IDVEC1,
     &              IDDEEQ,IDVEC2,IFM,NIV
      REAL*8        RBID,PIJ,DDOT,PII,PJJ
      COMPLEX*16    CBID
      CHARACTER*8   TABLE, CONCPT, TYP,BASE1,BASE2,K8B,CODENT,
     &              MATRAS
      CHARACTER*14  NU, NUMDD1, NUMDD2, NUMDDA
      CHARACTER*16  NOMCMD, TYPCON, NOMSYM, K16B, NOM,TYPBA1,TYPBA2,
     &              MATRI1,MATRI2,NOMCOM
      CHARACTER*19  MATR
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C     --- RECUPERATION DU RESULTAT ET DU MODE A TRAITER ---
      CALL JEMARQ()
      
      NOMCOM = 'MAC_MODES'
      
      CALL GETRES ( TABLE, TYPCON, NOMCMD )
C CREATION DE LA TABLE CONTENANT LE MAC
      CALL TBCRSD (TABLE, 'G' )
      CALL TITRE
C     ---RECUPERATION DU NIVEAU D'IMPRESSION---
C
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )

C RECUPERATION DE LA MATRICE ASSEMBLEE
      CALL GETVID ( ' ', 'MATR_ASSE'     , 1,1,1, MATRAS, N1 )
      IF (N1.NE.0) THEN
        CALL DISMOI('F', 'NOM_NUME_DDL', MATRAS, 'MATR_ASSE', IBID,
     +                                            NUMDDA,IER)
        CALL MTDSCR ( MATRAS )
        MATR=MATRAS
        CALL JEVEUO ( MATR//'.&INT', 'E', IMATRA )
        CALL DISMOI('F', 'NOM_NUME_DDL', MATRAS, 'MATR_ASSE', IBID,
     +              NUMDDA,IER)
        CALL DISMOI('F','NB_EQUA',MATRAS,'MATR_ASSE',NEQ,K8B,IER)
      ELSE
        MATR=' '
      ENDIF


C RECUPERATION DES BASES DE MODES
      CALL GETVID ( ' ', 'BASE_1'          , 1,1,1, BASE1, N2 )
      CALL GETVID ( ' ', 'BASE_2'          , 1,1,1, BASE2, N3 )
      
C RECUPERATION DU TYPE ET DU NBRE DE MODES DES BASES
      CALL GETTCO ( BASE1, TYPBA1 )
      CALL RSORAC ( BASE1, 'LONUTI', IBID, RBID, K8B, CBID, RBID,
     +              'ABSOLU', NBMOD1, 1, IBID )
      CALL GETTCO ( BASE2, TYPBA2 )
      CALL RSORAC ( BASE2, 'LONUTI', IBID, RBID, K8B, CBID, RBID,
     +              'ABSOLU', NBMOD2, 1, IBID )

C RECUPERATION DE LA NUMEROTATION DES BASES
      CALL JEVEUO(BASE1//'           .REFD','L',IADRI1)
      IF ((TYPBA1.EQ.'MODE_MECA').OR.(TYPBA1.EQ.'MODE_GENE')) THEN
         MATRI1 = ZK24(IADRI1)
      ELSE
         MATRI1 = ZK24(IADRI1+2)
      ENDIF
      IF (MATRI1.NE.' ') THEN
        CALL DISMOI('F','NOM_NUME_DDL',MATRI1,'MATR_ASSE',IBID,
     +                NUMDD1,IER)
      ELSE
        NUMDD1 = ZK24(IADRI1+1)(1:14)
      ENDIF
      CALL JEVEUO(BASE2//'           .REFD','L',IADRI2)
      IF ((TYPBA2.EQ.'MODE_MECA').OR.(TYPBA2.EQ.'MODE_GENE')) THEN
         MATRI2 = ZK24(IADRI2)
      ELSE
         MATRI2 = ZK24(IADRI2+2)
      ENDIF
      IF (MATRI2.NE.' ') THEN
        CALL DISMOI('F','NOM_NUME_DDL',MATRI2,'MATR_ASSE',IBID,
     +                NUMDD2,IER)
      ELSE
        NUMDD2 = ZK24(IADRI2+1)(1:14)
      ENDIF
      IF (NUMDD1.NE.NUMDD2) THEN
         CALL UTMESS('I',NOMCOM,'BASE MODALE 1 ET 2 AVEC'//
     +               ' NUMEROTATIONS INCOMPATIBLES')    
      ENDIF
      NU = NUMDD1(1:14)
      IF (MATR.NE.' ') THEN
        IF (NUMDD1.NE.NUMDDA) THEN
          CALL UTMESS('I',NOMCOM,'BASE MODALE 1 ET 2 AVEC'//
     +               ' NUMEROTATIONS INCOMPATIBLES')    
        ENDIF
        NU = NUMDDA(1:14)
      ENDIF
      CALL JEVEUO ( NU//'.NUME.DEEQ', 'L', IDDEEQ )
      
      CALL JEVEUO(NUMDD1//'.NUME.NEQU','L',LLNEQU)
      NEQ = ZI(LLNEQU)
      CALL WKVECT ( '&&OP0141.BASE1','V V R',NBMOD1*NEQ,
     &             IDBAS1)
      CALL WKVECT ( '&&OP0141.BASE2','V V R',NBMOD2*NEQ,
     &             IDBAS2)
      CALL WKVECT ( '&&OP0141.TEMP1','V V R',NEQ,
     &             IDVEC1)
      CALL WKVECT ( '&&OP0141.TEMP2','V V R',NEQ,
     &             IDVEC2)
      IF ((TYPBA1.EQ.'MODE_MECA').OR.(TYPBA1.EQ.'MODE_GENE')) THEN
         CALL COPMOD(BASE1,'DEPL',NEQ,NU,NBMOD1,ZR(IDBAS1))
      ELSE
         CALL COPMO2(BASE1,NEQ,NU,NBMOD1,ZR(IDBAS1))
      ENDIF
      IF ((TYPBA2.EQ.'MODE_MECA').OR.(TYPBA2.EQ.'MODE_GENE')) THEN
         CALL COPMOD(BASE2,'DEPL',NEQ,NU,NBMOD2,ZR(IDBAS2))
      ELSE
         CALL COPMO2(BASE2,NEQ,NU,NBMOD2,ZR(IDBAS2))
      ENDIF
      
C INITIALISATION DE LA TABLE DES MACS      
      NBPARA=3
      CALL WKVECT('&&OP0141.TYPE_PARA','V V K8 ',NBPARA,ITYP)
      CALL WKVECT('&&OP0141.NOM_PARA'     ,'V V K16',NBPARA,INOM)
      DO 20 I=1 , 2
        ZK8(ITYP+I-1)='I'
 20   CONTINUE
      ZK8(ITYP+2)='R'
      ZK16(INOM)='NUME_MODE_1'
      ZK16(INOM+1)='NUME_MODE_2'
      ZK16(INOM+2)='MAC'
      CALL TBAJPA (TABLE, 3, ZK16(INOM), ZK8(ITYP) )
      
      CALL WKVECT ( '&&OP0141.IJ','V V I',2,IND)

C BOUCLE DE CALCUL DES MACS
      DO 30 I = 1 , NBMOD1
        IF (MATR.NE.' ') THEN
          CALL MRMULT ( 'ZERO', IMATRA, ZR(IDBAS1+(I-1)*NEQ),
     &                    'R',ZR(IDVEC1),1)
          CALL ZERLAG ( ZR(IDVEC1), NEQ, ZI(IDDEEQ) )
        ELSE
          CALL DCOPY(NEQ,ZR(IDBAS1+(I-1)*NEQ),1,ZR(IDVEC1),1)
          CALL ZERLAG ( ZR(IDVEC1), NEQ, ZI(IDDEEQ) )
        ENDIF
            
        DO 40 J = 1 , NBMOD2
        IF (MATR.NE.' ') THEN
          CALL MRMULT ( 'ZERO', IMATRA, ZR(IDBAS2+(J-1)*NEQ),
     &                    'R',ZR(IDVEC2),1)
          CALL ZERLAG ( ZR(IDVEC2), NEQ, ZI(IDDEEQ) )
        ELSE
          CALL DCOPY(NEQ,ZR(IDBAS2+(J-1)*NEQ),1,ZR(IDVEC2),1)
        ENDIF
           PIJ = DDOT( NEQ, ZR(IDVEC1),1,
     &           ZR(IDBAS2+(J-1)*NEQ),1)
           PII = DDOT( NEQ, ZR(IDVEC1),1,
     &           ZR(IDBAS1+(J-1)*NEQ),1)
           PJJ = DDOT( NEQ, ZR(IDVEC2),1,
     &           ZR(IDBAS1+(J-1)*NEQ),1)
           PIJ = ABS((PIJ**2) / (PII * PJJ))
           ZI(IND)=I
           ZI(IND+1)=J
           CALL TBAJLI (TABLE, 3, ZK16(INOM), 
     &          ZI(IND), PIJ, CBID, K8B, 0 )
 40     CONTINUE
 30   CONTINUE
      IF ( NIV .GE. 2 ) THEN
      CALL TBIMPR(TABLE,' ','TABLEAU',IFM,3,ZK16(INOM),0,
     &   ' ','1PE12.5','RI')
      ENDIF

      CALL JEDEMA()
      END
