      SUBROUTINE OP0141()
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
     &              NEQ,IDBAS1,IDBAS2,IDBAS3,IDVEC3,
     &              I,J,NBPARA,INOM,ITYP,IND,IMATRA,IDVEC1,
     &              IDDEEQ,IDVEC2,IFM,NIV,LLNEQ1,NEQ1,LLNEQ2,NEQ2,IRET
      INTEGER       IDDL,IER
      REAL*8        RBID,PIJ,DDOT,PII,PJJ
      COMPLEX*16    CBID,DCMPLX,ZTEMP,DCONJG
      CHARACTER*1   TYPSCA
      CHARACTER*8   TABLE,BASE1,BASE2,K8B,
     &              MATRAS,REP
      CHARACTER*14  NU, NUMDD1, NUMDD2, NUMDDA
      CHARACTER*16  NOMCMD, TYPCON,TYPBA1,TYPBA2,
     &              MATRI1,MATRI2,DEPL
      CHARACTER*19  MATR
      CHARACTER*24  CHAMOL
      LOGICAL       C1,C2,ZCMPLX,IERI
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
      DATA DEPL /'DEPL            '/

C     --- RECUPERATION DU RESULTAT ET DU MODE A TRAITER ---
      CALL JEMARQ()

      CALL GETRES ( TABLE, TYPCON, NOMCMD )
C CREATION DE LA TABLE CONTENANT LE MAC
      CALL TBCRSD (TABLE, 'G' )
      CALL TITRE
C     ---RECUPERATION DU NIVEAU D'IMPRESSION---
C
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )

C RECUPERATION DE LA MATRICE ASSEMBLEE SI ELLE EXISTE
      CALL GETVID ( ' ', 'MATR_ASSE'     , 1,1,1, MATRAS, N1 )
      IF (N1.NE.0) THEN
C COOL ELLE EXISTE
        CALL MTDSCR ( MATRAS )
        MATR=MATRAS
        CALL JEVEUO ( MATR//'.&INT', 'E', IMATRA )
        CALL DISMOI('F', 'NOM_NUME_DDL', MATRAS, 'MATR_ASSE', IBID,
     &              NUMDDA,IER)
        CALL DISMOI('F','NB_EQUA',MATRAS,'MATR_ASSE',NEQ,K8B,IER)
      ELSE
C PAS COOL ELLE EXISTE PAS
        MATR=' '
      ENDIF

      IERI = .FALSE.
      CALL GETVTX ( ' ','IERI',1,1,1,REP,N1)
      IF ( N1.EQ.1 ) THEN
         IF (REP .EQ. 'OUI') IERI = .TRUE.
      ENDIF

C RECUPERATION DES BASES DE MODES
      CALL GETVID ( ' ', 'BASE_1'          , 1,1,1, BASE1, N2 )
      CALL GETVID ( ' ', 'BASE_2'          , 1,1,1, BASE2, N3 )

      C1 = .FALSE.
      C2 = .FALSE.

      CALL DCAPNO(BASE1,DEPL,1,CHAMOL)
      CALL JELIRA(CHAMOL,'TYPE',IBID,TYPSCA)
      IF (TYPSCA.EQ.'C') C1 = .TRUE.

      CALL DCAPNO(BASE2,DEPL,1,CHAMOL)
      CALL JELIRA(CHAMOL,'TYPE',IBID,TYPSCA)
      IF (TYPSCA.EQ.'C') C2 = .TRUE.

      IF (C1 .AND. C2) THEN
        ZCMPLX = .TRUE.
      ELSE
        ZCMPLX = .FALSE.
        IF (C1 .OR. C2) CALL U2MESS('F','ALGELINE5_71')
      ENDIF

C RECUPERATION DU TYPE ET DU NBRE DE MODES DES BASES
      CALL GETTCO ( BASE1, TYPBA1 )
      CALL RSORAC ( BASE1, 'LONUTI', IBID, RBID, K8B, CBID, RBID,
     &              'ABSOLU', NBMOD1, 1, IBID )
      CALL GETTCO ( BASE2, TYPBA2 )
      CALL RSORAC ( BASE2, 'LONUTI', IBID, RBID, K8B, CBID, RBID,
     &              'ABSOLU', NBMOD2, 1, IBID )

C RECUPERATION DE LA NUMEROTATION DES BASES
      CALL JEVEUO(BASE1//'           .REFD','L',IADRI1)
      IF ((TYPBA1(1:9).EQ.'MODE_MECA')
     &     .OR.(TYPBA1(1:9).EQ.'MODE_GENE')) THEN
C On passe par les matrices du REFD
        MATRI1 = ZK24(IADRI1)
        CALL EXISD('MATR_ASSE',MATRI1,IRET)
        IF (IRET.NE.0) THEN
          CALL DISMOI('F','NOM_NUME_DDL',MATRI1,'MATR_ASSE',IBID,
     &                NUMDD1,IER)
        ELSE
          NUMDD1 = ZK24(IADRI1+3)(1:14)
        ENDIF
      ELSE
C On passe par la numerotation du REFD
        NUMDD1 = ZK24(IADRI1+3)(1:14)
      ENDIF
      CALL JEVEUO(NUMDD1//'.NUME.NEQU','L',LLNEQ1)
      NEQ1 = ZI(LLNEQ1)

      CALL JEVEUO(BASE2//'           .REFD','L',IADRI2)
      IF ((TYPBA2(1:9).EQ.'MODE_MECA')
     &     .OR.(TYPBA2(1:9).EQ.'MODE_GENE')) THEN
        MATRI2 = ZK24(IADRI2)
        CALL EXISD('MATR_ASSE',MATRI2,IRET)
        IF (IRET.NE.0) THEN
          CALL DISMOI('F','NOM_NUME_DDL',MATRI2,'MATR_ASSE',IBID,
     &                NUMDD2,IER)
        ELSE
          NUMDD2 = ZK24(IADRI2+3)(1:14)
        ENDIF
      ELSE
        NUMDD2 = ZK24(IADRI2+3)(1:14)
      ENDIF
      CALL JEVEUO(NUMDD2//'.NUME.NEQU','L',LLNEQ2)
      NEQ2 = ZI(LLNEQ2)

      IF (NEQ1.NE.NEQ2) THEN
         CALL U2MESS('F','ALGELINE2_79')
      ENDIF

      IF (NUMDD1.NE.NUMDD2) THEN
        IF (NEQ1.NE.NEQ2) THEN
         CALL U2MESS('F','ALGELINE2_79')
        ELSE
         CALL U2MESS('I','ALGELINE2_80')
        ENDIF
      ENDIF
C PAR DEFAUT ON PREND EN REFERENCE LA NUMEROTATION DE LA BASE 1
      IF (MATR.NE.' ') THEN
        IF (NUMDD1.NE.NUMDDA) THEN
          CALL U2MESS('I','ALGELINE2_81')
        ENDIF
        NU = NUMDDA(1:14)
        CALL JEVEUO ( NU//'.NUME.DEEQ', 'L', IDDEEQ )
      ELSE
        NU = NUMDD1(1:14)
        NEQ=NEQ1
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
      IF (IERI) THEN
        ZK16(INOM+2)='IERI'
      ELSE
        ZK16(INOM+2)='MAC'
      ENDIF
      CALL TBAJPA (TABLE, 3, ZK16(INOM), ZK8(ITYP) )

      CALL WKVECT ( '&&OP0141.IJ','V V I',2,IND)

      IF (ZCMPLX) THEN

        CALL WKVECT ( '&&OP0141.BASE1','V V C',NBMOD1*NEQ,
     &             IDBAS1)
        CALL WKVECT ( '&&OP0141.BASE2','V V C',NBMOD2*NEQ,
     &             IDBAS2)
        CALL WKVECT ( '&&OP0141.BASE3','V V C',NEQ,
     &             IDBAS3)
        CALL WKVECT ( '&&OP0141.TEMP1','V V C',NEQ,
     &             IDVEC1)
        CALL WKVECT ( '&&OP0141.TEMP2','V V C',NEQ,
     &             IDVEC2)
        CALL WKVECT ( '&&OP0141.TEMP3','V V C',NEQ,
     &             IDVEC3)

        CALL COPMO3(BASE1,NEQ,NU,NBMOD1,ZC(IDBAS1))
        CALL COPMO3(BASE2,NEQ,NU,NBMOD2,ZC(IDBAS2))

C BOUCLE DE CALCUL DES MACS
        DO 30 I = 1 , NBMOD1
          PII=0.D0
          IF (MATR.NE.' ') THEN
            CALL MCMULT ( 'ZERO', IMATRA, ZC(IDBAS1+(I-1)*NEQ),
     &                    'R',ZC(IDVEC1),1)

            DO 10 IDDL = 1,NEQ
              IF(ZI(IDDEEQ-1+2*IDDL).LE.0)
     &          ZC(IDVEC1-1+IDDL) = DCMPLX(0.D0,0.D0)
 10         CONTINUE

          ELSE
            CALL ZCOPY(NEQ,ZC(IDBAS1+(I-1)*NEQ),1,ZC(IDVEC1),1)
          ENDIF

C PB AVEC ZDOTC DE BLAS POUR CERTAIN COMPILO -> CALCUL DIRECT
          ZTEMP = DCMPLX(0.0D0,0.0D0)
          DO 112 IDDL = 1,NEQ
            ZTEMP = ZTEMP +
     &      ZC(IDBAS1+(I-1)*NEQ-1+IDDL)*DCONJG(ZC(IDVEC1-1+IDDL))
 112      CONTINUE
          PII = ABS(ZTEMP)

          ZI(IND)=I

          DO 40 J = 1 , NBMOD2
            PIJ=0.D0
            PJJ=0.D0
            IF (MATR.NE.' ') THEN
              CALL MCMULT ( 'ZERO', IMATRA, ZC(IDBAS2+(J-1)*NEQ),
     &                    'C',ZC(IDVEC2),1)

              DO 50 IDDL = 1,NEQ
                IF(ZI(IDDEEQ-1+2*IDDL).LE.0)
     &            ZC(IDVEC2-1+IDDL) = DCMPLX(0.D0,0.D0)
 50           CONTINUE

            ELSE
              CALL ZCOPY(NEQ,ZC(IDBAS2+(J-1)*NEQ),1,ZC(IDVEC2),1)
            ENDIF

            ZTEMP = DCMPLX(0.0D0,0.0D0)
            DO 114 IDDL = 1,NEQ
              ZTEMP = ZTEMP +
     &        ZC(IDBAS2+(J-1)*NEQ-1+IDDL)*DCONJG(ZC(IDVEC2-1+IDDL))
 114        CONTINUE
            PJJ = ABS(ZTEMP)

            IF (IERI) THEN
              DO 115 IDDL = 1,NEQ
                ZC(IDBAS3-1+IDDL)=ZC(IDBAS1+(I-1)*NEQ-1+IDDL)
     &                          -ZC(IDBAS2+(J-1)*NEQ-1+IDDL)
 115          CONTINUE
              CALL MCMULT ( 'ZERO', IMATRA, ZC(IDBAS3),
     &                    'C',ZC(IDVEC3),1)
              DO 116 IDDL = 1,NEQ
                IF(ZI(IDDEEQ-1+2*IDDL).LE.0)
     &             ZC(IDVEC3-1+IDDL) = DCMPLX(0.D0,0.D0)
 116          CONTINUE

              ZTEMP = DCMPLX(0.0D0,0.0D0)
              DO 117 IDDL = 1,NEQ
                ZTEMP = ZTEMP +
     &            ZC(IDBAS3-1+IDDL)*DCONJG(ZC(IDVEC3-1+IDDL))
 117          CONTINUE
              PIJ = ABS(ZTEMP)

              PIJ = (PIJ**2) / (PII**2 + PJJ**2)
            ELSE
              ZTEMP = DCMPLX(0.0D0,0.0D0)
              DO 113 IDDL = 1,NEQ
                ZTEMP = ZTEMP +
     &          ZC(IDBAS1+(I-1)*NEQ-1+IDDL)*DCONJG(ZC(IDVEC2-1+IDDL))
 113          CONTINUE
              PIJ = ABS(ZTEMP)
              PIJ = (PIJ**2) / (PII * PJJ)
            ENDIF

            ZI(IND+1)=J
            CALL TBAJLI (TABLE, 3, ZK16(INOM),
     &          ZI(IND), PIJ, CBID, K8B, 0 )
 40       CONTINUE
 30     CONTINUE

      ELSE

        CALL WKVECT ( '&&OP0141.BASE1','V V R',NBMOD1*NEQ,
     &             IDBAS1)
        CALL WKVECT ( '&&OP0141.BASE2','V V R',NBMOD2*NEQ,
     &             IDBAS2)
        CALL WKVECT ( '&&OP0141.BASE3','V V R',NEQ,
     &             IDBAS3)
        CALL WKVECT ( '&&OP0141.TEMP1','V V R',NEQ,
     &             IDVEC1)
        CALL WKVECT ( '&&OP0141.TEMP2','V V R',NEQ,
     &             IDVEC2)
        CALL WKVECT ( '&&OP0141.TEMP3','V V R',NEQ,
     &             IDVEC3)

        CALL COPMO2(BASE1,NEQ,NU,NBMOD1,ZR(IDBAS1))
        CALL COPMO2(BASE2,NEQ,NU,NBMOD2,ZR(IDBAS2))

C BOUCLE DE CALCUL DES MACS
        DO 130 I = 1 , NBMOD1
          PII=0.D0
          IF (MATR.NE.' ') THEN
            CALL MRMULT ( 'ZERO', IMATRA, ZR(IDBAS1+(I-1)*NEQ),
     &                    'R',ZR(IDVEC1),1)
            CALL ZERLAG ( ZR(IDVEC1), NEQ, ZI(IDDEEQ) )
          ELSE
            CALL DCOPY(NEQ,ZR(IDBAS1+(I-1)*NEQ),1,ZR(IDVEC1),1)
          ENDIF

          PII = ABS(DDOT( NEQ, ZR(IDBAS1+(I-1)*NEQ),1,
     &           ZR(IDVEC1),1))

          ZI(IND)=I

          DO 140 J = 1 , NBMOD2
            PIJ=0.D0
            PJJ=0.D0
            IF (MATR.NE.' ') THEN
              CALL MRMULT ( 'ZERO', IMATRA, ZR(IDBAS2+(J-1)*NEQ),
     &                    'R',ZR(IDVEC2),1)
              CALL ZERLAG ( ZR(IDVEC2), NEQ, ZI(IDDEEQ) )
            ELSE
              CALL DCOPY(NEQ,ZR(IDBAS2+(J-1)*NEQ),1,ZR(IDVEC2),1)
            ENDIF

            PJJ = ABS(DDOT( NEQ, ZR(IDBAS2+(J-1)*NEQ),1,
     &           ZR(IDVEC2),1))

            IF (IERI) THEN
              CALL VDIFF(NEQ,ZR(IDBAS1+(I-1)*NEQ),
     &                     ZR(IDBAS2+(J-1)*NEQ),ZR(IDBAS3))
              CALL MRMULT ( 'ZERO', IMATRA, ZR(IDBAS3),
     &                    'R',ZR(IDVEC3),1)
              CALL ZERLAG ( ZR(IDVEC3), NEQ, ZI(IDDEEQ) )

              PIJ = ABS(DDOT( NEQ,ZR(IDBAS3) ,1,
     &           ZR(IDVEC3),1))

              PIJ = (PIJ**2) / (PII**2 + PJJ**2)
            ELSE
              PIJ = ABS(DDOT( NEQ,ZR(IDBAS1+(I-1)*NEQ) ,1,
     &           ZR(IDVEC2),1))

              PIJ = (PIJ**2) / (PII * PJJ)
            ENDIF

            ZI(IND+1)=J
            CALL TBAJLI (TABLE, 3, ZK16(INOM),
     &          ZI(IND), PIJ, CBID, K8B, 0 )
140       CONTINUE
130     CONTINUE

      ENDIF
C  FIN TEST SUR TYPE DE VECTEURS (C/R)

      IF ( NIV .GE. 2 ) THEN
        CALL TBIMPR(TABLE,' ','TABLEAU',IFM,3,ZK16(INOM),0,
     &   ' ','1PE12.5','RI')
      ENDIF

      CALL JEDEMA()
      END
