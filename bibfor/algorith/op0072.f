      SUBROUTINE OP0072()
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
C
C  CALCUL PROJECTION VECTEUR SUR BASE DE RITZ
C
C-----------------------------------------------------------------------
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      ---- DEBUT DES COMMUNS JEVEUX ----------------------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16         ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                                 ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C      ---- FIN DES COMMUNS JEVEUX ------------------------------------
C
      INTEGER       JSMDE
      CHARACTER*1  TYPVEC
      CHARACTER*8  NOMRES,BASEMO,VECTAS,NOMTYP,K8BID
      CHARACTER*14 NU,NUMDD1,NUMDD2,NUMGEN
      CHARACTER*16 TYPRES,NOMCOM,TYPBAS,MATRI2
      CHARACTER*24 MATRIC,KBID,DEEQ,TYPEBA
      COMPLEX*16   CBID,ZDOTC,DCMPLX
      REAL*8       DDOT, ZERO
C
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      ZERO = 0.D0
C
C --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
C
      CALL GETRES(NOMRES,TYPRES,NOMCOM)
      CALL GETVID(' ','NUME_DDL_GENE',0,1,1,NUMGEN,N0)
      CALL GETVID(' ','VECT_ASSE',0,1,1,VECTAS,N1)
      CALL GETVID(' ','VECT_ASSE_GENE',0,1,1,VECTAS,N3)
      CALL GETVID(' ','BASE',0,1,1,BASEMO,N4)
      CALL GETVTX(' ','TYPE_VECT',0,1,1,NOMTYP,N2)
      CALL GETTCO(BASEMO,TYPBAS)
C
C --- RECUPERATION DU NB DE MODES
C
      CALL RSORAC(BASEMO,'LONUTI',IBID,BID,K8BID,CBID,EBID,'ABSOLU',
     &            NBMODE,1,NBID)
C
C
      CALL JEVEUO(NUMGEN//'.SMOS.SMDE','L',JSMDE)

C --- VERIFICATION DE LA CONFORMITE DES NUMEROTATIONS
C     DES MODES ET DU VECTEUR ASSEMBLE
C
      CALL JEVEUO(VECTAS//'           .VALE','L',IADVEC)
      CALL JEVEUO(VECTAS//'           .REFE','L',IADREF)
      CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
      CALL JELIRA(VECTAS//'           .VALE','TYPE',IBID,TYPVEC)
      TYPEBA = ZK24(IADRIF+6)

      IF (TYPBAS(1:9).EQ.'MODE_MECA') THEN
        NUMDD1 = ZK24(IADREF+1)
        NU = NUMDD1
        MATRIC = ZK24(IADRIF)
        IF (TYPEBA(1:1).EQ.' ') THEN
          CALL EXISD('MATR_ASSE',MATRIC,IRET)
          IF (IRET.NE.0) THEN
            CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',IBID,
     &              NUMDD2,IRET)
          ELSE
            NUMDD2 = NUMDD1
          ENDIF
        ELSE
          NUMDD2 = ZK24(IADRIF+3)
        ENDIF

      ELSEIF (TYPBAS(1:9).EQ.'MODE_GENE') THEN
        NUMDD1=ZK24(IADREF+1)
        MATRIC = ZK24(IADRIF)
        MATRI2 = MATRIC(1:16)
        CALL JEVEUO(MATRI2//'   .REFA','L',JREFA)
        NUMDD2=ZK24(JREFA-1+2)
        NU = NUMDD1
      ENDIF
      IF (NUMDD1.NE.NUMDD2) THEN
        CALL U2MESS('I','ALGORITH9_41')
      ENDIF
C
C --- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
C

      IF ((TYPBAS(1:9).EQ.'MODE_MECA')) THEN
        CALL DISMOI('F','NB_EQUA',NU,'NUME_DDL',NEQ,KBID,IRET)
      ELSEIF (TYPBAS(1:9).EQ.'MODE_GENE') THEN
        CALL JEVEUO(NUMDD1//'.NUME.NEQU','L',LLNEQU)
        NEQ = ZI(LLNEQU)
      ENDIF
C
      DEEQ = NU//'.NUME.DEEQ'
      CALL JEVEUO(DEEQ,'L',IDDEEQ)
C
C --- CREATION DE L OBJET VECT_GENE RESULTAT
C
      IF (TYPVEC.EQ.'R') THEN
        CALL WKVECT(NOMRES//'           .VALE','G V R',NBMODE,IAVALE)
      ELSE
        CALL WKVECT(NOMRES//'           .VALE','G V C',NBMODE,IAVALE)
        CALL WKVECT('&&OP0072.VECTASC1','V V C',NEQ,IDVEC3)
      ENDIF
      CALL WKVECT(NOMRES//'           .REFE','G V K24',2,IAREFE)
      CALL WKVECT(NOMRES//'           .DESC','G V I',3,IADESC)
      CALL JEECRA(NOMRES//'           .DESC','DOCU',0,'VGEN')
C
C --- REMPLISSAGE DU .REFE ET .VALE
C
      ZK24(IAREFE) = BASEMO
      ZK24(IAREFE+1) = NUMGEN//'.NUME     '
      ZI(IADESC) = 1
      ZI(IADESC+1) = NBMODE

C   LE STOCKAGE EST-IL DIAGONAL ?
      IF (ZI(JSMDE-1+4).EQ.ZI(JSMDE-1+1)) THEN
        ZI(IADESC+2) = 1
      ELSE
        ZI(IADESC+2) = 2
      ENDIF
      CALL WKVECT('&&OP0072.BASEMO','V V R',NBMODE*NEQ,IDBASE)
C --- CONVERSION DE BASEMO A LA NUMEROTATION NU
      IF (TYPBAS.EQ.'MODE_GENE') THEN
        CALL COPMOD(BASEMO,'DEPL',NEQ,NU,NBMODE,ZR(IDBASE))
      ELSE
        CALL COPMO2(BASEMO,NEQ,NU,NBMODE,ZR(IDBASE))
      ENDIF
      IF (NOMTYP(1:4).EQ.'FORC') THEN
C
C --- PROJECTION D UN VECTEUR DE TYPE FORCE
C
        CALL WKVECT('&&OP0072.VECTASSE','V V R',NEQ,IDVECT)
        DO 10 I = 1,NBMODE
C
C --------- RECOPIE DU IEME MODE
C
          CALL DCOPY(NEQ,ZR(IDBASE+(I-1)*NEQ),1,ZR(IDVECT),1)
C
C
C ------- MISE A ZERO DES DDLS DE LAGRANGE
C
          CALL ZERLAG(ZR(IDVECT),NEQ,ZI(IDDEEQ))
C
C ------- PRODUIT SCALAIRE VECTASS * MODE
C
          IF (TYPVEC.EQ.'R') THEN
            ZR(IAVALE+I-1) = DDOT(NEQ,ZR(IDVECT),1,ZR(IADVEC),1)
          ELSE
            DO 666 J=1,NEQ
              ZC(IDVEC3+J-1)=DCMPLX(ZR(IDVECT+J-1),ZERO)
 666        CONTINUE
            ZC(IAVALE+I-1) = ZDOTC(NEQ,ZC(IDVEC3),1,ZC(IADVEC),1)
          ENDIF
10      CONTINUE
      ELSE
C
C --- PROJECTION D UN VECTEUR DE TYPE DEPL OU VITE
C
        CALL WKVECT('&&OP0072.VECTASS1','V V R',NEQ,IDVEC1)
        CALL WKVECT('&&OP0072.VECTASS2','V V R',NEQ,IDVEC2)
        IF (TYPVEC.EQ.'C') THEN
          CALL WKVECT('&&OP0072.VECTASC2','V V R',NEQ,IDVEC4)
        ENDIF
        CALL WKVECT('&&OP0072.MATRNORM','V V R',NBMODE*NBMODE,IAMATR)
C
C ----- CALCUL DE TMODE*MODE
C
        DO 20 I = 1,NBMODE
C
C ----- RECOPIE DU IEME MODE
C
          CALL DCOPY(NEQ,ZR(IDBASE+(I-1)*NEQ),1,ZR(IDVEC1),1)
C
C ------- MISE A ZERO DES DDLS DE LAGRANGE
C
          CALL ZERLAG(ZR(IDVEC1),NEQ,ZI(IDDEEQ))
C
C-------- PRODUIT SCALAIRE MODE(I)*MODE(J)
C
          DO 20 J = I,NBMODE
C
C ------- RECOPIE DU JEME MODE
C
            CALL DCOPY(NEQ,ZR(IDBASE+(J-1)*NEQ),1,ZR(IDVEC2),1)
C --------- MISE A ZERO DES DDLS DE LAGRANGE
C
            CALL ZERLAG(ZR(IDVEC2),NEQ,ZI(IDDEEQ))
C
C --------- PRODUIT SCALAIRE MODE(I)*MODE(J)
C
            PIJ = DDOT(NEQ,ZR(IDVEC1),1,ZR(IDVEC2),1)
            ZR(IAMATR+I+ (J-1)*NBMODE-1) = PIJ
            ZR(IAMATR+J+ (I-1)*NBMODE-1) = PIJ
20      CONTINUE
C
C ----- CALCUL DE LA PROJECTION
C
        DO 30 I = 1,NBMODE
C
C ------- RECOPIE DU IEME MODE
C
          CALL DCOPY(NEQ,ZR(IDBASE+(I-1)*NEQ),1,ZR(IDVEC1),1)
C
C ------- MISE A ZERO DES DDLS DE LAGRANGE
C
          CALL ZERLAG(ZR(IDVEC1),NEQ,ZI(IDDEEQ))
C
C ------- PRODUIT SCALAIRE VECTASS * MODE
C
          IF (TYPVEC.EQ.'R') THEN
            ZR(IDVEC2+I-1) = DDOT(NEQ,ZR(IDVEC1),1,ZR(IADVEC),1)
          ELSE
            DO 667 J=1,NEQ
              ZC(IDVEC3+J-1)=DCMPLX(ZR(IDVEC1+J-1),ZERO)
 667        CONTINUE
            ZC(IDVEC4+I-1) = ZDOTC(NEQ,ZC(IDVEC3),1,ZC(IADVEC),1)
          ENDIF
30      CONTINUE
C
C ----- FACTORISATION ET RESOLUTION SYSTEME
C
        CALL TRLDS(ZR(IAMATR),NBMODE,NBMODE,ICOD)
        IF (ICOD.NE.0) THEN
          CALL U2MESS('F','ALGORITH9_42')
        ENDIF
        IF (TYPVEC.EQ.'R') THEN
          CALL RRLDS(ZR(IAMATR),NBMODE,NBMODE,ZR(IDVEC2),1)
          CALL DCOPY(NBMODE,ZR(IDVEC2),1,ZR(IAVALE),1)
        ELSE
          CALL RCLDS(ZR(IAMATR),NBMODE,NBMODE,ZC(IDVEC4),1)
          CALL ZCOPY(NBMODE,ZC(IDVEC3),1,ZC(IAVALE),1)
        ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
