      SUBROUTINE PJ3DTR(CORTR3,CORRES,NUTM3D,ELRF3D)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/11/2004   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 CORRES,CORTR3
      CHARACTER*8  ELRF3D(9)
      INTEGER      NUTM3D(9)
C ----------------------------------------------------------------------
C     BUT :
C       TRANSFORMER CORTR3 EN CORRES EN UTILISANT LES FONC. DE FORME
C       DES MAILLES DU MAILLAGE1 (EN 3D ISOPARAMETRIQUE)
C
C  IN/JXIN   CORTR3   K16 : NOM DU CORRESP_2_MAILLA FAIT AVEC LES TETR4
C  IN/JXOUT  CORRES   K16 : NOM DU CORRESP_2_MAILLA FINAL
C  IN        NUTM3D(5) I  : NUMEROS DES 9 TYPES DE MAILLES 3D
C  IN        ELRF3D(5) K8  : NOMS DES 9 TYPES DE MAILLES 3D
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER     CNTETR(4,1),CNPENT(4,3),CNHEXA(4,6),CNPYRA(4,2)
      INTEGER     NBPG(10)
      REAL*8      KSI,ETA,DZETA,X1,X2,X3
      REAL*8      CRREFE(81), X(3), FF(27)
      CHARACTER*8 ELREFA, M1, M2, KB, FAPG(10)
C --- DEB --------------------------------------------------------------

      CALL JEMARQ()

C     0. DECOUPAGE DES ELEMNTS 3D EN TETRA (VOIR PJ3DC0) :
C     ----------------------------------------------------

C     0.1 : TETRAEDRE :
C     -----------------
      CNTETR(1,1)=1
      CNTETR(2,1)=2
      CNTETR(3,1)=3
      CNTETR(4,1)=4

C     0.2 : PENTAEDRE :
C     -----------------
      CNPENT(1,1)=1
      CNPENT(2,1)=3
      CNPENT(3,1)=6
      CNPENT(4,1)=5

      CNPENT(1,2)=1
      CNPENT(2,2)=6
      CNPENT(3,2)=4
      CNPENT(4,2)=5

      CNPENT(1,3)=1
      CNPENT(2,3)=3
      CNPENT(3,3)=5
      CNPENT(4,3)=2

C     0.3 : HEXAEDRE :
C     -----------------
      CNHEXA(1,1)=1
      CNHEXA(2,1)=4
      CNHEXA(3,1)=8
      CNHEXA(4,1)=6

      CNHEXA(1,2)=1
      CNHEXA(2,2)=8
      CNHEXA(3,2)=6
      CNHEXA(4,2)=5

      CNHEXA(1,3)=1
      CNHEXA(2,3)=4
      CNHEXA(3,3)=6
      CNHEXA(4,3)=2

      CNHEXA(1,4)=2
      CNHEXA(2,4)=4
      CNHEXA(3,4)=8
      CNHEXA(4,4)=7

      CNHEXA(1,5)=2
      CNHEXA(2,5)=8
      CNHEXA(3,5)=6
      CNHEXA(4,5)=7

      CNHEXA(1,6)=2
      CNHEXA(2,6)=4
      CNHEXA(3,6)=7
      CNHEXA(4,6)=3

C     0.4 : PYRAMIDE :
C     -----------------
      CNPYRA(1,1)=1
      CNPYRA(2,1)=2
      CNPYRA(3,1)=3
      CNPYRA(4,1)=5

      CNPYRA(1,2)=1
      CNPYRA(2,2)=3
      CNPYRA(3,2)=4
      CNPYRA(4,2)=5

C     1. RECUPERATION DES INFORMATIONS GENERALES :
C     -----------------------------------------------
      CALL JEVEUO(CORTR3//'.PJEF_NO','L',I1CONO)
      CALL JEVEUO(CORTR3//'.PJEF_NB','L',I1CONB)
      CALL JEVEUO(CORTR3//'.PJEF_NU','L',I1CONU)
      CALL JEVEUO(CORTR3//'.PJEF_CF','L',I1COCF)
      CALL JEVEUO(CORTR3//'.PJEF_TR','L',I1COTR)

      M1=ZK8(I1CONO-1+1)
      M2=ZK8(I1CONO-1+2)
      CALL DISMOI('F','NB_NO_MAILLA', M1,'MAILLAGE',NNO1,KB,IE)
      CALL DISMOI('F','NB_NO_MAILLA', M2,'MAILLAGE',NNO2,KB,IE)
      CALL DISMOI('F','NB_MA_MAILLA', M1,'MAILLAGE',NMA1,KB,IE)
      CALL DISMOI('F','NB_MA_MAILLA', M2,'MAILLAGE',NMA2,KB,IE)

      CALL JEVEUO('&&PJXXCO.LIMA1','L',IALIM1)
      CALL JEVEUO('&&PJXXCO.LINO1','L',IALIN1)
      CALL JEVEUO('&&PJXXCO.LINO2','L',IALIN2)
      CALL JEVEUO('&&PJXXCO.TETR4','L',IATR3)

      CALL JEVEUO(M1//'.CONNEX','L',IACNX1)
      CALL JEVEUO(JEXATR(M1//'.CONNEX','LONCUM'),'L',ILCNX1)
      CALL JEVEUO(M1//'.TYPMAIL','L',IATYMA)


C     2. ALLOCATION DE CORRES :
C     -----------------------------------------------
      CALL WKVECT(CORRES//'.PJEF_NO','V V K8',2,I2CONO)
      ZK8(I2CONO-1+1)=M1
      ZK8(I2CONO-1+2)=M2

C     2.1 REMPLISSAGE DE .PJEF_NB ET .PJEF_M1:
C     -----------------------------------------
      CALL WKVECT(CORRES//'.PJEF_NB','V V I',NNO2,I2CONB)
      CALL WKVECT(CORRES//'.PJEF_M1','V V I',NNO2,I2COM1)
      IDECA2=0
      DO 10, INO2=1,NNO2
C       ITR : TETR4 ASSOCIE A INO2
        ITR=ZI(I1COTR-1+INO2)
        IF (ITR.EQ.0) GO TO 10
C       IMA1 : MAILLE DE M1 ASSOCIE AU TETR4 ITR
        IMA1=ZI(IATR3+6*(ITR-1)+5)
        NBNO=ZI(ILCNX1+IMA1)-ZI(ILCNX1-1+IMA1)
        ZI(I2CONB-1+INO2)=NBNO
        ZI(I2COM1-1+INO2)=IMA1
        IDECA2=IDECA2+NBNO
10    CONTINUE

C     2.2 ALLOCATION DE .PJEF_NU ET .PJEF_CF:
C         (ET REMPLISSAGE DE CES 2 OBJETS)
C     ------------------------------------------------------
      CALL WKVECT(CORRES//'.PJEF_NU','V V I',IDECA2,I2CONU)
      CALL WKVECT(CORRES//'.PJEF_CF','V V R',IDECA2,I2COCF)
      IDECA1=0
      IDECA2=0
      DO 20, INO2=1,NNO2
C       ITR : TETR4 ASSOCIE A INO2
        ITR    = ZI(I1COTR-1+INO2)
        IF (ITR.EQ.0) GO TO 20
C       IMA1 : MAILLE DE M1 ASSOCIE AU TETR4 ITR
        IMA1 = ZI(IATR3+6*(ITR-1)+5)
C       ITYPM : TYPE DE LA MAILLE IMA1
        ITYPM = ZI(IATYMA-1+IMA1)
        NUTM   = INDIIS(NUTM3D,ITYPM,1,9)
        ELREFA = ELRF3D(NUTM)
        ITYP   = ZI(IATR3+6*(ITR-1)+6)
        NBNO   = ZI(ILCNX1+IMA1)-ZI(ILCNX1-1+IMA1)

        CALL ELRACA(ELREFA,NDIM,NNO,NNOS,NBFPG,FAPG,NBPG,CRREFE,VOL)

        IF ( NBNO .NE. NNO ) CALL UTMESS('F','PJ3DTR','BUG')

C       2.2.1 DETERMINATION DES COORDONEES DE INO2 DANS L'ELEMENT
C             DE REFERENCE : KSI , ETA ET DZETA
C     -----------------------------------------------------------
        KSI  =0.D0
        ETA  =0.D0
        DZETA=0.D0

        IF (ELREFA.EQ.'TE4' .OR. ELREFA.EQ.'T10') THEN
          DO 771,KK=1,4
            X1 = CRREFE(NDIM*(CNTETR(KK,ITYP)-1)+1)
            X2 = CRREFE(NDIM*(CNTETR(KK,ITYP)-1)+2)
            X3 = CRREFE(NDIM*(CNTETR(KK,ITYP)-1)+3)
            KSI   = KSI   + ZR(I1COCF-1+IDECA1+KK)*X1
            ETA   = ETA   + ZR(I1COCF-1+IDECA1+KK)*X2
            DZETA = DZETA + ZR(I1COCF-1+IDECA1+KK)*X3
771       CONTINUE

        ELSE IF (ELREFA.EQ.'PE6' .OR. ELREFA.EQ.'P15') THEN
          DO 772,KK=1,4
            X1 = CRREFE(NDIM*(CNPENT(KK,ITYP)-1)+1)
            X2 = CRREFE(NDIM*(CNPENT(KK,ITYP)-1)+2)
            X3 = CRREFE(NDIM*(CNPENT(KK,ITYP)-1)+3)
            KSI   = KSI   + ZR(I1COCF-1+IDECA1+KK)*X1
            ETA   = ETA   + ZR(I1COCF-1+IDECA1+KK)*X2
            DZETA = DZETA + ZR(I1COCF-1+IDECA1+KK)*X3
772       CONTINUE

        ELSE IF (ELREFA.EQ.'HE8' .OR. ELREFA.EQ.'H20' .OR.
     +                                ELREFA.EQ.'H27' ) THEN
          DO 773,KK=1,4
            X1 = CRREFE(NDIM*(CNHEXA(KK,ITYP)-1)+1)
            X2 = CRREFE(NDIM*(CNHEXA(KK,ITYP)-1)+2)
            X3 = CRREFE(NDIM*(CNHEXA(KK,ITYP)-1)+3)
            KSI   = KSI   + ZR(I1COCF-1+IDECA1+KK)*X1
            ETA   = ETA   + ZR(I1COCF-1+IDECA1+KK)*X2
            DZETA = DZETA + ZR(I1COCF-1+IDECA1+KK)*X3
773       CONTINUE

        ELSE IF (ELREFA.EQ.'PY5' .OR. ELREFA.EQ.'P13') THEN
          DO 774,KK=1,4
            X1 = CRREFE(NDIM*(CNPYRA(KK,ITYP)-1)+1)
            X2 = CRREFE(NDIM*(CNPYRA(KK,ITYP)-1)+2)
            X3 = CRREFE(NDIM*(CNPYRA(KK,ITYP)-1)+3)
            KSI   = KSI   + ZR(I1COCF-1+IDECA1+KK)*X1
            ETA   = ETA   + ZR(I1COCF-1+IDECA1+KK)*X2
            DZETA = DZETA + ZR(I1COCF-1+IDECA1+KK)*X3
774       CONTINUE

        ELSE
           CALL UTMESS('F','PJ3DTR','ELREFA INCONNU: '//ELREFA)
        END IF

        X(1) = KSI
        X(2) = ETA
        X(3) = DZETA

C       2.2.2 :
C       CALCUL DES F. DE FORME AUX NOEUDS POUR LE POINT KSI,ETA,DZETA:
C       --------------------------------------------------------------
        CALL ELRFVF ( ELREFA, X, 27, FF, NNO )

        DO 22,INO=1,NBNO
          NUNO = ZI(IACNX1+ ZI(ILCNX1-1+IMA1)-2+INO)
          ZI(I2CONU-1+IDECA2+INO) = NUNO
          ZR(I2COCF-1+IDECA2+INO) = FF(INO)
22      CONTINUE

        IDECA1=IDECA1+4
        IDECA2=IDECA2+NBNO

20    CONTINUE

9999  CONTINUE
      CALL JEDEMA()
      END
