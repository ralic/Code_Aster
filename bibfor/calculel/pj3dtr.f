      SUBROUTINE PJ3DTR(CORTR3,CORRES,NUTM3D,NOTM3D)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/05/99   AUTEUR VABHHTS J.PELLET 
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
      CHARACTER*8 NOTM3D(9),NOTM
      INTEGER NUTM3D(9)
C ----------------------------------------------------------------------
C     BUT :
C       TRANSFORMER CORTR3 EN CORRES EN UTILISANT LES FONC. DE FORME
C       DES MAILLES DU MAILLAGE1 (EN 3D ISOPARAMETRIQUE)
C
C  IN/JXIN   CORTR3   K16 : NOM DU CORRESP_2_MAILLA FAIT AVEC LES TETR4
C  IN/JXOUT  CORRES   K16 : NOM DU CORRESP_2_MAILLA FINAL
C  IN        NUTM3D(5) I  : NUMEROS DES 9 TYPES DE MAILLES 3D
C  IN        NOTM3D(5) K8  : NOMS DES 9 TYPES DE MAILLES 3D
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
      CHARACTER*8 M1,M2,KB
      REAL*8   KSI,ETA,DZETA,FF(27),DFF(3,27),XIN(27),YIN(27),ZIN(27)
      INTEGER CNTETR(4,1),CNPENT(4,3),CNHEXA(4,6),CNPYRA(4,2)
      REAL*8  CRTETR(10,3),CRPENT(15,3),CRHEXA(27,3),CRPYRA(13,3)
C --- DEB --------------------------------------------------------------
      CALL JEMARQ()


C     0. REMPLISSAGE DES TABLEAUX CONTENANT LES COORDONNEES DES NOEUDS
C        SOMMETS DES ELEMENTS DE REFERENCE : TETRA,PENTA,HEXA,PYRA :
C     -----------------------------------------------------------------

C     0.1 : TETRAEDRE :
C     -----------------
      CRTETR(1,1)=0.D0
      CRTETR(1,2)=1.D0
      CRTETR(1,3)=0.D0

      CRTETR(2,1)=0.D0
      CRTETR(2,2)=0.D0
      CRTETR(2,3)=1.D0

      CRTETR(3,1)=0.D0
      CRTETR(3,2)=0.D0
      CRTETR(3,3)=0.D0

      CRTETR(4,1)=1.D0
      CRTETR(4,2)=0.D0
      CRTETR(4,3)=0.D0

      CRTETR(5,1)=0.D0
      CRTETR(5,2)=0.5D0
      CRTETR(5,3)=0.5D0

      CRTETR(6,1)=0.D0
      CRTETR(6,2)=0.D0
      CRTETR(6,3)=0.5D0

      CRTETR(7,1)=0.D0
      CRTETR(7,2)=0.5D0
      CRTETR(7,3)=0.D0

      CRTETR(8,1)=0.5D0
      CRTETR(8,2)=0.5D0
      CRTETR(8,3)=0.D0

      CRTETR(9,1)=0.5D0
      CRTETR(9,2)=0.D0
      CRTETR(9,3)=0.5D0

      CRTETR(10,1)=0.5D0
      CRTETR(10,2)=0.D0
      CRTETR(10,3)=0.D0

      CNTETR(1,1)=1
      CNTETR(2,1)=2
      CNTETR(3,1)=3
      CNTETR(4,1)=4

C     0.2 : PENTAEDRE :
C     -----------------
      CRPENT(1,1)=-1.D0
      CRPENT(1,2)= 1.D0
      CRPENT(1,3)= 0.D0

      CRPENT(2,1)=-1.D0
      CRPENT(2,2)= 0.D0
      CRPENT(2,3)= 1.D0

      CRPENT(3,1)=-1.D0
      CRPENT(3,2)= 0.D0
      CRPENT(3,3)= 0.D0

      CRPENT(4,1)= 1.D0
      CRPENT(4,2)= 1.D0
      CRPENT(4,3)= 0.D0

      CRPENT(5,1)= 1.D0
      CRPENT(5,2)= 0.D0
      CRPENT(5,3)= 1.D0

      CRPENT(6,1)= 1.D0
      CRPENT(6,2)= 0.D0
      CRPENT(6,3)= 0.D0

      CRPENT(7,1)=-1.D0
      CRPENT(7,2)= 0.5D0
      CRPENT(7,3)= 0.5D0

      CRPENT(8,1)=-1.D0
      CRPENT(8,2)= 0.D0
      CRPENT(8,3)= 0.5D0

      CRPENT(9,1)=-1.D0
      CRPENT(9,2)= 0.5D0
      CRPENT(9,3)= 0.D0

      CRPENT(10,1)= 0.D0
      CRPENT(10,2)= 1.D0
      CRPENT(10,3)= 0.D0

      CRPENT(11,1)= 0.D0
      CRPENT(11,2)= 0.D0
      CRPENT(11,3)= 1.D0

      CRPENT(12,1)= 0.D0
      CRPENT(12,2)= 0.D0
      CRPENT(12,3)= 0.D0

      CRPENT(13,1)= 1.D0
      CRPENT(13,2)= 0.5D0
      CRPENT(13,3)= 0.5D0

      CRPENT(14,1)= 1.D0
      CRPENT(14,2)= 0.D0
      CRPENT(14,3)= 0.5D0

      CRPENT(15,1)= 1.D0
      CRPENT(15,2)= 0.5D0
      CRPENT(15,3)= 0.D0

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
      CRHEXA(1,1)=-1.D0
      CRHEXA(1,2)=-1.D0
      CRHEXA(1,3)=-1.D0

      CRHEXA(2,1)= 1.D0
      CRHEXA(2,2)=-1.D0
      CRHEXA(2,3)=-1.D0

      CRHEXA(3,1)= 1.D0
      CRHEXA(3,2)= 1.D0
      CRHEXA(3,3)=-1.D0

      CRHEXA(4,1)=-1.D0
      CRHEXA(4,2)= 1.D0
      CRHEXA(4,3)=-1.D0

      CRHEXA(5,1)=-1.D0
      CRHEXA(5,2)=-1.D0
      CRHEXA(5,3)= 1.D0

      CRHEXA(6,1)= 1.D0
      CRHEXA(6,2)=-1.D0
      CRHEXA(6,3)= 1.D0

      CRHEXA(7,1)= 1.D0
      CRHEXA(7,2)= 1.D0
      CRHEXA(7,3)= 1.D0

      CRHEXA(8,1)=-1.D0
      CRHEXA(8,2)= 1.D0
      CRHEXA(8,3)= 1.D0

      CRHEXA(9,1)= 0.D0
      CRHEXA(9,2)=-1.D0
      CRHEXA(9,3)=-1.D0

      CRHEXA(10,1)= 1.D0
      CRHEXA(10,2)= 0.D0
      CRHEXA(10,3)=-1.D0

      CRHEXA(11,1)= 0.D0
      CRHEXA(11,2)= 1.D0
      CRHEXA(11,3)=-1.D0

      CRHEXA(12,1)=-1.D0
      CRHEXA(12,2)= 0.D0
      CRHEXA(12,3)=-1.D0

      CRHEXA(13,1)=-1.D0
      CRHEXA(13,2)=-1.D0
      CRHEXA(13,3)= 0.D0

      CRHEXA(14,1)= 1.D0
      CRHEXA(14,2)=-1.D0
      CRHEXA(14,3)= 0.D0

      CRHEXA(15,1)= 1.D0
      CRHEXA(15,2)= 1.D0
      CRHEXA(15,3)= 0.D0

      CRHEXA(16,1)=-1.D0
      CRHEXA(16,2)= 1.D0
      CRHEXA(16,3)= 0.D0

      CRHEXA(17,1)= 0.D0
      CRHEXA(17,2)=-1.D0
      CRHEXA(17,3)= 1.D0

      CRHEXA(18,1)= 1.D0
      CRHEXA(18,2)= 0.D0
      CRHEXA(18,3)= 1.D0

      CRHEXA(19,1)= 0.D0
      CRHEXA(19,2)= 1.D0
      CRHEXA(19,3)= 1.D0

      CRHEXA(20,1)=-1.D0
      CRHEXA(20,2)= 0.D0
      CRHEXA(20,3)= 1.D0

      CRHEXA(21,1)= 0.D0
      CRHEXA(21,2)= 0.D0
      CRHEXA(21,3)=-1.D0

      CRHEXA(22,1)= 0.D0
      CRHEXA(22,2)=-1.D0
      CRHEXA(22,3)= 0.D0

      CRHEXA(23,1)= 1.D0
      CRHEXA(23,2)= 0.D0
      CRHEXA(23,3)= 0.D0

      CRHEXA(24,1)= 0.D0
      CRHEXA(24,2)= 1.D0
      CRHEXA(24,3)= 0.D0

      CRHEXA(25,1)=-1.D0
      CRHEXA(25,2)= 0.D0
      CRHEXA(25,3)= 0.D0

      CRHEXA(26,1)= 0.D0
      CRHEXA(26,2)= 0.D0
      CRHEXA(26,3)= 1.D0

      CRHEXA(27,1)= 0.D0
      CRHEXA(27,2)= 0.D0
      CRHEXA(27,3)= 0.D0

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
      CRPYRA(1,1)= 1.D0
      CRPYRA(1,2)= 0.D0
      CRPYRA(1,3)= 0.D0

      CRPYRA(2,1)= 0.D0
      CRPYRA(2,2)= 1.D0
      CRPYRA(2,3)= 0.D0

      CRPYRA(3,1)=-1.D0
      CRPYRA(3,2)= 0.D0
      CRPYRA(3,3)= 0.D0

      CRPYRA(4,1)= 0.D0
      CRPYRA(4,2)=-1.D0
      CRPYRA(4,3)= 0.D0

      CRPYRA(5,1)= 0.D0
      CRPYRA(5,2)= 0.D0
      CRPYRA(5,3)= 1.D0

      CRPYRA(6,1)= 0.5D0
      CRPYRA(6,2)= 0.5D0
C     CRPYRA(6,3)= 0.5D0
      CRPYRA(6,3)= 0.D0

      CRPYRA(7,1)=-0.5D0
      CRPYRA(7,2)= 0.5D0
      CRPYRA(7,3)= 0.D0

      CRPYRA(8,1)=-0.5D0
      CRPYRA(8,2)=-0.5D0
      CRPYRA(8,3)= 0.D0

      CRPYRA(9,1)= 0.5D0
      CRPYRA(9,2)=-0.5D0
      CRPYRA(9,3)= 0.D0

      CRPYRA(10,1)= 0.5D0
      CRPYRA(10,2)= 0.D0
      CRPYRA(10,3)= 0.5D0

      CRPYRA(11,1)= 0.D0
      CRPYRA(11,2)= 0.5D0
      CRPYRA(11,3)= 0.5D0

      CRPYRA(12,1)=-0.5D0
      CRPYRA(12,2)= 0.D0
      CRPYRA(12,3)= 0.5D0

      CRPYRA(13,1)= 0.D0
      CRPYRA(13,2)=-0.5D0
      CRPYRA(13,3)= 0.5D0

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

C     2.1 REMPLISSAGE DE .PJEF_NB :
C     -----------------------------
      CALL WKVECT(CORRES//'.PJEF_NB','V V I',NNO2,I2CONB)
      IDECA2=0
      DO 10, INO2=1,NNO2
C       ITR : TETR4 ASSOCIE A INO2
        ITR=ZI(I1COTR-1+INO2)
        IF (ITR.EQ.0) GO TO 10
C       IMA1 : MAILLE DE M1 ASSOCIE AU TETR4 ITR
        IMA1=ZI(IATR3+6*(ITR-1)+5)
        NBNO=ZI(ILCNX1+IMA1)-ZI(ILCNX1-1+IMA1)
        ZI(I2CONB-1+INO2)=NBNO
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
        ITR=ZI(I1COTR-1+INO2)
        IF (ITR.EQ.0) GO TO 20
C       IMA1 : MAILLE DE M1 ASSOCIE AU TETR4 ITR
        IMA1=ZI(IATR3+6*(ITR-1)+5)
C       ITYPM : TYPE DE LA MAILLE IMA1
        ITYPM=ZI(IATYMA-1+IMA1)
        NUTM=INDIIS(NUTM3D,ITYPM,1,9)
        NOTM=NOTM3D(NUTM)
        ITYP=ZI(IATR3+6*(ITR-1)+6)

C       2.2.1 DETERMINATION DES COORDONEES DE INO2 DANS L'ELEMENT
C             DE REFERENCE : KSI , ETA ET DZETA
C     -----------------------------------------------------------
        KSI  =0.D0
        ETA  =0.D0
        DZETA=0.D0
        NBNO=ZI(ILCNX1+IMA1)-ZI(ILCNX1-1+IMA1)

        IF (NOTM(1:4).EQ.'TETR') THEN
          DO 771,KK=1,4
            KSI=KSI    +ZR(I1COCF-1+IDECA1+KK)*CRTETR(CNTETR(KK,ITYP),1)
            ETA=ETA    +ZR(I1COCF-1+IDECA1+KK)*CRTETR(CNTETR(KK,ITYP),2)
            DZETA=DZETA+ZR(I1COCF-1+IDECA1+KK)*CRTETR(CNTETR(KK,ITYP),3)
771       CONTINUE
          DO 781,INO=1,NBNO
            XIN(INO)=CRTETR(INO,1)
            YIN(INO)=CRTETR(INO,2)
            ZIN(INO)=CRTETR(INO,3)
781       CONTINUE

        ELSE IF (NOTM(1:4).EQ.'PENT') THEN
          DO 772,KK=1,4
            KSI=KSI    +ZR(I1COCF-1+IDECA1+KK)*CRPENT(CNPENT(KK,ITYP),1)
            ETA=ETA    +ZR(I1COCF-1+IDECA1+KK)*CRPENT(CNPENT(KK,ITYP),2)
            DZETA=DZETA+ZR(I1COCF-1+IDECA1+KK)*CRPENT(CNPENT(KK,ITYP),3)
772       CONTINUE
          DO 782,INO=1,NBNO
            XIN(INO)=CRPENT(INO,1)
            YIN(INO)=CRPENT(INO,2)
            ZIN(INO)=CRPENT(INO,3)
782       CONTINUE

        ELSE IF (NOTM(1:4).EQ.'HEXA') THEN
          DO 773,KK=1,4
            KSI=KSI    +ZR(I1COCF-1+IDECA1+KK)*CRHEXA(CNHEXA(KK,ITYP),1)
            ETA=ETA    +ZR(I1COCF-1+IDECA1+KK)*CRHEXA(CNHEXA(KK,ITYP),2)
            DZETA=DZETA+ZR(I1COCF-1+IDECA1+KK)*CRHEXA(CNHEXA(KK,ITYP),3)
773       CONTINUE
          DO 783,INO=1,NBNO
            XIN(INO)=CRHEXA(INO,1)
            YIN(INO)=CRHEXA(INO,2)
            ZIN(INO)=CRHEXA(INO,3)
783       CONTINUE

        ELSE IF (NOTM(1:4).EQ.'PYRA') THEN
          DO 774,KK=1,4
            KSI=KSI    +ZR(I1COCF-1+IDECA1+KK)*CRPYRA(CNPYRA(KK,ITYP),1)
            ETA=ETA    +ZR(I1COCF-1+IDECA1+KK)*CRPYRA(CNPYRA(KK,ITYP),2)
            DZETA=DZETA+ZR(I1COCF-1+IDECA1+KK)*CRPYRA(CNPYRA(KK,ITYP),3)
774       CONTINUE
          DO 784,INO=1,NBNO
            XIN(INO)=CRPYRA(INO,1)
            YIN(INO)=CRPYRA(INO,2)
            ZIN(INO)=CRPYRA(INO,3)
784       CONTINUE
        END IF


C       2.2.2 :
C       CALCUL DES F. DE FORME AUX NOEUDS POUR LE POINT KSI,ETA,DZETA:
C       --------------------------------------------------------------
        CALL CALCFF(NOTM,NBNO,KSI,ETA,DZETA,XIN,YIN,ZIN,FF,DFF)
        DO 22,INO=1,NBNO
          NUNO=ZI(IACNX1+ ZI(ILCNX1-1+IMA1)-2+INO)
          ZI(I2CONU-1+IDECA2+INO)=NUNO
          ZR(I2COCF-1+IDECA2+INO)=FF(INO)
22      CONTINUE

        IDECA1=IDECA1+4
        IDECA2=IDECA2+NBNO
20    CONTINUE

9999  CONTINUE
      CALL JEDEMA()
      END
