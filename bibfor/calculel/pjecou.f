      SUBROUTINE PJECOU(MA1, MA2, NOMGMA, NOMGNO, CORRES)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GREFFET N.GREFFET
C ======================================================================
C     COMMANDE:  PROJ_CHAMP  METHODE:'COUPLAGE' (COUPLAGE IFS VIA YACS)
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*8  MA1, MA2
      CHARACTER*16 NOMGMA, NOMGNO, CORRES
C ======================================================================
C ======================================================================
      INTEGER          IRET, ITYPMA, NDIM, NBPG, IB, FLAG, IBT(15)
      INTEGER          NODEGL, INOL, INO2, MAILRF, NBPGRF
      INTEGER          INOG2, II, IMA1, IMA, INOM1, INOM2, INOM3
      INTEGER          NBMAG1, NBNOG2, NBNOG
      INTEGER          IALIM1, IALIN2, JCOOR1, JCOOR2, ICXMA1, JTYPMA
      INTEGER          IACONO, IACONB, IACOM1, IACONU, IACOCF
      INTEGER          LISTNO(27)
      REAL*8           NORMGL, RBID, NORMLO
      REAL*8           CON1M2(3),CON1M1(3), CON2M1(3), CON3M1(3)
      REAL*8           COBARY(3), KSI(2), FF(27), COEFNO(27), CRREFE(81)
      CHARACTER*1      KBID
      CHARACTER*8      NTYPMA, ELREF, CBT(15)
      CHARACTER*24     GRPMA, GRPNO
      LOGICAL          INMAIL
C ======================================================================

C     MAILLES UTILES DU MAILLAGE 1
C     ============================
      GRPMA = MA1//'.GROUPEMA'
      CALL JEEXIN(JEXNOM(GRPMA,NOMGMA),IRET)
      IF (IRET .EQ. 0) CALL U2MESK('F','ELEMENTS_62',1,NOMGMA)
      CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',NBMAG1,KBID)
      CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',IALIM1)


C     NOEUDS UTILES DU MAILLAGE 2
C     ===========================
      GRPNO = MA2//'.GROUPENO'
      CALL JEEXIN(JEXNOM(GRPNO,NOMGNO),IRET)
      IF (IRET .EQ. 0) CALL U2MESK('F','ELEMENTS_62',1,NOMGNO)
      CALL JELIRA(JEXNOM(GRPNO,NOMGNO),'LONMAX',NBNOG2,KBID)
      CALL JEVEUO(JEXNOM(GRPNO,NOMGNO),'L',IALIN2)


C     COORDONNEES DES NOEUDS DES MAILLAGES 1 ET 2
C     ===========================================
      CALL JEVEUO(MA1//'.COORDO    .VALE','L',JCOOR1)
      CALL JEVEUO(MA2//'.COORDO    .VALE','L',JCOOR2)


C     CREATION D'UNE SD CORRESP_2_MAILLA
C     ==================================
C PJEF_NO : DEVIENT PJXX_K1 DEPUIS 10.1.9
C      CALL WKVECT(CORRES//'.PJEF_NO','V V K8',2,       IACONO)
      CALL WKVECT(CORRES//'.PJXX_K1','V V K24',5,IACONO)
      ZK24(IACONO-1+3)='COUPLAGE'
      CALL WKVECT(CORRES//'.PJEF_NB','V V I', NBNOG2,  IACONB)
      CALL WKVECT(CORRES//'.PJEF_M1','V V I', NBNOG2,  IACOM1)
      CALL WKVECT(CORRES//'.PJEF_NU','V V I', 9*NBNOG2,IACONU)
      CALL WKVECT(CORRES//'.PJEF_CF','V V R', 9*NBNOG2,IACOCF)


C     AFFECTATION DES NOMS DES MAILLAGES
C     ==================================
      ZK24(IACONO-1+1)=MA1
      ZK24(IACONO-1+2)=MA2


C     CALCUL DES FONCTIONS DE FORMES DES NOEUDS DU MAILLAGE 2
C     =======================================================
      FLAG = 0
      DO 10 INOG2 = 1, NBNOG2

        INO2 = ZI(IALIN2-1+INOG2)
        DO 20 II = 1, 3
          CON1M2(II) = ZR(JCOOR2-1+3*(INO2-1)+II)
 20     CONTINUE
        MAILRF = 0

C       RECHERCHE DES MAILLES1 ASSOCIEES AU NOEUD1
C       ==========================================
        NORMGL = 1.D20
        DO 30 IMA1 = 1, NBMAG1
          IMA = ZI(IALIM1-1+IMA1)

C  ON RECUPERE LE NOM DU TYPE DE LA MAILLE
C  ET LE NOMBRE DE PTS D INTEGRATION ASSOCIE
C-----------------------------------------------------------------------
          CALL JELIRA(JEXNUM(MA1//'.CONNEX', IMA),'LONMAX',NBNOG,KBID)
          CALL JEVEUO(JEXNUM(MA1//'.CONNEX', IMA),'L',ICXMA1)
          CALL JEVEUO(MA1//'.TYPMAIL','L',JTYPMA)
          ITYPMA = ZI(JTYPMA-1 + IMA)
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),NTYPMA)
          II=5
          IF (NTYPMA(1:3).EQ.'SEG') II=4
          ELREF(1:2)=NTYPMA(1:2)
          ELREF(3:3)=NTYPMA(II:II)
          ELREF(4:8)='     '
          CALL ELRACA(ELREF,NDIM,NBPG,IB,IB,CBT,IBT,CRREFE,RBID)

C         CAS OU LA MAILLE EST LINEIQUE (SEG)
C         -----------------------------------
          IF (NTYPMA(1:3).EQ.'SEG') THEN
            INOM1 = ZI(ICXMA1-1+1)
            INOM2 = ZI(ICXMA1-1+2)
            DO 40 II = 1, 3
              CON1M1(II) = ZR(JCOOR1-1+3*(INOM1-1)+II)
              CON2M1(II) = ZR(JCOOR1-1+3*(INOM2-1)+II)
 40         CONTINUE
C VERSION ORIGINALE
C            CALL PJ3D4C(CON1M2,CON1M1,CON2M1,INMAIL,COBARY,NORMLO)
            CALL PJ3DA4(CON1M2,CON1M1,CON2M1,
     &                  COBARY(1),COBARY(2),NORMLO)
            KSI(1) = 0
            DO 50 II = 1, 2
              KSI(1) = KSI(1) + COBARY(II)*CRREFE(NDIM*(II-1)+1)
 50         CONTINUE
C
C         CAS OU LA MAILLE EST SURFACIQUE (TRIA)
C         --------------------------------------
          ELSEIF (NTYPMA(1:4).EQ.'TRIA') THEN
            INOM1 = ZI(ICXMA1-1+1)
            INOM2 = ZI(ICXMA1-1+2)
            INOM3 = ZI(ICXMA1-1+3)
            DO 60 II = 1, 3
              CON1M1(II) = ZR(JCOOR1-1+3*(INOM1-1)+II)
              CON2M1(II) = ZR(JCOOR1-1+3*(INOM2-1)+II)
              CON3M1(II) = ZR(JCOOR1-1+3*(INOM3-1)+II)
 60         CONTINUE
C            CALL PJ3D3C(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
C     &                  COBARY,NORMLO)
            CALL PJ3DA3(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
     &                  COBARY(1),COBARY(2),COBARY(3),NORMLO)
            INMAIL = .TRUE.
            KSI(1) = 0
            KSI(2) = 0
            DO 70 II = 1, 3
              KSI(1) = KSI(1) + COBARY(II)*CRREFE(NDIM*(II-1)+1)
              KSI(2) = KSI(2) + COBARY(II)*CRREFE(NDIM*(II-1)+2)
 70         CONTINUE

C         CAS OU LA MAILLE EST SURFACIQUE (QUAD)
C         --------------------------------------
          ELSE IF (NTYPMA(1:4).EQ.'QUAD') THEN

C           On teste le premier triangle de l'element
C           -----------------------------------------
            INOM1 = ZI(ICXMA1-1+1)
            INOM2 = ZI(ICXMA1-1+2)
            INOM3 = ZI(ICXMA1-1+3)
            DO 80 II = 1, 3
              CON1M1(II) = ZR(JCOOR1-1+3*(INOM1-1)+II)
              CON2M1(II) = ZR(JCOOR1-1+3*(INOM2-1)+II)
              CON3M1(II) = ZR(JCOOR1-1+3*(INOM3-1)+II)
 80         CONTINUE
C  VERSION ORIGINALE
C            CALL PJ3D3C(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
C     &                  COBARY,NORMLO)
            CALL PJ3DA3(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
     &                  COBARY(1),COBARY(2),COBARY(3),NORMLO)


C           On teste le second triangle de l'element
C           ----------------------------------------
            IF (.NOT.(INMAIL)) THEN
              INOM2 = ZI(ICXMA1-1+3)
              INOM3 = ZI(ICXMA1-1+4)
              DO 90 II = 1, 3
                CON2M1(II) = ZR(JCOOR1-1+3*(INOM2-1)+II)
                CON3M1(II) = ZR(JCOOR1-1+3*(INOM3-1)+II)
 90           CONTINUE
C  VERSION ORIGINALE
C            CALL PJ3D3C(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
C     &                  COBARY,NORMLO)
            CALL PJ3DA3(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
     &                  COBARY(1),COBARY(2),COBARY(3),NORMLO)
              KSI(1) = COBARY(1)*CRREFE(1)
              KSI(2) = COBARY(1)*CRREFE(2)
              DO 100 II = 2, 3
                KSI(1) = KSI(1) + COBARY(II)*CRREFE(NDIM*(II)+1)
                KSI(2) = KSI(2) + COBARY(II)*CRREFE(NDIM*(II)+2)
 100          CONTINUE
            ELSE
              KSI(1) = 0.D0
              KSI(2) = 0.D0
              DO 110 II = 1, 3
                KSI(1) = KSI(1) + COBARY(II)*CRREFE(NDIM*(II-1)+1)
                KSI(2) = KSI(2) + COBARY(II)*CRREFE(NDIM*(II-1)+2)
 110          CONTINUE
            ENDIF
          ELSE
C            WRITE (6,*) 'TYPE DE MAILLE NON RECONNUE : ',NTYPMA
            CALL U2MESK('F','COUPLAGEIFS_9',1,NTYPMA)
          ENDIF

C   SI LE POINT EST DANS LA MAILLE
C   ET SI LA DISTANCE EST INFERIEURE A LA REFERENCE ALORS SAUVEGARDE
C-----------------------------------------------------------------------
          IF (INMAIL.AND.(NORMLO.LT.NORMGL)) THEN
            NORMGL  = NORMLO
            CALL ELRFVF(ELREF, KSI, 27, FF, NBPG)
            NBPGRF = NBPG
            MAILRF = IMA
            DO 120 II = 1, NBPGRF
              LISTNO(II) = ZI(ICXMA1-1+II)
              COEFNO(II) = FF(II)
 120        CONTINUE
          ENDIF
 30     CONTINUE

C       AFFECTATION DU NOEUD LE PLUS PROCHE EN CAS DE PROBLEME
C       ======================================================
        IF (MAILRF.EQ.0) THEN
          NORMGL = 1.D20
          NODEGL = 0
          DO 130 IMA1 = 1, NBMAG1
            IMA = ZI(IALIM1-1+IMA1)
            CALL JELIRA(JEXNUM(MA1//'.CONNEX', IMA),'LONMAX',NBNOG,KBID)
            CALL JEVEUO(JEXNUM(MA1//'.CONNEX', IMA),'L',ICXMA1)
            DO 140 INOM1 = 1, NBNOG
              INOL = JCOOR1-1+3*(ZI(ICXMA1-1+INOM1)-1)
              NORMLO = 0.D0
              DO 150 II = 1, 3
                NORMLO = NORMLO + (ZR(INOL+II)-CON1M2(II))**2
 150          CONTINUE
              IF (NORMLO.LT.NORMGL) THEN
                NORMGL = NORMLO
                NODEGL = ZI(ICXMA1-1+INOM1)
              ENDIF
 140        CONTINUE
 130      CONTINUE
          MAILRF   = 0
          NBPGRF   = 1
          LISTNO(1) = NODEGL
          COEFNO(1) = 1.D0
        ENDIF

C       AFFECTATION DU NOEUD DANS LA STRUCTURE CORRES
C       =============================================
        ZI(IACONB-1+INOG2) = NBPGRF
        ZI(IACOM1-1+INOG2) = MAILRF
        DO 160 II = 1, NBPGRF
          ZI(IACONU-1+FLAG+II) = LISTNO(II)
          ZR(IACOCF-1+FLAG+II) = COEFNO(II)
 160    CONTINUE
        FLAG = FLAG + NBPGRF
 10   CONTINUE
C ======================================================================
C IMPRESSIONS POUR VERIFICATION
C      FLAG = 0
C      PRINT*,'GROUPE DE NOEUDS : ',NOMGNO
C      DO INOG2 = 1, NBNOG2
C        PRINT*,'NUMERO DU NOEUD (L/G)   : ',INOG2,ZI(IALIN2-1+INOG2)
C        PRINT*,'COORDONNEES             : ',
C     &(ZR(JCOOR2-1+3*(ZI(IALIN2-1+INOG2)-1)+II),II=1,3)
C        PRINT*,'MAILLE DE REFERENCE     : ',ZI(IACOM1-1+INOG2)
C        PRINT*,'NOMBRE DE NOEUDS        : ',ZI(IACONB-1+INOG2)
C        PRINT*,'LISTE DES NOEUDS        : ',(ZI(IACONU-1+FLAG+II),
C     &                                       II=1,ZI(IACONB-1+INOG2))
C        PRINT*,'COEFFICIENTS DES NOEUDS : ',(ZR(IACOCF-1+FLAG+II),
C     &                                       II=1,ZI(IACONB-1+INOG2))
C        PRINT*
C        FLAG = FLAG + ZI(IACONB-1+INOG2)
C      ENDDO
C ======================================================================
      END
