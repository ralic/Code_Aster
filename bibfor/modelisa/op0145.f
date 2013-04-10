      SUBROUTINE OP0145()
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
C-----------------------------------------------------------------------
C
C     OPERATEUR "DEFI_SPEC_TURB"
C
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER       IBID, DIM,MXVAL
      CHARACTER*1   TYPSPE
      CHARACTER*8   K8B,K8BID,INTSPE,CAELEM,MODELE,NOMZON
      CHARACTER*16  CONCEP, CMD, NOMMCF, MCFAC(9)
      CHARACTER*19  NOMU
      CHARACTER*24  VAIN, VARE, VATE, NNOE,CHNUMI
      INTEGER      IARG
C
C-----------------------------------------------------------------------
      INTEGER IANGL ,IFO ,IFONCT ,IINTER ,IMC ,IMCF ,IMCI 
      INTEGER INAT ,INATUR ,INOEUD ,IOCC ,ISPECT ,JVAVF 
      INTEGER LFON ,LNAT ,LNNOE ,LNOM ,LONG ,LVAIN ,LVARE 
      INTEGER LVATE ,NBMCL ,NNAP 
      REAL*8 RBID 
C-----------------------------------------------------------------------
      DATA MCFAC /'SPEC_LONG_COR_1','SPEC_LONG_COR_2',
     &            'SPEC_LONG_COR_3','SPEC_LONG_COR_4',
     &            'SPEC_CORR_CONV_1','SPEC_CORR_CONV_2',
     &            'SPEC_CORR_CONV_3',
     &            'SPEC_FONC_FORME','SPEC_EXCI_POINT'/
C ----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(NOMU,CONCEP,CMD)
C
      DO 10 IMCF = 1,9
         CALL GETFAC ( MCFAC(IMCF) , IOCC )
         IF (IOCC.EQ.1) GOTO 11
  10  CONTINUE
  11  CONTINUE
C     NBMCL EST AFFECTE A 12 , IL DOIT ETRE SUPERIEUR AU MAX DES NOMBRE
C     DE MC SIMPLES DES 9 MC FACTEURS
      NBMCL = 12
C
      NOMMCF=MCFAC(IMCF)

      READ(NOMMCF(6:6),'(A1)') TYPSPE
      IF (TYPSPE.EQ.'L') THEN
         READ(NOMMCF(15:15),'(I1)') ISPECT
      ELSE IF (TYPSPE.EQ.'F') THEN
         ISPECT = 11
      ELSE IF (TYPSPE.EQ.'C') THEN
         READ(NOMMCF(16:16),'(I1)') ISPECT
      ELSE
         ISPECT = 21
      ENDIF
C
C ----VERIFICATIONS AVANT EXECUTION----
C     =============================
C
      IF (ISPECT.EQ.21) THEN
        CALL GETVID(NOMMCF,'INTE_SPEC',1,IARG,0,K8BID,IINTER)
        IF (IINTER.NE.0) THEN
          CALL GETVTX(NOMMCF,'NATURE',1,IARG,0,K8BID,INATUR)
          CALL GETVR8(NOMMCF,'ANGLE',1,IARG,0,RBID ,IANGL )
          CALL GETVTX(NOMMCF,'NOEUD' ,1,IARG,0,K8BID,INOEUD)
          IF ( INATUR.NE.IANGL .OR. INATUR.NE.INOEUD
     &                         .OR. INOEUD.NE.IANGL ) THEN
            CALL U2MESS('F','MODELISA5_66')
          ENDIF
        ELSE
          CALL GETVTX(NOMMCF,'NOEUD',1,IARG,0,K8BID,INOEUD)
          IF (ABS(INOEUD).NE.1) THEN
            CALL U2MESS('F','MODELISA5_67')
          ENDIF
        ENDIF
      ENDIF
C
C ----FIN DES VERIFICATIONS AVANT EXECUTION----
C     =====================================
C
C ----VERIFICATIONS A L'EXECUTION----
C     ===========================
C
      IF (ISPECT.EQ.11 .OR. ISPECT.EQ.21) THEN
        CALL GETVID(NOMMCF,'INTE_SPEC',1,IARG,0,K8BID,IINTER)
        IF ( IINTER .NE. 0 ) THEN
          CALL GETVID(NOMMCF,'INTE_SPEC',1,IARG,1,INTSPE,IBID)
          CHNUMI = INTSPE//'.NUMI'
          CALL JELIRA(CHNUMI,'LONMAX',MXVAL,K8B)
          IF (ISPECT.EQ.11) THEN
            CALL GETVID(NOMMCF,'FONCTION',1,IARG,0,K8BID,IFONCT)
            DIM = ABS(IFONCT)
            DIM = DIM*(DIM+1)/2
            IF ( DIM .NE. MXVAL ) THEN
              CALL U2MESS('F','MODELISA5_68')
            ENDIF
          ELSE
            CALL GETVTX(NOMMCF,'NOEUD',1,IARG,0,K8BID,INOEUD)
            DIM = ABS(INOEUD)
            DIM = DIM*(DIM+1)/2
            IF ( DIM .NE. MXVAL ) THEN
              CALL U2MESS('F','MODELISA5_69')
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C ----FIN DES VERIFICATIONS A L'EXECUTION----
C     ===================================
C
C ----CREATION DES OBJETS ET REMPLISSAGE EN FONCTION DES----
C ----          DIFFERENTS TYPES DE SPECTRE             ----
C     ==================================================
C
C ----0.DENOMINATIONS DES OBJETS A CREER EN GLOBALE
C       -------------------------------------------
C
      VAIN = NOMU//'.VAIN'
      VARE = NOMU//'.VARE'
      VATE = NOMU//'.VATE'
      NNOE = NOMU//'.NNOE'
C
C
C ----1.MODELES "LONGUEUR DE CORRELATION"
C       ---------------------------------
C
      IF (ISPECT.LT.10.OR.NOMMCF(1:14).EQ.'SPEC_CORR_CONV') THEN
C
C ------1.1.CREATION DES OBJETS SUR LA BASE GLOBALE
C
        CALL WKVECT(VAIN,'G V I',1,LVAIN)
        CALL WKVECT(VARE,'G V R',NBMCL,LVARE)
        LONG = NBMCL + 1
        CALL WKVECT(VATE,'G V K16',LONG,LVATE)
        CALL WKVECT(NOMU//'.VAVF','G V K8',1,JVAVF)
C
C ------1.2.CREATION D'OBJETS SUR LA BASE VOLATILE
C
        CALL WKVECT('OP0145.TEMP.NOM','V V K16',NBMCL,LNOM)
C
C ------1.3.REMPLISSAGE DES OBJETS
C
        IF(NOMMCF.EQ.'SPEC_LONG_COR_1') THEN
           ZK16(LNOM)      ='LONG_COR        '
           ZK16(LNOM+1)    ='PROF_VITE_FLUI  '
           ZK16(LNOM+2)    ='VISC_CINE       '
        ELSEIF(NOMMCF.EQ.'SPEC_LONG_COR_2') THEN
           ZK16(LNOM)      ='LONG_COR        '
           ZK16(LNOM+1)    ='PROF_VITE_FLUI  '
           ZK16(LNOM+2)    ='FREQ_COUP       '
           ZK16(LNOM+3)    ='PHI0            '
           ZK16(LNOM+4)    ='BETA            '
        ELSEIF(NOMMCF.EQ.'SPEC_LONG_COR_3') THEN
           ZK16(LNOM)      ='LONG_COR        '
           ZK16(LNOM+1)    ='PROF_VITE_FLUI  '
           ZK16(LNOM+2)    ='FREQ_COUP       '
           ZK16(LNOM+3)    ='PHI0_1          '
           ZK16(LNOM+4)    ='BETA_1          '
           ZK16(LNOM+5)    ='PHI0_2          '
           ZK16(LNOM+6)    ='BETA_2          '
        ELSEIF(NOMMCF.EQ.'SPEC_LONG_COR_4') THEN
           ZK16(LNOM)      ='LONG_COR        '
           ZK16(LNOM+1)    ='PROF_VITE_FLUI  '
           ZK16(LNOM+2)    ='TAUX_VIDE       '
           ZK16(LNOM+3)    ='BETA            '
           ZK16(LNOM+4)    ='GAMMA           '
        ELSEIF(NOMMCF.EQ.'SPEC_CORR_CONV_1') THEN
           ZK16(LNOM)      ='LONG_COR_1      '
           ZK16(LNOM+1)    ='LONG_COR_2      '
           ZK16(LNOM+2)    ='VITE_FLUI       '
           ZK16(LNOM+3)    ='RHO_FLUI        '
           ZK16(LNOM+4)    ='FREQ_COUP       '
           ZK16(LNOM+5)    ='K               '
           ZK16(LNOM+6)    ='D_FLUI          '
           ZK16(LNOM+7)    ='COEF_VITE_FLUI_A'
           ZK16(LNOM+8)    ='COEF_VITE_FLUI_O'
           ZK16(LNOM+9)    ='METHODE         '
        ELSEIF(NOMMCF.EQ.'SPEC_CORR_CONV_2') THEN
           ZK16(LNOM)      ='FONCTION        '
           ZK16(LNOM+1)    ='VITE_FLUI       '
           ZK16(LNOM+2)    ='FREQ_COUP       '
           ZK16(LNOM+3)    ='METHODE         '
           ZK16(LNOM+4)    ='COEF_VITE_FLUI_A'
           ZK16(LNOM+5)    ='COEF_VITE_FLUI_O'
        ELSEIF(NOMMCF.EQ.'SPEC_CORR_CONV_3') THEN
           ZK16(LNOM)      ='TABLE_FONCTION  '
        ELSEIF(NOMMCF.EQ.'SPEC_FONC_FORME') THEN
           ZK16(LNOM)      ='INTE_SPEC       '
           ZK16(LNOM+1)    ='FONCTION        '
           ZK16(LNOM+2)    ='GRAPPE_1        '
           ZK16(LNOM+3)    ='NOEUD           '
           ZK16(LNOM+4)    ='CARA_ELEM       '
           ZK16(LNOM+5)    ='MODELE          '
        ELSEIF(NOMMCF.EQ.'SPEC_EXCI_POINT') THEN
           ZK16(LNOM)      ='INTE_SPEC      '
           ZK16(LNOM+1)    ='NATURE         '
           ZK16(LNOM+2)    ='ANGL           '
           ZK16(LNOM+3)    ='GRAPPE_2       '
           ZK16(LNOM+4)    ='RHO_FLUI       '
           ZK16(LNOM+5)    ='NOEUD          '
           ZK16(LNOM+6)    ='CARA_ELEM      '
           ZK16(LNOM+7)    ='MODELE         '
        ENDIF
        ZK16(LVATE) = NOMMCF
        IMCI = 0
        DO 20 IMC = 1,NBMCL
          IF(ZK16(LNOM+IMC-1).EQ.'PROF_VITE_FLUI  ') THEN
            CALL GETVID(NOMMCF,'PROF_VITE_FLUI',1,IARG,1,NOMZON,IBID)
            ZK16(LVATE+IMC) = NOMZON
            ZK8(JVAVF)=NOMZON
          ELSEIF(ZK16(LNOM+IMC-1).EQ.'METHODE         ') THEN
            CALL GETVTX(NOMMCF,'METHODE',1,IARG,1,NOMZON,IBID)
            ZK16(LVATE+IMC) = NOMZON
          ELSEIF(ZK16(LNOM+IMC-1).EQ.'FONCTION        ') THEN
            CALL GETVID(NOMMCF,'FONCTION',1,IARG,1,NOMZON,IBID)
            ZK16(LVATE+IMC) = NOMZON
          ELSEIF(ZK16(LNOM+IMC-1).EQ.'TABLE_FONCTION  ') THEN
            CALL GETVID(NOMMCF,'TABLE_FONCTION',1,IARG,1,NOMZON,IBID)
            ZK16(LVATE+IMC) = NOMZON
          ELSEIF(ZK16(LNOM+IMC-1).NE.'                ') THEN
             IMCI = IMCI + 1
             ZK16(LVATE+IMC) = ZK16(LNOM+IMC-1)
             CALL GETVR8(NOMMCF,ZK16(LNOM+IMC-1),1,IARG,1,
     &                   ZR(LVARE+IMCI-1),IBID)
          ENDIF
  20    CONTINUE
        ZI(LVAIN) = ISPECT
C
C
C ----2.MODELES "FONCTIONS DE FORME" ET "EXCITATIONS PONCTUELLES"
C       ---------------------------------------------------------
C
      ELSE
C
C ------2.1.CREATION ET REMPLISSAGE D'OBJETS COMMUNS
C
C ------2.1.1.OBJET .VAIN
C
        CALL WKVECT(VAIN,'G V I',3,LVAIN)
        CALL GETVTX(NOMMCF,'NOEUD',1,IARG,0,K8BID,NNAP)
        NNAP = ABS(NNAP)
        ZI(LVAIN) = ISPECT
        IF (IINTER.EQ.0) THEN
          ZI(LVAIN+1) = 1
        ELSE
          ZI(LVAIN+1) = 0
        ENDIF
        ZI(LVAIN+2) = NNAP
C
C ------2.1.2.OBJET .NNOE
C
        CALL WKVECT(NNOE,'G V K8',NNAP,LNNOE)
        CALL GETVTX(NOMMCF,'NOEUD',1,IARG,NNAP,ZK8(LNNOE),IBID)
C
C ------2.2.OBJETS .VATE ET .VARE
C
        LONG = 5
        IF (IINTER.NE.0) THEN
          IF (ISPECT.EQ.11) THEN
            IFONCT = ABS(IFONCT)
            LONG = 4 + IFONCT
          ELSE
            INOEUD = ABS(INOEUD)
            LONG = 4 + INOEUD
          ENDIF
        ENDIF
C
        CALL WKVECT(VATE,'G V K16',LONG,LVATE)
        CALL GETVID(NOMMCF,'CARA_ELEM',1,IARG,1,CAELEM,IBID)
        CALL GETVID(NOMMCF,'MODELE',1,IARG,1,MODELE,IBID)
        ZK16(LVATE)   = NOMMCF
        ZK16(LVATE+1) = CAELEM
        ZK16(LVATE+2) = MODELE
C
C ------2.2.1.MODELE "FONCTIONS DE FORME"
C
        IF (ISPECT.EQ.11) THEN
          IF (IINTER.EQ.0) THEN
            ZK16(LVATE+3) = 'GRAPPE_1'
            CALL GETVTX(NOMMCF,'GRAPPE_1',1,IARG,1,ZK16(LVATE+4),IBID)
          ELSE
            CALL WKVECT('OP0145.TEMP.FON','V V K8',IFONCT,LFON)
            CALL GETVID(NOMMCF,'FONCTION',1,IARG,IFONCT,ZK8(LFON),IBID)
            ZK16(LVATE+3) = INTSPE
            DO 30 IFO = 1,IFONCT
              ZK16(LVATE+3+IFO) = ZK8(LFON+IFO-1)
  30        CONTINUE
          ENDIF
C
C ------2.2.2.MODELE "EXCITATIONS PONCTUELLES"
C
        ELSE
          IF (IINTER.EQ.0) THEN
            ZK16(LVATE+3) = 'GRAPPE_2'
            CALL GETVTX(NOMMCF,'GRAPPE_2',1,IARG,1,ZK16(LVATE+4),IBID)
C
            CALL WKVECT(VARE,'G V R',1,LVARE)
            CALL GETVR8(NOMMCF,'RHO_FLUI',1,IARG,1,ZR(LVARE),IBID)
C
          ELSE
            CALL WKVECT('OP0145.TEMP.NAT','V V K8',INOEUD,LNAT)
            CALL GETVTX(NOMMCF,'NATURE',1,IARG,INOEUD,ZK8(LNAT),IBID)
            ZK16(LVATE+3) = INTSPE
            DO 40 INAT = 1,INOEUD
              ZK16(LVATE+3+INAT) = ZK8(LNAT+INAT-1)
  40        CONTINUE
C
            CALL WKVECT(VARE,'G V R',INOEUD,LVARE)
            CALL GETVR8(NOMMCF,'ANGLE',1,IARG,INOEUD,ZR(LVARE),IBID)
C
          ENDIF
C
        ENDIF
C
      ENDIF
C
      CALL TITRE
C
      CALL JEDEMA()
      END
