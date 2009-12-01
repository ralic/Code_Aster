      SUBROUTINE FGVDMG(NOMSYM,NOMSD,NOMMAT,NOMNAP,NOMFON,MEXPIC,
     &                  MCOMPT,MDOMAG,NBORD,NBPT,NTCMP,
     &                  NBCMP,NUMCMP,IMPR,VDOMAG)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8                    NOMMAT,NOMNAP,NOMFON
      CHARACTER*16      NOMSYM
      CHARACTER*19             NOMSD
      CHARACTER*(*)     MEXPIC,MCOMPT,MDOMAG
      REAL*8                              VDOMAG(*)
      INTEGER           NBPT,NBCMP,NUMCMP(*),NBORD
      INTEGER           NTCMP,IMPR
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 04/03/2009   AUTEUR ANGLES J.ANGLES 
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
C       ----------------------------------------------------------------
C       CREATION D UN VECTEUR AUX NOEUDS/PG  DE DOMMAGE
C       POUR LE MOMENT :
C       GRANDEUR 1D EQUIVALENTE (EQUI_GD)  = /VMIS_SG
C                                            /INVA_2_SG
C                                            (EQUI_ELNO/GA_SIGM/EPSI)
C       METHODE D EXTRACTION DES PICS      = RAINFLOW
C       METHODE DE COMPTAGE DES CYCLES     = RAINFLOW
C       METHODE CALCUL DU DOMMAGE UNITAIRE = /WOHLER
C                                            /MANSON_COFFIN
C                                            /TAHERI_MANSON
C                                            /TAHERI_MIXTE
C       METHODE DE CUMUL DU DOMMAGE        = LINEAIRE
C       ----------------------------------------------------------------
C       IN     NOMSYM    NOM SYMBOLIQUE OPTION EQUI_GD
C              NOMSD     NOM SD RESULTAT
C              NOMMAT    NOM DU CHAM_MATER
C              NOMNAP    NOM DE LA NAPPE POUR LOI TAHERI
C              NOMFON    NOM DE LA FONCTION POUR LOI TAHERI
C              MEXPIC    METHODE EXTRACTION DES PICS
C              MCOMPT    METHODE DE COMPTAGE DES CYCLES
C              MDOMAG    METHODE DE CALCUL DU DOMMAGE
C              NBORD     NOMBRE DE NUMEROS D'ORDRE
C              NBPT      NOMBRE DE POINTS DE CALCUL DU DOMMAGE
C              NTCMP     NOMBRE TOTAL DE COMPOSANTE OPTION EQUI_GD
C              NBCMP     NOMBRE DE COMPOSANTES DE EQUI_GD UTILISEES(1)
C              NUMCMP    NUMERO(S) DE LA(DES) COMPOSANTE(S) DE EQUI_GD
C              IMPR      NIVEAU IMPRESSION
C       OUT    VDOMAG    VECTEUR DOMMAGE AUX POINTS
C       ----------------------------------------------------------------
C       REMARQUE         DANS LE CAS OU IL Y A N COMPOSANTES POUR LA
C                        EQUI_GD , ON CALCULE LE DOMMAGE
C                        POUR CHAQUE COMPOSANTE ET ON 'NORME'
C       ----------------------------------------------------------------
C       ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
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
      CHARACTER*6        PGC
      COMMON  / NOMAJE / PGC
      CHARACTER*32       JEXNOM,        JEXNUM
C       ---------------------------------------------------------------
      CHARACTER*8     K8B,KCMP
      CHARACTER*16    K16B
      CHARACTER*19    CHEQUI
      CHARACTER*24    NOMDMG,NOMPIC
      CHARACTER*24    NOMITV,NOMRTV
      CHARACTER*24    VALK(3)
C
      REAL*8          DOMMAG
      REAL*8 VALR(2)
C
      INTEGER         IPT,IORD,ICMP
      INTEGER         IVCH,IVORD,IVPIC,IVITV,IVRTV,IVCYC,IVPT
      INTEGER         NUMSYM,IBID
      INTEGER VALI
C
C ---   VECTEURS DE TRAVAIL
C
      CALL JEMARQ()
      NOMDMG = '&&OP0151.EQUI_GD'
      NOMPIC = '&&OP0151.PICS'
      NOMITV = '&&OP0151.ITRAV'
      NOMRTV = '&&OP0151.RTRAV'
      CALL WKVECT( NOMPIC , 'V V R', NBORD+2     , IVPIC )
      CALL WKVECT( NOMITV , 'V V I', NBORD+2     , IVITV )
      CALL WKVECT( NOMRTV , 'V V R', NBORD+2     , IVRTV )
      CALL WKVECT( NOMDMG , 'V V R', NBORD       , IVPT  )
      CALL WKVECT('&&OP0151.SIGMAX','V V R',NBORD+2,IVMAX)
      CALL WKVECT('&&OP0151.SIGMIN','V V R',NBORD+2,IVMIN)
C
C --    VECTEUR DES NBORD NOMS DE CHAMPS POUR L OPTION NOMSYM
C
      CALL JENONU(JEXNOM(NOMSD//'.DESC',NOMSYM),NUMSYM)
      IF(NUMSYM.EQ.0) THEN
         VALK(1) = NOMSYM
         VALK(2) = NOMSD
         CALL U2MESK('F','PREPOST_51', 2 ,VALK)
      ENDIF
      CALL JEVEUO(JEXNUM(NOMSD//'.TACH',NUMSYM),'L',IVCH)
C
C ---   BOUCLE SUR LES COMPOSANTES DE LA EQUI_GD
C
      DO 50 ICMP = 1 , NBCMP
        IF(IMPR.GE.2 )  CALL CODENT(ICMP,'G',KCMP)
C
C ---     BOUCLE SUR LES POINTS
C
        DO 10 IPT = 1 , NBPT
          IF(IMPR.GE.2) THEN
            VALK (1) = KCMP
            VALI = IPT
            CALL U2MESG('I','PREPOST6_6',1,VALK,1,VALI,0,0.D0)
          ENDIF
C
C ---       CALCUL DU VECTEUR HISTOIRE DE LA EQUI_GD EN CE POINT
C           BOUCLE SUR LES NBORD NUMEROS D ORDRE
C
          DO 30 IORD = 1 , NBORD
            CHEQUI = ZK24(IVCH+IORD-1)(1:19)
C
            IF(CHEQUI.EQ.' ') THEN
               VALK(1) = CHEQUI
               VALK(2) = NOMSYM
               VALK(3) = NOMSD
               CALL U2MESK('F','PREPOST_52', 3 ,VALK)
            ENDIF
C
            IF ( (ICMP .EQ. 1).AND.(IPT .EQ. 1) ) THEN
               CALL JEVEUO(CHEQUI//'.CELV','L',IVORD)
               CALL JELIRA(CHEQUI//'.CELV','LONMAX',IBID,K8B)
               IF ( IBID .NE. (NBPT*NTCMP) ) THEN
                 VALK(2) = NOMSYM
                 VALI = NTCMP
                 CALL U2MESG('F','FATIGUE1_78',1,VALK(2),1,VALI,0,0.D0)
               ENDIF
            ENDIF
C
            CALL JEVEUO(CHEQUI//'.CELV','L',IVORD)
C
C -         STOCKAGE COMPOSANTE NUMCMP(ICMP)
            ZR(IVPT+IORD-1) = ZR(IVORD+(IPT-1)*NTCMP+NUMCMP(ICMP)-1)
C
C
 30       CONTINUE
C
C ---     POSSEDANT ENFIN LE VECTEUR HISTOIRE DE LA EQUI_GD EN CE POINT
C         ON VA POUVOIR CALCULER LE DOMMAGE RESULTANT EN UTILISANT :
C         METHODE D EXTRACTION DES PICS      = RAINFLOW
C         METHODE DE COMPTAGE DES CYCLES     = RAINFLOW
C         METHODE CALCUL DU DOMMAGE          = WOHLER_LINEAIRE
C
          IF(MCOMPT.EQ.'RAINFLOW') THEN
            CALL FGPIC2(MEXPIC,ZR(IVRTV),ZR(IVPT),NBORD,ZR(IVPIC),NPIC)
            CALL FGRAIN(ZR(IVPIC),NPIC,ZI(IVITV),NCYC,ZR(IVMIN),
     &         ZR(IVMAX))
          ELSEIF(MCOMPT(1:6).EQ.'TAHERI') THEN
           CALL FGCOTA(NBORD,ZR(IVPT),NCYC,ZR(IVMIN),ZR(IVMAX))
          ENDIF
          IF (NCYC .EQ. 0) THEN
             CALL U2MESI('F', 'FATIGUE1_77',1,NCYC)
          ENDIF

          IF(IMPR.GE.2) THEN
            VALI = NBORD
            VALR (1) = ZR(IVPT)
            VALR (2) = ZR(IVPT+1)
            CALL U2MESG('I','PREPOST6_7',0,' ',1,VALI,2,VALR)
            IF(MCOMPT.EQ.'RAINFLOW') THEN
              VALI = NPIC
              VALR (1) = ZR(IVPIC)
              VALR (2) = ZR(IVPIC+1)
              CALL U2MESG('I','PREPOST6_8',0,' ',1,VALI,2,VALR)
            ENDIF
            VALI = NCYC
            CALL U2MESG('I','PREPOST6_9',0,' ',1,VALI,0,0.D0)
            DO 223 J = 1 , NCYC
              VALI = J
              VALR (1) = ZR(IVMAX+J-1)
              VALR (2) = ZR(IVMIN+J-1)
              CALL U2MESG('I','PREPOST6_10',0,' ',1,VALI,2,VALR)
  223       CONTINUE
          ENDIF
C
C ---     CALCUL DU DOMMAGE AU POINT IPT ET STOCK DANS VECTEUR VDOMAG
C
          CALL FGDOMG(MDOMAG,NOMMAT,NOMNAP,NOMFON,
     &                  ZR(IVMIN),ZR(IVMAX),NCYC,DOMMAG)
C
          VDOMAG(IPT) = DOMMAG
          IF(IMPR.GE.2) THEN
            VALR (1) = DOMMAG
            CALL U2MESG('I','PREPOST6_11',0,' ',0,0,1,VALR)
          ENDIF
C
 10     CONTINUE
C
 50   CONTINUE
C
      CALL JEDEMA()
      END
