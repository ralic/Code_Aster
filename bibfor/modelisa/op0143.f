      SUBROUTINE OP0143(IER)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 06/07/2009   AUTEUR COURTOIS M.COURTOIS 
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
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C
C     OPERATEUR "DEFI_FLUI_STRU"
C
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
        INTEGER      OCVECT, OCPRHO, OCPVIS, OCPESA, OCRUGO, OCCAPA,
     &               OCGRIL, IRET, IFM, NIV
        INTEGER      DIMVI, DIMVK, DIMVR, DIMGM , DIMGR
        INTEGER      IUNIT, UNIT1, UNIT2, IBID1, IBID2, IOCC1, IOCC2
        INTEGER      IRHO, JRHO
        CHARACTER*2  CARAPA(4)
        CHARACTER*3  OUINON
        CHARACTER*8  K8BID, CAELEM
        CHARACTER*9  TYPAS(2), TPAS
        CHARACTER*16 CONCEP, CMD, NOMMCF, MCFAC(4)
        CHARACTER*19 NOMU
        CHARACTER*8 NOMU8
        CHARACTER*24 FSIC, FSVI, FSVR, FSVK, FSGM, FSCR , FSGR
        REAL*8       VECT(3), VALEPA(4), DE, EP 
C
        DATA TYPAS   /'CARRE_LIGN ','TRIA_LIGN'/
        DATA MCFAC   /'FAISCEAU_TRANS ','GRAPPE',
     &                'FAISCEAU_AXIAL ','COQUE_COAX'/
C=======================================================================
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV ( IFM, NIV )
      PI = R8PI()
C
      CALL GETRES(NOMU,CONCEP,CMD)
      NOMU8=NOMU
C
      DO 10 ITYPF2 = 1,4
         CALL GETFAC(MCFAC(ITYPF2),NBOCC)
         IF (NBOCC.GE.1) GOTO 11
  10  CONTINUE
  11  CONTINUE
      ITYPFL=ITYPF2
      NOMMCF=MCFAC(ITYPF2)

C=====================================================================
C ----VERIFICATIONS AVANT EXECUTION ----
C     =============================
      CALL TFVERI(CMD,NOMMCF,NBOCC,ITYPFL)
C
C=====================================================================
C ----LECTURE DES INFORMATIONS ET STOCKAGE -----
C     ====================================
      FSIC = NOMU//'.FSIC'
      FSVI = NOMU//'.FSVI'
      FSVR = NOMU//'.FSVR'
      FSVK = NOMU//'.FSVK'
      FSGM = NOMU//'.FSGM'
      FSCR = NOMU//'.FSCR'
      FSGR = NOMU//'.FSGR'
C
      CALL WKVECT(FSIC,'G V I',2,LFSIC)
      ZI(LFSIC) = ITYPFL
C
C-----------------------------------------------------------------------
C ----- 1.CAS D'UN FAISCEAU_TRANS
C       -------------------------
        IF (ITYPFL.EQ.1) THEN
C
C ---     STOCKAGE DE L'UNITE LOGIQUE
          CALL  GETVIS(NOMMCF,'UNITE_CD',1,1,1,UNIT1,IBID1)
          CALL  GETVIS(NOMMCF,'UNITE_CK',1,1,1,UNIT2,IBID2)
          CALL JEEXIN (NOMU8//'.UNIT_FAISCEAU',IRET)
          IF (IRET .EQ. 0) THEN
             CALL WKVECT(NOMU8//'.UNIT_FAISCEAU','G V I',2,IUNIT)
          ELSE
             CALL JEVEUO(NOMU8//'.UNIT_FAISCEAU','L',IUNIT)
          ENDIF
          ZI(IUNIT-1+1) = UNIT1
          ZI(IUNIT-1+2) = UNIT2
C
          JCM = 0
          JRHO = 1
          DO 20 IOCC = 1 , NBOCC
C ---     RECHERCHE DE LA DERNIERE OCCURENCE DES MOTS-CLES OBLIGATOIRES
            CALL GETVTX(NOMMCF,'COUPLAGE' ,IOCC,1,0,K8BID,ICOUP)
            IF(ICOUP.NE.0) JCOUP = IOCC
            CALL GETVID(NOMMCF,'CARA_ELEM'     ,IOCC,1,0,K8BID,ICARA)
            CALL GETVID(NOMMCF,'PROF_RHO_F_INT',IOCC,1,0,K8BID,IRHOI)
            CALL GETVID(NOMMCF,'PROF_RHO_F_EXT',IOCC,1,0,K8BID,IRHOE)
            CALL GETVTX(NOMMCF,'NOM_CMP       ',IOCC,1,0,K8BID,ICMP)
C ---     RECHERCHE DE LA DERNIERE OCCURENCE DES MOTS-CLES FACULTATIFS
            CALL GETVR8(NOMMCF,'COEF_MASS_AJOU',IOCC,1,0,RBID,ICM)
            IF(ICM.NE.0)   JCM = IOCC
            CALL GETVR8(NOMMCF,'RHO_TUBE'      ,IOCC,1,0,RBID,IRHO)
            IF(IRHO.NE.0)   JRHO=IOCC
            CALL GETVTX(NOMMCF,'TYPE_PAS'      ,IOCC,1,0,K8BID,ITPAS)
            IF(ITPAS.NE.0) JTPAS = IOCC
            CALL GETVIS(NOMMCF,'TYPE_RESEAU'   ,IOCC,1,0,IBID,ITRES)
            CALL GETVR8(NOMMCF,'PAS'           ,IOCC,1,0,RBID,IPAS)
            IF(IPAS.NE.0)  JPAS = IOCC
 20       CONTINUE
C
          NZEX = NBOCC
          CALL GETVTX(NOMMCF,'COUPLAGE' ,JCOUP,1,1,OUINON,IBID)
C
C --------1.1.SI PRISE EN COMPTE DU COUPLAGE
          IF (OUINON.EQ.'OUI') THEN
C
             ZI(LFSIC+1) = 1
C
             CALL WKVECT(FSVR,'G V R',3+2*NZEX,LFSVR)
             CALL WKVECT(FSVI,'G V I',2+2*NZEX,LFSVI)
             ZI(LFSVI+1) = NZEX
             DO 30 IOCC = 1 , NZEX
                CALL GETVIS(NOMMCF,'TYPE_RESEAU',IOCC,1,1,
     &                      ZI(LFSVI+1+IOCC),IBID)
                CALL GETVR8(NOMMCF,'CSTE_CONNORS',IOCC,1,2,
     &                      ZR(LFSVR+2*IOCC+1),IBID)
                CALL GETVIS(NOMMCF,'NB_CONNORS',IOCC,1,2,
     &                      ZI(LFSVI+1+NZEX+IOCC),IBID)
 30          CONTINUE
             CALL GETVTX(NOMMCF,'TYPE_PAS',JTPAS,1,1,TPAS,IBID)
             IF (TPAS .EQ. TYPAS(1)) THEN
                ZI(LFSVI) = 1
             ELSE
                ZI(LFSVI) = 2
             ENDIF
C
C ---------- PAS REDUIT
             CALL GETVR8(NOMMCF,'PAS',JPAS,1,1,PAS ,IBID)
             IF (JCM.EQ.0) THEN
                IF (ZI(LFSVI).EQ.2) THEN
C ---------------- RESEAU A PAS TRIANGULAIRE
                   Y = (0.96D0+0.5D0*PAS)*PAS
                ELSE
C ---------------- RESEAU A PAS CARRE
                   Y = (1.07D0+0.56D0*PAS)*PAS
                ENDIF
                ZR(LFSVR) = (PI*(Y*Y+1.D0))/(2.D0*(Y*Y-1.D0))
                IF ( NIV .EQ. 2 )   WRITE(IFM,1000) ZR(LFSVR)
             ELSE
                CALL GETVR8(NOMMCF,'COEF_MASS_AJOU',JCM,1,1,
     &                      ZR(LFSVR),IBID)
             ENDIF
             ZR(LFSVR+1) = PAS
C
             CALL GETVR8(NOMMCF,'RHO_TUBE',JRHO,1,1,ZR(LFSVR+2),IRHO)
C
C --------1.2.SI NON PRISE EN COMPTE DU COUPLAGE
          ELSE
C
             ZI(LFSIC+1) = 0
C
             CALL WKVECT(FSVI,'G V I',2,LFSVI)
             ZI(LFSVI+1) = NZEX
C
             CALL WKVECT(FSVR,'G V R',1,LFSVR)
             CALL GETVR8(NOMMCF,'COEF_MASS_AJOU',JCM,1,1,ZR(LFSVR),IBID)
C
          ENDIF
C
C --------1.3.DANS LES DEUX CAS CREATION ET REMPLISSAGE DU .FSVK
          CALL WKVECT(FSVK,'G V K8',4+NZEX,LFSVK)
          CALL GETVID(NOMMCF,'CARA_ELEM'     ,1,1,1,ZK8(LFSVK)  ,IBID)
          CALL GETVTX(NOMMCF,'NOM_CMP'       ,1,1,1,ZK8(LFSVK+1),IBID)
          CALL GETVID(NOMMCF,'PROF_RHO_F_INT',1,1,1,ZK8(LFSVK+2),IBID)
          CALL GETVID(NOMMCF,'PROF_RHO_F_EXT',1,1,1,ZK8(LFSVK+3),IBID)
          DO 40 IOCC = 1,NZEX
          CALL GETVID(NOMMCF,'PROF_VITE_FLUI',IOCC,1,1,
     &                ZK8(LFSVK+3+IOCC),IBID)
 40      CONTINUE
C
C -------1.4.VERIFICATION DES NUMERO ET NOMS DE ZONE D EXCITATION DU
C            FLUIDE
         DO 50 I = 1,NZEX-1
         DO 50 J = I+1,NZEX
            IF(ZK8(LFSVK+I+3).EQ.ZK8(LFSVK+J+3)) THEN
               CALL U2MESS('F','MODELISA5_65')
            ENDIF
 50      CONTINUE
C-----------------------------------------------------------------------
C ----- 2.CAS D'UNE GRAPPE
C       ------------------
        ELSE IF (ITYPFL.EQ.2) THEN
C
          CALL GETVTX(NOMMCF,'COUPLAGE',1,1,1,OUINON,IBID)
C
C --------2.1.SI PRISE EN COMPTE DU COUPLAGE
          IF (OUINON.EQ.'OUI') THEN
C
            ZI(LFSIC+1) = 1
C
            CALL WKVECT(FSVK,'G V K8',4,LFSVK)
            CALL GETVTX(NOMMCF,'GRAPPE_2' ,1,1,1,ZK8(LFSVK)  ,IBID)
            CALL GETVTX(NOMMCF,'NOEUD'    ,1,1,1,ZK8(LFSVK+1),IBID)
            CALL GETVID(NOMMCF,'CARA_ELEM',1,1,1,ZK8(LFSVK+2),IBID)
            CALL GETVID(NOMMCF,'MODELE'   ,1,1,1,ZK8(LFSVK+3),IBID)
C
C ----      STOCKAGE DE L'UNITE LOGIQUE
            CALL  GETVIS(NOMMCF,'UNITE_CA',1,1,1,UNIT1,IBID1)
            CALL  GETVIS(NOMMCF,'UNITE_KA',1,1,1,UNIT2,IBID2)
            CALL JEEXIN (NOMU8//'.UNIT_GRAPPES',IRET)
            IF (IRET .EQ. 0) THEN
              CALL WKVECT(NOMU8//'.UNIT_GRAPPES','G V I',2,IUNIT)
            ELSE
              CALL JEVEUO(NOMU8//'.UNIT_GRAPPES','L',IUNIT)
            ENDIF
C
            ZI(IUNIT-1+1) = UNIT1
            ZI(IUNIT-1+2) = UNIT2
C
            CALL WKVECT(FSVR,'G V R',2,LFSVR)
            CALL GETVR8(NOMMCF,'COEF_MASS_AJOU',1,1,0,RBID,ICM)
            IF (ICM.NE.0) THEN
              CALL GETVR8(NOMMCF,'COEF_MASS_AJOU',1,1,1,ZR(LFSVR),IBID)
            ELSE
              ZR(LFSVR) = PI * 1.078014D0
            ENDIF
            CALL GETVR8(NOMMCF,'RHO_FLUI',1,1,1,ZR(LFSVR+1),IBID)
C
C --------2.2.SI NON PRISE EN COMPTE DU COUPLAGE
          ELSE
            ZI(LFSIC+1) = 0
C
          ENDIF
C-----------------------------------------------------------------------
C ----- 3.CAS D'UN FAISCEAU_AXIAL
C       -------------------------
        ELSE IF (ITYPFL.EQ.3) THEN
C
          ZI(LFSIC+1) = 1
C
C --------3.1.DIMENSIONNEMENT ET CREATION DES OBJETS EN FONCTION
C --------    DU TYPE DE REPRESENTATION
C --------    (FAISCEAU EQUIVALENT OU FAISCEAU COMPLET)
C --------    SIMULTANEMENT ON DETERMINE LES OCCURENCES DU MOT-CLE
C --------    FACTEUR POUR LESQUELLES ON IRA CHERCHER LES INFORMATIONS
          IEQUIV = 1
          IF (NBOCC.EQ.1) THEN
            CALL GETVID(NOMMCF,'CARA_ELEM',1,1,0,K8BID,ICAEL)
            IF (ICAEL.NE.0) IEQUIV = 0
          ENDIF
C
          OCVECT = 1
          OCPRHO = 1
          OCPVIS = 1
          OCPESA = 0
          OCRUGO = 1
          OCCAPA = 1
          OCGRIL = 0
C
          IF (IEQUIV.EQ.0) THEN
C
            CALL GETVTX(NOMMCF,'GROUP_MA',1,1,0,K8BID,NBGRMA)
            NBGRMA = ABS(NBGRMA)
            CALL GETVR8(NOMMCF,'PESANTEUR',1,1,0,RBID,IPESA)
            IF (IPESA.NE.0) OCPESA = 1
            CALL GETVTX(NOMMCF,'CARA_PAROI',1,1,0,K8BID,NBCARA)
            NBCARA = ABS(NBCARA)
            IF (NBCARA.EQ.3) THEN
               IENCE = 1
               NBANGL = 0
            ELSE
               IENCE = 2
               NBANGL = 1
            ENDIF
            CALL GETVR8(NOMMCF,'COOR_GRILLE',1,1,0,RBID,NBGTOT)
            IF (NBGTOT.NE.0) THEN
                OCGRIL = 1
                NBGTOT = ABS(NBGTOT)
                CALL GETVR8(NOMMCF,'LONG_TYPG',1,1,0,RBID,NTYPG)
                NTYPG = ABS(NTYPG)
            ELSE
                NTYPG = 0
                NBGTOT = 0
            ENDIF
C
            IF (OCGRIL.NE.0) THEN
               DIMVI = 6 + NBGTOT
               DIMGR = NBGTOT + 6*NTYPG
            ELSE
               DIMVI = 5
               DIMGR = 0
            ENDIF
            DIMVK = 3
            DIMVR = 5 + NBCARA + NBANGL
            IF (NBGRMA.NE.0) THEN
               DIMGM = NBGRMA
            ELSE
               DIMGM = 1
            ENDIF
C
          ELSE
C
            NBGRMA = NBOCC
            CALL WKVECT('&&OP0143.TEMP.NBCR','V V I',NBOCC,LNBCR)
            NBCOOR = 0
C
            DO 60 IOCC = 1,NBOCC
               CALL GETVR8(NOMMCF,'VECT_X',IOCC,1,0,RBID,IVECT)
               IF (IVECT.NE.0) OCVECT = IOCC
               CALL GETVID(NOMMCF,'PROF_RHO_FLUI',IOCC,1,0,K8BID,IPRHO)
               IF (IPRHO.NE.0) OCPRHO = IOCC
               CALL GETVID(NOMMCF,'PROF_VISC_CINE',IOCC,1,0,K8BID,IPVIS)
               IF (IPVIS.NE.0) OCPVIS = IOCC
               CALL GETVR8(NOMMCF,'PESANTEUR',IOCC,1,0,RBID,IPESA)
               IF (IPESA.NE.0) OCPESA = IOCC
               CALL GETVR8(NOMMCF,'RUGO_TUBE',IOCC,1,0,RBID,IRUGO)
               IF (IRUGO.NE.0) OCRUGO = IOCC
               CALL GETVTX(NOMMCF,'CARA_PAROI',IOCC,1,0,K8BID,ICARA)
               IF (ICARA.NE.0) OCCAPA = IOCC
               CALL GETVR8(NOMMCF,'COOR_TUBE',IOCC,1,0,RBID,ICOOR)
               ICOOR = ABS(ICOOR)
               ZI(LNBCR+IOCC-1) = ICOOR
               NBCOOR = NBCOOR + ICOOR
               CALL GETVR8(NOMMCF,'COOR_GRILLE',IOCC,1,0,RBID,NBGTOT)
               IF (NBGTOT.NE.0) OCGRIL = IOCC
  60        CONTINUE
C
            CALL GETVTX(NOMMCF,'CARA_PAROI',OCCAPA,1,0,K8BID,NBCARA)
            NBCARA = ABS(NBCARA)
            IF (NBCARA.EQ.3) THEN
               IENCE = 1
               NBANGL = 0
            ELSE
               IENCE = 2
               NBANGL = 1
            ENDIF
            IF (OCGRIL.NE.0) THEN
               CALL GETVR8(NOMMCF,'COOR_GRILLE',OCGRIL,1,0,RBID,NBGTOT)
               NBGTOT = ABS(NBGTOT)
               CALL GETVR8(NOMMCF,'LONG_TYPG',OCGRIL,1,0,RBID,NTYPG)
               NTYPG = ABS(NTYPG)
               DIMVI = 6 + NBGRMA + 1 + NBGTOT
               DIMGR = NBGTOT + 6*NTYPG
            ELSE
               NBGTOT = 0
               NTYPG = 0
               DIMVI = 6 + NBGRMA
               DIMGR = 0
            ENDIF
            DIMVK = 2
            DIMVR = 5 + NBCARA + NBANGL + NBGRMA
            DIMGM = NBGRMA
C
            CALL WKVECT(FSCR,'G V R',NBCOOR,LFSCR)
C
          ENDIF
C
          CALL WKVECT(FSVI,'G V I' ,DIMVI,LFSVI)
          CALL WKVECT(FSVK,'G V K8',DIMVK,LFSVK)
          CALL WKVECT(FSVR,'G V R' ,DIMVR,LFSVR)
          CALL WKVECT(FSGM,'G V K8',DIMGM,LFSGM)
          IF (OCGRIL.NE.0) CALL WKVECT(FSGR,'G V R' ,DIMGR,LFSGR)
C
C --------3.2.RECUEIL DES INFORMATIONS COMMUNES AUX DEUX TYPES DE
C --------    REPRESENTATION
C --------3.2.1.OBJET .FSVI
          ZI(LFSVI) = IEQUIV
C
          CALL GETVR8(NOMMCF,'VECT_X',OCVECT,1,3,VECT(1),IBID)
          IF (VECT(1).EQ.1.D0) THEN
            ZI(LFSVI+1) = 1
          ELSE IF (VECT(2).EQ.1.D0) THEN
            ZI(LFSVI+1) = 2
          ELSE
            ZI(LFSVI+1) = 3
          ENDIF
C
          ZI(LFSVI+2) = IENCE
          ZI(LFSVI+3) = NBGRMA
          ZI(LFSVI+4) = NTYPG
C
          IF (OCGRIL.NE.0) THEN
             IF (IEQUIV.EQ.0) THEN
                ZI(LFSVI+5) = NBGTOT
                CALL GETVIS(NOMMCF,'TYPE_GRILLE',OCGRIL,1,NBGTOT,
     &                      ZI(LFSVI+6),IBID)
             ELSE
                ZI(LFSVI+6+NBGRMA) = NBGTOT
                CALL GETVIS(NOMMCF,'TYPE_GRILLE',OCGRIL,1,NBGTOT,
     &                      ZI(LFSVI+7+NBGRMA),IBID)
             ENDIF
          ENDIF
C
C --------3.2.2.OBJET .FSVK
          CALL GETVID(NOMMCF,'PROF_RHO_FLUI',OCPRHO,1,1,ZK8(LFSVK),IBID)
          CALL GETVID(NOMMCF,'PROF_VISC_CINE',OCPVIS,1,1,ZK8(LFSVK+1),
     &                IBID)
C
C --------3.2.3.OBJET .FSVR
          IF (OCPESA.NE.0) THEN
            CALL GETVR8(NOMMCF,'PESANTEUR',OCPESA,1,4,ZR(LFSVR),IBID)
          ELSE
            ZR(LFSVR)   =  9.81D0
            ZR(LFSVR+1) =  0.D0
            ZR(LFSVR+2) =  0.D0
            ZR(LFSVR+3) = -1.D0
          ENDIF
          CALL GETVR8(NOMMCF,'RUGO_TUBE',OCRUGO,1,1,ZR(LFSVR+4),IBID)
C
          CALL GETVTX(NOMMCF,'CARA_PAROI',OCCAPA,1,NBCARA,
     &                                             CARAPA(1),IBID)
          CALL GETVR8(NOMMCF,'VALE_PAROI',OCCAPA,1,NBCARA,
     &                                             VALEPA(1),IBID)
          IF (IENCE.EQ.1) THEN
            DO 70 ICAR = 1,NBCARA
              IF (CARAPA(ICAR).EQ.'YC') ZR(LFSVR+5) = VALEPA(ICAR)
              IF (CARAPA(ICAR).EQ.'ZC') ZR(LFSVR+6) = VALEPA(ICAR)
              IF (CARAPA(ICAR)(1:1).EQ.'R') ZR(LFSVR+7) = VALEPA(ICAR)
  70        CONTINUE
          ELSE
            DO 80 ICAR = 1,NBCARA
              IF (CARAPA(ICAR).EQ.'YC') ZR(LFSVR+5) = VALEPA(ICAR)
              IF (CARAPA(ICAR).EQ.'ZC') ZR(LFSVR+6) = VALEPA(ICAR)
              IF (CARAPA(ICAR).EQ.'HY') ZR(LFSVR+7) = VALEPA(ICAR)
              IF (CARAPA(ICAR).EQ.'HZ') ZR(LFSVR+8) = VALEPA(ICAR)
  80        CONTINUE
            CALL GETVR8(NOMMCF,'ANGL_VRIL',OCCAPA,1,1,ZR(LFSVR+9),IBID)
          ENDIF
C
C --------3.2.4.OBJET .FSGR
          IF (OCGRIL.NE.0) THEN
             CALL GETVR8(NOMMCF,'COOR_GRILLE',OCGRIL,1,NBGTOT,
     &                   ZR(LFSGR),IBID)
             CALL GETVR8(NOMMCF,'LONG_TYPG',OCGRIL,1,NTYPG,
     &                   ZR(LFSGR+NBGTOT),IBID)
             CALL GETVR8(NOMMCF,'LARG_TYPG',OCGRIL,1,NTYPG,
     &                   ZR(LFSGR+NBGTOT+NTYPG),IBID)
             CALL GETVR8(NOMMCF,'EPAI_TYPG',OCGRIL,1,NTYPG,
     &                   ZR(LFSGR+NBGTOT+2*NTYPG),IBID)
             CALL GETVR8(NOMMCF,'COEF_TRAI_TYPG',OCGRIL,1,NTYPG,
     &                   ZR(LFSGR+NBGTOT+3*NTYPG),IBID)
             CALL GETVR8(NOMMCF,'COEF_DPOR_TYPG',OCGRIL,1,NTYPG,
     &                   ZR(LFSGR+NBGTOT+4*NTYPG),IBID)
             CALL GETVR8(NOMMCF,'RUGO_TYPG',OCGRIL,1,NTYPG,
     &                   ZR(LFSGR+NBGTOT+5*NTYPG),IBID)
          ENDIF
C
C --------3.3.INFORMATIONS PARTICULIERES POUR UN FAISCEAU EQUIVALENT
          IF (IEQUIV.EQ.1) THEN
C
            ZI(LFSVI+5) = NBCOOR/2
            IDECVR = 5 + NBCARA + NBANGL
            IDECCR = 0
            DO 90 IOCC = 1,NBOCC
               NBCR = ZI(LNBCR+IOCC-1)
               ZI(LFSVI+6+IOCC-1) = NBCR/2
               CALL GETVR8(NOMMCF,'RAYON_TUBE',IOCC,1,1,
     &                     ZR(LFSVR+IDECVR+IOCC-1),IBID)
               CALL GETVTX(NOMMCF,'GROUP_MA',IOCC,1,1,
     &                     ZK8(LFSGM+IOCC-1),IBID)
               CALL GETVR8(NOMMCF,'COOR_TUBE',IOCC,1,NBCR,
     &                     ZR(LFSCR+IDECCR),IBID)
               IDECCR = IDECCR + NBCR
  90        CONTINUE
C
C --------3.4.INFORMATIONS PARTICULIERES POUR UN FAISCEAU COMPLET
          ELSE
C
            CALL GETVID(NOMMCF,'CARA_ELEM',1,1,1,ZK8(LFSVK+2),IBID)
            IF (NBGRMA.NE.0) THEN
              CALL GETVTX(NOMMCF,'GROUP_MA',1,1,NBGRMA,ZK8(LFSGM),IBID)
            ELSE
              CALL GETVTX(NOMMCF,'TRI_GROUP_MA',1,1,1,ZK8(LFSGM),IBID)
            ENDIF
C
          ENDIF
C-----------------------------------------------------------------------
C ----- 4.CAS DE COQUE_COAX
C       -------------------
        ELSE
C
          ZI(LFSIC+1) = 1
          CALL WKVECT(FSVI,'G V I',2,LFSVI)
          CALL GETVTX(NOMMCF,'MASS_AJOU',1,1,1,OUINON,IBID)
          IF (OUINON.EQ.'OUI') THEN
            ZI(LFSVI) = 1
          ELSE
            ZI(LFSVI) = 0
          ENDIF
          CALL GETVR8(NOMMCF,'VECT_X',1,1,3,VECT(1),IBID)
          IF (VECT(1).EQ.1.D0) THEN
            ZI(LFSVI+1) = 1
          ELSE IF (VECT(2).EQ.1.D0) THEN
            ZI(LFSVI+1) = 2
          ELSE
            ZI(LFSVI+1) = 3
          ENDIF
C
          CALL WKVECT(FSVK,'G V K8',3,LFSVK)
          CALL GETVID(NOMMCF,'CARA_ELEM',1,1,1,ZK8(LFSVK)  ,IBID)
          CALL GETVID(NOMMCF,'MATER_INT',1,1,1,ZK8(LFSVK+1),IBID)
          CALL GETVID(NOMMCF,'MATER_EXT',1,1,1,ZK8(LFSVK+2),IBID)
C
          CALL WKVECT(FSVR,'G V R',7,LFSVR)
          CALL GETVR8(NOMMCF,'RHO_FLUI' ,1,1,1,ZR(LFSVR)  ,IBID)
          CALL GETVR8(NOMMCF,'VISC_CINE',1,1,1,ZR(LFSVR+1),IBID)
          CALL GETVR8(NOMMCF,'RUGOSITE' ,1,1,1,ZR(LFSVR+2),IBID)
          CALL GETVR8(NOMMCF,'PDC_MOY_1',1,1,1,ZR(LFSVR+3),IBID)
          CALL GETVR8(NOMMCF,'PDC_DYN_1',1,1,1,ZR(LFSVR+4),IBID)
          CALL GETVR8(NOMMCF,'PDC_MOY_2',1,1,1,ZR(LFSVR+5),IBID)
          CALL GETVR8(NOMMCF,'PDC_DYN_2',1,1,1,ZR(LFSVR+6),IBID)
C
          CALL WKVECT(FSGM,'G V K8',2,LFSGM)
          CALL GETVTX(NOMMCF,'GROUP_MA_INT',1,1,1,ZK8(LFSGM)  ,IBID)
          CALL GETVTX(NOMMCF,'GROUP_MA_EXT',1,1,1,ZK8(LFSGM+1),IBID)
C
        ENDIF
C=======================================================================
C ----- IMPRESSION -----
C       ==========
        CALL GETVIS(' ','INFO',0,1,1,NIV,IBID)
        IF (NIV.EQ.2) CALL TFIMPR(NOMU)

      CALL JEDETC('G','&&OP0143',1)
      CALL JEDETC('G','AJGR2.FLAG',4)

 1000 FORMAT(1P,'    COEF_MASS_AJOU CALCULE: ',E12.5)

      CALL JEDEMA()
      END
