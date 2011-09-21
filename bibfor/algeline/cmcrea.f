      SUBROUTINE CMCREA(MAIN,MAOUT,NBOCC,MOTFAC,NUMOCC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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

      IMPLICIT NONE
      INTEGER  NBOCC,NUMOCC(NBOCC)
      CHARACTER*8  MAIN,MAOUT
      CHARACTER*16 MOTFAC(NBOCC)

C ----------------------------------------------------------------------
C  CREATION DE NOUVEAU MAILLAGE
C ----------------------------------------------------------------------
C IN        MAIN   K8  NOM DU MAILLAGE INITIAL
C IN/JXOUT  MAOUT  K8  NOM DU MAILLAGE TRANSFORME
C IN        NBOCC   I  NOMBRE D'OCCURENCES DES MOTS-CLES FACTEURS
C IN        MOTFAC K16 NOM DU MOT-CLE FACTEUR POUR CHAQUE OCCURENCE
C IN        NUMOCC  I  NUMERO DE L'OCCURENCE DU MOT-CLE MOTFAC(I)
C ----------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER NBNOMX,NBMAIN,NBGMIN,NBMAAJ,NBGMAJ,NBMATO,NBGMTO
      INTEGER NUMARE,NUMACO,NUMA,NBMA,NBGM,NBNO,MA,NO,GM
      INTEGER JADIN,JADOUT
      INTEGER JLNOMA,JLCONN,JLTYMA,JLNGMA,JLGPMA
      INTEGER JNOMA,JTYMA,JCONN,JNGMA,JGPMA
      INTEGER JDIM,ITYIN,ITYOUT
      INTEGER IRET,I,IB

      CHARACTER*8  K8B,KNUM8,PREFIX,GNO1,GNO2,NOMGMA,NOMMA
      CHARACTER*24 LINOMA, LICONN, LITYMA, LINGMA, LIGPMA
      CHARACTER*24 VALK
      CHARACTER*24 DIMIN,DIMOUT,NMAIN,NMAOUT,TMAIN,TMAOUT,CONIN,CONOUT
      CHARACTER*24 GMAIN,GMAOUT
      INTEGER      IARG

      DATA LINOMA /'&&CMCREA.LINOMA'/
      DATA LICONN /'&&CMCREA.LICONN'/
      DATA LITYMA /'&&CMCREA.LITYMA'/
      DATA LINGMA /'&&CMCREA.LINGMA'/
      DATA LIGPMA /'&&CMCREA.LIGPMA'/

C NOMA  NOMS DES MAILLES CREEES (VECTEUR DE K8)
C CONN  CONNECTIVITE DES MAILLES CREEES (LISTE)
C          NBR DE NOEUDS DE LA MAILLE (ICI, TOUJOURS 4),
C          NUMEROS DES NDS DE LA MAILLE
C TYMA  TYPE DES MAILLES CREEES (VECTEUR I)
C NGMA  NOMS DES GFROUP_MA CREES
C GPMA  LISTE DES MAILLES DES GROUP_MA CREES
C          NBR DE MAILLES DU GROUP_MA
C          NUMERO DES MAILLES (NEGATIF QUAND NOUVELLE MAILLE)
C ----------------------------------------------------------------------

      CALL JEMARQ()

C ----------------------------------------------------------------------
C                          INITIALISATION
C ----------------------------------------------------------------------

C    NOMBRE DE NOEUDS MAX. POUR UNE MAILLE :
      CALL DISMOI('F','NB_NO_MAX','&CATA','CATALOGUE',NBNOMX,K8B,IRET)

C    NOMBRE DE MAILLES, DE GROUP_MA
      CALL JEVEUO(MAIN//'.DIME','L',JADIN)
      NBMAIN = ZI(JADIN-1 + 3)
      CALL JELIRA(MAIN//'.GROUPEMA','NOMUTI',NBGMIN,K8B)


C    LISTE DES OBJETS CREES PAR CHAQUE OCCURENCE DES MOTS-CLES
      CALL WKVECT(LINOMA, 'V V K24', NBOCC, JLNOMA)
      CALL WKVECT(LICONN, 'V V K24', NBOCC, JLCONN)
      CALL WKVECT(LITYMA, 'V V K24', NBOCC, JLTYMA)
      CALL WKVECT(LINGMA, 'V V K24', NBOCC, JLNGMA)
      CALL WKVECT(LIGPMA, 'V V K24', NBOCC, JLGPMA)
      DO 10 I = 1, NBOCC
        CALL CODENT(I,'D0',KNUM8)
        ZK24(JLNOMA-1 + I) = LINOMA(1:15) // '.' // KNUM8
        ZK24(JLCONN-1 + I) = LICONN(1:15) // '.' // KNUM8
        ZK24(JLTYMA-1 + I) = LITYMA(1:15) // '.' // KNUM8
        ZK24(JLNGMA-1 + I) = LINGMA(1:15) // '.' // KNUM8
        ZK24(JLGPMA-1 + I) = LIGPMA(1:15) // '.' // KNUM8
 10   CONTINUE


C ----------------------------------------------------------------------
C    CREATION DES MAILLES, DES GROUP_MA, DES NOEUDS, DES GROUP_NO)
C           PARCOURS DES OCCURENCES DES MOTS-CLES FACTEURS
C ----------------------------------------------------------------------

      DO 20 I = 1, NBOCC

        IF (MOTFAC(I) .EQ. 'CREA_FISS') THEN

          CALL GETVTX(MOTFAC(I),'PREF_MAILLE',NUMOCC(I),IARG,1,
     &                PREFIX,IB)
          CALL GETVIS(MOTFAC(I),'PREF_NUME',NUMOCC(I),IARG,1,
     &                NUMARE,IB)
          CALL GETVTX(MOTFAC(I),'GROUP_NO_1',NUMOCC(I),IARG,1,
     &                GNO1  ,IB)
          CALL GETVTX(MOTFAC(I),'GROUP_NO_2',NUMOCC(I),IARG,1,
     &                GNO2  ,IB)
          CALL GETVTX(MOTFAC(I),'NOM',NUMOCC(I),IARG,1,
     &                NOMGMA,IB)

          CALL CMFISS(MAIN, GNO1, GNO2, PREFIX, NUMARE,NOMGMA,
     &      ZK24(JLNOMA-1+I),ZK24(JLCONN-1 + I),ZK24(JLTYMA-1 + I),
     &      ZK24(JLNGMA-1 + I),ZK24(JLGPMA-1 + I) )
        ELSE

          CALL U2MESK('F','ALGELINE_18',1,MOTFAC(I))
        END IF

 20   CONTINUE


C ----------------------------------------------------------------------
C                    DIMENSIONS DU NOUVEAU MAILLAGE
C ----------------------------------------------------------------------

      NBMAAJ = 0
      NBGMAJ = 0
      DO 30 I = 1, NBOCC

C      NOMBRE DE MAILLES AJOUTEES
        CALL JEEXIN(ZK24(JLNOMA-1+I),IRET)
        IF (IRET.NE.0) THEN
          CALL JELIRA(ZK24(JLNOMA-1+I),'LONMAX',NBMA,K8B)
          NBMAAJ = NBMAAJ + NBMA
        END IF

C      NOMBRE DE GROUP_MA AJOUTES
        CALL JEEXIN(ZK24(JLNGMA-1+I),IRET)
        IF (IRET.NE.0) THEN
          CALL JELIRA(ZK24(JLNGMA-1+I),'LONMAX',NBGM,K8B)
          NBGMAJ = NBGMAJ + NBGM
        END IF

 30   CONTINUE

      NBMATO = NBMAIN + NBMAAJ
      NBGMTO = NBGMIN + NBGMAJ



C ----------------------------------------------------------------------
C                   CREATION DU NOUVEAU MAILLAGE
C ----------------------------------------------------------------------

C - CREATION DES OBJETS ET DUPLICATION DE LA PARTIE COMMUNE

C    OBJET .DIME
      DIMIN  = MAIN //'.DIME'
      DIMOUT = MAOUT//'.DIME'
      CALL JEDUPO(DIMIN ,'G',DIMOUT,.FALSE.)
      CALL JEVEUO(DIMOUT,'E',JDIM)
      ZI(JDIM-1 + 3) = NBMATO

C    OBJET .NOMMAI
      NMAIN  = MAIN //'.NOMMAI'
      NMAOUT = MAOUT//'.NOMMAI'
      CALL JECREO(NMAOUT,'G N K8')
      CALL JEECRA(NMAOUT,'NOMMAX', NBMATO,' ')
      DO 100 MA = 1,NBMAIN
        CALL JENUNO(JEXNUM(NMAIN,MA),NOMMA)
        CALL JECROC(JEXNOM(NMAOUT,NOMMA))
 100  CONTINUE

C    OBJET .TYPMAIL
      TMAIN  = MAIN //'.TYPMAIL'
      TMAOUT = MAOUT//'.TYPMAIL'
      CALL WKVECT(TMAOUT,'G V I',NBMATO,ITYOUT)
      CALL JEVEUO(TMAIN ,'L',ITYIN)
      DO 110 MA = 1,NBMAIN
        ZI(ITYOUT-1+MA) = ZI(ITYIN-1+MA)
 110  CONTINUE

C    OBJET .CONNEX
      CONIN  = MAIN //'.CONNEX'
      CONOUT = MAOUT//'.CONNEX'
      CALL JECREC(CONOUT,'G V I','NU','CONTIG','VARIABLE',NBMATO)
      CALL JEECRA(CONOUT,'LONT',NBNOMX*NBMATO,' ')
      DO 120 MA = 1, NBMAIN
        CALL JELIRA(JEXNUM(CONIN ,MA),'LONMAX',NBNO,K8B)
        CALL JEECRA(JEXNUM(CONOUT,MA),'LONMAX',NBNO,K8B)
        CALL JEVEUO(JEXNUM(CONIN ,MA),'L',JADIN )
        CALL JEVEUO(JEXNUM(CONOUT,MA),'E',JADOUT)
        DO 130 NO = 0, NBNO-1
          ZI(JADOUT+NO) = ZI(JADIN+NO)
 130    CONTINUE
 120  CONTINUE

C    OBJET .GROUPMA
      GMAIN  = MAIN //'.GROUPEMA'
      GMAOUT = MAOUT//'.GROUPEMA'
      CALL JECREC(GMAOUT,'G V I','NO','DISPERSE','VARIABLE',NBGMTO)
      DO 140 GM = 1,NBGMIN
        CALL JENUNO(JEXNUM(GMAIN ,GM),NOMGMA)
        CALL JECROC(JEXNOM(GMAOUT,NOMGMA))
        CALL JELIRA(JEXNUM(GMAIN ,GM),'LONUTI',NBMA,K8B)
        CALL JEECRA(JEXNOM(GMAOUT,NOMGMA),'LONMAX',NBMA,K8B)
        CALL JEECRA(JEXNOM(GMAOUT,NOMGMA),'LONUTI',NBMA,K8B)
        CALL JEVEUO(JEXNUM(GMAIN ,GM)    ,'L',JADIN )
        CALL JEVEUO(JEXNOM(GMAOUT,NOMGMA),'E',JADOUT)
        DO 150 MA = 0, NBMA - 1
          ZI(JADOUT+MA) = ZI(JADIN+MA)
 150    CONTINUE
 140  CONTINUE

C    DUPLICATION A L'IDENTIQUE .NOMNOE, .GROUPENO, .COORDO
C    (TANT QUE D'AUTRES MOTS CLES NE SONT PAS TRAITES)
      CALL JEDUPO(MAIN//'.NOMNOE'  ,'G',MAOUT//'.NOMNOE'  ,.FALSE.)
      CALL JEDUPO(MAIN//'.GROUPENO','G',MAOUT//'.GROUPENO',.FALSE.)
      CALL COPISD('CHAMP_GD','G',MAIN//'.COORDO',MAOUT//'.COORDO')
      CALL JEVEUO(MAOUT//'.COORDO    .REFE','E',JADOUT)
      ZK24(JADOUT) = MAOUT

C    DUPLICATION A L'IDENTIQUE DES AUTRES OBJETS NON TRAITES
      CALL JEDUPO(MAIN//'.NOMACR'  ,'G',MAOUT//'.NOMACR'  ,.FALSE.)
      CALL JEDUPO(MAIN//'.PARA_R'  ,'G',MAOUT//'.PARA_R'  ,.FALSE.)
      CALL JEDUPO(MAIN//'.SUPMAIL' ,'G',MAOUT//'.SUPMAIL' ,.FALSE.)
      CALL JEDUPO(MAIN//'.TYPL'    ,'G',MAOUT//'.TYPL'    ,.FALSE.)
      CALL JEDUPO(MAIN//'.ABS_CURV','G',MAOUT//'.ABS_CURV',.FALSE.)


C - AJOUT DES NOUVELLES MAILLES, DES NOUVEAUX GROUP_MA

      NUMACO = NBMAIN
      DO 200 I = 1, NBOCC

C  -    AJOUT DE NOUVELLES MAILLES
        NBMA = 0
        CALL JEEXIN(ZK24(JLNOMA-1+I),IRET)
        IF (IRET.NE.0) THEN
          CALL JELIRA(ZK24(JLNOMA-1+I),'LONMAX',NBMA,K8B)
          CALL JEVEUO(ZK24(JLNOMA-1+I),'L',JNOMA)
          CALL JEVEUO(ZK24(JLTYMA-1+I),'L',JTYMA)
          CALL JEVEUO(ZK24(JLCONN-1+I),'L',JCONN)
          DO 300 MA = 1, NBMA

C          INSERTION DANS LE .NOMMAI
            NOMMA = ZK8(JNOMA-1 + MA)
            CALL JEEXIN(JEXNOM(NMAOUT,NOMMA),IRET)
            IF (IRET.EQ.0) THEN
              CALL JECROC(JEXNOM(NMAOUT,NOMMA))
            ELSE
              VALK = NOMMA
              CALL U2MESG('F', 'ALGELINE4_7',1,VALK,0,0,0,0.D0)
            END IF

C          INSERTION DANS LE .TYPMAIL
            ZI(ITYOUT-1 + NUMACO + MA) = ZI(JTYMA-1 + MA)

C          INSERTION DANS LE .CONNEX
            NBNO = ZI(JCONN)
            CALL JEECRA(JEXNUM(CONOUT,MA+NUMACO),'LONMAX',NBNO,K8B)
            CALL JEVEUO(JEXNUM(CONOUT,MA+NUMACO),'E',JADOUT)
            DO 310 NO = 1, NBNO
              ZI(JADOUT-1+NO) = ZI(JCONN+NO)
 310        CONTINUE
            JCONN = JCONN + 1 + NBNO

 300      CONTINUE
        END IF


C   -   AJOUT DE NOUVEAUX GROUP_MA
        CALL JEEXIN(ZK24(JLNGMA-1+I),IRET)
        IF (IRET.NE.0) THEN
          CALL JELIRA(ZK24(JLNGMA-1+I),'LONMAX',NBGM,K8B)
          CALL JEVEUO(ZK24(JLNGMA-1+I),'L',JNGMA)
          CALL JEVEUO(ZK24(JLGPMA-1+I),'L',JGPMA)
          DO 400 GM = 1, NBGM

C          INSERTION DANS LE .GROUPEMA
            NOMGMA = ZK8(JNGMA-1 + GM)
            CALL JEEXIN(JEXNOM(GMAOUT,NOMGMA),IRET)
            IF (IRET.EQ.0) THEN
              CALL JECROC(JEXNOM(GMAOUT,NOMGMA))
            ELSE
              VALK = NOMGMA
              CALL U2MESG('F', 'ALGELINE4_9',1,VALK,0,0,0,0.D0)
            END IF

            NBMA = ZI(JGPMA)
            CALL JEECRA(JEXNOM(GMAOUT,NOMGMA),'LONMAX',NBMA,K8B)
            CALL JEECRA(JEXNOM(GMAOUT,NOMGMA),'LONUTI',NBMA,K8B)
            CALL JEVEUO(JEXNOM(GMAOUT,NOMGMA),'E',JADOUT)

            DO 410 MA = 1, NBMA
              NUMA = ZI(JGPMA+MA)
              IF (NUMA.LE.0) NUMA = -NUMA + NUMACO
              ZI(JADOUT-1+MA) = NUMA
 410        CONTINUE
            JGPMA = JGPMA + 1 + NBMA

 400      CONTINUE
        END IF

        NUMACO = NUMACO + NBMA
 200  CONTINUE

      CALL JEDETC('V','&&CMCREA',1)
      CALL JEDEMA()
      END
