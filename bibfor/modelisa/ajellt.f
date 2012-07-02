      SUBROUTINE AJELLT (LIGREZ, NOMAZ, NBMA, LIMAZ, TYPELZ, PHENOZ,
     +                   MODELZ, NBNO, LINOZ)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C
C
C ======================================================================
C ------------------------------------------------------------
C     BUT : AFFECTATION DE L'OBJET DE TYPE LIGRET
C           ET DE NOM LIGREZ
C           ON STOCKE LA LISTE DE MAILLES LIMA DANS LE VECTEUR
C           LIGRET//'.LIMA'
C           QUAND IL S'AGIT DE MAILLES TARDIVES, ON A LIMA(1) = 0,
C           ALORS ON STOCKE LES NOEUDS DE LA MAILLE TARDIVE DANS
C           LE VECTEUR LIGRET//'.LINO'
C           QUAND TYPEL N'EST PAS DEFINI, ON RECUPERE LE TYPE DES
C           MAILLES VIA LA MODELISATION ET LE PHENOMENE .
C
C
C  ARGUMENT       E/S    TYPE          ROLE
C
C  LIGREZ         IN      K19     NOM DU LIGRET A AFFECTER
C                 JXVAR
C  NOMAZ          IN      K8      NOM DU MAILLAGE SUR LEQUEL S'APPUIE
C                                 LE LIGRET
C  NBMA           IN      I       NOMBRE DE MAILLES A AFFECTER
C  LIMAZ          IN      K24     NOM DU VECTEUR JEVEUX CONTENANT
C                                 LA LISTE DES NUMEROS DE MAILLES
C  TYPELZ         IN      K16     TYPE DES MAILLES A AFFECTER
C  PHENOZ         IN      K16     PHENOMENE ASSOCIE AU MODELE
C  MODELZ         IN      K16     MODELISATION ASSOCIEE AU MODELE
C  NBNO           IN      I       NOMBRE DE NOEUDS DE LA MAILLE
C                                 TARDIVE A AFFECTER
C  LINOZ          IN      K24     NOM DU VECTEUR JEVEUX CONTENANT
C                                 LA LISTE DES NUMEROS DE NOEUDS
C-------------------------------------------------------------
C
C ====================== DEBUT DES DECLARATIONS ========================
      INCLUDE 'jeveux.h'
C
C ----- ARGUMENTS
      CHARACTER*(*) LIGREZ, NOMAZ, TYPELZ, PHENOZ, MODELZ,
     +              LIMAZ, LINOZ
C ----- VARIABLES LOCALES -------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IDAPMA ,IDAPNO ,IDLIMA ,IDLINO ,IDLITY ,IDMATA 
      INTEGER IDMODE ,IDNBMA ,IDNOMA ,IDPHEN ,IDPOMA ,IDPONO ,IMODL 
      INTEGER IRET ,IRET1 ,IRET2 ,ITYP ,JDLIMA ,JDLINO ,JDPM 
      INTEGER JDTM ,LOLIMA ,LOLIMX ,LOLINO ,LOLINX ,LOPOMX ,LOPONX 
      INTEGER MATARD ,NBMA ,NBMADI ,NBMAIL ,NBMAX ,NBNO ,NBNODI 
      INTEGER NBNOX ,NLOLIM ,NLOLIN ,NUMAIL ,NUTYPM 
C-----------------------------------------------------------------------
      PARAMETER (NBMAIL = 10000)
C
      CHARACTER*8  NOMA, K8BID
      CHARACTER*16 PHENO, MODELI, TYPEL
      CHARACTER*19 LIGRET
      CHARACTER*24 LIMA, LINO, TYPMAI
C ====================== DEBUT DU CODE EXECUTABLE ======================
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      NOMA   = NOMAZ
      PHENO  = PHENOZ
      MODELI = MODELZ
      LIGRET = LIGREZ
      TYPEL  = TYPELZ
      LIMA   = LIMAZ
      LINO   = LINOZ
      TYPMAI = NOMA//'.TYPMAIL'
C
      MATARD = 0
C
C --- ON VERIFIE SI LE LIGREL EXISTE ,S'IL N'EXISTE PAS, ON LE CREE :
C     -------------------------------------------------------------
      CALL JEEXIN(LIGRET//'.LGRF',IRET)
C
      IF (IRET.EQ.0) THEN
        CALL CRELGT('V',LIGRET)
      ENDIF
C
C --- VECTEUR DE LA LISTE DES MAILLES CUMULEES DU LIGRET :
C     --------------------------------------------------
      CALL JEVEUO(LIGRET//'.LIMA','E',IDLIMA)
C
C --- VECTEUR DES TYPES DES MAILLES CUMULEES DU LIGRET :
C     ------------------------------------------------
      CALL JEVEUO(LIGRET//'.LITY','E',IDLITY)
C
C --- NOM DE LA MODELISATION :
C     ----------------------
      CALL JEVEUO(LIGRET//'.MODE','E',IDMODE)
C
C --- NOM DU PHENOMENE :
C     ----------------
      CALL JEVEUO(LIGRET//'.PHEN','E',IDPHEN)
C
C --- TABLEAU DE POINTEURS DANS LA LISTE DES MAILLES :
C     ----------------------------------------------
      CALL JEVEUO(LIGRET//'.POMA','E',IDPOMA)
C
C --- TABLEAU DE POINTEURS DANS LA LISTE DES NOEUDS :
C     ---------------------------------------------
      CALL JEVEUO(LIGRET//'.PONO','E',IDPONO)
C
C --- NOM DU MAILLAGE :
C     ---------------
      CALL JEVEUO(LIGRET//'.LGRF','E',IDNOMA)
C
C --- NOMBRE DE MAILLES TARDIVES :
C     --------------------------
      CALL JEVEUO(LIGRET//'.MATA','E',IDMATA)
C
C --- VECTEUR DE LA LISTE DES NOEUDS CUMULES DU LIGRET :
C     ------------------------------------------------
      CALL JEVEUO(LIGRET//'.LINO','E',IDLINO)
C
C --- NOMBRE D'AFFECTATIONS DE MAILLES :
C     --------------------------------
      CALL JEVEUO(LIGRET//'.APMA','E',IDAPMA)
C
C --- NOMBRE D'AFFECTATIONS DE NOEUDS :
C     -------------------------------
      CALL JEVEUO(LIGRET//'.APNO','E',IDAPNO)
C
C --- NOMBRE D'AFFECTATIONS DE MAILLES :
C     --------------------------------
      CALL JEVEUO(LIGRET//'.NBMA','E',IDNBMA)
C
      ZI(IDNBMA) = ZI(IDNBMA) + NBMA
C
C --- ON AFFECTE UNE FOIS POUR TOUTES LE NOM DU MAILLAGE :
C     --------------------------------------------------
      IF (IRET.EQ.0) THEN
        ZK8(IDNOMA) = NOMA
      ENDIF
C
C --- RECUPERATION DE LA LISTE DES MAILLES A AFFECTER :
C     ===============================================
      IF (LIMA.NE.' ') THEN
        CALL JEEXIN(LIMA,IRET1)
        IF (IRET1.EQ.0) THEN
          CALL WKVECT(LIMA,'V V I',1,JDLIMA)
        ELSE
          CALL JEVEUO(LIMA,'L',JDLIMA)
        ENDIF
      ENDIF
C
C --- RECUPERATION DE LA LISTE DES NOEUDS A AFFECTER :
C     ===============================================
      IF (LINO.NE.' ') THEN
        CALL JEEXIN(LINO,IRET2)
        IF (IRET2.EQ.0) THEN
          CALL WKVECT(LINO,'V V I',1,JDLINO)
        ELSE
          CALL JEVEUO(LINO,'L',JDLINO)
        ENDIF
      ENDIF
C
C --- VERIFICATION DE L'ADEQUATION DE L'AFFECTATION DES MAILLES
C --- A LA LISTE DES MAILLES CUMULEES :
C     ===============================
      IF (ZI(JDLIMA).GT.0.AND.NBMA.GE.1) THEN
C
C ---   NOMBRE DE MAILLES DEJA AFFECTEES :
C       --------------------------------
        CALL JELIRA(LIGRET//'.LIMA','LONUTI',LOLIMA,K8BID)
C
C ---   LONGUEUR DU VECTEUR LIGRET.LIMA :
C       -------------------------------
        CALL JELIRA(LIGRET//'.LIMA','LONMAX',LOLIMX,K8BID)
C
C ---   NOMBRE DE MAILLES DISPONIBLES :
C       -----------------------------
        NBMADI = LOLIMX - LOLIMA
C
C ---   REAJUSTEMENT EVENTUEL DES VECTEURS LIMA ET LITY :
C       -----------------------------------------------
        IF (NBMA.GT.NBMADI) THEN
          NLOLIM = NBMA - NBMADI
          NBMAX  = LOLIMX+MAX(NLOLIM,NBMAIL)
          CALL JUVECA(LIGRET//'.LIMA',NBMAX)
          CALL JEVEUO(LIGRET//'.LIMA','E',IDLIMA)
          CALL JUVECA(LIGRET//'.LITY',NBMAX)
          CALL JEVEUO(LIGRET//'.LITY','E',IDLITY)
        ENDIF
C
C ---   VERIFICATION DE L'ADEQUATION DE LA TAILLE DU VECTEUR
C ---   DES POINTEURS DANS LA LISTE DE MAILLES :
C       --------------------------------------
C
C ---   NOMBRE D'AFFECTATIONS DE MAILLES :
C       --------------------------------
        ZI(IDAPMA) = ZI(IDAPMA) + 1
C
C ---   LONGUEUR DU VECTEUR LIGRET.POMA :
C       -------------------------------
        CALL JELIRA(LIGRET//'.POMA','LONMAX',LOPOMX,K8BID)
C
C ---   REAJUSTEMENT EVENTUEL DU VECTEUR POMA :
C       -------------------------------------
        IF (ZI(IDAPMA).GE.LOPOMX) THEN
          CALL JUVECA(LIGRET//'.POMA',2*LOPOMX)
          CALL JEVEUO(LIGRET//'.POMA','E',IDPOMA)
        ENDIF
C
      ENDIF
C
C --- VERIFICATION DE L'ADEQUATION DE L'AFFECTATION DES NOEUDS
C --- A LA LISTE DES NOEUDS CUMULES :
C     =============================
      IF (ZI(JDLIMA).EQ.0.AND.NBMA.EQ.1) THEN
C
C ---   NOMBRE DE NOEUDS DEJA AFFECTES :
C       ------------------------------
        CALL JELIRA(LIGRET//'.LINO','LONUTI',LOLINO,K8BID)
C
C ---   LONGUEUR DU VECTEUR LIGRET.LINO :
C       -------------------------------
        CALL JELIRA(LIGRET//'.LINO','LONMAX',LOLINX,K8BID)
C
C ---   NOMBRE DE NOEUDS DISPONIBLES :
C       ----------------------------
        NBNODI = LOLINX - LOLINO
C
C ---   REAJUSTEMENT EVENTUEL DU VECTEUR LINO :
C       -------------------------------------
        IF (NBNO.GT.NBNODI) THEN
          NLOLIN = NBNO - NBNODI
          NBNOX  = LOLINX+MAX(NLOLIN,NBMAIL)
          CALL JUVECA(LIGRET//'.LINO',NBNOX)
          CALL JEVEUO(LIGRET//'.LINO','E',IDLINO)
        ENDIF
C
C ---   VERIFICATION DE L'ADEQUATION DE LA TAILLE DU VECTEUR
C ---   DES POINTEURS DANS LA LISTE DE NOEUDS :
C       -------------------------------------
C
C ---   NOMBRE D'AFFECTATIONS DE NOEUDS :
C       -------------------------------
        ZI(IDAPNO) = ZI(IDAPNO) + 1
C
C ---   LONGUEUR DU VECTEUR LIGRET.PONO :
C       -------------------------------
        CALL JELIRA(LIGRET//'.PONO','LONMAX',LOPONX,K8BID)
C
C ---   REAJUSTEMENT EVENTUEL DU VECTEUR PONO :
C       -------------------------------------
        IF (ZI(IDAPNO).GT.LOPONX) THEN
          CALL JUVECA(LIGRET//'.PONO',2*LOPONX)
          CALL JEVEUO(LIGRET//'.PONO','E',IDPONO)
        ENDIF
C
      ENDIF
C
C --- AFFECTATION DES MAILLES TARDIVES :
C     ================================
      IF (ZI(JDLIMA).EQ.0.AND.NBMA.EQ.1) THEN
C
C ---   ON INCREMENTE LE NOMBRE DE MAILLES TARDIVES :
C       -------------------------------------------
        ZI(IDMATA) = ZI(IDMATA) + 1
        MATARD = MATARD + 1
C
C ---   AFFECTATION DU VECTEUR DES NOEUDS DU LIGRET :
C       -------------------------------------------
        DO 10 I = 1, NBNO
          ZI(IDLINO+ZI(IDPONO+MATARD-1)+I-1) = ZI(JDLINO+I-1)
  10    CONTINUE
C
        ZI(IDPONO+MATARD) = ZI(IDPONO+MATARD-1) + NBNO
C
        CALL JEECRA(LIGRET//'.LINO','LONUTI',ZI(IDPONO+MATARD),K8BID)
C
C --- AFFECTATION DES MAILLES PHYSIQUES :
C     =================================
      ELSE
C
C ---   AFFECTATION DU TYPE DES MAILLES :
C       -------------------------------
        IF (TYPEL.EQ.' ') THEN
          CALL JENONU(JEXNOM('&CATA.'//PHENO(1:13)//'.MODL',MODELI),
     +                IMODL)
          CALL JEVEUO(JEXNUM('&CATA.'//PHENO,IMODL),'L',JDPM)
          CALL JEVEUO(TYPMAI,'L',JDTM)
        ELSE
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE',TYPEL),ITYP)
        ENDIF
C
C
C ---   AFFECTATION DE LA LISTE DES MAILLES CUMULEES :
C       --------------------------------------------
        DO 20 I = 1, NBMA
          ZI(IDLIMA+ZI(IDPOMA+ZI(IDAPMA)-1)+I-1) = ZI(JDLIMA+I-1)
          IF (TYPEL.EQ.' ') THEN
            NUMAIL = ZI(JDLIMA+I-1)
            NUTYPM = ZI(JDTM+NUMAIL-1)
            ITYP   = ZI(JDPM+NUTYPM-1)
          ELSE
              CALL JENONU(JEXNOM('&CATA.TE.NOMTE',TYPEL),ITYP)
          ENDIF
C
          ZI(IDLITY+ZI(IDPOMA+ZI(IDAPMA)-1)+I-1) = ITYP
  20    CONTINUE
C
        ZI(IDNBMA) = ZI(IDNBMA) + NBMA
C
C ---   VECTEUR DE POINTEURS DANS LE VECTEUR DES MAILLES :
C       ------------------------------------------------
        ZI(IDPOMA+ZI(IDAPMA)) = ZI(IDPOMA+ZI(IDAPMA)-1) + NBMA
C
        CALL JEECRA(LIGRET//'.LIMA','LONUTI',ZI(IDPOMA+ZI(IDAPMA)),
     +              K8BID)
C
      ENDIF
C
C --- AFFECTATION DE LA MODELISATION AU LIGRET :
C     ----------------------------------------
      ZK16(IDMODE) = MODELI
C
C --- AFFECTATION DU PHENOMENE AU LIGRET :
C     ----------------------------------
      ZK16(IDPHEN) = PHENO
C
      CALL JEDEMA()
C
      END
