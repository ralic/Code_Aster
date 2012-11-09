      SUBROUTINE LGTLGR (BASEZ, LIGREY, LIGREZ)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     BUT : CREATION ET AFFECTATION DE L'OBJET DE TYPE LIGREL
C           ET DE NOM LIGREZ A PARTIR DE L'OBJET DE TYPE LIGRET
C           ET DE NOM LIGREY SUR LA BASE BASEZ.
C
C
C  ARGUMENT       E/S    TYPE          ROLE
C
C  BASEZ          IN      K1      NOM DE LA BASE
C  LIGREY         IN      K19     NOM DU LIGRET SERVANT A CREER
C                                 LE LIGREL LIGREZ
C  LIGREZ         IN      K19     NOM DU LIGREL A CREER ET AFFECTER
C                 JXVAR
C-------------------------------------------------------------
C
C ====================== DEBUT DES DECLARATIONS ========================
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
C
C ----- ARGUMENTS
      CHARACTER*(*) BASEZ, LIGREY, LIGREZ
C ----- VARIABLES LOCALES -------------------------------
      CHARACTER*1  BASE
      CHARACTER*8  MOLOC
      CHARACTER*19 LIGRET, LIGREL
C ====================== DEBUT DU CODE EXECUTABLE ======================
C
C-----------------------------------------------------------------------
      INTEGER I ,IBID ,IDAPMA ,IDAPNO ,IDLIGI ,IDLIMA ,IDLINO
      INTEGER IDLITY ,IDMODE ,IDNEMA ,IDPHEN ,IDPOMA ,IDPONO ,IER
      INTEGER IJ ,IMODL ,IRET ,J ,JDNBNO ,JDPM ,K
      INTEGER K1 ,LONLIE ,NBAPMA ,NBAPNO ,NBMATO ,NBMATY ,NBNO
      INTEGER NBNO2 ,NBNOTO ,NTYPOI ,NUTYP1 ,NUTYPE
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      BASE   = BASEZ
      LIGRET = LIGREY
      LIGREL = LIGREZ
C
C --- ON VERIFIE SI LE LIGREL EXISTE ,S'IL N'EXISTE PAS, ON
C --- S'ARRETE EN ERREUR FATALE :
C     -------------------------
      CALL JEEXIN(LIGRET//'.LGRF',IRET)
      CALL ASSERT(IRET.NE.0)
C
C --- NUMERO DU TYPE ASSOCIE A DES MAILLES TARDIVES :
C     ---------------------------------------------
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','POI1'),NTYPOI)
C
C --- RECUPERATION DES ATTRIBUTS DU LIGRET :
C     ====================================
C
C --- VECTEUR DE LA LISTE DES MAILLES CUMULEES DU LIGRET :
C     --------------------------------------------------
      CALL JEVEUO(LIGRET//'.LIMA','L',IDLIMA)
C
C --- VECTEUR DES TYPES DES MAILLES CUMULEES DU LIGRET :
C     ------------------------------------------------
      CALL JEVEUO(LIGRET//'.LITY','L',IDLITY)
C
C --- NOM DE LA MODELISATION :
C     ----------------------
      CALL JEVEUO(LIGRET//'.MODE','L',IDMODE)
C
C --- NOM DU PHENOMENE :
C     ----------------
      CALL JEVEUO(LIGRET//'.PHEN','L',IDPHEN)
C
C --- TABLEAU DE POINTEURS DANS LA LISTE DES MAILLES :
C     ----------------------------------------------
      CALL JEVEUO(LIGRET//'.POMA','L',IDPOMA)
C
C --- TABLEAU DE POINTEURS DANS LA LISTE DES NOEUDS :
C     ---------------------------------------------
      CALL JEVEUO(LIGRET//'.PONO','L',IDPONO)
C
C --- VECTEUR DE LA LISTE DES NOEUDS CUMULES DU LIGRET :
C     ------------------------------------------------
      CALL JEVEUO(LIGRET//'.LINO','L',IDLINO)
C
C --- NOMBRE D'AFFECTATIONS DE MAILLES AU LIGRET :
C     ------------------------------------------
      CALL JEVEUO(LIGRET//'.APMA','L',IDAPMA)
C
C --- NOMBRE D'AFFECTATIONS DE NOEUDS AU LIGRET :
C     -----------------------------------------
      CALL JEVEUO(LIGRET//'.APNO','L',IDAPNO)
C
      NBAPMA = ZI(IDAPMA)
      NBAPNO = ZI(IDAPNO)
C
C --- NOMBRE TOTAL DE MAILLES :
C     -----------------------
      NBMATO = ZI(IDPOMA+NBAPMA)
C
C --- NOMBRE TOTAL DE NOEUDS :
C     ----------------------
      NBNOTO = ZI(IDPONO+NBAPNO)
C
      K1 = 1
      NUTYP1 = ZI(IDLITY)
      DO 10 I = 1, NBMATO
        NUTYPE = ZI(IDLITY+I-1)
        IF (NUTYPE.NE.NUTYP1) THEN
          NUTYP1 = NUTYPE
          K1 = K1 + 1
        ENDIF
  10  CONTINUE
C
C --- ON CREE LE .LIEL SI LE NOMBRE DE MAILLES EST NON NUL :
C     ----------------------------------------------------
      IF (NBMATO+NBNOTO.GT.0) THEN
C
        CALL JECREC(LIGREL//'.LIEL',BASE//' V I','NU','CONTIG',
     &              'VARIABLE',K1+NBNOTO)
C
C ---   LONGUEUR DU LIGREL.LIEL :
C       -----------------------
        LONLIE = NBMATO + K1 + NBNOTO + NBAPNO
C
C ---   AFFECTATION DE LA LONGUEUR :
C       --------------------------
        CALL JEECRA(LIGREL//'.LIEL','LONT',LONLIE,' ')
C
C ---   CREATION DE L'OBJET LIGREL.NBNO :
C       -------------------------------
        CALL JEEXIN(LIGREL//'.NBNO',IRET)
        IF (IRET.EQ.0) THEN
          CALL WKVECT(LIGREL//'.NBNO',BASE//' V I',1,JDNBNO)
          ZI(JDNBNO) = NBNOTO
        ENDIF
C
C ---   CREATION DE L'OBJET LIGREL.LGRF :
C       -------------------------------
        CALL JEDUPO(LIGRET//'.LGRF','V',LIGREL//'.LGRF',.FALSE.)
        CALL JEECRA(LIGREL//'.LGRF','DOCU',IBID,'MECA')
C
        NUTYP1 = ZI(IDLITY)
        K = 0
        NBMATY = 0
        IJ = 0
C
C ---   BOUCLE SUR LE NOMBRE D'AFFECTATIONS DU LIGRET PAR DES MAILLES :
C       -------------------------------------------------------------
        DO 20 I = 1, NBMATO
C
          NUTYPE = ZI(IDLITY+I-1)
C
          IF (NUTYPE.NE.NUTYP1) THEN
C
            K = K + 1
C
            IF (K.EQ.K1) GOTO 32
C
C ---       CREATION DU IEME OBJET DE COLLECTION :
C           ------------------------------------
            CALL JECROC(JEXNUM(LIGREL//'.LIEL',K))
C
C ---       LONGUEUR DU IEME OBJET DE COLLECTION :
C           ------------------------------------
            CALL JEECRA(JEXNUM(LIGREL//'.LIEL',K),'LONMAX',NBMATY+1,
     &                  ' ')
C
C ---       AFFECTATION DU IEME OBJET DE COLLECTION :
C           ---------------------------------------
            CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',K),'E',IDLIGI)
C
            DO 30 J = 1, NBMATY
              IJ = IJ + 1
              ZI(IDLIGI+J-1) = ZI(IDLIMA+IJ-1)
  30        CONTINUE
C
            ZI(IDLIGI+NBMATY) = NUTYP1
            NUTYP1 = NUTYPE
            NBMATY = 1
C
          ELSE
C
            NBMATY = NBMATY + 1
C
          ENDIF
C

  32        CONTINUE
C
  20    CONTINUE
C
            IF (NUTYP1.NE.0) THEN
            K = K + 1
C
C ---       CREATION DU IEME OBJET DE COLLECTION :
C           ------------------------------------
            CALL JECROC(JEXNUM(LIGREL//'.LIEL',K))
C
C ---       LONGUEUR DU IEME OBJET DE COLLECTION :
C           ------------------------------------
            CALL JEECRA(JEXNUM(LIGREL//'.LIEL',K),'LONMAX',NBMATY+1,' ')
C
C ---       AFFECTATION DU IEME OBJET DE COLLECTION :
C           ---------------------------------------
            CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',K),'E',IDLIGI)
C
            DO 31 J = 1, NBMATY
              IJ = IJ + 1
              ZI(IDLIGI+J-1) = ZI(IDLIMA+IJ-1)
  31        CONTINUE
C
            ZI(IDLIGI+NBMATY) = NUTYP1
            ENDIF
C
C --- RECHERCHE DU TYPE DES POI1 :
C     --------------------------
        CALL JENONU(JEXNOM('&CATA.'//ZK16(IDPHEN)(1:13)//'.MODL',
     &              ZK16(IDMODE)),IMODL)
        CALL JEVEUO(JEXNUM('&CATA.'//ZK16(IDPHEN),IMODL),'L',JDPM)
C
        NUTYPE   = ZI(JDPM+NTYPOI-1)
C
C ---   BOUCLE SUR LE NOMBRE D'AFFECTATIONS DU LIGRET PAR DES NOEUDS :
C       ------------------------------------------------------------
        DO 50 I = 1, NBAPNO
C
C ---     NOMBRE DE NOEUDS POUR LA IEME OCCURENCE :
C         ---------------------------------------
          NBNO = ZI(IDPONO+I) - ZI(IDPONO+I-1)
C
C ---     CREATION DU IEME OBJET DE COLLECTION :
C         ------------------------------------
          CALL JECROC(JEXNUM(LIGREL//'.LIEL',I+K))
C
C ---     LONGUEUR DU IEME OBJET DE COLLECTION :
C         ------------------------------------
          CALL JEECRA(JEXNUM(LIGREL//'.LIEL',I+K),
     &               'LONMAX',NBNO+1,' ')
C
C ---     AFFECTATION DU IEME OBJET DE COLLECTION :
C         ---------------------------------------
          CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',I+K),'E',IDLIGI)
C
          DO 60 J = 1, NBNO
            ZI(IDLIGI+J-1) = -J
  60      CONTINUE
C
          ZI(IDLIGI+NBNO) = NUTYPE
C
  50    CONTINUE
C
      ENDIF
C
C --- ON CREE LE .NEMA SI LE NOMBRE DE NOEUDS EST NON NUL :
C     ---------------------------------------------------
      IF (NBNOTO.GT.0) THEN
C
        CALL JECREC(LIGREL//'.NEMA',BASE//' V I','NU','CONTIG',
     &              'VARIABLE',NBNOTO)
C
C ---   AFFECTATION DE LA LONGUEUR DU LIGREL.NEMA :
C       -----------------------------------------
        CALL JEECRA(LIGREL//'.NEMA','LONT',2*NBNOTO,' ')
C
C ---   BOUCLE SUR LE NOMBRE D'AFFECTATIONS DU LIGRET PAR DES NOEUDS :
C       ------------------------------------------------------------
        DO 70 I = 1, NBNOTO
C
C ---     NOMBRE DE NOEUDS POUR LA IEME OCCURENCE :
C         ---------------------------------------
          NBNO = ZI(IDPONO+I) - ZI(IDPONO+I-1)
          IF (NBNO.GT.0) THEN
            NBNO2 = NBNO
          ELSE
            NBNO2 = -NBNO
          ENDIF
C
C ---     CREATION DU IEME OBJET DE COLLECTION :
C         ------------------------------------
          CALL JECROC(JEXNUM(LIGREL//'.NEMA',I))
C
C ---     LONGUEUR DU IEME OBJET DE COLLECTION :
C         ------------------------------------
          CALL JEECRA(JEXNUM(LIGREL//'.NEMA',I),'LONMAX',2*NBNO2,' ')
C
C ---     AFFECTATION DU IEME OBJET DE COLLECTION :
C         ---------------------------------------
          CALL JEVEUO(JEXNUM(LIGREL//'.NEMA',I),'E',IDNEMA)
C
          ZI(IDNEMA+1-1) = ZI(IDLINO+ZI(IDPONO+I-1))
          ZI(IDNEMA+2-1) = 1
C
  70    CONTINUE
C
      ENDIF
C
C --- RECUPERATION DU MODE LOCAL ASSOCIE AU PHENOMENE :
C     -----------------------------------------------
      CALL DISMOI('F','NOM_MOLOC',ZK16(IDPHEN),'PHENOMENE',
     &            IBID,MOLOC,IER)
C
C --- ADAPTATION DE LA TAILLE DES GRELS :
C     ---------------------------------
      CALL ADALIG(LIGREL)
      CALL CORMGI(BASE,LIGREL)
      CALL INITEL(LIGREL)
C
      CALL JEDEMA()
C
      END
