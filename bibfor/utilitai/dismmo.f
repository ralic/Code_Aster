      SUBROUTINE DISMMO ( CODMES, QUESTI, NOMOBZ, REPI, REPKZ, IERD )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER        REPI, IERD
      CHARACTER*(*)  QUESTI, CODMES, NOMOBZ, REPKZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/10/2004   AUTEUR CIBHHPD L.SALMONA 
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
C     --     DISMOI(MODELE)
C    IN:
C       CODMES : CODE DES MESSAGES A EMETTRE : 'F', 'A', ...
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
C    OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C ----------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNUM, JEXNOM, JEXATR, JEXR8
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*1   K1BID
      CHARACTER*4   PHEN4, TYTM
      CHARACTER*8   KBID, MA, NOMOB
      CHARACTER*16  NOPHEN, NOMTE, NOMODL, NOMOD2
      CHARACTER*19  NOLIG
      CHARACTER*24 NEMA
      CHARACTER*32  REPK
C DEB ------------------------------------------------------------------
C
C -----  FONCTIONS FORMULES
C     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
C                     IMA ETANT UNE MAILLE DU MAILLAGE.
      NUMGLM(NUMAIL,INO)=ZI(IAMACO-1+ZI(ILMACO+NUMAIL-1)+INO-1)
C
      CALL JEMARQ()
      NOMOB = NOMOBZ
      REPK  = REPKZ
      NOLIG = NOMOB//'.MODELE'
C
      CALL JEVEUO(NOLIG//'.NOMA','L',IANOMA)
      MA = ZK8(IANOMA-1+1)
C
C     --------------------------------
      IF (QUESTI.EQ.'NOM_LIGREL') THEN
C     --------------------------------
         REPK= NOLIG
C
C     -----------------------------------
      ELSE IF (QUESTI.EQ.'Z_CST') THEN
C     -----------------------------------
       CALL DISMZC(CODMES, QUESTI, NOLIG,REPI, REPK, IERD)

C     -----------------------------------
      ELSE IF (QUESTI.EQ.'DIM_GEOM') THEN
C     -----------------------------------
         CALL DISMLG(CODMES,QUESTI,NOLIG,REPI,REPK,IERD)
C     -----------------------------------------
      ELSE IF (QUESTI.EQ.'ELEM_VOLU_QUAD') THEN
C     -----------------------------------------
         CALL DISMQU(CODMES,QUESTI,NOLIG,REPI,REPK,IERD)
C
C     -------------------------------------
      ELSE IF (QUESTI.EQ.'NOM_MAILLA') THEN
C     -------------------------------------
         REPK= MA
C
C     ------------------------------------------
      ELSE IF( (QUESTI.EQ.'NB_NO_MAILLA') .OR.
     +         (QUESTI.EQ.'NB_MA_MAILLA') .OR.
     +         (QUESTI.EQ.'NB_NO_SS_MAX') ) THEN
C     ------------------------------------------
         CALL DISMMA(CODMES,QUESTI,MA,REPI,REPK,IERD)
C
C     ------------------------------------------
      ELSE IF( (QUESTI.EQ.'NB_SM_MAILLA') .OR.
     +         (QUESTI.EQ.'NB_SS_ACTI'  ) .OR.
     +         (QUESTI.EQ.'NB_NL_MAILLA') ) THEN
C     ------------------------------------------
         CALL JEEXIN(NOMOB//'.SSSA',IRET)
         IF (IRET.EQ.0) THEN
           REPI=0
         ELSE
           CALL JEVEUO(NOMOB//'.SSSA','L',IASSSA)
           CALL JELIRA(NOMOB//'.SSSA','LONMAX',N1,KBID)
           IF (QUESTI.EQ.'NB_SM_MAILLA') THEN
             REPI= ZI(IASSSA-1+N1-2)
           ELSE IF (QUESTI.EQ.'NB_SS_ACTI') THEN
             REPI= ZI(IASSSA-1+N1-1)
           ELSE IF (QUESTI.EQ.'NB_NL_MAILLA') THEN
             REPI= ZI(IASSSA-1+N1)
           END IF
         END IF
C
C     -------------------------------------------
      ELSE IF ( (QUESTI.EQ.'PHENOMENE'   ) .OR.
     +          (QUESTI.EQ.'MODELISATION') ) THEN
C     -------------------------------------------
        CALL JELIRA(NOLIG//'.NOMA','DOCU',IBID,PHEN4)
        IF (PHEN4.EQ.'MECA') THEN
          NOPHEN='MECANIQUE'
        ELSE IF (PHEN4.EQ.'THER') THEN
          NOPHEN='THERMIQUE'
        ELSE IF (PHEN4.EQ.'ACOU') THEN
          NOPHEN='ACOUSTIQUE'
        ELSE
          CALL UTMESS(CODMES,'DISMMO','PHENOMENE INCONNU : '//PHEN4)
        END IF
        REPK= NOPHEN
C
        IF  (QUESTI.EQ.'MODELISATION') THEN
C            ------------------------
          CALL JEEXIN(NOLIG//'.LIEL',IRET)
          IF (IRET.EQ.0) GO TO 41
          CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
          IF (NBGREL.LE.0) GO TO 41
          ICO  = 0
          NOMODL=' '
          DO 4,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
            CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL= ZI(IALIEL -1 +NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL DISMTE(CODMES,'MODELISATION',NOMTE,REPI,REPK,IERD)
            NOMOD2=REPK(1:16)
            IF ((NOMODL.NE.NOMOD2).AND.(NOMOD2.NE.' ')) THEN
              ICO =ICO+1
              NOMODL=NOMOD2
            END IF
 4        CONTINUE
          IF (ICO.EQ.1) THEN
            REPK=NOMODL
            GO TO 42
          END IF
 41       CONTINUE
          REPK=' '
 42       CONTINUE
        END IF
C
C     ------------------------------------
      ELSE IF  (QUESTI.EQ.'EXI_ELEM') THEN
C     ------------------------------------
        REPK='NON'
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.GT.0) REPK= 'OUI'
C
C     -----------------------------------------------------------------
      ELSE IF ((QUESTI.EQ.'EXI_RDM'   ).OR.(QUESTI.EQ.'EXI_POUX'  ).OR.
     +         (QUESTI.EQ.'EXI_THM_CT').OR.(QUESTI.EQ.'EXI_THM_VR').OR.
     +         (QUESTI.EQ.'EXI_TUYAU' ).OR.(QUESTI.EQ.'EXI_COQ3D' ).OR.
     +         (QUESTI.EQ.'EXI_COQ1D' ).OR.(QUESTI.EQ.'EXI_PLAQUE').OR.
     &         (QUESTI.EQ.'EXI_GRAD_VARI')
     +        ) THEN
C     -----------------------------------------------------------------
         CALL DISMLG(CODMES,QUESTI,NOLIG,REPI,REPK,IERD)
C
C     ----------------------------------------
      ELSE IF  (QUESTI.EQ.'BESOIN_MATER') THEN
C     ----------------------------------------
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.GT.0) THEN
         CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
         REPK='NON'
         DO 6,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
            CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL= ZI(IALIEL -1 +NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL DISMTE(CODMES,'MODELISATION',NOMTE,REPI,REPK,IERD)
            NOMODL=REPK(1:16)
            IF   (NOMODL(1:4).NE.'DIS_') THEN
                REPK='OUI'
                GO TO 9999
            END IF
 6       CONTINUE
        ELSE
          REPK='NON'
        END IF
C
C     --------------------------------------
      ELSE IF  (QUESTI.EQ.'EXI_ELTVOL') THEN
C     --------------------------------------
C          (EXISTENCE D'ELEMENTS DONT LA MAILLE EST VOLUMIQUE)
C
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.GT.0) THEN
         CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
         REPK='NON'
         DO 7,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
            CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL= ZI(IALIEL -1 +NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL DISMTE(CODMES,'TYPE_TYPMAIL',NOMTE,REPI,TYTM,IERD)
            IF   (TYTM.EQ.'VOLU') THEN
                REPK='OUI'
                GO TO 9999
            END IF
 7       CONTINUE
        ELSE
          REPK='NON'
        END IF
C     ----
      ELSE
C     ----
         REPK = QUESTI
         CALL UTMESS(CODMES,'DISMMO:',
     +                  'LA QUESTION : "'//REPK//'" EST INCONNUE')
         GO TO 9998
      END IF
      GO TO 9999
C
C     -- SORTIE ERREUR :
C     ------------------
 9998 CONTINUE
      IERD=1
      REPI=0
      REPK='ERREUR'
      IF (CODMES.EQ.'F') CALL UTMESS('F','DISMMO','PROBLEME DISMOI.')
C
C     -- SORTIE NORMALE :
C     ------------------
      IERD=0
 9999 CONTINUE
      REPKZ = REPK

C
      CALL JEDEMA()
      END
