      SUBROUTINE DISMMO(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
      IMPLICIT NONE
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI,NOMOBZ,REPKZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/03/2010   AUTEUR PELLET J.PELLET 
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
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
C    OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C ----------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      CHARACTER*32 JEXNUM
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER IAUX,IBID,IALIEL,IANOMA,ICO,ICP,IGREL
      INTEGER IRET,ITYPEL,NBGREL,NEL,LXLGUT
C
      CHARACTER*1 K1BID
      CHARACTER*4 TYTM
      CHARACTER*8 MA,NOMOB
      CHARACTER*16 NOMTE,NOMODL,NOMOD2
      CHARACTER*19 NOLIG
      CHARACTER*32 REPK,K32BID
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMOB=NOMOBZ
      REPK=' '
      NOLIG=NOMOB//'.MODELE'
C
      CALL JEVEUO(NOLIG//'.LGRF','L',IANOMA)
      MA=ZK8(IANOMA-1+1)
C
C     --------------------------------
      IF (QUESTI.EQ.'NOM_LIGREL') THEN
C     --------------------------------
        REPK=NOLIG
C
C     -----------------------------------
      ELSEIF (QUESTI.EQ.'Z_CST') THEN
C     -----------------------------------
        CALL DISMZC(QUESTI,NOLIG,REPI,REPK,IERD)

C     -----------------------------------
      ELSEIF ((QUESTI.EQ.'DIM_GEOM') .OR.
     &        (QUESTI.EQ.'NB_SM_MAILLA') .OR.
     &        (QUESTI.EQ.'NB_SS_ACTI') .OR.
     &        (QUESTI.EQ.'NB_NL_MAILLA') .OR.
     &        (QUESTI.EQ.'CALC_RIGI') .OR. (QUESTI.EQ.'PHENOMENE')) THEN
C     -----------------------------------
        CALL DISMLG(QUESTI,NOLIG,REPI,REPK,IERD)
C     -----------------------------------------
      ELSEIF (QUESTI.EQ.'ELEM_VOLU_QUAD') THEN
C     -----------------------------------------
        CALL DISMQU(QUESTI,NOLIG,REPI,REPK,IERD)

C     -------------------------------------
      ELSEIF (QUESTI.EQ.'NOM_MAILLA') THEN
C     -------------------------------------
        REPK=MA

C     -------------------------------------------
      ELSEIF (QUESTI.EQ.'MODELISATION') THEN
C     -------------------------------------------
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.EQ.0)GOTO 20
        CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
        IF (NBGREL.LE.0)GOTO 20

        ICO=0
        NOMODL=' '

        DO 10,IGREL=1,NBGREL
          CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
          CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
          ITYPEL=ZI(IALIEL-1+NEL)
          CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
          CALL DISMTE('MODELISATION',NOMTE,REPI,REPK,IERD)
          NOMOD2=REPK(1:16)

C           -- ON ESPERE QUE LES NOMTE '#PLUSIEURS' SONT DES ELEMENTS
C              DE BORD ET QUE L'ON PEUT LES IGNORER ET QU'IL EN RESTE
C              D'AUTRES PLUS SIGNIFICATIFS :
          IF (NOMOD2.NE.'#PLUSIEURS') THEN
            IF (NOMODL.NE.NOMOD2) THEN
              ICO=ICO+1
              NOMODL=NOMOD2
            ENDIF
          ENDIF
   10   CONTINUE
        CALL ASSERT(ICO.GE.1)

        IF (ICO.EQ.1) THEN
          REPK=NOMODL
        ELSEIF (ICO.GT.1) THEN
          REPK='#PLUSIEURS'
        ENDIF
        GOTO 30

   20   CONTINUE
        REPK='#AUCUNE'

   30   CONTINUE

C     -------------------------------------------
      ELSEIF (QUESTI.EQ.'MODELISATION_THM') THEN
C     -------------------------------------------
        K32BID='NON'
C
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.EQ.0)GOTO 50
        CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
        IF (NBGREL.LE.0)GOTO 50
        ICO=0
        ICP=0
        NOMODL=' '

        DO 40,IGREL=1,NBGREL
          CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
          CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
          ITYPEL=ZI(IALIEL-1+NEL)
          CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
          CALL DISMTE('MODELISATION',NOMTE,REPI,REPK,IERD)

          NOMOD2=REPK(1:16)
          IF ((NOMOD2(1:6).EQ.'D_PLAN') .OR.
     &        (NOMOD2(1:6).EQ.'C_PLAN')) THEN
            IBID=8
          ELSEIF (NOMOD2(1:4).EQ.'AXIS') THEN
            IBID=6
          ELSEIF (NOMOD2(1:2).EQ.'3D') THEN
            IBID=4
          ELSE
            GOTO 40

          ENDIF
          IF (NOMOD2(IBID:IBID).EQ.'H') THEN
            IF (NOMOD2(IBID:IBID+4).EQ.'HH2MD' .OR.
     &          NOMOD2(IBID:IBID+4).EQ.'HH2MS' .OR.
     &          NOMOD2(IBID:IBID+2).EQ.'HH2' .OR.
     &          NOMOD2(IBID:IBID+1).EQ.'HH' .OR.
     &          NOMOD2(IBID:IBID+2).EQ.'HHM' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'HHMD' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'HHMS' .OR.
     &          NOMOD2(IBID:IBID+1).EQ.'HM' .OR.
     &          NOMOD2(IBID:IBID+2).EQ.'HMD' .OR.
     &          NOMOD2(IBID:IBID+2).EQ.'HMS') THEN
              ICO=ICO+1
              IAUX=LXLGUT(NOMOD2)
              IF (NOMOD2(IAUX-1:IAUX).EQ.'_P') THEN
                ICP=ICP+1
              ENDIF
            ENDIF
          ELSEIF (NOMOD2(IBID:IBID).EQ.'T') THEN
            IF (NOMOD2(IBID:IBID+2).EQ.'THH' .OR.
     &          NOMOD2(IBID:IBID+4).EQ.'THH2D' .OR.
     &          NOMOD2(IBID:IBID+5).EQ.'THH2MD' .OR.
     &          NOMOD2(IBID:IBID+5).EQ.'THH2MS' .OR.
     &          NOMOD2(IBID:IBID+4).EQ.'THH2S' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'THHD' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'THHM' .OR.
     &          NOMOD2(IBID:IBID+4).EQ.'THHMD' .OR.
     &          NOMOD2(IBID:IBID+4).EQ.'THHMS' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'THHS' .OR.
     &          NOMOD2(IBID:IBID+2).EQ.'THM' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'THMD' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'THMS' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'THVD' .OR.
     &          NOMOD2(IBID:IBID+3).EQ.'THVS') THEN
              ICO=ICO+1
              IAUX=LXLGUT(NOMOD2)
              IF (NOMOD2(IAUX-1:IAUX).EQ.'_P') THEN
                ICP=ICP+1
              ENDIF
            ENDIF
          ENDIF

   40   CONTINUE
        REPK=K32BID
        IF (ICO.GT.0) THEN
          IF (ICP.GT.0) THEN
            REPK='OUI_P'
          ELSE
            REPK='OUI'
          ENDIF
        ENDIF
   50   CONTINUE
C

C     ------------------------------------------
      ELSEIF ((QUESTI.EQ.'NB_NO_MAILLA') .OR.
     &        (QUESTI.EQ.'NB_MA_MAILLA') .OR.
     &        (QUESTI.EQ.'NB_NO_SS_MAX')) THEN
C     ------------------------------------------
        CALL DISMMA(QUESTI,MA,REPI,REPK,IERD)

C     ------------------------------------
      ELSEIF (QUESTI.EQ.'EXI_ELEM') THEN
C     ------------------------------------
        REPK='NON'
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.GT.0)REPK='OUI'

C     -----------------------------------------------------------------
      ELSEIF ((QUESTI.EQ.'EXI_RDM') .OR. (QUESTI.EQ.'EXI_POUX') .OR.
     &        (QUESTI(1:7).EQ.'EXI_THM') .OR.
     &        (QUESTI.EQ.'EXI_TUYAU') .OR. (QUESTI.EQ.'EXI_COQ3D') .OR.
     &        (QUESTI.EQ.'EXI_COQ1D') .OR. (QUESTI.EQ.'EXI_PLAQUE') .OR.
     &        (QUESTI.EQ.'EXI_COQUE') .OR.
     &        (QUESTI.EQ.'EXI_GRILLE')) THEN
C     -----------------------------------------------------------------
        CALL DISMLG(QUESTI,NOLIG,REPI,REPK,IERD)
C
C     ----------------------------------------
      ELSEIF (QUESTI.EQ.'BESOIN_MATER') THEN
C     ----------------------------------------
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.GT.0) THEN
          CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
          REPK='NON'
          DO 60,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
            CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL=ZI(IALIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL DISMTE('MODELISATION',NOMTE,REPI,REPK,IERD)
            NOMODL=REPK(1:16)
            IF (NOMODL(1:4).NE.'DIS_') THEN
              REPK='OUI'
              GOTO 90

            ENDIF
   60     CONTINUE
        ELSE
          REPK='NON'
        ENDIF
C
C     --------------------------------------
      ELSEIF (QUESTI.EQ.'EXI_ELTVOL') THEN
C     --------------------------------------
C          (EXISTENCE D'ELEMENTS DONT LA MAILLE EST VOLUMIQUE)
C
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.GT.0) THEN
          CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
          REPK='NON'
          DO 70,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
            CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL=ZI(IALIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL DISMTE('TYPE_TYPMAIL',NOMTE,REPI,TYTM,IERD)
            IF (TYTM.EQ.'VOLU') THEN
              REPK='OUI'
              GOTO 90

            ENDIF
   70     CONTINUE
        ELSE
          REPK='NON'
        ENDIF


      ELSE
C     ----
        GOTO 80

      ENDIF

      GOTO 90


C     -- SORTIE ERREUR :
C     ------------------
   80 CONTINUE
      IERD=1

C     -- SORTIE NORMALE :
C     ------------------
   90 CONTINUE
      IERD=0
      REPKZ=REPK

C
      CALL JEDEMA()
      END
