      SUBROUTINE DISMMO(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI,NOMOBZ,REPKZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/03/2013   AUTEUR CHEIGNON E.CHEIGNON 
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
C     --     DISMOI(MODELE)
C    IN:
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
C    OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C ----------------------------------------------------------------------
C
      INTEGER IALIEL,IANOMA,ICO,IGREL,JNFIS
      INTEGER IRET,ITYPEL,NBGREL,NEL
C
      CHARACTER*1 K1BID
      CHARACTER*4 TYTM
      CHARACTER*8 MA,NOMOB
      CHARACTER*16 NOMTE,NOMODL,NOMOD2
      CHARACTER*19 NOLIG
      CHARACTER*32 REPK
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
      REPK  = ' '
      REPI  = 0
      IERD = 0

      NOMOB=NOMOBZ
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
     &        (QUESTI.EQ.'NB_NL_MAILLA') .OR. (QUESTI.EQ.'AXIS') .OR.
     &        (QUESTI.EQ.'EXI_AXIS') .OR. (QUESTI.EQ.'CALC_RIGI') .OR.
     &        (QUESTI.EQ.'PHENOMENE') .OR. (QUESTI.EQ.'EXI_AMOR') .OR.
     &        (QUESTI.EQ.'EXI_RDM') .OR. (QUESTI.EQ.'EXI_POUX') .OR.
     &        (QUESTI(1:7).EQ.'EXI_THM') .OR.
     &        (QUESTI.EQ.'EXI_TUYAU') .OR. (QUESTI.EQ.'EXI_COQ3D') .OR.
     &        (QUESTI.EQ.'EXI_COQ1D') .OR. (QUESTI.EQ.'EXI_PLAQUE') .OR.
     &        (QUESTI.EQ.'EXI_COQUE') .OR. (QUESTI.EQ.'EXI_GRILLE') .OR.
     &        (QUESTI.EQ.'EXI_STRX')  .OR. (QUESTI.EQ.'EXI_STR2')) THEN
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

C     ------------------------------------------
      ELSEIF ((QUESTI.EQ.'NB_NO_MAILLA') .OR.
     &        (QUESTI.EQ.'NB_MA_MAILLA') .OR.
     &        (QUESTI.EQ.'NB_NO_SS_MAX')) THEN
C     ------------------------------------------
        CALL DISMMA(QUESTI,MA,REPI,REPK,IERD)

C     ------------------------------------
      ELSEIF (QUESTI.EQ.'NB_FISS_XFEM') THEN
C     ------------------------------------
        CALL JEEXIN(NOMOB//'.NFIS',IRET)
        IF (IRET.GT.0) THEN
          CALL JEVEUO(NOMOB//'.NFIS','L',JNFIS)
          REPI=ZI(JNFIS)
        ELSE
          REPI=0
        ENDIF

C     ------------------------------------
      ELSEIF (QUESTI.EQ.'EXI_ELEM') THEN
C     ------------------------------------
        REPK='NON'
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.GT.0)REPK='OUI'

C     ----------------------------------------
      ELSEIF (QUESTI.EQ.'BESOIN_MATER') THEN
C     ----------------------------------------
        CALL JEEXIN(NOLIG//'.LIEL',IRET)
        IF (IRET.GT.0) THEN
          CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
          REPK='NON'
          DO 40,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
            CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL=ZI(IALIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL DISMTE('MODELISATION',NOMTE,REPI,REPK,IERD)
            NOMODL=REPK(1:16)
            IF (NOMODL(1:4).NE.'DIS_') THEN
              REPK='OUI'
              GOTO 70

            ENDIF
   40     CONTINUE
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
          DO 50,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
            CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL=ZI(IALIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL DISMTE('TYPE_TYPMAIL',NOMTE,REPI,TYTM,IERD)
            IF (TYTM.EQ.'VOLU') THEN
              REPK='OUI'
              GOTO 70

            ENDIF
   50     CONTINUE
        ELSE
          REPK='NON'
        ENDIF


      ELSE
C     ----
        GOTO 60

      ENDIF

      GOTO 70


C     -- SORTIE ERREUR :
C     ------------------
   60 CONTINUE
      IERD=1
      GOTO 80

C     -- SORTIE NORMALE :
C     ------------------
   70 CONTINUE
      IERD=0
      REPKZ=REPK

C
   80 CONTINUE
      CALL JEDEMA()
      END
