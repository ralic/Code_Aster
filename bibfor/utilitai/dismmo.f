      SUBROUTINE DISMMO ( CODMES, QUESTI, NOMOBZ, REPI, REPKZ, IERD )
      IMPLICIT NONE
      INTEGER        REPI, IERD
      CHARACTER*(*)  QUESTI, CODMES, NOMOBZ, REPKZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/09/2007   AUTEUR DURAND C.DURAND 
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
      CHARACTER*32      JEXNUM
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C
      INTEGER IBID,IALIEL,IAMACO,IANOMA,IASSSA,ICO,IGREL
      INTEGER ILMACO,INO,IRET,ITYPEL,N1,NBGREL,NEL
C
      CHARACTER*1   K1BID
      CHARACTER*4   PHEN4, TYTM
      CHARACTER*8   KBID, MA, NOMOB
      CHARACTER*16  NOPHEN, NOMTE, NOMODL, NOMOD2
      CHARACTER*19  NOLIG
      CHARACTER*32  REPK, K32BID
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMOB = NOMOBZ
      REPK  = REPKZ
      NOLIG = NOMOB//'.MODELE'
C
      CALL JEVEUO(NOLIG//'.LGRF','L',IANOMA)
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
      ELSE IF ((QUESTI.EQ.'DIM_GEOM')      .OR.
     &        (QUESTI.EQ.'NB_SM_MAILLA')   .OR.
     &        (QUESTI.EQ.'NB_SS_ACTI'  )   .OR.
     &        (QUESTI.EQ.'NB_NL_MAILLA')   .OR.
     &        (QUESTI.EQ.'PHENOMENE'   ) ) THEN
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
     &         (QUESTI.EQ.'NB_MA_MAILLA') .OR.
     &         (QUESTI.EQ.'NB_NO_SS_MAX') ) THEN
C     ------------------------------------------
         CALL DISMMA(CODMES,QUESTI,MA,REPI,REPK,IERD)
C
C     -------------------------------------------
      ELSE IF (  (QUESTI.EQ.'MODELISATION') .OR.
     &          (QUESTI.EQ.'MODELISATION_THM') ) THEN
C     -------------------------------------------
C
          IF  ( QUESTI.EQ.'MODELISATION' ) THEN
            K32BID = ' '
          ELSE
            K32BID = 'NON'
          ENDIF
C
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
            IF  ( QUESTI.EQ.'MODELISATION' ) THEN
              IF ((NOMODL.NE.NOMOD2).AND.(NOMOD2.NE.' ')) THEN
                ICO =ICO+1
                NOMODL=NOMOD2
              ENDIF
            ELSE
              IF (( NOMOD2(1:6).EQ.'D_PLAN' )  .OR.
     &            ( NOMOD2(1:6).EQ.'C_PLAN' )) THEN
                IBID = 8
              ELSEIF ( NOMOD2(1:4).EQ.'AXIS' ) THEN
                IBID = 6
              ELSEIF ( NOMOD2(1:2).EQ.'3D' ) THEN
                IBID = 4
              ELSE
                GOTO 4
              ENDIF
              IF ( NOMOD2(IBID:IBID).EQ.'H' ) THEN
                IF ( NOMOD2(IBID:IBID+4).EQ.'HH2MD' .OR.
     &               NOMOD2(IBID:IBID+4).EQ.'HH2MS' .OR.
     &               NOMOD2(IBID:IBID+2).EQ.'HH2'   .OR.
     &               NOMOD2(IBID:IBID+1).EQ.'HH'    .OR.
     &               NOMOD2(IBID:IBID+2).EQ.'HHM'   .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'HHMD'  .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'HHMS'  .OR.
     &               NOMOD2(IBID:IBID+1).EQ.'HM'    .OR.
     &               NOMOD2(IBID:IBID+2).EQ.'HMD'   .OR.
     &               NOMOD2(IBID:IBID+2).EQ.'HMS' ) THEN
                  ICO = ICO + 1
                ENDIF
              ELSEIF ( NOMOD2(IBID:IBID).EQ.'T' ) THEN
                IF ( NOMOD2(IBID:IBID+2).EQ.'THH'    .OR.
     &               NOMOD2(IBID:IBID+4).EQ.'THH2D'  .OR.
     &               NOMOD2(IBID:IBID+5).EQ.'THH2MD' .OR.
     &               NOMOD2(IBID:IBID+5).EQ.'THH2MS' .OR.
     &               NOMOD2(IBID:IBID+4).EQ.'THH2S'  .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'THHD'   .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'THHM'   .OR.
     &               NOMOD2(IBID:IBID+4).EQ.'THHMD'  .OR.
     &               NOMOD2(IBID:IBID+4).EQ.'THHMS'  .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'THHS'   .OR.
     &               NOMOD2(IBID:IBID+2).EQ.'THM'    .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'THMD'   .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'THMS'   .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'THVD'   .OR.
     &               NOMOD2(IBID:IBID+3).EQ.'THVS' ) THEN
                  ICO = ICO + 1
                ENDIF
              ENDIF
            ENDIF
 4        CONTINUE
          REPK = K32BID
          IF  ( QUESTI.EQ.'MODELISATION' ) THEN
            IF (ICO.EQ.1) THEN
              REPK=NOMODL
            ENDIF
          ELSE
            IF (ICO.GT.0) THEN
              REPK='OUI'
            ENDIF
          ENDIF
 41       CONTINUE
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
     &         (QUESTI.EQ.'EXI_THM_CT').OR.(QUESTI.EQ.'EXI_THM_VR').OR.
     &         (QUESTI.EQ.'EXI_TUYAU' ).OR.(QUESTI.EQ.'EXI_COQ3D' ).OR.
     &         (QUESTI.EQ.'EXI_COQ1D' ).OR.(QUESTI.EQ.'EXI_PLAQUE').OR.
     &         (QUESTI.EQ.'EXI_COQUE' ).OR.(QUESTI.EQ.'EXI_GRILLE')
     &        ) THEN
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
         CALL U2MESK(CODMES,'UTILITAI_49',1,REPK)
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
      CALL ASSERT(CODMES.NE.'F')
C
C     -- SORTIE NORMALE :
C     ------------------
 9999 CONTINUE
      IERD=0
      REPKZ = REPK

C
      CALL JEDEMA()
      END
