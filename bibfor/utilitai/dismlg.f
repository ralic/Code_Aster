      SUBROUTINE DISMLG(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI,REPKZ,NOMOBZ
C ----------------------------------------------------------------------
C MODIF UTILITAI  DATE 04/09/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C ======================================================================
C     --     DISMOI(LIGREL)
C    IN:
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
C    OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)

C ----------------------------------------------------------------------

      INTEGER DIMGE(3)
      LOGICAL MELANG,LTEATT
      CHARACTER*1 K1BID
      CHARACTER*8 KBID,CALCRI,MAILLA,NOMACR,MODELE
      CHARACTER*16 NOMTE,PHENOM,NOMODL,TYVOIS
      CHARACTER*19 NOMOB
      CHARACTER*32 REPK
      INTEGER JLGRF,IRET,NBGREL,IGREL,NEL,ITYPEL,JSSSA,N1
      INTEGER IGE2,IGR,JLIEL,ITE,IGE1,IGE3,NBGR
      INTEGER IEXI,IEXI2,JPART,ICO
      INTEGER JNOMAC,NBSM,ISM,JNBNO,IBID,DIMGE1
C DEB ------------------------------------------------------------------

      CALL JEMARQ()
      REPK  = ' '
      REPI  = 0
      IERD = 0

      NOMOB=NOMOBZ

C     --------------------------------
      IF (QUESTI.EQ.'NOM_MAILLA') THEN
C     --------------------------------
        CALL JEVEUO(NOMOB//'.LGRF','L',JLGRF)
        REPK=ZK8(JLGRF-1+1)

C     --------------------------------
      ELSEIF (QUESTI.EQ.'PARTITION') THEN
C     --------------------------------
        REPK=' '
        CALL JEVEUO(NOMOB//'.LGRF','L',JLGRF)
        MODELE=ZK8(JLGRF-1+2)
        IF (MODELE.NE.' ') THEN
          CALL JEEXIN(MODELE//'.PARTIT',IEXI)
          IF (IEXI.GT.0) THEN
            CALL JEVEUO(MODELE//'.PARTIT','L',JPART)
            REPK=ZK8(JPART-1+1)
          ENDIF
        ENDIF

C     -----------------------------------
      ELSEIF (QUESTI.EQ.'EXI_ELEM') THEN
C     -----------------------------------
        CALL JEEXIN(NOMOB//'.LIEL',IEXI)
        REPK='NON'
        IF (IEXI.GT.0)REPK='OUI'


C     -----------------------------------------------------------------
      ELSEIF ((QUESTI.EQ.'EXI_VF')) THEN
C     -----------------------------------------------------------------
        REPK='NON'
        CALL JEEXIN(NOMOB//'.LIEL',IEXI)
        IF (IEXI.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',NBGREL,K1BID)
          DO 10,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOMOB//'.LIEL',IGREL),'L',JLIEL)
            CALL JELIRA(JEXNUM(NOMOB//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL=ZI(JLIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            IF (LTEATT(NOMTE,'VF_AVEC_VOISIN','OUI')) THEN
              REPK='OUI'
              GOTO 10

            ENDIF
   10     CONTINUE
        ENDIF

C     -----------------------------------------------------------------
      ELSEIF ((QUESTI.EQ.'BESOIN_VOISIN')) THEN
C     -----------------------------------------------------------------
        REPK='NON'
        CALL JEEXIN(NOMOB//'.LIEL',IEXI)
        IF (IEXI.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',NBGREL,K1BID)
          DO 20,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOMOB//'.LIEL',IGREL),'L',JLIEL)
            CALL JELIRA(JEXNUM(NOMOB//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL=ZI(JLIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL TEATTR(NOMTE,'C','TYPE_VOISIN',TYVOIS,IRET)
            IF (IRET.EQ.0) THEN
              REPK='OUI'
              GOTO 20

            ENDIF
   20     CONTINUE
        ENDIF



C     -----------------------------------------------------------------
      ELSEIF ((QUESTI.EQ.'EXI_RDM') .OR. (QUESTI.EQ.'EXI_POUX') .OR.
     &        (QUESTI(1:7).EQ.'EXI_THM') .OR.
     &        (QUESTI.EQ.'EXI_TUYAU') .OR. (QUESTI.EQ.'EXI_COQ3D') .OR.
     &        (QUESTI.EQ.'EXI_COQ1D') .OR. (QUESTI.EQ.'EXI_GRILLE') .OR.
     &        (QUESTI.EQ.'EXI_PLAQUE') .OR. (QUESTI.EQ.'EXI_COQUE') .OR.
     &        (QUESTI.EQ.'CALC_RIGI') .OR. (QUESTI.EQ.'EXI_STRX')) THEN

C     -----------------------------------------------------------------
        CALL JEEXIN(NOMOB//'.LIEL',IEXI)
        IF (IEXI.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',NBGREL,K1BID)
          REPK='NON'
          DO 30,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOMOB//'.LIEL',IGREL),'L',JLIEL)
            CALL JELIRA(JEXNUM(NOMOB//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL=ZI(JLIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)

            IF (QUESTI.EQ.'EXI_RDM') THEN
              CALL DISMTE('MODELISATION',NOMTE,REPI,NOMODL,IERD)
              IF ((NOMODL(1:3).EQ.'DKT') .OR.
     &            (NOMODL(1:3).EQ.'DST') .OR.
     &            (NOMODL(1:3).EQ.'Q4G') .OR.
     &            (NOMODL(1:5).EQ.'CABLE') .OR.
     &            (NOMODL(1:4).EQ.'POU_') .OR.
     &            (NOMODL(1:5).EQ.'BARRE') .OR.
     &            (NOMODL(1:4).EQ.'DIS_') .OR.
     &            (NOMODL(1:5).EQ.'TUYAU') .OR.
     &            (NOMODL(3:7).EQ.'_DIS_') .OR.
     &            (NOMODL(1:6).EQ.'GRILLE') .OR.
     &            (NOMODL(1:5).EQ.'COQUE')) THEN
                REPK='OUI'
                GOTO 110

              ENDIF

            ELSEIF (QUESTI.EQ.'CALC_RIGI') THEN
              REPK='NON'
              CALL DISMTE(QUESTI,NOMTE,REPI,CALCRI,IERD)
              IF (CALCRI.EQ.'OUI') THEN
                REPK='OUI'
                GOTO 110

              ENDIF

            ELSEIF (QUESTI.EQ.'EXI_COQUE') THEN
              CALL DISMTE('MODELISATION',NOMTE,REPI,NOMODL,IERD)
              IF (NOMODL(1:5).EQ.'COQUE') THEN
                REPK='OUI'
                GOTO 110

              ENDIF

            ELSEIF (QUESTI.EQ.'EXI_GRILLE') THEN
              CALL DISMTE('MODELISATION',NOMTE,REPI,NOMODL,IERD)
              REPK='NON'
              IF (NOMODL(1:6).EQ.'GRILLE') THEN
                REPK='OUI'
                GOTO 110

              ENDIF

            ELSEIF ((QUESTI.EQ.'EXI_COQ3D') .OR.
     &              (QUESTI.EQ.'EXI_COQ1D')) THEN
              CALL DISMTE('MODELISATION',NOMTE,REPI,NOMODL,IERD)
              IF (NOMODL(1:8).EQ.'COQUE_3D') THEN
                REPK='OUI'
                GOTO 110

              ENDIF

            ELSEIF (QUESTI.EQ.'EXI_PLAQUE') THEN
              CALL DISMTE('MODELISATION',NOMTE,REPI,NOMODL,IERD)
              IF ((NOMODL(1:3).EQ.'DKT') .OR.
     &            (NOMODL(1:3).EQ.'DST') .OR.
     &            (NOMODL(1:3).EQ.'Q4G')) THEN
                REPK='OUI'
                GOTO 110

              ENDIF

            ELSEIF (QUESTI.EQ.'EXI_TUYAU') THEN
              IF ((NOMTE.EQ.'MET3SEG3') .OR. (NOMTE.EQ.'MET3SEG4') .OR.
     &            (NOMTE.EQ.'MET6SEG3')) THEN
                REPK='OUI'
                GOTO 110

              ENDIF

            ELSEIF (QUESTI.EQ.'EXI_POUX') THEN
              IF ((NOMTE.EQ.'MECA_POU_D_E') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_EM') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_T') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_TG') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_TGM') .OR.
     &            (NOMTE.EQ.'MECA_POU_C_T')) THEN
                REPK='OUI'
                GOTO 110

              ENDIF

            ELSEIF (QUESTI.EQ.'EXI_STRX') THEN
              IF ((NOMTE.EQ.'MECA_POU_D_EM') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_TGM')) THEN
                REPK='OUI'
                GOTO 110

              ENDIF


            ELSEIF (QUESTI.EQ.'EXI_THM') THEN
              IF (LTEATT(NOMTE,'THM','OUI')) THEN
                REPK='OUI'
                IF ((NOMTE.EQ.'HM_D_PLAN_SE3_P') .OR.
     &              (NOMTE.EQ.'HM_DPQ8_P') .OR.
     &              (NOMTE.EQ.'HM_DPTR6_P')) THEN
                  REPK='OUI_P'
                ENDIF
                GOTO 110

              ENDIF

            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
   30     CONTINUE
        ELSE
          REPK='NON'
        ENDIF

C     ------------------------------------------
      ELSEIF ((QUESTI.EQ.'NB_SM_MAILLA') .OR.
     &        (QUESTI.EQ.'NB_SS_ACTI') .OR.
     &        (QUESTI.EQ.'NB_NL_MAILLA')) THEN
C     ------------------------------------------
        CALL JEEXIN(NOMOB//'.SSSA',IEXI)
        IF (IEXI.EQ.0) THEN
          REPI=0
        ELSE
          CALL JEVEUO(NOMOB//'.SSSA','L',JSSSA)
          CALL JELIRA(NOMOB//'.SSSA','LONMAX',N1,KBID)
          IF (QUESTI.EQ.'NB_SM_MAILLA') THEN
            REPI=ZI(JSSSA-1+N1-2)
          ELSEIF (QUESTI.EQ.'NB_SS_ACTI') THEN
            REPI=ZI(JSSSA-1+N1-1)
          ELSEIF (QUESTI.EQ.'NB_NL_MAILLA') THEN
            REPI=ZI(JSSSA-1+N1)
          ENDIF
        ENDIF

C     ---------------------------------------
      ELSEIF (QUESTI.EQ.'NB_NO_MAILLA') THEN
C     ---------------------------------------
        CALL JEVEUO(NOMOB//'.LGRF','L',JLGRF)
        CALL DISMMA(QUESTI,ZK8(JLGRF),REPI,REPK,IERD)

C     ---------------------------------------
      ELSEIF (QUESTI.EQ.'NB_MA_MAILLA') THEN
C     ---------------------------------------
        CALL JEVEUO(NOMOB//'.LGRF','L',JLGRF)
        CALL DISMMA(QUESTI,ZK8(JLGRF),REPI,REPK,IERD)

C     -----------------------------------
      ELSEIF (QUESTI.EQ.'DIM_GEOM') THEN
C     -----------------------------------
        REPI=0
        IGE2=0
        CALL JEEXIN(NOMOB//'.LIEL',IEXI)
        IF (IEXI.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',NBGR,K1BID)
          DIMGE(1)=0
          DIMGE(2)=0
          DIMGE(3)=0
          MELANG=.FALSE.
          DO 40,IGR=1,NBGR
            CALL JEVEUO(JEXNUM(NOMOB//'.LIEL',IGR),'L',JLIEL)
            CALL JELIRA(JEXNUM(NOMOB//'.LIEL',IGR),'LONMAX',N1,K1BID)
            ITE=ZI(JLIEL-1+N1)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITE),NOMTE)
            CALL DISMTE(QUESTI,NOMTE,IGE1,REPK,IERD)
            CALL ASSERT((IGE1.GE.0) .AND. (IGE1.LE.3))
            IF ((IGE2.EQ.0) .AND. (IGE1.NE.0))IGE2=IGE1
            IF ((IGE1*IGE2.GT.0) .AND. (IGE1.NE.IGE2))MELANG=.TRUE.
            IF (IGE1.GT.0)DIMGE(IGE1)=1
   40     CONTINUE
          IF (MELANG) THEN
            IGE3=+100*DIMGE(1)
            IGE3=IGE3+10*2*DIMGE(2)
            IGE3=IGE3+1*3*DIMGE(3)
            IGE2=IGE3
          ENDIF
        ENDIF
C        -- SI IL EXISTE DES MACRO-ELEMENTS, IL FAUT EN TENIR COMPTE :
        CALL JEEXIN(NOMOB//'.SSSA',IEXI2)
        IF (IEXI2.GT.0) THEN
          CALL JELIRA(NOMOB//'.SSSA','LONMAX',N1,KBID)
          CALL JEVEUO(NOMOB//'.SSSA','L',JSSSA)
          CALL JEVEUO(NOMOB//'.LGRF','L',JLGRF)
          MAILLA=ZK8(JLGRF-1+1)
          CALL JEVEUO(MAILLA//'.NOMACR','L',JNOMAC)
          NBSM=N1-3
          DO 50,ISM=1,NBSM
            IF (ZI(JSSSA-1+ISM).EQ.1) THEN
              NOMACR=ZK8(JNOMAC-1+ISM)
              CALL DISMML(QUESTI,NOMACR,IGE1,REPK,IERD)
              CALL ASSERT(IGE1.GE.0 .AND. IGE1.LE.123)
              IF (IGE2.NE.IGE1) THEN
                IGE2=DIMGE1(IGE2,IGE1)
              ENDIF
            ENDIF
   50     CONTINUE
        ENDIF
        REPI=IGE2


C     ----------------------------------
      ELSEIF (QUESTI.EQ.'NB_GREL') THEN
C     ----------------------------------
        CALL JEEXIN(NOMOB//'.LIEL',IEXI)
        IF (IEXI.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',REPI,K1BID)
        ELSE
          REPI=0
        ENDIF

C     ------------------------------------
      ELSEIF (QUESTI.EQ.'NB_MA_SUP') THEN
C     ------------------------------------
        CALL JEEXIN(NOMOB//'.NEMA',IEXI)
        IF (IEXI.GT.0) THEN
          CALL JELIRA(NOMOB//'.NEMA','NUTIOC',REPI,K1BID)
        ELSE
          REPI=0
        ENDIF

C     -----------------------------------------
      ELSEIF (QUESTI.EQ.'NB_NO_SUP') THEN
C     -----------------------------------------
        CALL JEVEUO(NOMOB//'.NBNO','L',JNBNO)
        REPI=ZI(JNBNO)

C     -------------------------------------
      ELSEIF (QUESTI.EQ.'NOM_MODELE') THEN
C     -------------------------------------
        CALL JEVEUO(NOMOB//'.LGRF','L',JLGRF)
        REPK=ZK8(JLGRF-1+2)

C     ------------------------------------
      ELSEIF (QUESTI.EQ.'PHENOMENE') THEN
C     ------------------------------------
        CALL JELIRA(NOMOB//'.LGRF','DOCU',IBID,PHENOM)
        IF (PHENOM(1:4).EQ.'MECA') THEN
          REPK='MECANIQUE'
        ELSEIF (PHENOM(1:4).EQ.'THER') THEN
          REPK='THERMIQUE'
        ELSEIF (PHENOM(1:4).EQ.'ACOU') THEN
          REPK='ACOUSTIQUE'
        ELSE
          CALL U2MESK('F','UTILITAI_63',1,PHENOM)
        ENDIF


C     -----------------------------------------------------------------
      ELSEIF ((QUESTI.EQ.'EXI_AXIS'.OR.QUESTI.EQ.'AXIS')) THEN
C     -----------------------------------------------------------------
        REPK='NON'
        CALL JEEXIN(NOMOB//'.LIEL',IEXI)
        ICO=0
        IF (IEXI.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',NBGREL,K1BID)
          DO 60,IGREL=1,NBGREL
            CALL JEVEUO(JEXNUM(NOMOB//'.LIEL',IGREL),'L',JLIEL)
            CALL JELIRA(JEXNUM(NOMOB//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL=ZI(JLIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            IF (LTEATT(NOMTE,'AXIS','OUI')) THEN
              ICO=ICO+1
            ENDIF
   60     CONTINUE
          IF (QUESTI.EQ.'EXI_AXIS' .AND. ICO.GT.0)REPK='OUI'
          IF (QUESTI.EQ.'AXIS' .AND. ICO.EQ.NBGREL)REPK='OUI'
        ENDIF


C     ------------------------------------
      ELSEIF (QUESTI.EQ.'EXI_AMOR') THEN
C     ------------------------------------
        REPK='NON'
C        -- SI IL EXISTE DES ELEMENTS "ABSORBANT" :
        CALL JELIRA(NOMOB//'.LIEL','NUTIOC',NBGREL,KBID)
        DO 70 IGREL=1,NBGREL
          CALL JEVEUO(JEXNUM(NOMOB//'.LIEL',IGREL),'L',JLIEL)
          CALL JELIRA(JEXNUM(NOMOB//'.LIEL',IGREL),'LONMAX',NEL,KBID)
          ITYPEL=ZI(JLIEL-1+NEL)
          CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
          IF ((NOMTE(1:9).EQ.'MEAB_FACE') .OR.
     &        (NOMTE(1:6).EQ.'MEPASE')) THEN
            REPK='OUI'
            GOTO 80

          ENDIF
   70   CONTINUE
   80   CONTINUE
C        -- SI IL EXISTE DES MACRO-ELEMENTS, IL FAUT LES EXAMINER :
        IF (REPK.EQ.'NON') THEN
          CALL JEEXIN(NOMOB//'.SSSA',IEXI2)
          IF (IEXI2.GT.0) THEN
            CALL JELIRA(NOMOB//'.SSSA','LONMAX',N1,KBID)
            CALL JEVEUO(NOMOB//'.SSSA','L',JSSSA)
            CALL JEVEUO(NOMOB//'.LGRF','L',JLGRF)
            MAILLA=ZK8(JLGRF-1+1)
            CALL JEVEUO(MAILLA//'.NOMACR','L',JNOMAC)
            NBSM=N1-3
            DO 90,ISM=1,NBSM
              IF (ZI(JSSSA-1+ISM).EQ.1) THEN
                NOMACR=ZK8(JNOMAC-1+ISM)
                CALL DISMML(QUESTI,NOMACR,IBID,REPK,IERD)
                IF (REPK.EQ.'OUI')GOTO 100
              ENDIF
   90       CONTINUE
          ENDIF
        ENDIF
  100   CONTINUE

C     ----
      ELSE
C     ----
        IERD=1
      ENDIF

  110 CONTINUE
      REPKZ=REPK
      CALL JEDEMA()
      END
