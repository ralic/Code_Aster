      SUBROUTINE DISMLG(CODMES,QUESTI,NOMOBZ,REPI,REPKZ,IERD)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI,CODMES,REPKZ,NOMOBZ
C ----------------------------------------------------------------------
C MODIF UTILITAI  DATE 17/06/2003   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C     --     DISMOI(LIGREL)
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
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER DIMGE(3)
      LOGICAL MELANG
      CHARACTER*1 K1BID
      CHARACTER*8 KBID
      CHARACTER*16 NOMTE,PHENOM,NOMODL
      CHARACTER*19 NOMOB
      CHARACTER*32 REPK
C DEB ------------------------------------------------------------------

      CALL JEMARQ()
      NOMOB = NOMOBZ
      REPK = REPKZ

C     --------------------------------
      IF (QUESTI.EQ.'NOM_MAILLA') THEN
C     --------------------------------
        CALL JEVEUO(NOMOB//'.NOMA','L',IANOMA)
        REPK = ZK8(IANOMA)

C     -----------------------------------
      ELSE IF (QUESTI.EQ.'EXI_ELEM') THEN
C     -----------------------------------
        CALL JEEXIN(NOMOB//'.LIEL',IRET)
        REPK = 'NON'
        IF (IRET.GT.0) REPK = 'OUI'

C     -----------------------------------------------------------------
      ELSE IF ((QUESTI.EQ.'EXI_RDM') .OR. (QUESTI.EQ.'EXI_POUX') .OR.
     &         (QUESTI.EQ.'EXI_THM_CT') .OR.
     &         (QUESTI.EQ.'EXI_THM_VR') .OR.
     &         (QUESTI.EQ.'EXI_TUYAU') .OR. (QUESTI.EQ.'EXI_COQ3D') .OR.
     &         (QUESTI.EQ.'EXI_COQ1D') .OR.
     &         (QUESTI.EQ.'EXI_PLAQUE') .OR.
     &         (QUESTI.EQ.'EXI_GRAD_VARI') ) THEN
C     -----------------------------------------------------------------
        CALL JEEXIN(NOMOB//'.LIEL',IRET)
        IF (IRET.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',NBGREL,K1BID)
          REPK = 'NON'
          DO 10,IGREL = 1,NBGREL
            CALL JEVEUO(JEXNUM(NOMOB//'.LIEL',IGREL),'L',IALIEL)
            CALL JELIRA(JEXNUM(NOMOB//'.LIEL',IGREL),'LONMAX',NEL,K1BID)
            ITYPEL = ZI(IALIEL-1+NEL)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)

            IF (QUESTI.EQ.'EXI_RDM') THEN
              CALL DISMTE(CODMES,'MODELISATION',NOMTE,REPI,REPK,IERD)
              NOMODL = REPK(1:16)
              IF ((NOMODL.EQ.'DKT') .OR. (NOMODL.EQ.'DST') .OR.
     &            (NOMODL.EQ.'Q4G') .OR. (NOMODL(1:5).EQ.'CABLE') .OR.
     &            (NOMODL(1:4).EQ.'POU_') .OR. (NOMODL.EQ.'BARRE') .OR.
     &            (NOMODL(1:4).EQ.'DIS_') .OR.
     &            (NOMODL(1:5).EQ.'TUYAU') .OR.
     &            (NOMODL(3:7).EQ.'_DIS_') .OR.
     &            (NOMODL(1:6).EQ.'GRILLE') .OR.
     &            (NOMODL(1:5).EQ.'COQUE')) THEN
                REPK = 'OUI'
                GO TO 40
              END IF

            ELSE IF (QUESTI.EQ.'EXI_GRAD_VARI')THEN

              CALL DISMTE(CODMES,'MODELISATION',NOMTE,REPI,REPK,IERD)
              NOMODL = REPK(1:16)
              I7 = INDEX(NOMODL,'GRAD_VARI')
              IF (I7.NE.0) THEN
                REPK = 'OUI'
                GO TO 40
              END IF


            ELSE IF ((QUESTI.EQ.'EXI_COQ3D') .OR.
     &               (QUESTI.EQ.'EXI_COQ1D')) THEN

              CALL DISMTE(CODMES,'MODELISATION',NOMTE,REPI,REPK,IERD)
              NOMODL = REPK(1:16)

              IF (NOMODL(1:8).EQ.'COQUE_3D') THEN
                REPK = 'OUI'
                GO TO 40
              END IF

            ELSE IF (QUESTI.EQ.'EXI_PLAQUE') THEN
              CALL DISMTE(CODMES,'MODELISATION',NOMTE,REPI,REPK,IERD)
              NOMODL = REPK(1:16)
              IF ((NOMODL.EQ.'DKT') .OR. (NOMODL.EQ.'DST') .OR.
     &            (NOMODL.EQ.'Q4G')) THEN
                REPK = 'OUI'
                GO TO 40
              END IF

            ELSE IF (QUESTI.EQ.'EXI_TUYAU') THEN
              IF ((NOMTE.EQ.'MET3SEG3') .OR. (NOMTE.EQ.'MET3SEG4') .OR.
     &            (NOMTE.EQ.'MET6SEG3')) THEN
                REPK = 'OUI'
                GO TO 40
              END IF

            ELSE IF (QUESTI.EQ.'EXI_POUX') THEN
              IF ((NOMTE.EQ.'MECA_POU_D_E') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_EM') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_T') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_TG') .OR.
     &            (NOMTE.EQ.'MECA_POU_D_TGM') .OR.
     &            (NOMTE.EQ.'MECA_POU_C_T')) THEN
                REPK = 'OUI'
                GO TO 40
              END IF


            ELSE IF (QUESTI.EQ.'EXI_THM_CT') THEN
              I7 = INDEX(NOMTE,'THM_LI')
              IF (I7.NE.0) THEN
                REPK = 'OUI'
                GO TO 40
              ELSE
                IF (NOMTE(1:2).EQ.'SO') THEN
                  REPK = 'OUI'
                  GO TO 40
                END IF

              END IF
            ELSE
              IF (QUESTI.EQ.'EXI_THM_VR') THEN
                I7 = INDEX(NOMTE,'THM_NOSY')
                IF (I7.NE.0) THEN
                  REPK = 'OUI'
                  GO TO 40
                ELSE
                  IF (NOMTE(1:2).EQ.'SN') THEN
                    REPK = 'OUI'
                    GO TO 40
                  END IF
                END IF


              ELSE
                CALL UTMESS('F','DISMMO','STOP 1')
              END IF
            END IF
   10     CONTINUE
        ELSE
          REPK = 'NON'
        END IF

C     -------------------------------------
      ELSE IF (QUESTI.EQ.'NB_SS_ACTI') THEN
C     -------------------------------------
        CALL JEEXIN(NOMOB(1:8)//'.SSSA',IRET)
        IF (IRET.EQ.0) THEN
          REPI = 0
        ELSE
          CALL JEVEUO(NOMOB(1:8)//'.SSSA','L',IASSSA)
          CALL JELIRA(NOMOB(1:8)//'.SSSA','LONMAX',N1,KBID)
          REPI = ZI(IASSSA-1+N1-1)
        END IF

C     ---------------------------------------
      ELSE IF (QUESTI.EQ.'NB_NO_MAILLA') THEN
C     ---------------------------------------
        CALL JEVEUO(NOMOB//'.NOMA','L',IANOMA)
        CALL DISMMA(CODMES,QUESTI,ZK8(IANOMA),REPI,REPK,IERD)

C     ---------------------------------------
      ELSE IF (QUESTI.EQ.'NB_MA_MAILLA') THEN
C     ---------------------------------------
        CALL JEVEUO(NOMOB//'.NOMA','L',IANOMA)
        CALL DISMMA(CODMES,QUESTI,ZK8(IANOMA),REPI,REPK,IERD)

C     -----------------------------------
      ELSE IF (QUESTI.EQ.'DIM_GEOM') THEN
C     -----------------------------------
        REPI = 0
        CALL JEEXIN(NOMOB//'.LIEL',IRET)
        IF (IRET.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',NBGR,K1BID)
          IGE2 = 0
          DIMGE(1) = 0
          DIMGE(2) = 0
          DIMGE(3) = 0
          MELANG = .FALSE.
          DO 20,IGR = 1,NBGR
            CALL JEVEUO(JEXNUM(NOMOB//'.LIEL',IGR),'L',IAGREL)
            CALL JELIRA(JEXNUM(NOMOB//'.LIEL',IGR),'LONMAX',N1,K1BID)
            ITE = ZI(IAGREL-1+N1)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITE),NOMTE)
            CALL DISMTE(CODMES,QUESTI,NOMTE,IGE1,REPK,IERD)
            IF ((IGE1.LT.0) .OR. (IGE1.GT.3)) CALL UTMESS('F','DISMLG',
     &          'STOP 1')
            IF ((IGE2.EQ.0) .AND. (IGE1.NE.0)) IGE2 = IGE1
            IF ((IGE1*IGE2.GT.0) .AND. (IGE1.NE.IGE2)) MELANG = .TRUE.
            IF (IGE1.GT.0) DIMGE(IGE1) = 1
   20     CONTINUE
          IF (MELANG) THEN
            IGE3 = +100*DIMGE(1)
            IGE3 = IGE3 + 10*2*DIMGE(2)
            IGE3 = IGE3 + 1*3*DIMGE(3)
            IGE2 = IGE3
          END IF
          REPI = IGE2
        END IF
C        -- SI IL EXISTE DES MACRO-ELEMENTS : ON AJOUTE 1000
        CALL JEEXIN(NOMOB(1:8)//'.SSSA',IRET2)
        IF (IRET2.GT.0) REPI = 1000 + REPI

C     ----------------------------------
      ELSE IF (QUESTI.EQ.'NB_GREL') THEN
C     ----------------------------------
        CALL JEEXIN(NOMOB//'.LIEL',IRET)
        IF (IRET.GT.0) THEN
          CALL JELIRA(NOMOB//'.LIEL','NUTIOC',REPI,K1BID)
        ELSE
          REPI = 0
        END IF

C     ------------------------------------
      ELSE IF (QUESTI.EQ.'NB_MA_SUP') THEN
C     ------------------------------------
        CALL JEEXIN(NOMOB//'.NEMA',IRET)
        IF (IRET.GT.0) THEN
          CALL JELIRA(NOMOB//'.NEMA','NUTIOC',REPI,K1BID)
        ELSE
          REPI = 0
        END IF

C     -----------------------------------------
      ELSE IF (QUESTI.EQ.'NB_NO_SUP') THEN
C     -----------------------------------------
        CALL JEVEUO(NOMOB//'.NBNO','L',IANBNO)
        REPI = ZI(IANBNO)

C     -------------------------------------
      ELSE IF (QUESTI.EQ.'NOM_MODELE') THEN
C     -------------------------------------

C        -- DANGER : CETTE QUESTION EST RESOLUE DE FACON SALE
C           ON UTILISE UNE CONVENTION DE NOM NON ECRITE ..

        IF (NOMOB(9:19).EQ.'.MODELE') THEN
C           -- C'EST UN LIGREL DE MODELE
          REPK = NOMOB(1:8)
        ELSE IF (NOMOB(14:19).EQ.'.LIGRE') THEN
C           -- C'EST UN LIGREL DE CHARGE
          CALL JEVEUO(NOMOB(1:13)//'.MODEL.NOMO','L',IANOMO)
          REPK = ZK8(IANOMO)
        ELSE
C           -- CE N'EST NI UN LIGREL DE CHARGE, NI UN LIGREL DE MODELE
          REPK = ' '
        END IF

C     ------------------------------------
      ELSE IF (QUESTI.EQ.'PHENOMENE') THEN
C     ------------------------------------

        IF (NOMOB(9:15).EQ.'.MODELE') GO TO 30
        IF (NOMOB(9:13).EQ.'.LIGR') GO TO 30

C        SI C'EST UN LIGREL DE CHARGE, ON TROUVE LE PHENOMENE PAR
C        UNE CONVENTION DE NOM SUR LE LIGREL: 'TH' OU 'ME':

        IF (NOMOB(12:13).EQ.'TH') THEN
          REPK = 'THERMIQUE'
          GO TO 40
        ELSE IF (NOMOB(12:13).EQ.'ME') THEN
          REPK = 'MECANIQUE'
          GO TO 40
        ELSE IF (NOMOB(12:13).EQ.'AC') THEN
          REPK = 'ACOUSTIQUE'
          GO TO 40
        ELSE
          CALL UTMESS(CODMES,'DISMLG',
     &                'ON NE SAIT PAS ASSOCIER DE PHENOMENE A '//
     &                'CE LIGREL : '//NOMOB)
          IERD = 1
          GO TO 40
        END IF

   30   CONTINUE

C        SI C'EST UN LIGREL DE MODELE, ON REGARDE LE DOCU DU .NOMA :
        CALL JELIRA(NOMOB//'.NOMA','DOCU',IBID,PHENOM)
        IF (PHENOM(1:4).EQ.'MECA') THEN
          REPK = 'MECANIQUE'
        ELSE IF (PHENOM(1:4).EQ.'THER') THEN
          REPK = 'THERMIQUE'
        ELSE IF (PHENOM(1:4).EQ.'ACOU') THEN
          REPK = 'ACOUSTIQUE'
        ELSE IF (PHENOM(1:4).EQ.'NON_') THEN
          REPK = 'NON_LOCAL'
        ELSE
          CALL UTMESS(CODMES,'DISMLG','PHENOMENE INCONNU : '//PHENOM)
        END IF

C     ----
      ELSE
C     ----
        REPK = QUESTI
        CALL UTMESS(CODMES,'DISMLG:','LA QUESTION : "'//REPK//
     &              '" EST INCONNUE')
        IERD = 1
        GO TO 40
      END IF

   40 CONTINUE
      REPKZ = REPK
      CALL JEDEMA()
      END
