      SUBROUTINE PJXXCO(TYPCAL,METHOD,LCORRE,ISOLE,
     &                  RESUIN,CHAM1,
     &                  MOA1,MOA2,
     &                  NOMA1,NOMA2,CNREF)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C
C COMMANDE:  PROJ_CHAMP
C BUT : ROUTINE "CHAPEAU" CONCERNANT LA SD LCORRESP_2_MAILLA
C
C  ON REGARDE LES TYPES DE CHAMPS A PROJETER
C    ON EMET DES MESSAGES D'ALARME SI LA METHODE NE PEUT LES PROJETER
C    (EX. : 'COLLOCATION' NE SAIT PAS TRAITER LES CHAM_ELEM ELGA)
C
C  SI TOUT EST COHERENT, ON APPELLE :
C    PJEFCO VIA LE 1ER ARGT DE LA SD LCORRESP_2_MAILLA ('COLLOCATION')
C    PJELCO VIA LE 2ND ARGT DE LA SD LCORRESP_2_MAILLA ('ECLA_PG')
C
C  LE CAS DE LA METHODE 'NUAGE_DEG' EST PLUS PARTICULIER :
C    ON FAIT DONC UN TEST A PART
C
      IMPLICIT   NONE
C
C 0.1. ==> ARGUMENTS
C

      LOGICAL ISOLE
      CHARACTER*4 TYPCAL
      CHARACTER*8 RESUIN
      CHARACTER*8 MOA1,MOA2,CORRU
      CHARACTER*8 NOMA1,NOMA2,CNREF
      CHARACTER*16 LCORRE(2),K16BID,CORTMP
      CHARACTER*19 CHAM1,METHOD

C
C 0.2. ==> COMMUNS
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24,VALK(2)
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C 0.3. ==> VARIABLES LOCALES
C
C
      LOGICAL LNOEU,LELNO,LELEM,LELGA


C DEB ------------------------------------------------------------------
      CALL JEMARQ()



C 1. CAS DE LA METHODE 'NUAGE_DEG'
C --------------------------------

      IF (METHOD(1:10).EQ.'NUAGE_DEG_') THEN
        CALL ASSERT(TYPCAL.EQ.'1ET2')
        CALL PJNGCO(LCORRE(1),NOMA1,NOMA2,METHOD,CNREF,'V')

      ELSE

C 2. AUTRE METHODE
C --------------------------------


C       -- SI TYPCAL='1' => 'COLLOCATION' OU 'COUPLAGE' SEULEMENT :
        IF (TYPCAL.EQ.'1') THEN
          CALL ASSERT(RESUIN.EQ.' ' .AND. CHAM1.EQ.' ')
          CALL ASSERT(METHOD.EQ.'COLLOCATION'.OR.METHOD.EQ.'COUPLAGE')
          CALL GETRES(CORRU,K16BID,K16BID)
          CORTMP='&&PJXXCO.CORRES'
          IF (METHOD.EQ.'COLLOCATION') THEN
            CALL PJEFCO(MOA1,MOA2,CORTMP,'V')
          ELSE IF (METHOD.EQ.'COUPLAGE') THEN
C   METHODE POUR LE COUPLAGE VIA YACS AVEC SATURENE POUR IFS
            CALL PJEFTC(NOMA1,NOMA2,CORTMP,'V')
          ENDIF
          CALL COPISD('CORRESP_2_MAILLA','G',CORTMP,CORRU)
          CALL DETRSD('CORRESP_2_MAILLA',CORTMP)


        ELSE
          CALL ASSERT(TYPCAL.EQ.'1ET2')

C         -- QUELS SONT LES TYPES DE CHAMPS A PROJETER ?
          CALL PJTYCO(ISOLE,RESUIN,CHAM1,
     &                LNOEU,LELNO,LELEM,LELGA)


C         -- VERIFICATION DE LA COHERENCE DE LA DEMANDE
C         -- FORMULEE PAR L'UTILISATEUR
          IF ((METHOD.EQ.'ECLA_PG') .AND. (.NOT.LELGA)) THEN
            VALK(1) = METHOD
            VALK(2) = CHAM1
            CALL U2MESK('F','CALCULEL5_33', 2 ,VALK)
          ENDIF

          IF (       (METHOD.EQ.'COLLOCATION')
     &         .AND. (.NOT.LNOEU)
     &         .AND. (.NOT.LELNO)
     &         .AND. (.NOT.LELEM)) THEN
            VALK(1) = METHOD
            VALK(2) = CHAM1
            CALL U2MESK('F','CALCULEL5_33', 2 ,VALK)
          ENDIF



C         -- ON UTILISE LCORRE(1) OU LCORRE(2) SUIVANT LE TYPE DE CHAMP

          IF ((LNOEU) .OR. (LELNO) .OR. (LELEM)) THEN
            CALL PJEFCO(MOA1,MOA2,LCORRE(1),'V')
          ENDIF


          IF (LELGA .AND. ISOLE) THEN
            IF ((METHOD.EQ.'ECLA_PG') .OR. (METHOD.EQ.'AUTO')) THEN
              CALL PJELCO(MOA1,MOA2,CHAM1,LCORRE(2),'V')
            ENDIF
          ENDIF
        ENDIF
      ENDIF



      CALL JEDEMA()
      END
