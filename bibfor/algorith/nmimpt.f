      SUBROUTINE NMIMPT(SDIMPR,NATURZ,TITRE ,LIGNE ,ARGR  ,
     &                  ARGI  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      CHARACTER*24  SDIMPR
      CHARACTER*(*) NATURZ
      CHARACTER*(*) LIGNE,TITRE(3)
      REAL*8        ARGR(*)
      INTEGER       ARGI(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (AFFICHAGE)
C
C IMPRESSION DE L'EN-TETE
C
C ----------------------------------------------------------------------
C
C
C IN  SDIMPR : SD AFFICHAGE
C IN  NATURE : NATURE DU CALCUL
C             IMPLICITE  : CALCUL STATIQUE ET DYNAMIQUE IMPLICITE
C             EXPLICITE  : CALCUL DYNAMIQUE EXPLICITE
C             FLAMBSTA   : CALCUL DES MODES DE FLAMBEMENT EN STATIQUE
C             FLAMBDYN   : CALCUL DES MODES DE FLAMBEMENT EN DYNAMIQUE
C             VIBRDYNA   : CALCUL DES MODES VIBRATOIRES
C IN  TITRE  : EN-TETE DU TABLEAU DE CONVERGENCE
C IN  LIGNE  : LIGNE POINTILLEE DE SEPARATION
C IN  ARGR   : ARGUMENTS DE TYPE REEL
C IN  ARGI   : ARGUMENTS EVENTUELS DE TYPE ENTIER
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C

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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*24     IMPINF
      INTEGER          JIMPIN
      INTEGER          LARGE
      CHARACTER*9      NATURE
      REAL*8           TIME
      INTEGER          ILIGNE,IBID,LENIVO
      INTEGER          IFM,IUNIFI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      NATURE = NATURZ
      TIME   = ARGR(1)
      IFM    = IUNIFI('MESSAGE')
C
C --- RECUPERATION ET VERIFICATION DES PARAMETRES
C
      IMPINF = SDIMPR(1:14)//'.INFO'
      CALL JEVEUO(IMPINF,'L',JIMPIN)
      LARGE  = ZI(JIMPIN-1+2)
C
C --- LIGNE DE SEPARATION
C
      CALL AFFICH('MESSAGE',' ')
      CALL NMIMPX(SDIMPR,IFM   ,LIGNE)
      CALL AFFICH('MESSAGE',' ')
C
C --- ENTETE
C
      IF (NATURE.EQ.'IMPLICITE') THEN
        LENIVO = ARGI(1)
        IF (LENIVO.EQ.0) THEN
          CALL U2MESR('I','MECANONLINE6_6',1,TIME)
        ELSE
          CALL U2MESG('I','MECANONLINE6_1',0,' ',1,LENIVO,1,TIME)
        ENDIF
        CALL IMPSDR(SDIMPR,'INCR_INST',' ',TIME,IBID)
        CALL AFFICH('MESSAGE',' ')
        CALL NMIMPX(SDIMPR,IFM   ,LIGNE)
        DO 30 ILIGNE = 1,3
          CALL IMPFOK(TITRE(ILIGNE),LARGE,IFM   )
 30     CONTINUE
        CALL NMIMPX(SDIMPR,IFM   ,LIGNE)
      ELSEIF (NATURE.EQ.'EXPLICITE') THEN
        LENIVO = ARGI(1)
        IF (LENIVO.EQ.0) THEN
          CALL U2MESR('I','MECANONLINE6_6',1,TIME)
        ELSE
          CALL U2MESG('I','MECANONLINE6_1',0,' ',1,LENIVO,1,TIME)
        ENDIF
        CALL AFFICH('MESSAGE',' ')
      ELSEIF (NATURE.EQ.'FLAMBSTA') THEN
        CALL U2MESS('I','MECANONLINE6_2')
        CALL AFFICH('MESSAGE',' ')
      ELSEIF (NATURE.EQ.'FLAMBDYN') THEN
        CALL U2MESS('I','MECANONLINE6_2')
        CALL AFFICH('MESSAGE',' ')
      ELSEIF (NATURE.EQ.'VIBRDYNA') THEN
        CALL U2MESS('I','MECANONLINE6_3')
        CALL AFFICH('MESSAGE',' ')
      ELSE
        WRITE(6,*) 'NATURE: ',NATURE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END
