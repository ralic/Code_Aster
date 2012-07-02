      SUBROUTINE CHPCHD(CHIN,TYPE,CELMOD,PROL0,BASE,CHOU)
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) CHIN,CHOU,BASE,CELMOD,TYPE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE PELLET J.PELLET
C -----------------------------------------------------------------
C  BUT : CHANGER LE SUPPORT GEOMETRIQUE D'UN CHAMP
C -----------------------------------------------------------------

C CHIN IN/JXIN  K19 : NOM DU CHAMP A CHANGER
C                     TYPE AUTORISE POUR CHIN :
C        NOEU, CART, ELGA, ELNO, CESE
C
C CHOU IN/JXOUT K19 : NOM DU CHAMP RESULTAT
C BASE IN       K1  : BASE DE CREATION DE CHOU : /'G' / 'V'
C TYPE IN       K19 : TYPE DE SUPPORT GEOMETRIQUE VOULU POUR CHOU
C                     /'NOEU' /'CART' /'ELNO' /ELGA' /'ELEM'

C ARGUMENTS UTILISES SI TYPE=ELNO/ELGA/ELEM :
C   PROL0 IN   K3  :
C        /'OUI' : LE CHAM_ELEM CHOU EST PROLONGE
C         PAR DES VALEURS NULLES LA OU IL N'EST PAS DEFINI.
C        /'NON' : ERREUR <F> SI IL EXISTE DES
C         DES VALEURS DE CHOU QUI NE SONT PAS AFFECTEES DANS CHIN
C   CELMOD IN/JXIN  K19 : NOM D'UN CHAM_ELEM "MODELE" SI TYPE='EL..'

C  LES CAS TRAITES AUJOURD'HUI SONT :

C         /'NOEU->ELNO'   : CHAM_NO -> CHAM_ELEM/ELNO
C         /'NOEU->ELGA'   : CHAM_NO -> CHAM_ELEM/ELGA

C         /'CART->ELEM'   : CARTE   -> CHAM_ELEM/ELEM
C         /'CART->ELGA'   : CARTE   -> CHAM_ELEM/ELGA
C         /'CART->ELNO'   : CARTE   -> CHAM_ELEM/ELNO
C         /'CART->NOEU'   : CARTE   -> CHAM_NO

C         /'ELGA->NOEU'   : CHAM_ELEM/ELGA    -> CHAM_NO
C         /'ELGA->ELNO'   : CHAM_ELEM/ELGA    -> CHAM_ELEM/ELNO
C         /'ELNO->NOEU'   : CHAM_ELEM/ELNO    -> CHAM_NO
C         /'ELNO->ELGA'   : CHAM_ELEM/ELNO    -> CHAM_ELEM/ELGA

C         /'CESE->ELNO'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELNO
C         /'CESE->ELGA'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELGA
C         /'CESE->ELEM'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELEM
C -----------------------------------------------------------------

      INTEGER IB,IRET,NNCP,IBID
      CHARACTER*3 PROL0
      CHARACTER*8 MA,MA2,TYCHI,NOMGD,PARAM,MOIN
      CHARACTER*16 CAS,OPTION,NOMCMD,KBID
      CHARACTER*19 CESMOD,CES1,CNS1,MNOGA,LIGREL,CES2
      CHARACTER*24 VALK(4)

C     ------------------------------------------------------------------
      MNOGA = '&&CHPCHD.MANOGA'
      CESMOD = '&&CHPCHD.CESMOD'


C 1- CALCUL DE:
C      MA    : MAILLAGE ASSOCIE A CHIN
C      TYCHI : TYPE DU CHAMP CHIN (CART/NOEU/ELNO/ELGA/CESE)
C      NOMGD : NOM DE LA GRANDEUR ASSOCIEE A CHIN
C ------------------------------------------------------------------

      CALL DISMOI('F','NOM_MAILLA',CHIN,'CHAMP',IB,MA,IB)
      CALL DISMOI('F','TYPE_CHAMP',CHIN,'CHAMP',IB,TYCHI,IB)
      CALL DISMOI('F','NOM_GD',CHIN,'CHAMP',IB,NOMGD,IB)
      CALL ASSERT(TYCHI.EQ.'NOEU'.OR.TYCHI.EQ.'CART'.OR.
     &            TYCHI.EQ.'ELNO'.OR.TYCHI.EQ.'ELGA'.OR.
     &            TYCHI.EQ.'CESE')


C 2.  -- SI TYPE = 'EL..' : ON CREE UN CHAM_ELEM_S "MODELE" : CESMOD
C         LIGREL: NOM DU LIGREL ASSOCIE A CHOU
C ---------------------------------------------------------------
      IF (TYPE(1:2).EQ.'EL') THEN
        CALL ASSERT(CELMOD.NE.' ')
        CALL DISMOI('F','NOM_LIGREL',CELMOD,'CHAM_ELEM',IB,LIGREL,IB)
        CALL DISMOI('F','NOM_OPTION',CELMOD,'CHAM_ELEM',IB,OPTION,IB)
        CALL DISMOI('F','NOM_PARAM',CELMOD,'CHAM_ELEM',IB,PARAM,IB)
        CALL DISMOI('F','NOM_MAILLA',LIGREL,'LIGREL',IB,MA2,IB)
        IF (MA.NE.MA2) THEN
          CALL DISMOI('F','NOM_MODELE',LIGREL,'LIGREL',IB,MOIN,IB)
          VALK(1) = CHIN
          VALK(2) = MOIN
          VALK(3) = MA
          VALK(4) = MA2
          CALL U2MESK('F','CALCULEL4_59',4,VALK)
        ENDIF
        CALL CELCES(CELMOD,'V',CESMOD)
      ENDIF


C 3.  -- CALCUL DE CAS :
C ---------------------------------------
C     SONT TRAITES AUJOURD'HUI :

C         /'NOEU->ELNO'   : CHAM_NO -> CHAM_ELEM/ELNO
C         /'NOEU->ELGA'   : CHAM_NO -> CHAM_ELEM/ELGA

C         /'CART->ELEM'   : CARTE   -> CHAM_ELEM/ELEM
C         /'CART->ELGA'   : CARTE   -> CHAM_ELEM/ELGA
C         /'CART->ELNO'   : CARTE   -> CHAM_ELEM/ELNO
C         /'CART->NOEU'   : CARTE   -> CHAM_NO

C         /'ELGA->NOEU'   : CHAM_ELEM/ELGA    -> CHAM_NO
C         /'ELGA->ELNO'   : CHAM_ELEM/ELGA    -> CHAM_ELEM/ELNO
C         /'ELNO->NOEU'   : CHAM_ELEM/ELNO    -> CHAM_NO
C         /'ELNO->ELGA'   : CHAM_ELEM/ELNO    -> CHAM_ELEM/ELGA

C         /'CESE->ELNO'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELNO
C         /'CESE->ELGA'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELGA
C         /'CESE->ELEM'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELEM

      CAS = ' '
      CAS(1:4) = TYCHI
      CAS(5:6) = '->'
      CAS(7:10) = TYPE


C 4.  TRAITEMENT DES DIFFERENTS CAS DE FIGURE :
C ----------------------------------------------
      NNCP = 0
      IF (CAS.EQ.'NOEU->ELGA') THEN
C     ----------------------------------
        CNS1 = '&&CHPCHD.CNS1'
        CES1 = '&&CHPCHD.CES1'
        CALL MANOPG(LIGREL,OPTION,PARAM,MNOGA)

        CALL CNOCNS(CHIN,'V',CNS1)
        CALL CNSCES(CNS1,'ELGA',CESMOD,MNOGA,'V',CES1)
        CALL DETRSD('CHAM_NO_S',CNS1)
        CALL DETRSD('CHAM_ELEM_S',MNOGA)

        CALL CESCEL(CES1,LIGREL,OPTION,PARAM,PROL0,NNCP,BASE,CHOU,'F',
     &              IBID)
        CALL DETRSD('CHAM_ELEM_S',CES1)


      ELSEIF (CAS.EQ.'ELNO->ELGA') THEN
C     ----------------------------------
        CES1 = '&&CHPCHD.CES1'
        CES2 = '&&CHPCHD.CES2'
        CALL MANOPG(LIGREL,OPTION,PARAM,MNOGA)

        CALL CELCES(CHIN,'V',CES1)
        CALL CESCES(CES1,'ELGA',CESMOD,MNOGA,' ','V',CES2)
        CALL DETRSD('CHAM_ELEM_S',CES1)
        CALL DETRSD('CHAM_ELEM_S',MNOGA)

        CALL CESCEL(CES2,LIGREL,OPTION,PARAM,PROL0,NNCP,BASE,CHOU,'F',
     &              IBID)
        CALL DETRSD('CHAM_ELEM_S',CES2)


      ELSEIF (CAS.EQ.'NOEU->ELNO') THEN
C     ----------------------------------------------------------------
        CNS1 = '&&CHPCHD.CNS1'
        CES1 = '&&CHPCHD.CES1'

        CALL CNOCNS(CHIN,'V',CNS1)
        CALL CNSCES(CNS1,'ELNO',CESMOD,' ','V',CES1)
        CALL DETRSD('CHAM_NO_S',CNS1)

        CALL CESCEL(CES1,LIGREL,OPTION,PARAM,PROL0,NNCP,BASE,CHOU,'F',
     &              IBID)
        CALL DETRSD('CHAM_ELEM_S',CES1)


      ELSEIF ((CAS.EQ.'ELNO->NOEU') .OR. (CAS.EQ.'ELGA->NOEU') .OR.
     &        (CAS.EQ.'CART->NOEU')) THEN
C     ----------------------------------------------------------------
        CNS1 = '&&CHPCHD.CNS1'
        CES1 = '&&CHPCHD.CES1'

        IF (CAS(1:4).EQ.'ELNO') THEN
          CALL CELCES(CHIN,'V',CES1)

        ELSEIF (CAS(1:4).EQ.'ELGA') THEN
          CALL CELCES(CHIN,'V',CES1)
          CALL CELFPG(CHIN,'&&CHPCHD.CELFPG',IRET)

        ELSEIF (CAS(1:4).EQ.'CART') THEN
          CALL CARCES(CHIN,'ELNO',' ','V',CES1,'A',IRET)

        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

        CALL CESCNS(CES1,'&&CHPCHD.CELFPG','V',CNS1,' ',IRET)
        CALL CNSCNO(CNS1,' ','NON',BASE,CHOU,'F',IBID)

        CALL DETRSD('CHAM_NO_S',CNS1)
        CALL DETRSD('CHAM_ELEM_S',CES1)
        CALL JEDETR('&&CHPCHD.CELFPG')


      ELSEIF (CAS(1:8).EQ.'CART->EL') THEN
C     ----------------------------------------------------------------
        CALL ASSERT(LIGREL.NE.' ')
        CES1 = '&&CHPCHD.CES1'
        CALL CARCES(CHIN,CAS(7:10),CESMOD,'V',CES1,'A',IB)

        CALL CESCEL(CES1,LIGREL,OPTION,PARAM,PROL0,NNCP,BASE,CHOU,'F',
     &              IBID)
        CALL DETRSD('CHAM_ELEM_S',CES1)

        IF (NNCP.NE.0) THEN
          CALL GETRES(KBID,KBID,NOMCMD)
          IF (NOMCMD.EQ.'CREA_CHAMP') THEN
            VALK(1) = CHOU(1:8)
            VALK(2) = OPTION
            VALK(3) = PARAM
            CALL U2MESK('A','CALCULEL6_77',3,VALK)
          ENDIF
        ENDIF


      ELSEIF (CAS(1:8).EQ.'CESE->EL') THEN
C     ----------------------------------------------------------------
        CALL ASSERT(LIGREL.NE.' ')
        CES1 = '&&CHPCHD.CES1'
        CALL CESCES(CHIN,CAS(7:10),CESMOD,' ',' ','V',CES1)
        CALL CESCEL(CES1,LIGREL,OPTION,PARAM,PROL0,NNCP,BASE,CHOU,'F',
     &              IBID)
        CALL DETRSD('CHAM_ELEM_S',CES1)

        IF (NNCP.NE.0) THEN
          CALL GETRES(KBID,KBID,NOMCMD)
          IF (NOMCMD.EQ.'CREA_CHAMP') THEN
            VALK(1) = CHOU(1:8)
            VALK(2) = OPTION
            VALK(3) = PARAM
            CALL U2MESK('A','CALCULEL6_77',3,VALK)
          ENDIF
        ENDIF


      ELSEIF (CAS.EQ.'ELGA->ELNO') THEN
C     ----------------------------------------------------------------
        CES1 = '&&CHPCHD.CES1'
        CES2 = '&&CHPCHD.CES2'

        CALL CELCES(CHIN,'V',CES1)
        CALL CELFPG(CHIN,'&&CHPCHD.CELFPG',IRET)
        CALL ASSERT(IRET.EQ.0)
        CALL CESCES(CES1,'ELNO',CESMOD,' ','&&CHPCHD.CELFPG','V',CES2)
        CALL CESCEL(CES2,LIGREL,OPTION,PARAM,PROL0,NNCP,BASE,CHOU,'F',
     &              IBID)

        CALL DETRSD('CHAM_ELEM_S',CES1)
        CALL DETRSD('CHAM_ELEM_S',CES2)
        CALL JEDETR('&&CHPCHD.CELFPG')


      ELSEIF (CAS.EQ.'NOEU->ELEM') THEN
C     ----------------------------------------------------------------
        CNS1 = '&&CHPCHD.CNS1'
        CES1 = '&&CHPCHD.CES1'

        CALL CNOCNS(CHIN,'V',CNS1)
        CALL CNSCES(CNS1,'ELEM',CESMOD,' ','V',CES1)
        CALL DETRSD('CHAM_NO_S',CNS1)

        CALL CESCEL(CES1,LIGREL,OPTION,PARAM,PROL0,NNCP,BASE,CHOU,'F',
     &              IBID)
        CALL DETRSD('CHAM_ELEM_S',CES1)


      ELSE
C       CAS NON ENCORE PROGRAMME
        CALL U2MESK('F','CALCULEL_5',1,CAS)
      ENDIF




C     -- MENAGE :
C     ------------
      IF (TYPE(1:2).EQ.'EL') CALL DETRSD('CHAM_ELEM_S',CESMOD)

      END
