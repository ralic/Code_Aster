      SUBROUTINE CHPCHD(CHIN,TYPE,LIGREL,PROL0,BASE,CHOU)
      IMPLICIT  NONE
      CHARACTER*(*) CHIN,CHOU,BASE,LIGREL,TYPE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 06/12/2000   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE VABHHTS J.PELLET
C A_UTIL
C -----------------------------------------------------------------
C  BUT : CHANGER LE SUPPORT GEOMETRIQUE D'UN CHAMP
C -----------------------------------------------------------------

C CHIN IN/JXIN  K19 : NOM DU CHAMP A CHANGER
C CHOU IN/JXOUT K19 : NOM DU CHAMP RESULTAT
C BASE IN       K1  : BASE DE CREATION DE CHOU : /'G' / 'V'
C TYPE IN       K19 : TYPE DE SUPPORT GEOMETRIQUE VOULU POUR CHOU
C                     /'NOEU' /'CART' /'ELNO' /ELGA'
C PROL0 IN      K3  : (ARGUMENT UTILISE SI TYPE=ELGA OU ELNO)
C                     /'OUI' : LE CHAM_ELEM CHOU EST PROLONGE
C                      PAR DES VALEURS NULLES LA OU IL N'EST
C                      PAS DEFINI.
C                     /'NON' : ERREUR <F> SI IL EXISTE DES
C                      DES VALEURS DE CHOU QUI NE SONT PAS
C                      AFFECTEES DANS CHIN
C LIGREL IN/JXIN  K19 : NOM DU LIGREL (OU ' ')
C                   LIGREL EST NECESSAIRE SI PASSAGE NOEUD <-> GAUSS
C -----------------------------------------------------------------


      INTEGER IB,IRET
      CHARACTER*3 PROL0
      CHARACTER*8 MA,MA2,TYCHI,NOMGD,NOMPAR
      CHARACTER*16 CAS,OPTION
      CHARACTER*19 CESMOD,CES1,CNS1,CELMOD,MNOGA,MGANO
C     -----------------------------------------------------------------


C 1- CALCUL DE:
C      MA    : MAILLAGE ASSOCIE A CHIN
C      TYCHI : TYPE DU CHAMP CHIN (CART/NOEU/ELNO/ELGA)
C      NOMGD : NOM DE LA GRANDEUR ASSOCIEE A CHIN

C     ------------------------------------------------------------------

      CALL DISMOI('F','NOM_MAILLA',CHIN,'CHAMP',IB,MA,IB)
      CALL DISMOI('F','TYPE_CHAMP',CHIN,'CHAMP',IB,TYCHI,IB)
      CALL DISMOI('F','NOM_GD',CHIN,'CHAMP',IB,NOMGD,IB)

      IF (LIGREL.NE.' ') THEN
        CALL DISMOI('F','NOM_MAILLA',LIGREL,'LIGREL',IB,MA2,IB)
        IF (MA.NE.MA2) CALL UTMESS('F','CHPCHD','MAILLAGES DIFFERENTS.')
      END IF


C 3.  -- CALCUL DE CAS :
C -------------------------------
C         /'NOEU->ELNO'   : CHAM_NO -> ELNO
C         /'NOEU->ELGA'   : CHAM_NO -> ELGA
C         /'CART->ELGA'   : CARTE   -> ELGA
C         /'CART->ELNO'   : CARTE   -> ELNO
C         /'CART->NOEU'   : CARTE   -> CHAM_NO
C         /'ELGA->NOEU'   : ELGA   -> CHAM_NO
C         /'ELNO->NOEU'   : ELNO   -> CHAM_NO

      CAS = ' '
      CAS(1:4) = TYCHI(1:4)
      CAS(5:6) = '->'
      CAS(7:10) = TYPE


C 4.  TRAITEMENT DES DIFFERENTS CAS DE FIGURE :
C ----------------------------------------------

      IF (CAS.EQ.'NOEU->ELGA') THEN
C     ----------------------------------
        IF (LIGREL.EQ.' ') CALL UTMESS('F','CHPCHD','IL FAUT UN LIGREL')

        CNS1 = '&&CHPCHD.CNS1'
        CES1 = '&&CHPCHD.CES1'
        MNOGA = '&&CHPCHD.MANOGA'
        CALL MANOPG(LIGREL,MNOGA)

        OPTION = 'TOU_INI_ELGA'
        NOMPAR = 'P'//NOMGD
        CELMOD = '&&CHPCHD.CELMOD'
        CESMOD = '&&CHPCHD.CESMOD'
        CALL ALCHML(LIGREL,OPTION,NOMPAR,'V',CELMOD,IB,' ')
        IF (IB.EQ.1) CALL UTMESS('F','CHPCHD','CHAMP RESULTAT VIDE.')
        CALL CELCES(CELMOD,'V',CESMOD)
        CALL DETRSD('CHAM_ELEM',CELMOD)

        CALL CNOCNS(CHIN,'V',CNS1)
        CALL CNSCES(CNS1,'ELGA',CESMOD,MNOGA,'V',CES1)
        CALL DETRSD('CHAM_NO_S',CNS1)
        CALL DETRSD('CHAM_ELEM_S',MNOGA)

        CALL CESCEL(CES1,LIGREL,' ',' ',PROL0,BASE,CHOU)
        CALL DETRSD('CHAM_ELEM_S',CES1)
        CALL DETRSD('CHAM_ELEM_S',CESMOD)


      ELSE IF (CAS.EQ.'NOEU->ELNO') THEN
C     ----------------------------------------------------------------
        CNS1 = '&&CHPCHD.CNS1'
        CES1 = '&&CHPCHD.CES1'

        OPTION = 'TOU_INI_ELNO'
        NOMPAR = 'P'//NOMGD
        CELMOD = '&&CHPCHD.CELMOD'
        CESMOD = '&&CHPCHD.CESMOD'
        CALL ALCHML(LIGREL,OPTION,NOMPAR,'V',CELMOD,IB,' ')
        IF (IB.EQ.1) CALL UTMESS('F','CHPCHD','CHAMP RESULTAT VIDE.')
        CALL CELCES(CELMOD,'V',CESMOD)
        CALL DETRSD('CHAM_ELEM',CELMOD)

        CALL CNOCNS(CHIN,'V',CNS1)
        CALL CNSCES(CNS1,'ELNO',CESMOD,' ','V',CES1)
        CALL DETRSD('CHAM_NO_S',CNS1)

        CALL CESCEL(CES1,LIGREL,' ',' ',PROL0,BASE,CHOU)
        CALL DETRSD('CHAM_ELEM_S',CES1)
        CALL DETRSD('CHAM_ELEM_S',CESMOD)


      ELSE IF ((CAS.EQ.'ELNO->NOEU') .OR. (CAS.EQ.'ELGA->NOEU') .OR.
     &         (CAS.EQ.'CART->NOEU')) THEN
C     ----------------------------------------------------------------
        CNS1 = '&&CHPCHD.CNS1'
        CES1 = '&&CHPCHD.CES1'

        MGANO = ' '
        IF (CAS(1:4).EQ.'ELNO') THEN
          CALL CELCES(CHIN,'V',CES1)
        ELSE IF (CAS(1:4).EQ.'ELGA') THEN
          CALL CELCES(CHIN,'V',CES1)
          MGANO = '&&CHPCHD.MAGANO'
          CALL UTMESS('F','CHPCHD','GA -> NO A FAIRE ...')
C         CALL MANOPG(LIGREL,MNOGA)
        ELSE IF (CAS(1:4).EQ.'CART') THEN
          CALL CARCES(CHIN,'ELNO',' ','V',CES1,IRET)
        END IF
        CALL CESCNS(CES1,MGANO,'V',CNS1)
        CALL CNSCNO(CNS1,' ',BASE,CHOU)

        CALL DETRSD('CHAM_NO_S',CNS1)
        CALL DETRSD('CHAM_ELEM_S',CES1)


      ELSE IF ((CAS.EQ.'CART->ELGA') .OR. (CAS.EQ.'CART->ELNO')) THEN
C     ----------------------------------------------------------------
        IF (LIGREL.EQ.' ') CALL UTMESS('F','CHPCHD','IL FAUT MODELE')

        OPTION = 'TOU_INI_'//CAS(7:10)
        NOMPAR = 'P'//NOMGD
        CELMOD = '&&CHPCHD.CELMOD'
        CESMOD = '&&CHPCHD.CESMOD'
        CALL ALCHML(LIGREL,OPTION,NOMPAR,'V',CELMOD,IB,' ')
        IF (IB.EQ.1) CALL UTMESS('F','CHPCHD','CHAMP VIDE.')
        CALL CELCES(CELMOD,'V',CESMOD)
        CALL DETRSD('CHAM_ELEM',CELMOD)

        CES1 = '&&CHPCHD.CES1'
        CALL CARCES(CHIN,CAS(7:10),CESMOD,'V',CES1,IB)

        CALL CESCEL(CES1,LIGREL,' ',' ',PROL0,BASE,CHOU)
        CALL DETRSD('CHAM_ELEM_S',CES1)
        CALL DETRSD('CHAM_ELEM_S',CESMOD)


      ELSE
        CALL UTMESS('F','CHPCHD','NON PROGRAMME:'//CAS)
      END IF



   10 CONTINUE

      END
