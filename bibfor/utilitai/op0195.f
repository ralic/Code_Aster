      SUBROUTINE OP0195(IER)
      IMPLICIT  NONE
      INTEGER IER
C     -----------------------------------------------------------------
C MODIF UTILITAI  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C RESPONSABLE VABHHTS J.PELLET
C     COMMANDE CREA_CHAMP
C     -----------------------------------------------------------------
      INTEGER N1,IB,IFM,NIV,IRET,I11,I12,IBID
      CHARACTER*3 PROL0
      CHARACTER*4 TYCHR
      CHARACTER*8 KBID,MO,MA,CHOU,NOMGD,CHIN,NOMGD2,NOMPAR,MA2
      CHARACTER*16 TYCHR1,OPERA,OPTIO2,TYPCO
      CHARACTER*19 LIGREL,CARTEM,CELMOD,PRCHN1,CNS1,CH1
      CHARACTER*8 NU1
C     -----------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)


C 1- CALCUL DE:
C      OPERA: OPERATION A EFFECTUER
C      MO: MODELE (OU ' ')
C      MA: MAILLAGE (OU ' ')
C      CHOU  : CHAMP RESULTAT
C      TYCHR : TYPE DU CHAMP RESULTAT (CART/NOEU/ELNO/ELGA/ELEM)
C      NOMGD : GRANDEUR ASSOCIEE A CHOU
C      PROL0 :/'OUI' POUR PROLONGER PAR ZERO LE CHAM_ELEM RESULTAT

C     ------------------------------------------------------------------

      CALL GETVTX(' ','OPERATION',0,1,1,OPERA,IB)
      CALL GETVID(' ','MODELE',0,1,1,MO,N1)
      IF (N1.EQ.0) MO = ' '
      CALL GETVID(' ','MAILLAGE',0,1,1,MA,N1)
      IF (N1.EQ.0) MA = ' '
      CALL GETRES(CHOU,TYPCO,KBID)
      CALL GETVTX(' ','TYPE_CHAM',0,1,1,TYCHR1,IB)
      TYCHR = TYCHR1(1:4)
      NOMGD = TYCHR1(6:13)
      CALL GETVTX(' ','PROL_ZERO',0,1,1,PROL0,IB)


C 2.  CALCUL DE MA,LIGREL  + QUELQUES VERIFICATIONS   :
C     -------------------------------------------------------------
      LIGREL = ' '

      IF (TYCHR(1:2).EQ.'EL') THEN
        IF ((OPERA.EQ.'AFFE') .OR. (OPERA.EQ.'ASSE') .OR.
     &      (OPERA.EQ.'DISC')) THEN
          IF (MO.EQ.' ') CALL UTMESS('F','OP0195',
     &                               'POUR TYPE_RESU:''EL..'''//
     &                          ' IL FAUT RENSEIGNER LE MOT CLE MODELE.'
     &                               )
          LIGREL = MO//'.MODELE'
        END IF
      END IF

      IF (MO.NE.' ') THEN
        CALL DISMOI('F','NOM_MAILLA',MO,'MODELE',IB,MA2,IB)
        IF ((MA.NE.' ') .AND. (MA.NE.MA2)) CALL UTMESS('F','OP0195',
     &      'MAILLAGE ET MODELE INCOHERENTS.')
        MA = MA2
      END IF



C 3.  TRAITEMENT DU MOT CLE OPERATION :
C     -------------------------------------------------------------


      IF (OPERA.EQ.'AFFE') THEN
C     -----------------------------------------
        IF (TYCHR.EQ.'NOEU') THEN
          CALL CNOAFF(MA,NOMGD,'G',CHOU)
        ELSE IF (TYCHR.EQ.'CART') THEN
          CALL CARAFF(MA,NOMGD,'G',CHOU)
        ELSE IF (TYCHR(1:2).EQ.'EL') THEN
          CARTEM = '&&OP0195.CARTEM'
          CALL CARAFF(MA,NOMGD,'V',CARTEM)
          CALL CHPCHD(CARTEM,TYCHR,LIGREL,PROL0,'G',CHOU)
          CALL DETRSD('CHAMP_GD',CARTEM)
        END IF


      ELSE IF (OPERA.EQ.'ASSE') THEN
C     -----------------------------------------
C     CALCUL DE CELMOD :
        IF (TYCHR(1:2).EQ.'EL') THEN
          CELMOD = '&&OP0195.CELMOD'
          OPTIO2 = 'TOU_INI_'//TYCHR
          NOMPAR = 'P'//NOMGD
          CALL ALCHML(LIGREL,OPTIO2,NOMPAR,'V',CELMOD,IRET,' ')
          IF (IRET.NE.0) CALL UTMESS('F','OP0195','OPTION: '//OPTIO2//
     &                         'NON PREVUE POUR LES ELEMENTS DU MODELE.'
     &                               )
        ELSE
          CELMOD = ' '
        END IF

        CALL CHPASS(TYCHR,MA,CELMOD,NOMGD,PROL0,CHOU)


      ELSE IF (OPERA.EQ.'EVAL') THEN
C     -----------------------------------------
        CALL CHPEVA(CHOU)


      ELSE IF (OPERA.EQ.'DISC') THEN
C     -----------------------------------------
        CALL GETVID(' ','CHAM_GD',0,1,1,CHIN,IB)
        CALL DISMOI('F','NOM_GD',CHIN,'CHAMP',IB,NOMGD2,IB)
        IF (NOMGD.NE.NOMGD2) CALL UTMESS('F','OP0195',
     &                            'GRANDEURS DIFFERENTES POUR :'//CHIN//
     &                            'ET :'//TYCHR1)
        CALL CHPCHD(CHIN,TYCHR,LIGREL,PROL0,'G',CHOU)


      ELSE IF (OPERA.EQ.'EXTR') THEN
C     -----------------------------------------
        CALL CHPREC(CHOU)

      END IF



C 4.  SI ON A CREE UN CHAM_NO, ON PEUT CHANGER SA NUMEROTATION :
C --------------------------------------------------------------
      IF (TYCHR.EQ.'NOEU') THEN
        CALL GETVID(' ','CHAM_NO',0,1,1,CH1,I11)
        CALL GETVID(' ','NUME_DDL',0,1,1,NU1,I12)
        IF ((I11+I12).GT.0) THEN
          PRCHN1 = ' '
          IF (I11.GT.0) CALL DISMOI('F','PROF_CHNO',CH1,'CHAM_NO',IBID,
     &                              PRCHN1,IBID)
          IF (I12.GT.0) CALL DISMOI('F','PROF_CHNO',NU1,'NUME_DDL',IBID,
     &                              PRCHN1,IBID)

          CNS1 = '&&OP0195.CNS1'
          CALL CNOCNS(CHOU,'V',CNS1)
          CALL CNSCNO(CNS1,PRCHN1,'G',CHOU)
          CALL DETRSD('CHAM_NO_S',CNS1)
        END IF
      END IF



C 5.  SI INFO:2    ON IMPRIME LE CHAMP RESULTAT :
C ----------------------------------------------
      IF (NIV.EQ.2) CALL IMPRSD('CHAMP',CHOU,IFM,
     &                      'CHAMP RESULTAT DE LA COMMANDE CREA_CHAMP :'
     &                          )


C 6.  AJOUT DU TITRE :
C -----------------------------------------------------
      CALL TITRE()


C 7.  VERIFICATIONS FINALES :
C -----------------------------------------------------
      CALL DISMOI('F','NOM_GD',CHOU,'CHAMP',IB,NOMGD2,IB)
      IF (TYPCO(1:8).EQ.'CHAM_NO_') THEN
        NOMGD = TYPCO(9:16)
      ELSE IF (TYPCO(1:6).EQ.'CARTE_') THEN
        NOMGD = TYPCO(7:16)
      ELSE IF (TYPCO(1:10).EQ.'CHAM_ELEM_') THEN
        NOMGD = TYPCO(11:16)
      ELSE
        CALL UTMESS('F','OP0195','STOP1')
      END IF
      IF (NOMGD.NE.NOMGD2) CALL UTMESS('F','OP0195',
     &                          'GRANDEURS INCOHERENTES:'//NOMGD//
     &                          ' ET '//NOMGD2)


C 8. MENAGE :
C -----------------------------------------------------
      CALL DETRSD('CHAMP_GD','&&OP0195.CELMOD')
      CALL JEDEMA()

      END
