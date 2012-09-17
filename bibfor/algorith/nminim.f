      SUBROUTINE NMINIM(SDSUIV,SDIMPR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/09/2012   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      INCLUDE      'jeveux.h'
      CHARACTER*24 SDIMPR,SDSUIV
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - INITIALISATIONS)
C
C INITIALISATION DES INFORMATIONS POUR L'IMPRESSION
C
C ----------------------------------------------------------------------
C
C
C IN  SDIMPR : SD AFFICHAGE
C IN  SDSUIV : SD SUIVI_DDL
C
C ----------------------------------------------------------------------
C
      INTEGER      NBCOLO,LARCOL,TITHAU
      PARAMETER    (NBCOLO=29,LARCOL=16,TITHAU=3)
      CHARACTER*15 TYPCOZ(NBCOLO),ORDCOZ(NBCOLO)
      CHARACTER*16 TITR1Z(NBCOLO),TITR2Z(NBCOLO),TITR3Z(NBCOLO)
      CHARACTER*1  TYPVAZ(NBCOLO)
      CHARACTER*4  CNOVAZ(NBCOLO)
C
      INTEGER      IFM,NIV
      CHARACTER*24 SDTABC,SLCOLO,SDCOLO
      CHARACTER*15 TYPCOL,ORDCOL
      CHARACTER*16 TITLI1,TITLI2,TITLI3
      CHARACTER*1  TYPVAL,INDSUI
      INTEGER      ICOLO,ICOLO1,ICOLO2,ISUIV,UNITE,TITCOM,TITCO1
      LOGICAL      LCSV
      CHARACTER*24 SUIINF
      INTEGER      JSUIIN
      INTEGER      NBCOLT,NBSUIV
      CHARACTER*24 DDLTIT
      INTEGER      JDDLTI
      CHARACTER*4  CNOVAL
C
C --- ORDRE DE DEFINITION DONNE L'ORDRE D'AFFICHAGE
C
      DATA ORDCOZ /'INCR_INST','BOUC_GEOM','BOUC_FROT',
     &             'BOUC_CONT','ITER_NUME','RESI_RELA',
     &             'RELA_NOEU','RESI_MAXI','MAXI_NOEU',
     &             'RESI_REFE','REFE_NOEU','RESI_COMP',
     &             'COMP_NOEU','RELI_NBIT','RELI_COEF',
     &             'PILO_COEF','MATR_ASSE','DEBORST  ',
     &             'CTCD_NBIT','CONT_NEWT','FROT_NEWT',
     &             'GEOM_NEWT','CTCC_CYCL','BOUC_VALE',
     &             'BOUC_NOEU','FROT_NOEU','GEOM_NOEU',
     &             'FETI_NBIT','ITER_TIME'/
C
      DATA TYPCOZ /'ITER_NUME','INCR_INST','RESI_RELA',
     &             'RESI_MAXI','RESI_REFE','RESI_COMP',
     &             'RELA_NOEU','MAXI_NOEU','REFE_NOEU',
     &             'COMP_NOEU','RELI_NBIT','RELI_COEF',
     &             'PILO_COEF','MATR_ASSE','DEBORST  ',
     &             'CTCD_NBIT','BOUC_GEOM','BOUC_FROT',
     &             'BOUC_CONT','CONT_NEWT','FROT_NEWT',
     &             'GEOM_NEWT','CTCC_CYCL','BOUC_VALE',
     &             'BOUC_NOEU','FROT_NOEU','GEOM_NOEU',
     &             'FETI_NBIT','ITER_TIME'/
C
      DATA TITR1Z /
     &         '     NEWTON     ','   INCREMENT    ','     RESIDU     ',
     &         '     RESIDU     ','     RESIDU     ','     RESIDU     ',
     &         ' RESI_GLOB_RELA ',' RESI_GLOB_MAXI ',' RESI_REFE_RELA ',
     &         ' RESI_COMP_RELA ','  RECH.  LINE.  ','  RECH.  LINE.  ',
     &         '    PILOTAGE    ','     OPTION     ','     DEBORST    ',
     &         '     CONTACT    ','     CONTACT    ','     CONTACT    ',
     &         '     CONTACT    ','     CONTACT    ','     CONTACT    ',
     &         '     CONTACT    ','     CONTACT    ','     CONTACT    ',
     &         '     CONTACT    ','     CONTACT    ','     CONTACT    ',
     &         '     FETI       ','     NEWTON     '/
C
      DATA TITR2Z /
     &         '    ITERATION   ','    INSTANT     ','     RELATIF    ',
     &         '     ABSOLU     ','  PAR REFERENCE ',' PAR COMPOSANTE ',
     &         '     MAXIMUM    ','     MAXIMUM    ','     MAXIMUM    ',
     &         '     MAXIMUM    ','    NB. ITER    ','  COEFFICIENT   ',
     &         '  COEFFICIENT   ','   ASSEMBLAGE   ','                ',
     &         '    DISCRET     ','    BCL. GEOM.  ','    BCL. FROT.  ',
     &         '    BCL. CONT.  ','   NEWTON GENE  ','   NEWTON GENE  ',
     &         '   NEWTON GENE  ','      INFOS     ','     CRITERE    ',
     &         '     CRITERE    ','   NEWTON GENE  ','   NEWTON GENE  ',
     &         '    NB. ITER    ','  TEMPS CALCUL  '/
C
      DATA TITR3Z /
     &         '                ','                ',' RESI_GLOB_RELA ',
     &         ' RESI_GLOB_MAXI ',' RESI_REFE_RELA ',' RESI_COMP_RELA ',
     &         '    AU POINT    ','    AU POINT    ','    AU POINT    ',
     &         '    AU POINT    ','                ','      RHO       ',
     &         '      ETA       ','                ','                ',
     &         '    NB. ITER    ','    ITERATION   ','    ITERATION   ',
     &         '    ITERATION   ','   VARI. CONT.  ','   CRIT. FROT.  ',
     &         '   CRIT. GEOM.  ','    CYCLAGES    ','    VALEUR      ',
     &         '    MAX. LIEU   ',' LIEU MAX FROT. ',' LIEU MAX GEOM. ',
     &         '                ','                '/
C
      DATA TYPVAZ /'I','R','R',
     &             'R','R','R',
     &             'K','K','K',
     &             'K','I','R',
     &             'R','K','K',
     &             'I','I','I',
     &             'I','I','R',
     &             'R','K','R',
     &             'K','K','K',
     &             'I','R'/
C
      DATA CNOVAZ /'ERRE','ERRE','SANS',
     &             'SANS','SANS','ERRE',
     &             'ERRE','ERRE','SANS',
     &             'SANS','SANS','SANS',
     &             'VIDE','VIDE','ERRE',
     &             'ERRE','ERRE','ERRE',
     &             'ERRE','ERRE','ERRE',
     &             'ERRE','SANS','SANS',
     &             'SANS','SANS','ERRE',
     &             'SANS','ERRE'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... INITIALISATIONS IMPRESSIONS'
      ENDIF
C
C --- TABLEAU DE CONVERGENCE
C
      SDTABC = '&&NMINIM.SDTABC'
      CALL OBCREA('TABLEAU',SDTABC)
      CALL OBSETO(SDIMPR,'TABLEAU_CONV',SDTABC)
C
C --- TRANSFERT DES PARAMETRES
C    
      CALL OBTRAN(SDIMPR,'TABL_CONV_CSV',SDTABC,'SORTIE_CSV')
      CALL OBTRAN(SDIMPR,'UNIT_CONV_CSV',SDTABC,'UNITE_CSV' )
C
C --- NOM DE LA LISTE DES COLONNES DISPONIBLES
C
      SLCOLO = '&&NMINIM.SLCOLO'
      CALL OBSETO(SDTABC,'COLONNES_DISPOS',SLCOLO)
C
C --- NOMBRE DE COLONNES POUR LES SUIVIS EN TEMPS REEL
C
      SUIINF = SDSUIV(1:14)//'     .INFO'
      CALL JEVEUO(SUIINF,'L',JSUIIN)
      NBSUIV = ZI(JSUIIN+1-1)
      IF (NBSUIV.GT.9)  CALL U2MESI('F','IMPRESSION_3',1,NBSUIV)
C
C --- TITRE DES COLONNES POUR LES SUIVIS EN TEMPS REEL
C
      IF (NBSUIV.NE.0) THEN
        DDLTIT = SDSUIV(1:14)//'     .TITR'
        CALL JEVEUO(DDLTIT,'L',JDDLTI)
      ENDIF
C
C --- NOMBRE TOTAL DE COLONNES
C
      NBCOLT = NBCOLO + NBSUIV
C
C --- CREATION DE LA LISTE DES COLONNES DISPONIBLES
C
      CALL OBLCRE(SLCOLO,'TABLEAU_COLONNE','TYPE_COLONNE',NBCOLT)
C
C --- CREATION DE TOUTES LES COLONNES DISPONIBLES
C
      DO 10 ICOLO = 1,NBCOLO
        TYPCOL = TYPCOZ(ICOLO)
        TITLI1 = TITR1Z(ICOLO)
        TITLI2 = TITR2Z(ICOLO)
        TITLI3 = TITR3Z(ICOLO)
        TYPVAL = TYPVAZ(ICOLO)
        CNOVAL = CNOVAZ(ICOLO)
        CALL OBCLCR('NMINIM',TYPCOL,LARCOL,TITHAU,TITLI1,
     &              TITLI2  ,TITLI3,TYPVAL,CNOVAL,SDCOLO)
  10  CONTINUE
C
C --- AFFECTATION DES COLONNES DANS LA LISTE (DANS L'ORDRE !)
C
      DO 15 ICOLO1 = 1,NBCOLO
        ORDCOL = ORDCOZ(ICOLO1)
        ICOLO  = 0
        DO 16 ICOLO2 = 1,NBCOLO
          TYPCOL = TYPCOZ(ICOLO2)
          IF (TYPCOL.EQ.ORDCOL) ICOLO = ICOLO2
  16    CONTINUE
        CALL ASSERT(ICOLO.NE.0)
        TYPCOL = TYPCOZ(ICOLO)
        CALL OBLGEN('NMINIM',TYPCOL,SDCOLO)
        CALL OBLSOI(SLCOLO,ICOLO1,SDCOLO)
  15  CONTINUE
C
C --- CREATION DES COLONNES POUR LE SUIVI EN TEMPS REEL
C
      DO 20 ISUIV = 1,NBSUIV
        CALL IMPFOI(0,1,ISUIV,INDSUI)
        TYPCOL = 'SUIV_DDL'//INDSUI
        TITLI1 = ZK16(JDDLTI+3*(ISUIV-1)+1-1)
        TITLI2 = ZK16(JDDLTI+3*(ISUIV-1)+2-1)
        TITLI3 = ZK16(JDDLTI+3*(ISUIV-1)+3-1)
        TYPVAL = 'R'
        CNOVAL = 'ERRE'
        CALL OBCLCR('NMINIM',TYPCOL,LARCOL,TITHAU,TITLI1,
     &              TITLI2  ,TITLI3,TYPVAL,CNOVAL,SDCOLO)
  20  CONTINUE
C
C --- AFFECTATION DES COLONNES DANS LA LISTE
C
      DO 25 ISUIV = 1,NBSUIV
        CALL IMPFOI(0,1,ISUIV,INDSUI)
        TYPCOL = 'SUIV_DDL'//INDSUI
        CALL OBLGEN('NMINIM',TYPCOL,SDCOLO)
        CALL OBLSOI(SLCOLO,ISUIV+NBCOLO,SDCOLO)
  25  CONTINUE
C
C --- RECUPERATION HAUTEUR TITRE COMMUNE POUR TABLEAU
C
      CALL OBLGOI(SLCOLO,1,SDCOLO)
      CALL OBGETI(SDCOLO,'HAUTEUR_TITRE',TITCO1)
      DO 30 ICOLO = 2,NBCOLT
        CALL OBLGOI(SLCOLO,ICOLO ,SDCOLO)
        CALL OBGETI(SDCOLO,'HAUTEUR_TITRE',TITCOM)
        IF (TITCO1.NE.TITCOM) CALL ASSERT(.FALSE.)
 30   CONTINUE
      CALL OBSETI(SDTABC,'HAUTEUR_TITRE',TITCOM)
C
C --- OUVERTURE DU FICHIER
C
      CALL OBGETB(SDTABC,'SORTIE_CSV',LCSV  )
      CALL OBGETI(SDTABC,'UNITE_CSV' ,UNITE )
      IF (LCSV) CALL ULOPEN(UNITE ,' '   ,' '   ,'NEW','O'   )
C
      CALL JEDEMA()

      END
