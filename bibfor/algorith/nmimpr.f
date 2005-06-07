      SUBROUTINE NMIMPR(PHASE,NATURZ,ARGZ, ARGR, ARGI)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C RESPONSABLE PBADEL P.BADEL
C TOLE CRP_20
      IMPLICIT NONE

      INTEGER       ARGI(*)
      REAL*8        ARGR(*)
      CHARACTER*(*) NATURZ, ARGZ(*)
      CHARACTER*4   PHASE
      CHARACTER*9   NATURE

C ----------------------------------------------------------------------
C  GESTION DES IMPRESSIONS DE LA COMMANDE STAT_NON_LINE
C ----------------------------------------------------------------------
C IN  PHASE  : 'INIT' INITIALISATION
C              'TITR' AFFICAGE DE L'EN TETE DES PAS DE TEMPS
C              'IMPR' IMPRESSION
C IN  NATURE : NATURE DE L'IMPRESSION
C              'ASSE_MATR' -> ASSEMBLAGE DE LA MATRICE
C              'ARCH_INIT' -> TITRE ARCHIVAGE ETAT INITIAL
C              'ARCHIVAGE' -> STOCKAGE DES CHAMPS
C              'ITER_MAXI' -> MAXIMUM ITERATIONS ATTEINT
C              'SUBDIVISE' -> SUBDIVISION DU PAS DE TEMPS
C              'CONV_OK  ' -> CONVERGENCE ATTEINTE
C              'GEOM_MIN'  -> BOUCLE EN MOINS
C              'GEOM_MAX'  -> BOUCLE SUPPLEMENTAIRE
C              'FIXE_NON'  -> BOUCLE SUPPLEMENTAIRE SUR POINT FIXE
C                             DU CONTACT-FROTTEMENT
C              'ETAT_CONV' -> PARAMETRES DE CONVERGENCE
C              'ECHEC_LDC' -> ECHEC DE L'INTEGRATION DE LA LDC
C              'ECHEC_PIL' -> ECHEC DU PILOTAGE
C              'ECHEC_CON' -> ECHEC DE TRAITEMENT DU CONTACT
C              'CONT_SING' -> MATRICE DE CONTACT SINGULIERE
C              'MATR_SING' -> MATRICE DU SYSTEME SINGULIERE
C              'MAXI_RELA' -> CONVERGENCE SUR RESI_GLOB_MAXI
C                             QUAND RESI_GLOB_RELA & CHARGEMENT=0
C              'BCL_SEUIL' -> NUMERO BOUCLE SEUIL CONTACT ECP
C              'BCL_GEOME' -> NUMERO BOUCLE GEOMETRIE CONTACT ECP
C              'BCL_CTACT' -> NUMERO BOUCLE CONTRAINTE ACTIVE
C                             CONTACT ECP
C              'CNV_SEUIL' -> CONVERGENCE BOUCLE SEUIL CONTACT ECP
C              'CNV_GEOME' -> CONVERGENCE BOUCLE GEOMETRIE CONTACT ECP
C              'CNV_CTACT' -> CONVERGENCE BOUCLE CONTRAINTE ACTIVE
C                             CONTACT ECP
C IN  ARGZ   : ARGUMENTS EVENTUELS DE TYPE TEXTE
C IN  ARGR   : ARGUMENTS EVENTUELS DE TYPE REEL
C IN  ARGI   : ARGUMENTS EVENTUELS DE TYPE ENTIER
C ----------------------------------------------------------------------

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
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

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      INTEGER      NBCOL, NBAFF
      PARAMETER   (NBCOL = 11, NBAFF=10)

      LOGICAL       DELOCA, PILOTA, RECHLI, AFF(NBCOL)
      INTEGER       NOCC, MESS, IUNIFI, LARGE, POS, I, K, IFONC
      CHARACTER*1   TYPE(NBCOL)
      CHARACTER*2   NBCH
      CHARACTER*4   ARG4
      CHARACTER*14  ARG14
      CHARACTER*16  NOMCMD, TITCOL(3,NBCOL), ARG16
      CHARACTER*16  INF16(NBCOL-NBAFF)
      CHARACTER*22  ARG22
      CHARACTER*29  ARG29
      CHARACTER*32  ARG32
      CHARACTER*36  ARG36
      CHARACTER*40  ARG40
      CHARACTER*42  ARG42
      CHARACTER*51  ARG51
      CHARACTER*54  ARG54
      CHARACTER*255 PLEINE, COLONN, TITRE(3), TAMPON

      SAVE  AFF, LARGE, PLEINE, COLONN, TITRE, INF16

C ----------------------------------------------------------------------
      DATA TITCOL
     &   / '   ITERATIONS   ','                ','                ',
     &     '     RESIDU     ','    RELATIF     ',' RESI_GLOB_RELA ',
     &     '     RESIDU     ','     ABSOLU     ',' RESI_GLOB_MAXI ',
     &     '     RESIDU     ','  PAR REFERENCE ','  CONTRAINTES   ',
     &     '   ITERATIONS   ','   RECH. LIN.   ','                ',
     &     '  COEFFICIENT   ','   RECH. LIN.   ','      RHO       ',
     &     '   PARAMETRE    ','    PILOTAGE    ','  ETA_PILOTAGE  ',
     &     '  ECART ABSOLU  ','   LAGRANGIEN   ',' RESI_DUAL_ABSO ',
     &     '   INCREMENT    ','   LAGRANGIEN   ',' RESI_PRIM_ABSO ',
     &     '   ITERATIONS   ','   LAGRANGIEN   ','                ',
     &     '     OPTION     ','   ASSEMBLAGE   ','                '/

      DATA  TYPE   /'I','R','R','R','I','R','R','R','R','I','K'/
C ----------------------------------------------------------------------



      MESS   = IUNIFI ('MESSAGE')
      NATURE = NATURZ


C ======================================================================
C                   INITIALISATION DE L'IMPRESSION
C ======================================================================

      IF (PHASE.EQ.'INIT') THEN

C -- DETERMINATION DE L'ENVIRONNEMENT

C    0 -> RECHERCHE LINEAIRE
C    1 -> PILOTAGE
C    2 -> LOIS NON LOCALES

        CALL JEVEUO('&&OP0070.IMPR.FONC','L',IFONC)


C -- INITIALISATION DE L'AFFICHAGE

C      COLONNES A AFFICHER
        AFF(1) = .TRUE.
        AFF(2) = .TRUE.
        AFF(3) = .TRUE.
        AFF(4) = ZL(IFONC+3) 
        AFF(5) = ZL(IFONC)
        AFF(6) = ZL(IFONC)
        AFF(7) = ZL(IFONC+1)
        AFF(8) = ZL(IFONC+2)
        AFF(9) = ZL(IFONC+2)
        AFF(10) = ZL(IFONC+2)
        AFF(11)= .TRUE.

C      LARGEUR TOTALE DE L'AFFICHAGE
        LARGE = 1
        DO 10 I = 1,NBCOL
          IF (AFF(I)) LARGE = LARGE+17
 10     CONTINUE
        IF (LARGE.GT.250) CALL UTMESS('F','NMIMPR',
     &    'TROP DE COLONNES A AFFICHER (DVLP)')

C      LIGNES DE SEPARATION
        DO 20 I = 1,LARGE
          PLEINE(I:I) = '-'
          IF (MOD(I-1,17) .EQ. 0) THEN
            COLONN(I:I) = '|'
          ELSE
            COLONN(I:I) = ' '
          END IF
 20     CONTINUE

C      LIGNES DE TITRE
        TITRE(1) = COLONN(1:LARGE)
        TITRE(2) = COLONN(1:LARGE)
        TITRE(3) = COLONN(1:LARGE)
        POS = 2
        DO 30 I = 1,NBCOL
          IF (AFF(I)) THEN
            DO 32 K = 1,3
              TITRE(K)(POS:POS+15) = TITCOL(K,I)
 32         CONTINUE
            POS = POS+17
          END IF
 30     CONTINUE
        GOTO 9999
      END IF


C ======================================================================
C                        AFFICHAGE DE L'EN-TETE
C ======================================================================

      IF (PHASE .EQ. 'TITR') THEN
        DO 40 I = 1,NBCOL - NBAFF
          INF16(I) = ' '
 40     CONTINUE

        WRITE(MESS,*)
        WRITE(MESS,*)
        WRITE(MESS,900) ARGR(1)
        WRITE(MESS,*)
 900    FORMAT ('INSTANT DE CALCUL : ',1PE16.9)

        WRITE(MESS,'(A)') PLEINE  (1:LARGE)
        WRITE(MESS,'(A)') TITRE(1)(1:LARGE)
        WRITE(MESS,'(A)') TITRE(2)(1:LARGE)
        WRITE(MESS,'(A)') TITRE(3)(1:LARGE)
        WRITE(MESS,'(A)') PLEINE  (1:LARGE)
        GOTO 9999
      END IF


C ======================================================================
C                       GESTION DES IMPRESSIONS
C ======================================================================

C -- ASSEMBLAGE MATRICE

      IF (NATURE .EQ. 'ASSE_MATR') THEN
        INF16(1)  = ARGZ(1)


C -- ARCHIVAGE ETAT INITIAL

      ELSE IF (NATURE .EQ. 'ARCH_INIT') THEN

 950    FORMAT('ARCHIVAGE DE L''ETAT INITIAL')
        WRITE (MESS,*)
        WRITE (MESS,*)
        WRITE (MESS,950)
        WRITE (MESS,*)


C -- ARCHIVAGE DES CHAMPS

      ELSE IF (NATURE .EQ. 'ARCHIVAGE') THEN

 1000   FORMAT(1P,3X,'CHAMP STOCKE : ',A16,' INSTANT : ',1PE12.5,
     &         '  NUMERO D''ORDRE : ',I5)
 1001   FORMAT(1P,3X,'CHAMP STOCKE : ',A16,' INSTANT : NON PRECISE ',
     &         '  NUMERO D''ORDRE : ',I5)

        ARG16 = ARGZ(1)
        WRITE (MESS,1000) ARG16,ARGR(1),ARGI(1)

      ELSE IF (NATURE .EQ. 'TPS_PAS') THEN

        WRITE(MESS,*)
        WRITE(MESS,666) ARGR(1)
        WRITE(MESS,*)
 666    FORMAT ('TEMPS CPU CONSOMME DANS CE PAS DE TEMPS : ',1PE16.9)


C -- ECHEC DANS L'INTEGRATION DE LA LDC

      ELSE IF (NATURE .EQ. 'ECHEC_LDC') THEN

        ARG51  = 'ECHEC DANS L''INTEGRATION DE LA LOI DE COMPORTEMENT'
        POS    = (LARGE-51) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+51) = ARG51
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        ARG36  = 'SUBDIVISER LE PAS DE TEMPS'
        POS    = (LARGE-25) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+26) = ARG36
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)


C -- ECHEC DANS LE PILOTAGE

      ELSE IF (NATURE .EQ. 'ECHEC_PIL') THEN

        ARG22  = 'ECHEC DANS LE PILOTAGE'
        POS    = (LARGE-22) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+22) = ARG22
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        ARG36  = 'SUBDIVISER LE PAS DE TEMPS'
        POS    = (LARGE-25) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+26) = ARG36
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)


C -- NOMBRE MAX D'ITERATIONS ATTEINT

      ELSE IF (NATURE .EQ. 'ITER_MAXI') THEN

        ARG36  = 'NOMBRE MAXIMUM D''ITERATIONS ATTEINT'
        POS    = (LARGE-36) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+35) = ARG36
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)

C -- ECHEC DE TRAITEMENT DU CONTACT

      ELSE IF (NATURE .EQ. 'ECHEC_CON') THEN

        ARG36  = 'ECHEC DANS LE TRAITEMENT DU CONTACT'
        POS    = (LARGE-34) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+35) = ARG36
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        ARG36  = 'SUBDIVISER LE PAS DE TEMPS'
        POS    = (LARGE-25) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+26) = ARG36
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)


C -- MATRICE DE CONTACT SIGULIERE

      ELSE IF (NATURE .EQ. 'CONT_SING') THEN

        ARG36  = 'MATRICE DE CONTACT SINGULIERE'
        POS    = (LARGE-28) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+35) = ARG36
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)



C -- DEBORST

      ELSE IF (NATURE(1:8) .EQ. 'DE_BORST') THEN

        ARG36  =   'CPLAN DEBORST: SIZZ NON NUL ON ITERE'
        POS    = (LARGE-36) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+35) = ARG36
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)



C -- MATRICE DU SYSTEME SINGULIERE

      ELSE IF (NATURE(1:9) .EQ. 'MATR_SING') THEN

        ARG36  =   'MATRICE DU SYSTEME SINGULIERE'
        POS    = (LARGE-29) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+28) = ARG36
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)



C -- SUBDIVISION DU PAS DE TEMPS

      ELSE IF (NATURE .EQ. 'SUBDIVISE') THEN

        WRITE(NBCH,'(I2)') ARGI(1)
        ARG42  = 'SUBDIVISION DU PAS DE TEMPS EN ' //NBCH// ' SOUS PAS'
        POS    = (LARGE-42) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+41) = ARG42
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)


C -- CRITERES DE CONVERGENCE ATTEINTS

      ELSE IF (NATURE .EQ. 'CONV_OK  ') THEN

        ARG36  = 'CRITERE(S) DE CONVERGENCE ATTEINT(S)'
        POS    = (LARGE-36) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+35) = ARG36
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)


C --- RESI_GLOB_RELA ET CHARGEMENT = 0, CONVERGENCE SUR RESI_GLOB_MAXI

      ELSE IF (NATURE .EQ. 'MAXI_RELA') THEN

        ARG40  = 'ATTENTION : CONVERGENCE ATTEINTE AVEC'
        POS    = (LARGE-37) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+36) = ARG40
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)

        WRITE(ARG14,'(1PE11.4)') ARGR(1)
        ARG36  = 'CRITERE RESI_GLOB_MAXI='//ARG14
        POS    = (LARGE-34) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+33) = ARG36
        WRITE (MESS,'(A)') TAMPON(1:LARGE)

        ARG36  = 'POUR CAUSE DE CHARGEMENT PRESQUE NUL'
        POS    = (LARGE-36) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+35) = ARG36
        WRITE (MESS,'(A)') TAMPON(1:LARGE)


C
C -- BOUCLE EN MOINS DU A L'ABSENCE DE REACTUALISATION GEOMETRIQUE
C
      ELSE IF (NATURE .EQ. 'GEOM_MIN ') THEN

        ARG22  = 'SUR GEOMETRIE INITIALE'
        POS    = (LARGE-21) / 2
        TAMPON = ' '
        TAMPON(POS:POS+21) = ARG22
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)
C
C -- BOUCLE SUPPLEMENTAIRE DU AUX GRANDS DEPLACEMENTS
C
      ELSE IF (NATURE .EQ. 'AUTO_GEO ') THEN

        IF (ARGI(1).EQ.1) THEN
           ARG4   = 'AVEC'
           POS    = (LARGE-32) / 2
           TAMPON = ' '
           TAMPON(POS:POS+5) = ARG4
           POS = POS + 5
           WRITE (ARG4,'(I2)') ARGI(1)
           TAMPON(POS:POS+3) = ARG4
           POS = POS + 3
           ARG29  = 'REACTUALISATION GEOMETRIQUE'
        ELSE
           ARG4   = 'AVEC'
           POS    = (LARGE-36) / 2
           TAMPON = ' '
           TAMPON(POS:POS+5) = ARG4
           POS = POS + 5
           WRITE (ARG4,'(I2)') ARGI(1)
           TAMPON(POS:POS+3) = ARG4
           POS = POS + 3
           ARG29  = 'REACTUALISATIONS GEOMETRIQUES'
        ENDIF
        TAMPON(POS:POS+29) = ARG29
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)
C
C -- BOUCLE SUPPLEMENTAIRE ATTENTE POINT FIXE CONTACT
C
      ELSE IF (NATURE .EQ. 'FIXE_NON ') THEN

        ARG36  = 'ATTENTE POINT FIXE POUR LE CONTACT'
        POS    = (LARGE-36) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+35) = ARG36
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,*)
C
C -- BOUCLE DE SEUIL CONTACT ECP
C
      ELSE IF (NATURE .EQ. 'BCL_SEUIL') THEN

        ARG54  ='BOUCLE DE SEUIL COULOMB N='
        POS    =  (52-32) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+26) = ARG54
        POS = POS + 28
        WRITE (ARG54,'(I2)') ARGI(1)
        TAMPON(POS:POS+2) = ARG54
        WRITE (MESS,'(A)') PLEINE  (1:52)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,'(A)') PLEINE  (1:52)
        WRITE (MESS,*)
C
C -- BOUCLE DE CONTRAINTES ACTIVES CONTACT ECP
C
      ELSE IF (NATURE .EQ. 'BCL_CTACT') THEN

        ARG54  ='BOUCLE DE CONTRAINTES ACTIVES N='
        POS    =  1
        TAMPON = ' '
        TAMPON(POS:POS+32) = ARG54
        POS = POS + 34
        WRITE (ARG54,'(I2)') ARGI(1)
        TAMPON(POS:POS+2) = ARG54
        WRITE (MESS,'(A)') PLEINE  (1:37)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,'(A)') PLEINE  (1:37)
        WRITE (MESS,*)
C
C -- BOUCLE DE GEOMETRIE CONTACT ECP
C
      ELSE IF (NATURE .EQ. 'BCL_GEOME') THEN

        ARG54  ='BOUCLE DE GEOMETRIE N='
        POS    =  (LARGE-28) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+22) = ARG54
        POS = POS + 24
        WRITE (ARG54,'(I2)') ARGI(1)
        TAMPON(POS:POS+2) = ARG54
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
C
C -- CONVERGENCE BOUCLE DE SEUIL CONTACT ECP
C
      ELSE IF (NATURE .EQ. 'CNV_SEUIL') THEN

        ARG54  ='CONVERGENCE BOUCLE SEUIL EN='
        POS    =  (52-30) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+27) = ARG54
        POS = POS + 29
        WRITE (ARG54,'(I2)') ARGI(1)
        TAMPON(POS:POS+2) = ARG54
        WRITE (MESS,'(A)') PLEINE  (1:52)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,'(A)') PLEINE  (1:52)
        WRITE (MESS,*)
        WRITE (MESS,*)
C
C -- CONVERGENCE BOUCLE DE CONTRAINTES ACTIVES CONTACT ECP
C
      ELSE IF (NATURE .EQ. 'CNV_CTACT') THEN

        ARG54  ='CONVERGENCE CONTRAINTES ACTIVES EN='
        POS    =  1
        TAMPON = ' '
        TAMPON(POS:POS+34) = ARG54
        POS = POS + 36
        WRITE (ARG54,'(I2)') ARGI(1)
        TAMPON(POS:POS+2) = ARG54
        WRITE (MESS,'(A)') PLEINE  (1:39)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,'(A)') PLEINE  (1:39)
        WRITE (MESS,*)
        WRITE (MESS,*)
C
C -- CONVERGENCE BOUCLE DE GEOMETRIE CONTACT ECP
C
      ELSE IF (NATURE .EQ. 'CNV_GEOME') THEN

        ARG54  ='CONVERGENCE DE LA BOUCLE DE GEOMETRIE EN='
        POS    =  (LARGE-42) / 2 + 1
        TAMPON = ' '
        TAMPON(POS:POS+40) = ARG54
        POS = POS + 42
        WRITE (ARG54,'(I2)') ARGI(1)
        TAMPON(POS:POS+2) = ARG54
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,'(A)') TAMPON(1:LARGE)
        WRITE (MESS,'(A)') PLEINE  (1:LARGE)
        WRITE (MESS,*)
        WRITE (MESS,*)

C
C -- ETAT DE LA CONVERGENCE
C
      ELSE IF (NATURE .EQ. 'ETAT_CONV') THEN

        TAMPON = COLONN(1:LARGE)
        POS = 2
        DO 100 I = 1, NBAFF
          IF (AFF(I)) THEN
            ARG16 = ' '
            IF (TYPE(I) .EQ. 'R') THEN
              WRITE (ARG16(2:13),'(1PE12.5)') ARGR(I)
              ARG16(15:15) = ARGZ(I)
            ELSE
              WRITE (ARG16(6:10),'(I5)') NINT(ARGR(I))
              ARG16(15:15) = ARGZ(I)
            END IF
            TAMPON(POS:POS+15) = ARG16
            POS = POS + 17
          END IF
 100    CONTINUE

        DO 110 I = 1, NBCOL-NBAFF
          IF (AFF(I+NBAFF)) THEN
            ARG16 = ' '
            IF (TYPE(I+NBAFF) .EQ. 'K') THEN
              ARG16 = INF16(I)
              INF16(I) = ' '
           END IF
            TAMPON(POS:POS+15) = ARG16
            POS = POS + 17
          END IF
 110    CONTINUE

        WRITE (MESS,'(A)') TAMPON(1:LARGE)


      ELSE
        CALL UTMESS('F','NMIMPR','NATURE DE MESSAGE ILLICITE (DVLP)')
      END IF

 9999 CONTINUE
      END
