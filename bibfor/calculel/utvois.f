      SUBROUTINE UTVOIS(TYPMAC,L2D,LMAJ,NARET,NSOMM,NSOMM2,POINC1,
     &                  POINC2,ITYP,ELREFE,ELREF2,NDEGRE,POIDS1,POIDS2,
     &                  IDFDX,IDFDX2,IDFDY,IDFDY2,NPGF,NPGF2,LLUMPE)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 21/01/2004   AUTEUR CIBHHLV L.VIVAN 
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
C RESPONSABLE BOITEAU O.BOITEAU
C TOLE CRP_21
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL DES CARACTERISTIQUES DES MAILLES
C                          VOISINES POUR AERER TE0003 ET RECUPERATION
C                          DES ADRESSES JEVEUX DE LEUR GEOMETRIE/FFORME
C
C IN TYPMAC  : TYPE DE L'ELEMENT COURANT.
C IN L2D     : FLAG EGALE A TRUE SI ON EST EN 2D.
C IN LMAJ    : FLAG EGALE A TRUE SI ON A UNE SOURCE NON NULLE.
C IN LLUMPE  : FLAG EGALE A TRUE SI ON EST EN LUMPE.
C OUT NARET  : NBRE D'ARETES (EN 2D) OU DE FACE (EN 3D)
C OUT NSOMM/2  : NBRE DE NOEUDS SOMMETS DE L'ARETE (EN 2D) OU DE(S)
C              (LA) FACE(S) (EN 3D).
C OUT POINC1/2 : POIDS DE NEWTON-COTES (POUR LES ARETES EN 2D)
C OUT ITYP    : TYPE DE MAILLE (EN 3D)
C OUT ELREFE,ELREF2 : TYPE D'ARETES OU DE(S) FACE(S)
C OUT NDEGRE  : DEGRE D'INTERPOLATION
C OUT POIDS1/2 : POIDS DE GAUSS (POUR LES FACES EN 3D).
C OUT IDFDX/2  : ADRESSES JEVEUX DES DERIVEES EN X DES FFORMES DES
C                FACES AUX SOMMETS (CF. INIOBJ ET CALCFF) (EN 3D)
C OUT IDFDY/2  : IDEM EN Y
C OUT NPGF/2   : NBRE POINT DE GAUSS FACE 1 ET 2 (POUR PENTAEDRE)
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       MSG:UTMESS.
C       JEVEUX: JEEXIN,JEVEUO.
C     FONCTIONS INTRINSEQUES:
C       AUCUNE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       20/09/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
C       05/02/02 (OB): EXTENSION AUX EFS LUMPES.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER NARET,NSOMM,NSOMM2,ITYP,NDEGRE,IDFDX,IDFDX2,IDFDY,IDFDY2,
     &        NPGF,NPGF2,NBELR, NDIM, NNO, NNOS, NPG1, IPOIDS, 
     &                  IVF, JGANO
      REAL*8 POINC1,POINC2,POIDS1(9),POIDS2(9)
      LOGICAL L2D,LMAJ,LLUMPE
      CHARACTER*8 ELREFE,ELREF2,TYPMAC

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES
      INTEGER I,IFFBUF,IRET
      CHARACTER*1 NOEU
      CHARACTER*2 FORM
      CHARACTER*24 CHVAL,CHVAL2

      ELREFE = ' '
      ELREF2 = ' '
      IF (L2D) THEN
C CAS 2D
        FORM = TYPMAC(1:2)
C CALCUL NBRE ARETES
        IF (FORM.EQ.'TR') THEN
          NARET = 3
        ELSE IF (FORM.EQ.'QU') THEN
          NARET = 4
        ELSE
          CALL UTMESS('F','UTVOIS',
     &                    '! CALCUL NARET 2D: TYPELEM INCONNU !')
        ENDIF
        NOEU = TYPMAC(5:5)
C CALCUL NBRE SOMMETS ARETES ET POIDS DE NEWTON-COTES DE L'ARETE
        IF (NOEU.EQ.'6' .OR. NOEU.EQ.'8' .OR. NOEU.EQ.'9') THEN
          NSOMM = 3
          NDEGRE = 2
          ELREFE = 'SEG3'
C INIT. POIDS DE NEWTON-COTES (POINTS EXTREMES 1/POINT CENTRAL 2)
          POINC1 = 1.D0/3.D0
          POINC2 = 4.D0/3.D0
        ELSE IF (NOEU.EQ.'3' .OR. NOEU.EQ.'4') THEN
          NSOMM = 2
          NDEGRE = 1
          ELREFE = 'SEG2'
          POINC1 = 1.D0
          POINC2 = 0.D0
        ELSE
          CALL UTMESS('F','UTVOIS',
     &                 '! CALCUL NSOMM 2D: TYPELEM INCONNU !')
        ENDIF

      ELSE
C CAS 3D
C HEXAEDRE --> FACE4 OU FACE8 OU FACE9
        IF(TYPMAC(1:4).EQ.'HEXA') THEN
          NARET = 6
          ITYP = 1
          IF(TYPMAC(5:5).EQ.'8') THEN
            ELREFE = 'QU4'
            NSOMM = 4
            NDEGRE = 1
            DO 31 I=1,4
              POIDS1(I) = 1.D0
   31       CONTINUE
          ELSE IF(TYPMAC(5:6).EQ.'20') THEN
            IF (LLUMPE) THEN
              CALL UTMESS('F','UTVOIS',
     &        '! PAS DE LUMPE EN 3D P2: HEXA20_D --> FACE8_D !')
            ENDIF
            ELREFE = 'QU8'
            NSOMM = 8
            NDEGRE = 2
            DO 32 I=1,4
              POIDS1(I) = 1.D0/9.D0
   32       CONTINUE
            DO 33 I=5,8
              POIDS1(I) = 4.D0/9.D0
   33       CONTINUE
          ELSE IF(TYPMAC(5:6).EQ.'27') THEN
            IF (LLUMPE) THEN
              CALL UTMESS('F','UTVOIS',
     &        '! PAS DE LUMPE EN 3D P2: HEXA27 --> FACE9_D !')
            ENDIF
            ELREFE = 'QU9'
            NSOMM = 9
            NDEGRE = 2
            DO 34 I=1,4
              POIDS1(I) = 1.D0/9.D0
   34       CONTINUE
            DO 35 I=5,8
              POIDS1(I) = 4.D0/9.D0
   35       CONTINUE
            POIDS1(9) = 16.D0/9.D0
          ENDIF

C PENTAEDRE --> FACE3/4 OU 6/8
        ELSE IF( TYPMAC(1:5).EQ.'PENTA') THEN
          NARET = 5
          ITYP = 2
          IF( TYPMAC(6:6).EQ.'6') THEN
            ELREFE = 'TR3'
            ELREF2 = 'QU4'
            NSOMM = 3
            NSOMM2 = 4
            NDEGRE = 1
            DO 36 I=1,3
              POIDS1(I) = 1.D0/6.D0
   36       CONTINUE
            DO 37 I=1,4
              POIDS2(I) = 1.D0
   37       CONTINUE
          ELSE IF( TYPMAC(6:7).EQ.'15') THEN
            IF (LLUMPE) THEN
              CALL UTMESS('F','UTVOIS',
     &        '! PAS DE LUMPE EN 3D P2: PENTA15_D --> FACE6/8_D !')
            ENDIF
            ELREFE = 'TR6'
            ELREF2 = 'QU8'
            NSOMM = 6
            NSOMM2 = 8
            NDEGRE = 2
            DO 38 I=1,3
              POIDS1(I) = 0.D0
   38       CONTINUE
            DO 39 I=4,6
              POIDS1(I) = 1.D0/6.D0
   39       CONTINUE
            DO 40 I=1,4
              POIDS2(I) = 1.D0/9.D0
   40       CONTINUE
            DO 41 I=5,8
              POIDS2(I) = 4.D0/9.D0
   41       CONTINUE
          ENDIF

C TETRADRE --> FACE3 OU FACE6
        ELSE IF( TYPMAC(1:5).EQ.'TETRA') THEN
          NARET = 4
          ITYP = 3
          IF( TYPMAC(6:6).EQ.'4') THEN
            ELREFE = 'TR3'
            NSOMM = 3
            NDEGRE = 1
            DO 42 I=1,3
              POIDS1(I) = 1.D0/6.D0
   42       CONTINUE
          ELSE IF( TYPMAC(6:7).EQ.'10') THEN
            IF (LLUMPE) THEN
              CALL UTMESS('F','UTVOIS',
     &        '! PAS DE LUMPE EN 3D P2: TETRA10_D --> FACE6_D !')
            ENDIF
            ELREFE = 'TR6'
            NSOMM = 6
            NDEGRE = 2
            DO 43 I=1,3
              POIDS1(I) = 0.D0
   43       CONTINUE
            DO 44 I=4,6
              POIDS1(I) = 1.D0/6.D0
   44      CONTINUE
          ENDIF
        ELSE
          CALL UTMESS('F','UTVOIS',
     &           '! CALCUL NARET/NSOMM 3D: TYPELEM INCONNU !')
        ENDIF

        IF (L2D) THEN
          CHVAL = '&INEL.'//ELREFE//'.FFORMES'
          CALL JEEXIN(CHVAL,IRET)
          IF (IRET.LE.0) CALL UTMESS('F','UTVOIS',
     &              '! L''OBJET CHVAL DES SEGMENTS EST INEXISTANT !')
          IF (NARET.EQ.5) THEN
            CHVAL2 = '&INEL.'//ELREF2//'.FFORMES'
            CALL JEEXIN(CHVAL2,IRET)
            IF (IRET.LE.0) CALL UTMESS('F','UTVOIS',
     &             '! L''OBJET CHVAL2 DES SEGMENTS EST INEXISTANT !')
          ENDIF
          NPGF = NSOMM
          CALL JEVEUO(CHVAL,'L',IFFBUF)
          IDFDX  = IFFBUF + NSOMM*NPGF
          IDFDY  = IDFDX  + 1
          IF(NARET.EQ.5) THEN
C NEWTON-COTES ON IMPOSE NPGF2 = NSOMM2
            NPGF2 = NSOMM2
            CALL JEVEUO(CHVAL2,'L',IFFBUF)
            IDFDX2  = IFFBUF + NSOMM2*NPGF2
            IDFDY2  = IDFDX2  + 1
          ENDIF
        ELSE
          CALL ELREF4 ( ELREFE, 'MASS', NDIM, NNO, NNOS, NPG1, IPOIDS, 
     &                  IVF, IDFDX, JGANO )
          IDFDY  = IDFDX  + 1
          NPGF = NSOMM
          IF(NARET.EQ.5) THEN
            CALL ELREF4 ( ELREF2, 'MASS', NDIM, NNO, NNOS, NPG1, IPOIDS,
     &                    IVF, IDFDX2, JGANO )
            IDFDY2  = IDFDX2  + 1
            NPGF2 = NSOMM2
          ENDIF
        ENDIF

      ENDIF

C MAUVAIS CALCUL EN P1 SI FORCE VOLUMIQUE NON NULLE
      IF ((NDEGRE.EQ.1).AND.LMAJ) CALL UTMESS('A','UTVOIS',
     &         '! P2 OBLIGEATOIRE AVEC TERME SOURCE NON NUL !')
      END
