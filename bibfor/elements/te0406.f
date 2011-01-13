      SUBROUTINE TE0406 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16    OPTION , NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/01/2011   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ......................................................................
C     FONCTION  :  CALCUL DES OBJETS ELEMENTS FINIS EN DYNAMIQUE
C                  LINEAIRE
C                  COQUE_3D
C
C     OPTIONS   :  MASS_MECA      MATRICE DE MASSE COHERENTE
C                  M_GAMMA        FORCE NODALE D INERTIE
C                  ECIN_ELEM ENERGIE CINETIQUE D UN MODE PROPRE
C
C     ARGUMENTS :
C     DONNEES   :      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C ......................................................................
C
C     FONCTION  :  CALCUL DES OBJETS ELEMENTS FINIS EN DYNAMIQUE
C                  LINEAIRE
C                  COQUE_3D
C
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR ,DDOT
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
C
C
C---- DECLARATIONS STANDARDS
C
      INTEGER IGEOM
C
      INTEGER LZI , LZR , JCARA
C
      INTEGER NB1 , NB2
C
      INTEGER     INTSN , NPGSN
      INTEGER     INTE  , NPGE
C
      REAL * 8 RHO , EPAIS , CTOR
C
      REAL * 8 VECTA ( 9 , 2 , 3 )
      REAL * 8 VECTN ( 9 , 3 ) , VECTPT ( 9 , 2 , 3 )
C
      REAL * 8 VECTG ( 2 , 3 ) , VECTT ( 3 , 3 )
C
      REAL * 8 JM1 ( 3 , 3 ) , DETJ
C
      INTEGER I  , J ,IRET
      INTEGER JD
      INTEGER             KOMPT
C
      INTEGER       IMATUU,IACCE,IVECT
C
      INTEGER       JENER , JFREQ , IU
C
      REAL * 8      MAS ( 2601 ) , MASU ( 51 )
      REAL * 8    MANTN ( 2601 )
C
      REAL * 8 BID33 ( 3 , 3 )
C
      REAL * 8 MATN ( 3 , 51 ) , MATNT ( 51 , 3 )
C
      PARAMETER       ( NPGE = 2 )
      REAL * 8 EPSVAL ( NPGE ) , KSI3S2
C
      REAL * 8 XMIN
C
      REAL * 8 R8PREM
C
      INTEGER IN , ICOMPO
C
      INTEGER II , JJ
C
C---- DECLARATIONS ROTATION GLOBAL LOCAL AU NOEUDS
C
      INTEGER IMAS
C
      REAL * 8 LAM0  ( 3 , 3 )
      REAL * 8 MASRG ( 3 , 3 )
      REAL * 8 MASRL ( 3 , 3 )
      REAL * 8 MNN
C
C DEB
C
C---- TEST D'EXISTENCE "COMPOR"
C
      CALL TECACH ( 'NNN', 'PCOMPOR', 1, ICOMPO,IRET )
      IF ( ICOMPO .NE. 0 ) THEN
         IF ( ZK16(ICOMPO+2).EQ.'GROT_GDEP' ) THEN
            CALL U2MESS('F','ELEMENTS3_91')
        ENDIF
      ENDIF
C
C---- LES NOMBRES
C
      EPSVAL ( 1 ) = - 1.D0 / SQRT ( 3.D0 )
      EPSVAL ( 2 ) =   1.D0 / SQRT ( 3.D0 )
C
C---- RECUPERATION DES POINTEURS ( L : LECTURE, E : ECRITURE )
C
C....... GEOMETRIE ( COORDONNEES DES NOEUDS )
C
         CALL JEVECH ( 'PGEOMER' , 'L' , IGEOM )
C
C---- RECUPERATION DES OBJETS INITIALISES ( SAUF NPGSR )
C
C....... LES ENTIERS
C
         CALL JEVETE ( '&INEL.'//NOMTE(1:8)//'.DESI' , ' ' , LZI )
C
C------- NOMBRE DE NOEUDS ( NB1 : SERENDIP , NB2 : LAGRANGE )
C
         NB1   = ZI ( LZI - 1 + 1 )
         NB2   = ZI ( LZI - 1 + 2 )
C
C------- NBRE POINTS INTEGRATIONS ( NPGSR : REDUITE , NPGSN : NORMALE )
C
         NPGSN = ZI ( LZI - 1 + 4 )
C
C....... LES REELS ( FONCTIONS DE FORMES, DERIVEES ET POIDS )
C
         CALL JEVETE ( '&INEL.'//NOMTE(1:8)//'.DESR' , ' ' , LZR )
C
C------ CARACTERISTIQUES DE COQUE
C
        CALL JEVECH ( 'PCACOQU' , 'L' , JCARA )
C
C------ COEFFICIENT DE MASSE AUTOURS DE LA NORMALE
C
        CTOR  = ZR ( JCARA + 4 )
C
C------ MASSE VOLUMIQUE ET EPAISSEUR
C
        CALL DXROEP ( RHO , EPAIS )
C
C---- INITIALISATION
C
      CALL R8INIR ( 51 * 51 , 0.D0 , MAS   , 1 )
      CALL R8INIR ( 51 * 51 , 0.D0 , MANTN , 1 )
C
C---- VECTEURS DE BASE AUX NOEUDS
C
      CALL VECTAN(NB1,NB2, ZR(IGEOM) , ZR(LZR) ,VECTA,VECTN,VECTPT)
C
C---- BOUCLE SUR LES POINTS D INTEGRATION NORMALE SUR L EPAISSEUR
C
      DO 600 INTE = 1 , NPGE
C
C------- COORDONNEE ISOPARAMETRIQUE SUR L EPAISSEUR  DIVISEE PAR DEUX
C
         KSI3S2 = EPSVAL ( INTE ) / 2.D0
C
C------- BOUCLE SUR LES POINTS D INTEGRATION NORMALE
C
         DO 610 INTSN = 1 , NPGSN
C
C---------- VECTEUR LOCAUX
C
            CALL VECTGT ( 1 , NB1 , ZR ( IGEOM ) , KSI3S2 , INTSN ,
     &                    ZR ( LZR ) , EPAIS , VECTN , VECTG , VECTT )
C
C---------- CALCUL DE DETJ
C
            CALL JACBM1 ( EPAIS , VECTG , VECTT , BID33 , JM1 , DETJ )
C
C---------  MATRICE N
C
            CALL MATRN( NB1 , NB2 , ZR (LZR) , KSI3S2 , EPAIS , INTSN ,
     &                  VECTN ,
     &                  MATN )
C
C---------- TRANSPOSE DE MATN
C
            CALL TRANSP ( MATN  , 3           , 3 , 6 * NB1 + 3  ,
     &                    MATNT , 6 * NB1 + 3 )
C
C---------- PRODUIT MANT * MATN
C
            CALL PROMAT
     &           ( MATNT , 6 * NB1 + 3 , 6 * NB1 + 3 , 3           ,
     &             MATN  , 3           , 3           , 6 * NB1 + 3 ,
     &             MANTN )
C
C---------- INTEGRATION NUMERIQUE
C
            DO 200 J = 1 , 6 * NB1 + 3
               DO 210 I = 1 , 6 * NB1 + 3
                      JD = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
                  MAS ( JD ) = MAS ( JD ) +
     & (
     &   RHO * MANTN ( JD ) * ZR ( LZR - 1 + 127 +       INTSN - 1 ) *
     &   DETJ * 1.D0
     & )
 210           CONTINUE
 200        CONTINUE
C
C
C
 610     CONTINUE
 600  CONTINUE
C
C
      XMIN = 1.D0 / R8PREM ( )
C
C---- EN CHAQUE NOEUD
C
      DO 401 IN = 1 , NB2
C
C------- ON CONSTRUIT LAMBDA0
C
         DO 411 II = 1 , 3
            LAM0 ( II , 1 ) = VECTPT ( IN , 1 , II )
            LAM0 ( II , 2 ) = VECTPT ( IN , 2 , II )
            LAM0 ( II , 3 ) = VECTN  ( IN ,     II )
 411     CONTINUE
C
C------- ON CONSTRUIT MASRG
C
         IF ( IN . LE . NB1 ) THEN
C
C-------------- NOEUDS DE SERENDIP
                DO 431 JJ = 1 , 3
                   DO 441 II = 1 , 3
                       J    = 6 * ( IN - 1 ) + JJ + 3
                       I    = 6 * ( IN - 1 ) + II + 3
                       IMAS = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
                       MASRG ( II , JJ ) = MAS ( IMAS )
 441               CONTINUE
 431            CONTINUE
C
         ELSE
C
C-------------- SUPERNOEUD
                DO 451 JJ = 1 , 3
                   DO 461 II = 1 , 3
                       J    = 6 * NB1        + JJ
                       I    = 6 * NB1        + II
                       IMAS = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
                       MASRG ( II , JJ ) = MAS ( IMAS )
 461               CONTINUE
 451            CONTINUE
C
         ENDIF
C
C------- ROTATION DE MASRG : LOCALES --> GLOBALES
C
C        MASRL =  ( LAMBDA0 )   * SIGMT * ( LAMBDA0 ) T
C
         CALL  BTKB ( 3 , 3 , 3 , MASRG , LAM0  , BID33 , MASRL )
C
C------- ON COMPARE LES DEUX PREMIERS TERMES DIAGONAUX DE MASRL
C
         IF ( MASRL ( 1 , 1 ) .LT. XMIN ) XMIN = MASRL ( 1 , 1 )
         IF ( MASRL ( 2 , 2 ) .LT. XMIN ) XMIN = MASRL ( 2 , 2 )
C
 401  CONTINUE
C
CCC   MNN = 1.D-3 * XMIN
      MNN = CTOR * XMIN
C
C------- AFFECTATION
C
         DO 301 IN = 1 , NB2
C
            IF ( IN . LE . NB1 ) THEN
C
C-------------- NOEUDS DE SERENDIP
                DO 331 JJ = 1 , 3
                   DO 341 II = 1 , 3
                       J    = 6 * ( IN - 1 ) + JJ + 3
                       I    = 6 * ( IN - 1 ) + II + 3
               MAS ( ( 6 * NB1 + 3 ) * ( J - 1 ) + I ) =
     &         MAS ( ( 6 * NB1 + 3 ) * ( J - 1 ) + I )
     &         + MNN  * VECTN ( IN , II ) * VECTN ( IN , JJ )
 341               CONTINUE
 331            CONTINUE
C
            ELSE
C
C-------------- SUPERNOEUD
                DO 351 JJ = 1 , 3
                   DO 361 II = 1 , 3
                       J    = 6 * NB1        + JJ
                       I    = 6 * NB1        + II
               MAS ( ( 6 * NB1 + 3 ) * ( J - 1 ) + I ) =
     &         MAS ( ( 6 * NB1 + 3 ) * ( J - 1 ) + I )
     &         + MNN  * VECTN ( IN , II ) * VECTN ( IN , JJ )
 361               CONTINUE
 351            CONTINUE
C
            ENDIF
C
 301     CONTINUE
C
C
C
COOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
C
C-----------------------------------------------
      IF      ( OPTION .EQ. 'MASS_MECA' )      THEN
C-----------------------------------------------
C
C======= STOCKAGE DE LA PARTIE TRIANGULAIRE SUPERIEURE
C
C------- ADRESSE DE LA PARTIE TRIANGULAIRE SUPERIEURE DE LA MASSE
C
         CALL JEVECH ( 'PMATUUR' , 'E' , IMATUU )
C
         KOMPT = 0
C
         DO 900  J = 1 , 6 * NB1 + 3
            DO 910  I = 1 , J
               KOMPT = KOMPT + 1
      ZR ( IMATUU - 1 + KOMPT )=MAS ( ( 6 * NB1 + 3 ) * ( J - 1 ) + I )
 910          CONTINUE
 900       CONTINUE
C
C
C--------------------------------------------
C
      ELSE IF ( OPTION .EQ. 'M_GAMMA'        ) THEN
C--------------------------------------------
C
C
C
C======= CALCUL ET STOCKAGE DE LA FORCE NODALE D INERTIE
C
C------- ADRESSE DE L'ACCELERATION NODALE
C
         CALL JEVECH ( 'PDEPLAR' , 'L' , IACCE )
C
C------- ADRESSE DE LA FORCE NODALE D INERTIE
C
         CALL JEVECH ( 'PVECTUR' , 'E' , IVECT )
C
         CALL PMAVEC('ZERO', 6 * NB1 + 3 , MAS ,ZR(IACCE),ZR(IVECT))
C
C---------------------------------------------
      ELSE IF ( OPTION .EQ. 'ECIN_ELEM' ) THEN
C---------------------------------------------
C
C======= CALCUL ET STOCKAGE DE L ENERGIE CINETIQUE
C
C------- LECTURE DE L'ADRESSE
C
         CALL JEVECH ( 'PENERCR' , 'E' , JENER )
C
C------- ADRESSE DU MODE
C
         CALL JEVECH ( 'PDEPLAR' , 'L' , IU    )
C
C------- ADRESSE DE LA FREQUENCE
C
         CALL JEVECH ( 'PFREQR'  , 'L' , JFREQ   )
C
         CALL R8INIR ( 51 , 0.D0 , MASU , 1 )
C
         CALL PMAVEC('ZERO', 6 * NB1 + 3 , MAS ,ZR ( IU ), MASU )
C
         ZR (JENER)=DDOT(6 * NB1 + 3,ZR (IU ),1,MASU,1)
C
C------- VITESSE = OMEGA * MODE
C
         ZR ( JENER ) = 0.5D0 * ZR ( JFREQ ) * ZR ( JENER )
C
C------- ENERGIE DE MEMBRANE = ENERGIE TOTALE
C        ENERGIE DE FLEXION  = ENERGIE TOTALE
C
         CALL R8INIR ( 2 , ZR ( JENER ) , ZR ( JENER + 1 ) , 1 )
C
C
C---------------------------------------------
      ELSE
C---------------------------------------------
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
C
C---------------------------------------------
      ENDIF
C---------------------------------------------
C
      END
