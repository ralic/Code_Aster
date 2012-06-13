      SUBROUTINE VDPNLR ( OPTION , NOMTE , CODRET )
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
      INTEGER              CODRET
C
C ......................................................................
C     FONCTION  :  CALCUL DES OBJETS ELEMENTS FINIS EN NON LINEAIRE
C                  GEOMETRIQUE AVEC GRANDES ROTATIONS
C                  COQUE_3D
C
C     ARGUMENTS :
C     DONNEES   :      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C     OPTIONS   :
C                  RIGI_MECA_TANG : MATRICE TANGENTE DE RIGDITE
C                                   PHASE DE PREDICTION AU DEBUT DE
C                                   CHAQUE PAS
C
C                  RAPH_MECA      : CONTRAINTES CAUCHY ET FORCE INTERNE
C                                   SANS MATRICE TANGENTE DE RIGIDITE
C                                   REAC_ITER : 0 DANS STAT_NON_LINE
C
C                  FULL_MECA      : RAPH_MECA + RIGI_MECA_TANG
C                                   ITERATION TYPIQUE DE NEWTON
C
C ......................................................................
C
      REAL*8 R8VIDE,DDOT
C
C---- DECLARATIONS BIDONS
C
      REAL * 8 BID33 ( 3 , 3 )
C
C---- DECLARATIONS LOCALES
C
      INTEGER I  ,  J
      INTEGER IN
      INTEGER      JD
      INTEGER II , JJ
      INTEGER             K1,JNBSPI
C
C---- DECLARATIONS RIGIDITE GEOMETRIQUE
C
      REAL * 8 ETILD  ( 5 ) , STILD  ( 5 )
      REAL * 8 ETILDM  ( 5 )
      REAL * 8 EPS2D ( 4 ) , DEPS2D ( 4 )
      REAL * 8 SIGN ( 4 ) , SIGMA ( 4 ) , DSIDEP ( 6 , 6 )
      REAL * 8 DETILD  ( 5 ), RBID
      REAL * 8 GXZ, GYZ
      REAL * 8                STLIS ( 5 , 4 )
      REAL * 8 BARS ( 9 , 9 )
      REAL * 8 VECNI ( 3 )  , ANTNI ( 3 , 3 )
      REAL * 8 VECZN ( 27 )
      REAL * 8 ANTZI ( 3 , 3 )
      REAL * 8 RIGNC ( 3  , 3  )
C
C---- DECLARATIONS STANDARDS
C
      INTEGER IGEOM , ICONTP , IMATUN , IVECTU , IVARIP , COD
      INTEGER ICONTM , IVARIX
      INTEGER LZI , LZR , JCARA
      INTEGER NB1 , NB2  , NDIMV
C
C---- DECLARATIONS PROPRES COQUE_3D NON LINEAIRE
C
      REAL * 8 MATC ( 5 , 5 )
      REAL * 8 DTILD ( 5 , 5 )
      INTEGER     INTE , INTSR , INTSN
      INTEGER            KNTSR
      REAL * 8 EPTOT , KAPPA , CTOR
      INTEGER     NPGE      , NPGSR , NPGSN, KSP
      PARAMETER ( NPGE = 3 )
      REAL * 8 VECTA ( 9 , 2 , 3 )
      REAL * 8 VECTN  ( 9 , 3 ) , VECTPT ( 9 , 2 , 3 )
      REAL * 8 VECNPH ( 9 , 3 )
      REAL * 8 VECPHM( 9 , 3 )
      REAL * 8 VECTG ( 2 , 3 ) , VECTT ( 3 , 3 )
      REAL * 8 JM1 ( 3 , 3 ) , DETJ
      REAL * 8 HSC ( 5 , 9 )
      REAL * 8 JDN1RI ( 9 , 51 ) , JDN1RC ( 9 , 51 )
      REAL * 8 JDN1NI ( 9 , 51 ) , JDN1NC ( 9 , 51 )
      REAL * 8                     JDN2RC ( 9 , 51 )
      REAL * 8                     JDN2NC ( 9 , 51 )
      REAL * 8                     JD2RCM ( 9 , 51 )
      REAL * 8                     JD2NCM ( 9 , 51 )
      REAL * 8 J1DN3 ( 9 , 27 )
      REAL * 8 BTILD3 ( 5 , 27 )
      REAL * 8 KSI3S2
C
C---- DECLARATIONS COUCHES
C
      INTEGER ICOMPO, NBCOU
      INTEGER ICOU
      REAL * 8 ZIC  , ZMIN , EPAIS , COEF
C
C---- DECLARATIONS COQUE NON LINEAIRE
C
      REAL * 8 VRIGNC ( 2601 ) , VRIGNI ( 2601 )
      REAL * 8 VRIGRC ( 2601 ) , VRIGRI ( 2601 )
      REAL * 8 KNN
      INTEGER IUP , IUM
      INTEGER IUD
      REAL * 8 B1SU  ( 5 , 51 ) , B2SU  ( 5 , 51 )
      REAL * 8 B1SUM  ( 5 , 51 ) , B2SUM  ( 5 , 51 )
      REAL * 8 B1SRC ( 2 , 51 , 4 )
      REAL * 8 B2SRC ( 2 , 51 , 4 )
      REAL * 8 B1SRCM ( 2 , 51 , 4 )
      REAL * 8 B2SRCM ( 2 , 51 , 4 )
      REAL * 8 B1MNC ( 3 , 51 ) , B1MNI ( 3 , 51 )
      REAL * 8 B2MNC ( 3 , 51 ) , B2MNI ( 3 , 51 )
      REAL * 8 B1MNCM ( 3 , 51 ) , B1MNIM ( 3 , 51 )
      REAL * 8 B2MNCM ( 3 , 51 ) , B2MNIM ( 3 , 51 )
      REAL * 8 B1MRI ( 3 , 51 , 4 )
      REAL * 8 B2MRI ( 3 , 51 , 4 )
      REAL * 8 B1MRIM ( 3 , 51 , 4 )
      REAL * 8 B2MRIM ( 3 , 51 , 4 )
      REAL * 8 DUDXRI ( 9 ) , DUDXNI ( 9 )
      REAL * 8 DUDXRC ( 9 ) , DUDXNC ( 9 )
      REAL * 8 DUDRIM ( 9 ) , DUDNIM ( 9 )
      REAL * 8 DUDRCM ( 9 ) , DUDNCM ( 9 )
      REAL * 8 VECU ( 8 , 3 ) , VECTHE ( 9 , 3 )
      REAL * 8 VECUM( 8 , 3 ) , VECTHM ( 9 , 3 )
      REAL * 8 VECPE  ( 51 )
      REAL * 8 VECPEM ( 51 )
C
C---- DECLARATIONS ROTATION GLOBAL LOCAL AU NOEUDS
C
C      INTEGER IRIG
C
      REAL * 8 BLAM ( 9 , 3 , 3 )
      REAL * 8 BLAMM ( 9 , 3 , 3 )

C
      REAL * 8 THETA ( 3 ) , THETAN
      REAL * 8 TMOIN1 ( 3 , 3 )  , TM1T ( 3 , 3 )
      REAL * 8 TERM   ( 3 )
C
C
C---- LES REALS
      INTEGER ICODRE ( 26 )
      CHARACTER * 8 NOMRES ( 26 ), TYPMOD ( 2 )
      CHARACTER * 10 PHENOM
      INTEGER IMATE, ICARCR, IINSTM, IINSTP, IVARIM 
      INTEGER NBVARI, ITAB(8), LGPG, K2  
      INTEGER NBV ,IRET
      REAL * 8 RAC2,ANGMAS(3)
      REAL * 8 LC
      REAL * 8 VALRES ( 26 )
      REAL * 8  CISAIL
C
      RAC2 = SQRT(2.D0)
      TYPMOD(1) = 'C_PLAN  '
      TYPMOD(2) = '        '
      CODRET=0
C
C DEB
C
C
C______________________________________________________________________
C
C---- CALCUL COMMUNS A TOUTES LES OPTIONS
C______________________________________________________________________
C
C---- LE NOMBRE DE COUCHES
C
      CALL JEVECH('PMATERC','L',IMATE)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL JEVECH ( 'PVARIMR' , 'L' , IVARIM )
      CALL JEVECH ( 'PMATERC' , 'L' , IMATE )
      CALL JEVECH ( 'PINSTMR' , 'L' , IINSTM )
      CALL JEVECH ( 'PINSTPR' , 'L' , IINSTP )
      CALL JEVECH ( 'PCOMPOR' , 'L' , ICOMPO )
C
      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU=ZI(JNBSPI-1+1)
C
      IF ( NBCOU . LE . 0 )
     &   CALL U2MESS('F','ELEMENTS_12')
C
      IF ( NBCOU . GT . 10 )
     &   CALL U2MESS('F','ELEMENTS_13')
C
      READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
      CALL TECACH('OON','PVARIMR',7,ITAB,IRET)
C      LGPG = MAX(ITAB(6),1)*ITAB(7) resultats faux sur Bull avec ifort
      IF (ITAB(6).LE.1) THEN
         LGPG=ITAB(7)
      ELSE
         LGPG = ITAB(6)*ITAB(7)
      ENDIF
      CALL JEVECH('PCARCRI','L',ICARCR)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)

      IF (PHENOM.EQ.'ELAS') THEN
        NBV = 2
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
      ELSE
        CALL U2MESS('F','ELEMENTS_42')
      END IF
C______________________________________________________________________
C
C---- RECUPERATION DES POINTEURS ( L : LECTURE )
C______________________________________________________________________
C
C....... GEOMETRIE INITIALE ( COORDONNEES INITIALE DES NOEUDS )
C
         CALL JEVECH ( 'PGEOMER' , 'L' , IGEOM )
C
C---- RECUPERATION DES OBJETS INITIALISES
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
         NPGSR = ZI ( LZI - 1 + 3 )
         NPGSN = ZI ( LZI - 1 + 4 )
C
C....... LES REELS ( FONCTIONS DE FORMES, DERIVEES ET POIDS )
C
         CALL JEVETE ( '&INEL.'//NOMTE(1:8)//'.DESR' , ' ' , LZR )
C
C______________________________________________________________________
C
C---- RECUPERATION DES POINTEURS ( E : ECRITURE ) SELON OPTION
C______________________________________________________________________
C
      IF ( OPTION ( 1 :  9 ) . EQ . 'RAPH_MECA'      . OR .
     &     OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             ) THEN
C
C------- CONTRAINTES DE CAUCHY AUX POINTS DE GAUSS
C
         CALL JEVECH ( 'PCONTPR' , 'E' , ICONTP )
C
C------- VECTEUR DES FORCES INTERNES
C
         CALL JEVECH ( 'PVECTUR' , 'E' , IVECTU )
C
C------- VARIABLES INTERNES INACTIVES DANS NOTRE CAS
C
         CALL JEVECH ( 'PVARIPR' , 'E' , IVARIP )
C
      ELSE
C       -- POUR AVOIR UN TABLEAU BIDON A DONNER A NMCOMP :
        IVARIP = IVARIM
      ENDIF
        NDIMV=LGPG*NPGSN
        CALL JEVECH('PVARIMP','L',IVARIX)
        CALL DCOPY(NDIMV,ZR(IVARIX),1,ZR(IVARIP),1)
C
      IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_TANG' . OR .
     &     OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             ) THEN
C
C------- MATRICE TANGENTE DE RIGIDITE ET INITIALISATION
C
         CALL JEVECH ( 'PMATUNS' , 'E' , IMATUN )
C
C------- INITIALISATION DES MATRICES GEOMETRIQUES
C
C------- NORMAL   COMPLET ( CONTRAINTES MEMBRANE FLEXION )
C
         CALL R8INIR ( 51 * 51 , 0.D0 , VRIGNC , 1 )
C
C------- NORMAL INCOMPLET ( CONTRAINTES MEMBRANE FLEXION )
C
         CALL R8INIR ( 51 * 51 , 0.D0 , VRIGNI , 1 )
C
C------- REDUIT INCOMPLET ( CONTRAINTES SHEAR            )
C
         CALL R8INIR ( 51 * 51 , 0.D0 , VRIGRI , 1 )
C
C------- REDUIT   COMPLET ( CONTRAINTES SHEAR            )
C
         CALL R8INIR ( 51 * 51 , 0.D0 , VRIGRC , 1 )
C
C------- INITIALISATION DE VECZN AVANT INTEGRATION
C
         CALL R8INIR ( 27      , 0.D0 , VECZN  , 1 )
C
      ENDIF
C______________________________________________________________________
C
C---- CARACTERISTIQUES DE COQUE
C
      CALL JEVECH ( 'PCACOQU' , 'L' , JCARA )
C
C---- EPAISSEUR TOTALE
C
      EPTOT = ZR ( JCARA     )
C
C---- COEFFICIENT DE CORRECTION DU SHEAR
C
      KAPPA = ZR ( JCARA + 3 )
C
C---- COEFFICIENT DE RIGIDITE AUTOUR DE LA TRANSFORMEE DE LA NORMALE
C
      CTOR  = ZR ( JCARA + 4 )
C
C---- COORDONNEE MINIMALE SUIVANT L EPAISSEUR
C
      ZMIN  = - EPTOT / 2.D0
C
C---- EPAISSEUR D UNE COUCHE
C
      EPAIS =   EPTOT / NBCOU
C
C______________________________________________________________________
C
C______________________________________________________________________
C
C---- RECUPERATION DE L ADRESSE DES VARIABLES NODALES TOTALES
C     QUI NE POSSEDE PAS LE MEME SENS POUR LES DEPLACEMENTS
C     ET LES ROTATIONS
C
C---- A L INSTANT MOINS  ( PAS PRECEDENT )
C
      CALL JEVECH ( 'PDEPLMR' , 'L' , IUM    )
      CALL JEVECH ( 'PCONTMR',  'L',  ICONTM )
C
C---- A L INSTANT PLUS  ( DEPUIS LE PAS PRECEDENT PAS PRECEDENT )
C
      CALL JEVECH ( 'PDEPLPR' , 'L' , IUP    )
C______________________________________________________________________
C
C---- ENTRE DEUX ITERATIONS
C
      CALL JEVECH ( 'PDDEPLA' , 'L' , IUD    )
C
C______________________________________________________________________
C
C
C---- REPERE LOCAUX AUX NOEUDS SUR LA CONFIGURATION INITIALE
C
      CALL VECTAN(NB1,NB2, ZR(IGEOM) , ZR(LZR) ,VECTA,VECTN,VECTPT)
C
C---- DEPLACEMENT TOTAL AUX NOEUDS DE SERENDIP
C
      CALL R8INIR ( 8 * 3 , 0.D0 , VECU   , 1 )
      CALL R8INIR ( 8 * 3 , 0.D0 , VECUM   , 1 )
C
      DO 101 IN = 1 , NB1
         DO 111 II = 1 , 3
C
           VECU   ( IN , II ) = ZR ( IUM - 1 + 6 * ( IN - 1 ) + II     )
     &                        + ZR ( IUP - 1 + 6 * ( IN - 1 ) + II     )
           VECUM  ( IN , II ) = ZR ( IUM - 1 + 6 * ( IN - 1 ) + II     )
C
 111     CONTINUE
 101  CONTINUE
C
C---- ROTATION TOTALE AUX NOEUDS
C
      CALL R8INIR ( 9 * 3 , 0.D0 , VECTHE , 1 )
      CALL R8INIR ( 9 * 3 , 0.D0 , VECTHM , 1 )
C
      IF ( ZK16 ( ICOMPO+2 ) . EQ . 'GROT_GDEP' ) THEN
C
C------- EN ACCORD AVEC LA MISE A JOUR DES GRANDES ROTATIONS AUFAURE
C
C------- NOEUD DE SERENDIP
C
         DO 201 IN = 1 , NB1
            DO 211 II = 1 , 3
           VECTHE ( IN , II ) = ZR ( IUP - 1 + 6 * ( IN - 1 ) + II + 3 )
           VECTHM ( IN , II ) = ZR ( IUM - 1 + 6 * ( IN - 1 ) + II + 3 )
 211        CONTINUE
 201     CONTINUE
C
C------- SUPERNOEUD
C
         DO 221 II = 1 , 3
           VECTHE ( NB2, II ) = ZR ( IUP - 1 + 6 * ( NB1    ) + II     )
           VECTHM ( NB2, II ) = ZR ( IUM - 1 + 6 * ( NB1    ) + II     )
 221     CONTINUE
C
      ELSE
C
C------- EN ACCORD AVEC LA MISE A JOUR CLASSIQUE DE STAT_NON_LINE
C
C------- NOEUDS DE SERENDIP
C
         DO 202 IN = 1 , NB1
            DO 212 II = 1 , 3
           VECTHE ( IN , II ) = ZR ( IUM - 1 + 6 * ( IN - 1 ) + II + 3 )
     &                        + ZR ( IUP - 1 + 6 * ( IN - 1 ) + II + 3 )
           VECTHM ( IN , II ) = ZR ( IUM - 1 + 6 * ( IN - 1 ) + II + 3 )
 212        CONTINUE
 202     CONTINUE
C
C--------- SUPERNOEUD
C
         DO 222 II = 1 , 3
           VECTHE ( NB2, II ) = ZR ( IUM - 1 + 6 * ( NB1    ) + II     )
     &                        + ZR ( IUP - 1 + 6 * ( NB1    ) + II     )
           VECTHM ( NB2, II ) = ZR ( IUM - 1 + 6 * ( NB1    ) + II     )
 222     CONTINUE
C
      END IF
C
C---- TRANSFORMEES NORMALES ET MATRICES DE ROTATION AUX NOEUDS
C
      CALL VECTRN ( NB2 , VECTPT , VECTN  , VECTHE , VECNPH , BLAM )
      CALL VECTRN ( NB2 , VECTPT , VECTN  , VECTHM , VECPHM , BLAMM )
C
C---- VECTEUR PE DES VARIABLES NODALES TOTALES GENERALISEES
C
      CALL VECTPE ( NB1 , NB2 , VECU , VECTN , VECNPH , VECPE )
      CALL VECTPE ( NB1 , NB2 , VECUM , VECTN , VECPHM , VECPEM )
C
C______________________________________________________________________
C
C---- INITIALISATION DES OPERATEURS DE DEFORMATION A EXTRAPOLER
C
C---- MEMBRANE REDUIT INCOMPLET
C
      CALL R8INIR ( 3 * 51 * 4 , 0.D0 , B1MRI , 1 )
      CALL R8INIR ( 3 * 51 * 4 , 0.D0 , B1MRIM , 1 )
C
      CALL R8INIR ( 3 * 51 * 4 , 0.D0 , B2MRI , 1 )
      CALL R8INIR ( 3 * 51 * 4 , 0.D0 , B2MRIM , 1 )
C
C---- SHEAR    REDUIT   COMPLET
C
      CALL R8INIR ( 2 * 51 * 4 , 0.D0 , B1SRC , 1 )
      CALL R8INIR ( 2 * 51 * 4 , 0.D0 , B1SRCM , 1 )
C
      CALL R8INIR ( 2 * 51 * 4 , 0.D0 , B2SRC , 1 )
      CALL R8INIR ( 2 * 51 * 4 , 0.D0 , B2SRCM , 1 )
C
C---- COMPTEUR DES POINTS D INTEGRATIONS ( EPAISSEUR * SURFACE )
C
C
C==== BOUCLE SUR LES COUCHES
C
      DO 600 ICOU = 1 , NBCOU
C
C======= BOUCLE SUR LES POINTS D INTEGRATION SUR L EPAISSEUR
C
         DO 610 INTE = 1 , NPGE
C
C---------- POSITION SUR L EPAISSEUR ET POIDS D INTEGRATION
C
            IF      ( INTE . EQ . 1 ) THEN
C
               ZIC = ZMIN + ( ICOU - 1 ) * EPAIS
C
               COEF = 1.D0 / 3.D0
C
            ELSE IF ( INTE . EQ . 2 ) THEN
C
               ZIC = ZMIN + EPAIS / 2.D0 + ( ICOU - 1 ) * EPAIS
C
               COEF = 4.D0 / 3.D0
C
            ELSE
C
               ZIC = ZMIN + EPAIS + ( ICOU - 1 ) * EPAIS
C
               COEF = 1.D0 / 3.D0
C
            ENDIF
C
C---------- COORDONNEE ISOP.  SUR L EPAISSEUR  DIVISEE PAR DEUX
C
            KSI3S2 = ZIC / EPAIS
C
C========== 1 ERE BOUCLE SUR POINTS INTEGRATION REDUITE SURFACE MOYENNE
C
            DO 620 INTSR = 1 , NPGSR
C
               CALL VECTGT ( 0 , NB1 , ZR ( IGEOM ) , KSI3S2 , INTSR ,
     &                    ZR ( LZR ) , EPAIS , VECTN , VECTG , VECTT )
C
               CALL JACBM1 ( EPAIS , VECTG , VECTT ,BID33 , JM1 , DETJ )
C
C------------- J1DN1RI ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
C                                          INDC = 0 INCOMPLET
               CALL JM1DN1
     & ( 0 , 0 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSR ,
     &                                                   JM1 , JDN1RI )
C
C------------- CALCUL DE    DUDXRI ( 9 ) REDUIT INCOMPLET
C
               CALL PROMAT
     &           ( JDN1RI , 9           , 9           , 6 * NB1 + 3 ,
     &             VECPE  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             DUDXRI )
               CALL PROMAT
     &           ( JDN1RI , 9           , 9           , 6 * NB1 + 3 ,
     &             VECPEM  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             DUDRIM )
C
C+++++++++++++ B1MRI ( 3 , 51 , 4 ) MEMBRANE REDUIT INCOMPLET
C              B2MRI ( 3 , 51 , 4 )
C
               CALL MATBMR ( NB1   , VECTT , DUDXRI , INTSR , JDN1RI ,
     &                       B1MRI , B2MRI )
               CALL MATBMR ( NB1   , VECTT , DUDRIM , INTSR , JDN1RI ,
     &                       B1MRIM , B2MRIM )
C
C------------- J1DN1RC ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
C                                          INDC = 1 COMPLET
C
               CALL JM1DN1
     & ( 0 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSR ,
     &                                                   JM1 , JDN1RC )
C
C------------- CALCUL DE    DUDXRC ( 9 ) REDUIT COMPLET
C
               CALL PROMAT
     &           ( JDN1RC , 9           , 9           , 6 * NB1 + 3 ,
     &             VECPE  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             DUDXRC )
               CALL PROMAT
     &           ( JDN1RC , 9           , 9           , 6 * NB1 + 3 ,
     &             VECPEM  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             DUDRCM )
C
C------------- J1DN2RC ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
C                                          INDC = 1 COMPLET
C
               CALL JM1DN2
     & ( 0 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSR ,
     &                                          VECNPH , JM1 , JDN2RC )
               CALL JM1DN2
     & ( 0 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSR ,
     &                                        VECPHM , JM1 , JD2RCM )
C
C+++++++++++++ B1SRC ( 2 , 51 , 4 ) SHEAR REDUIT COMPLET
C              B2SRC ( 2 , 51 , 4 )
C
               CALL MATBSR ( NB1    , VECTT  , DUDXRC , INTSR ,
     &                       JDN1RC , JDN2RC ,
     &                       B1SRC  , B2SRC  )
               CALL MATBSR ( NB1    , VECTT  , DUDRCM , INTSR ,
     &                       JDN1RC , JD2RCM ,
     &                       B1SRCM  , B2SRCM  )
C
C========== FIN 1 ERE BOUCLE NPGSR
C
 620        CONTINUE
C---------- INITIALISATION DES CONTRAINTES A LISSER
C
            IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_TANG' . OR .
     &           OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             )
     &                     CALL R8INIR ( 5 * 4 , 0.D0 , STLIS , 1 )
C
C========== BOUCLE SUR POINTS INTEGRATION NORMALE SURFACE MOYENNE
C
            DO 630 INTSN = 1 , NPGSN
C
CC
               CALL VECTGT ( 1 , NB1 , ZR ( IGEOM ) , KSI3S2 , INTSN ,
     &                    ZR ( LZR ) , EPAIS , VECTN , VECTG , VECTT )
C
               CALL JACBM1 ( EPAIS , VECTG , VECTT ,BID33 , JM1 , DETJ )
C
C------------- J1DN1NC ( 9 , 6 * NB1 + 3 ) INDN = 1 NORMAL
C                                          INDC = 1 COMPLET
C
               CALL JM1DN1
     & ( 1 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSN ,
     &                                                   JM1 , JDN1NC )
C
C------------- CALCUL DE     DUDXNC ( 9 ) NORMAL COMPLET
C
               CALL PROMAT
     &           ( JDN1NC , 9           , 9           , 6 * NB1 + 3 ,
     &             VECPE  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             DUDXNC )
               CALL PROMAT
     &           ( JDN1NC , 9           , 9           , 6 * NB1 + 3 ,
     &             VECPEM  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             DUDNCM )
C
C------------- J1DN2NC ( 9 , 6 * NB1 + 3 ) INDN = 1 NORMAL
C                                          INDC = 1 COMPLET
C
               CALL JM1DN2
     & ( 1 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSN ,
     &                                          VECNPH , JM1 , JDN2NC )
               CALL JM1DN2
     & ( 1 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSN ,
     &                                        VECPHM , JM1 , JD2NCM )
C
C+++++++++++++ B1MNC ( 3 , 51 ) MEMBRANE NORMAL COMPLET
C              B2MNC ( 3 , 51 )
C
             CALL MATBMN ( NB1   , VECTT , DUDXNC , JDN1NC , JDN2NC ,
     &                       B1MNC , B2MNC )
             CALL MATBMN ( NB1   , VECTT , DUDNCM , JDN1NC , JD2NCM ,
     &                       B1MNCM , B2MNCM )
C
C------------- J1DN1NI ( 9 , 6 * NB1 + 3 ) INDN = 1 NORMAL
C                                          INDC = 0 INCOMPLET
C
               CALL JM1DN1
     & ( 1 , 0 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSN ,
     &                                                   JM1 , JDN1NI )
C
C------------- CALCUL DE     DUDXNI ( 9 ) NORMAL INCOMPLET
C
               CALL PROMAT
     &           ( JDN1NI , 9           , 9           , 6 * NB1 + 3 ,
     &             VECPE  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             DUDXNI )
               CALL PROMAT
     &           ( JDN1NI , 9           , 9           , 6 * NB1 + 3 ,
     &             VECPEM  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             DUDNIM )
C
C+++++++++++++ B1MNI ( 3 , 51 ) MEMBRANE NORMAL INCOMPLET
C              B2MNI ( 3 , 51 )
C
               CALL MATBMN ( NB1   , VECTT , DUDXNI , JDN1NI , JDN1NI ,
     &                       B1MNI , B2MNI )
               CALL MATBMN ( NB1   , VECTT , DUDNIM , JDN1NI , JDN1NI ,
     &                       B1MNIM , B2MNIM )
C
C============= B1SU ( 5 , 51 ) SUBSTITUTION TOTAL
C              B2SU ( 5 , 51 ) SUBSTITUTION DIFFERENTIEL
C
               CALL MATBSU ( NB1   , ZR ( LZR ) , NPGSR , INTSN ,
     &                       B1MNC , B2MNC      , B1MNI , B2MNI ,
     &                       B1MRI , B2MRI      , B1SRC , B2SRC ,
     &                       B1SU  , B2SU  )
               CALL MATBSU ( NB1   , ZR ( LZR ) , NPGSR , INTSN ,
     &                       B1MNCM , B2MNCM      , B1MNIM , B2MNIM ,
     &                       B1MRIM , B2MRIM      , B1SRCM , B2SRCM ,
     &                       B1SUM  , B2SUM  )
C
C------------- LA  DEFORMATION TOTALE  DE GREEN LAGRANGE ETILD ( 5 )
C
               CALL PROMAT
     &           ( B1SU   , 5           , 5           , 6 * NB1 + 3 ,
     &             VECPE  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             ETILD )
               CALL PROMAT
     &           ( B1SUM   , 5           , 5           , 6 * NB1 + 3 ,
     &             VECPEM  , 6 * NB1 + 3 , 6 * NB1 + 3 , 1           ,
     &             ETILDM )

C
C------------ INCREMENENT DE DEFORMATION
C
                DO 635 I = 1,5
                  DETILD(I) = ETILD(I)-ETILDM(I)
 635            CONTINUE
C
              EPS2D(1) = ETILDM(1)
              EPS2D(2) = ETILDM(2)
              EPS2D(3) = 0.D0
              EPS2D(4) = ETILDM(3)/RAC2
C
              DEPS2D(1) = DETILD(1)
              DEPS2D(2) = DETILD(2)
              DEPS2D(3) = 0.D0
              DEPS2D(4) = DETILD(3)/RAC2
C
              GXZ = ETILDM(4) + DETILD(4)
              GYZ = ETILDM(5) + DETILD(5)
C
              K2 = LGPG* (INTSN-1) + (NPGE* (ICOU-1)+INTE-1)*NBVARI
C
C------- CONTRAINTES DE CAUCHY = PK2 AUX POINTS DE GAUSS INSTANT MOINS
C
              K1=6*((INTSN-1)*NPGE*NBCOU + (ICOU-1)*NPGE +INTE - 1)
              DO 636 I = 1,3
              SIGN(I) = ZR( ICONTM - 1 + K1 + I )
 636          CONTINUE
              SIGN(4) = ZR( ICONTM - 1 + K1 + 4 )*RAC2

C - LOI DE COMPORTEMENT
C --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C --- INITIALISE A R8VIDE (ON NE S'EN SERT PAS)
              CALL R8INIR(3,  R8VIDE(), ANGMAS ,1)
C -    APPEL A LA LOI DE COMPORTEMENT
              KSP= (ICOU-1)*NPGE + INTE

              CALL NMCOMP('MASS',INTSN,KSP,2,TYPMOD,ZI(IMATE),
     &               ZK16(ICOMPO),ZR(ICARCR),ZR(IINSTM),ZR(IINSTP),
     &                    4,EPS2D,DEPS2D,
     &                    4,SIGN,ZR(IVARIM+K2),
     &                    OPTION,
     &                    ANGMAS,
     &                    1,LC,
     &                    SIGMA,ZR(IVARIP+K2),36,DSIDEP,1,RBID,COD)

            IF (PHENOM.EQ.'ELAS') THEN
              NBV = 2
              NOMRES(1) = 'E'
              NOMRES(2) = 'NU'
            ELSE
              CALL U2MESS('F','ELEMENTS_42')
            END IF

              CALL RCVALB('MASS',INTSN,KSP,'+',ZI(IMATE),' ',PHENOM,
     &                    0,' ',0.D0,NBV,NOMRES,VALRES,ICODRE,1)

      CISAIL = VALRES(1)/ (1.D0+VALRES(2))

C           COD=1 : ECHEC INTEGRATION LOI DE COMPORTEMENT
C           COD=3 : C_PLAN DEBORST SIGZZ NON NUL
            IF (COD.NE.0) THEN
               IF (CODRET.NE.1) THEN
                  CODRET=COD
               ENDIF
            ENDIF
C
C------------- LA  CONTRAINTE TOTALE  PK2 STILD ( 5 )
C
C
          IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_TANG' . OR .
     &           OPTION ( 1 :  9 ) . EQ . 'FULL_MECA' ) THEN
C
              DTILD(1,1) = DSIDEP(1,1)
              DTILD(1,2) = DSIDEP(1,2)
              DTILD(1,3) = DSIDEP(1,4)/RAC2
              DTILD(1,4) = 0.D0
              DTILD(1,5) = 0.D0

              DTILD(2,1) = DSIDEP(2,1)
              DTILD(2,2) = DSIDEP(2,2)
              DTILD(2,3) = DSIDEP(2,4)/RAC2
              DTILD(2,4) = 0.D0
              DTILD(2,5) = 0.D0

              DTILD(3,1) = DSIDEP(4,1)/RAC2
              DTILD(3,2) = DSIDEP(4,2)/RAC2
              DTILD(3,3) = DSIDEP(4,4)/2.D0
              DTILD(3,4) = 0.D0
              DTILD(3,5) = 0.D0

              DTILD(4,1) = 0.D0
              DTILD(4,2) = 0.D0
              DTILD(4,3) = 0.D0
              DTILD(4,4) = CISAIL*KAPPA/2.D0
              DTILD(4,5) = 0.D0

              DTILD(5,1) = 0.D0
              DTILD(5,2) = 0.D0
              DTILD(5,3) = 0.D0
              DTILD(5,4) = 0.D0
              DTILD(5,5) = CISAIL*KAPPA/2.D0
C
              DO 750 I = 1,5
                DO 750 J = 1 , 5
                 MATC(I,J) = DTILD(I,J)
 750          CONTINUE
C
            ENDIF
C
            IF (OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_TANG') THEN
                STILD ( 1 ) = SIGN ( 1 )
                STILD ( 2 ) = SIGN ( 2 )
                STILD ( 3 ) = SIGN ( 4 ) / RAC2
            ELSE
                STILD ( 1 ) = SIGMA ( 1 )
                STILD ( 2 ) = SIGMA ( 2 )
                STILD ( 3 ) = SIGMA ( 4 ) / RAC2
            ENDIF
                STILD ( 4 ) = CISAIL*KAPPA*GXZ/2.D0
                STILD ( 5 ) = CISAIL*KAPPA*GYZ/2.D0
C
      IF ( OPTION ( 1 :  9 ) . EQ . 'RAPH_MECA'      . OR .
     &     OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             ) THEN
C
C------- CONTRAINTES DE CAUCHY = PK2 AUX POINTS DE GAUSS
C
        K1=6*((INTSN-1)*NPGE*NBCOU + (ICOU-1)*NPGE +INTE - 1)
        ZR ( ICONTP - 1 + K1 + 1 ) = STILD ( 1 )
        ZR ( ICONTP - 1 + K1 + 2 ) = STILD ( 2 )
C
        ZR ( ICONTP - 1 + K1 + 3 ) = 0.D0
C
        ZR ( ICONTP - 1 + K1 + 4 ) = STILD ( 3 )
C
        ZR ( ICONTP - 1 + K1 + 5 ) = STILD ( 4 )
        ZR ( ICONTP - 1 + K1 + 6 ) = STILD ( 5 )
C
C------------- FINT ( 6 * NB1 + 3 )  =     INTEGRALE  DE
C              ( B2SU ( 5 , 6 * NB1 + 3 ) ) T * STILD ( 5 ) *
C              POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
C
               CALL BTSIG ( 6 * NB1 + 3 , 5 ,
     &                   ZR (LZR - 1 + 127 + INTSN - 1) * DETJ * COEF ,
     &                   B2SU , STILD , ZR ( IVECTU ) )
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C------------- VARIABLES INTERNES INACTIVES COMPORTEMENT NON PLASTIQUE
C
      ENDIF
C
C
      IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_TANG' . OR .
     &     OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             ) THEN
C
C------------- INTEGRATION DES CONTRAINTES LISSEES
C
               DO 700 KNTSR = 1 , NPGSR
C
                  DO 710 I = 1 , 5
C
                     STLIS ( I , KNTSR ) = STLIS ( I , KNTSR ) +
     & ZR ( LZR - 1 + 702 + 4 * ( INTSN - 1 ) + KNTSR ) * STILD ( I ) *
     & ZR ( LZR - 1 + 127 +       INTSN - 1 )
C
 710              CONTINUE
 700           CONTINUE
C
C------------- KM ( 6 * NB1 + 3 , 6 * NB1 + 3 )  =     INTEGRALE  DE
C                ( B2SU ( 5 , 6 * NB1 + 3 ) ) T * MATC ( 5 , 5 ) *
C                  B2SU ( 5 , 6 * NB1 + 3 )
C                POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
C
               CALL  BTDBMA ( B2SU , MATC ,
     &                  ZR (LZR - 1 + 127 + INTSN - 1) * DETJ * COEF ,
     &                  5 , 6 * NB1 + 3 , ZR ( IMATUN ) )
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
CRR
CRR   RIGIDITE GEOMETRIQUE NON CLASSIQUE TOUT
CRR
C
C---------- POUR LE TERME NON CLASSIQUE
C           HSC ( 5 , 9 ) = H ( 5 , 6 )  * S ( 6 , 9 )
C
            CALL HSACO ( VECTT , DUDXNC , HSC )
C
C---------- CALCUL DE
C           J1DN3( 9 , 3 * NB2 )=JTILDM1( 9 , 9 )*DNDQSI3( 9 , 3 * NB2 )
C
            CALL JM1DN3 ( NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSN ,
     &                    JM1 , J1DN3 )
C---------- CALCUL DE
C           BTILD3 ( 5 , 27 ) = HSC ( 5 , 9 ) * J1DN3 ( 9 , 3 * NB2 )
C
            CALL PROMAT ( HSC     , 5 , 5 , 9       ,
     &                    J1DN3   , 9 , 9 , 3 * NB2 ,
     &                    BTILD3  )
C
C---------- VECZN ( 27 )  =     INTEGRALE  DE
C           ( BTILD3 ( 5 , 27 ) ) T * STILD ( 5 ) *
C           POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
C
            CALL BTSIG ( 3 * NB2 , 5 ,
     &                   ZR (LZR - 1 + 127 + INTSN - 1) * DETJ * COEF ,
     &                   BTILD3 , STILD , VECZN )
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C----------------------------------------------------------------------
CRR
CRR   RIGIDITE GEOMETRIQUE CLASSIQUE MEMBRANE FLEXION
CRR
C
C------------- ANNULATION DU SHEAR
C---------------------------------
C
               CALL R8INIR ( 2 , 0.D0 , STILD ( 4 ) , 1 )
C
C------------- BARS ( 9 , 9 )
C
               CALL TILBAR ( STILD , VECTT , BARS )
C
C------------- VRIGNC  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTEGRALE
C              ( JDN2NC ( 9 , 6 * NB1 + 3 ) ) T * BARS   ( 9 , 9 )
C           *                               JDN2NC ( 9 , 6 * NB1 + 3 ) *
C              POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
C
               CALL  BTDBMA ( JDN2NC , BARS   ,
     &                  ZR (LZR - 1 + 127 + INTSN - 1) * DETJ * COEF ,
     &                      9 , 6 * NB1 + 3 , VRIGNC )
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C------------- VRIGNI  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTEGRALE
C              ( JDN1NI ( 9 , 6 * NB1 + 3 ) ) T * BARS   ( 9 , 9 )
C           *                               JDN1NI ( 9 , 6 * NB1 + 3 ) *
C              POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
C
               CALL  BTDBMA ( JDN1NI , BARS ,
     &                  ZR (LZR - 1 + 127 + INTSN - 1) * DETJ * COEF ,
     &                        9 , 6 * NB1 + 3 , VRIGNI )
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      ENDIF
C
C========== FIN BOUCLE NPGSN
C
 630        CONTINUE
C
      IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_TANG' . OR .
     &     OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             ) THEN
C
C========== 2 EME BOUCLE SUR POINTS INTEGRATION REDUITE SURFACE MOYENNE
C
            DO 640 INTSR = 1 , NPGSR
C
               CALL VECTGT ( 0 , NB1 , ZR ( IGEOM ) , KSI3S2 , INTSR ,
     &                    ZR ( LZR ) , EPAIS , VECTN , VECTG , VECTT )
C
               CALL JACBM1 ( EPAIS , VECTG , VECTT ,BID33 , JM1 , DETJ )
C
C------------- J1DN1RI ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
C                                          INDC = 0 INCOMPLET
C
               CALL JM1DN1
     & ( 0 , 0 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSR ,
     &                                                   JM1 , JDN1RI )
C
C
CIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
C
C------------- RESTITUTION DES CONTRAINTES LISSEES MEMBRANE FLEXION
C
               DO 800 I = 1 , 3
                  STILD ( I ) = STLIS ( I , INTSR )
 800           CONTINUE
C
C------------- ANNULATION DU SHEAR
C
               CALL R8INIR ( 2 , 0.D0 , STILD ( 4 ) , 1 )
C
C------------- BARS ( 9 , 9 )
C
               CALL TILBAR ( STILD , VECTT , BARS )
C
C------------- VRIGRI  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTEGRALE
C              ( JDN1RI ( 9 , 6 * NB1 + 3 ) ) T * BARS   ( 9 , 9 )
C           *                               JDN1RI ( 9 , 6 * NB1 + 3 ) *
C                                      DETJ * POIDS EPAISSEUR
CDDDDDDDDDDDDD
C------------- PAS D INTEGRATION REDUITE SURFACE MOYENNE
CDDDDDDDDDDDDD
C
               CALL  BTDBMA ( JDN1RI , BARS ,
     &                                                   DETJ * COEF ,
     &                        9 , 6 * NB1 + 3 , VRIGRI )
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C------------- J1DN2RC ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
C                                          INDC = 1 COMPLET
C
               CALL JM1DN2
     & ( 0 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSR ,
     &                                          VECNPH , JM1 , JDN2RC )
C
C------------- ANNULATION DE MEMBRANE FLEXION
C
               CALL R8INIR ( 3 , 0.D0 , STILD ( 1 ) , 1 )
C
C------------- RESTITUTION DES CONTRAINTES LISSEES DE SHEAR
C
               DO 850 I = 4 , 5
                  STILD ( I ) = STLIS ( I , INTSR )
 850           CONTINUE
C
C------------- BARS ( 9 , 9 )
C
               CALL TILBAR ( STILD , VECTT , BARS )
C
C------------- VRIGRC  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTEGRALE
C              ( JDN2RC ( 9 , 6 * NB1 + 3 ) ) T * BARS   ( 9 , 9 )
C           *                               JDN2RC ( 9 , 6 * NB1 + 3 ) *
C              POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
C
CDDDDDDDDDDDDD
C------------- PAS D INTEGRATION REDUITE SURFACE MOYENNE
CDDDDDDDDDDDDD
C
               CALL  BTDBMA ( JDN2RC , BARS ,
     &                                                   DETJ * COEF ,
     &                        9 , 6 * NB1 + 3 , VRIGRC )
C
C========== FIN 2 EME BOUCLE NPGSR
C
 640        CONTINUE
C
      ENDIF
C
C-------- FIN BOUCLE NPGE
C
 610      CONTINUE
C
C---- FIN BOUCLE NBCOU
C
 600  CONTINUE
C
      IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_TANG' . OR .
     &     OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             ) THEN
C
C------- AFFECTATION DE LA RIGIDITE GEOMETRIQUE
C
         DO 400 JD = 1 , ( 6 * NB1 + 3 ) * ( 6 * NB1 + 3 )
            ZR ( IMATUN - 1 + JD ) = ZR     ( IMATUN - 1 + JD )
     &                             + VRIGNC (              JD )
     &                             - VRIGNI (              JD )
     &                             + VRIGRI (              JD )
     &                             + VRIGRC (              JD )
C
 400     CONTINUE
C
C------- AFFECTATION DE LA RIGIDITE NON CLASSIQUE RIGNC ( 3 , 3 )
C
         DO 500 IN = 1 , NB2
C
C---------- MATRICE ANTISYMETRIQUE    ANTZI ( 3 , 3 ) AU NOEUD
C
            CALL ANTISY ( VECZN ( ( IN - 1 ) * 3 + 1 ) , 1.D0 , ANTZI )
C
C---------- TRANSFOR DE NORMALE ET SA MATRICE ANTISYM AU NOEUD
C
            DO 520 II = 1 , 3
               VECNI ( II ) = VECNPH ( IN , II )
 520        CONTINUE
C
            CALL ANTISY ( VECNI , 1.D0 , ANTNI )
C
C---------- RIGIDITE NON CLASSIQUE RIGN ( 3 , 3 ) NON SYMETRIQUE
C
            CALL PROMAT ( ANTZI , 3 , 3 , 3 ,
     &                    ANTNI , 3 , 3 , 3 ,
     &                    RIGNC )
C
C--------- RIGIDITE NON CLASSIQUE DESACTIVEE
C
C            IF ( IN . LE . NB1 ) THEN
C
C------------- NOEUDS DE SERENDIP
C
C               DO 531 JJ = 1 , 3
C                  J    = 6 * ( IN - 1 ) + JJ + 3
C                  DO 541 II = 1 , 3
C                     I    = 6 * ( IN - 1 ) + II + 3
C                     IRIG = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
C                     ZR ( IMATUN-1 + IRIG ) = ZR ( IMATUN-1 + IRIG  )
C     &       +  RIGNC ( II , JJ )
C
C 541              CONTINUE
C 531           CONTINUE
C
C            ELSE
C
C------------- SUPERNOEUD
C               DO 532 JJ = 1 , 3
C                  J    = 6 * NB1        + JJ
C                  DO 542 II = 1 , 3
C                     I    = 6 * NB1        + II
C                     IRIG = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
C                     ZR ( IMATUN-1 + IRIG ) = ZR ( IMATUN-1 + IRIG  )
C     &       +  RIGNC ( II , JJ )
C
C 542              CONTINUE
C 532           CONTINUE
C
C            ENDIF
C
 500     CONTINUE
C
C------- ROTATION DE TOUTE LA MATRICE AU REPERE LOCAL
C
         CALL ROGLLO ( NB1 , NB2 , ZR ( IMATUN ) , BLAM , CTOR , KNN )
C
      ELSE
C
C++++ MATRICE ELASTIQUE
C
      KNN = 0. D0
C
      END IF
C
C++++ SECOND MEMBRE DES FORCES INTERIEURES
C
C++++++++ BOUCLE SUR LES NOEUDS DE ROTATION
C
          DO 900 IN = 1 , NB2
C
C+++++++++++ ROTATION AUTOUR DE LA NORMALE INITIALE
C
             DO 910 II = 1 , 3
               VECNI ( II ) = VECTN  ( IN , II )
               THETA ( II ) = VECTHE ( IN , II )
 910         CONTINUE
C
             THETAN=DDOT(3,THETA,1,VECNI,1)
C
C+++++++++++ MATRICE T MOIUNS 1 DE THETA
C
             CALL GDT ( THETA , TMOIN1 )
C
C
C
C+++++++++++ SON TRANSPOSE
C
             CALL TRANSP ( TMOIN1 , 3 , 3 , 3 ,   TM1T , 3 )
C
C+++++++++++ PRODUIT T MOINS 1 T FOIS VECNI
C
             CALL PROMAT ( TM1T   , 3 , 3 , 3 ,
     &                     VECNI  , 3 , 3 , 1 ,
     &                     TERM )
C
C
C
C
      IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_TANG' . OR .
     &     OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             ) THEN
C
            IF ( IN . LE . NB1 ) THEN
C
C-------------- AFFECTATION
C
C-------------- NOEUDS DE SERENDIP
                DO 331 JJ = 1 , 3
                   J    = 6 * ( IN - 1 ) + JJ + 3
                   DO 341 II = 1 , 3
                       I    = 6 * ( IN - 1 ) + II + 3
               ZR ( IMATUN - 1 + ( 6 * NB1 + 3 ) * ( J - 1 ) + I ) =
     &         ZR ( IMATUN - 1 + ( 6 * NB1 + 3 ) * ( J - 1 ) + I )
     &         + KNN  * TERM   ( II ) * TERM   ( JJ )
 341               CONTINUE
 331            CONTINUE
C
            ELSE
C
C-------------- SUPERNOEUD
                DO 351 JJ = 1 , 3
                   J    = 6 * NB1        + JJ
                   DO 361 II = 1 , 3
                       I    = 6 * NB1        + II
               ZR ( IMATUN - 1 + ( 6 * NB1 + 3 ) * ( J - 1 ) + I ) =
     &         ZR ( IMATUN - 1 + ( 6 * NB1 + 3 ) * ( J - 1 ) + I )
     &         + KNN  * TERM   ( II ) * TERM   ( JJ )
 361               CONTINUE
 351            CONTINUE
C
            ENDIF
C
      END IF
C
C
C
      IF ( OPTION ( 1 :  9 ) . EQ . 'RAPH_MECA'      . OR .
     &     OPTION ( 1 :  9 ) . EQ . 'FULL_MECA'             ) THEN
C
            IF ( IN . LE . NB1 ) THEN
C
                DO 920 II = 1 , 3
C
                   ZR ( IVECTU - 1 + 6 * ( IN - 1 ) + II + 3 ) =
     &             ZR ( IVECTU - 1 + 6 * ( IN - 1 ) + II + 3 ) +
     &                                KNN  * TERM(II) * THETAN
C
 920            CONTINUE
C
            ELSE
C
                DO 930 II = 1 , 3
                   ZR ( IVECTU - 1 + 6 * ( IN - 1 ) + II     ) =
     &             ZR ( IVECTU - 1 + 6 * ( IN - 1 ) + II     ) +
     &                                KNN * TERM(II) * THETAN
 930            CONTINUE
C
            END IF
C
C
      END IF
C
 900      CONTINUE
C
C
C FIN
C
      END
