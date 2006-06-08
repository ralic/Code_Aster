      SUBROUTINE TE0402 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/06/2004   AUTEUR CIBHHPD S.VANDENBERGHE 
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
C
      IMPLICIT NONE
C
      CHARACTER*16        OPTION , NOMTE
C
C ......................................................................
C     FONCTION :  CALCUL DE LA MATRICE DES CONTRAINTES INITIALES
C                 POUR LE FLAMBEMENT LINEAIRE
C
C                 COQUE_3D
C
C                 OPTION :  RIGI_MECA_GEOM 
C
C    ARGUMENTS :
C    DONNEES   :       OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C
C
C---- DECLARATIONS BIDONS
C
      REAL * 8 BID33 ( 3 , 3 )
C
C
C
C
C---- DECLARATIONS LOCALES
C
      INTEGER I    , J  
      INTEGER IN 
      INTEGER II   , JJ 
      INTEGER IRIG
      INTEGER             KOMPT
      INTEGER             KPGS 
C
C
      REAL * 8 SIGMTD  ( 5 )
C
      REAL * 8 SIGMT    ( 3 ,  3 )
C
      REAL * 8 SIGMA    ( 3 ,  3 )
C
      REAL * 8 BARSIG   ( 9 , 9 ) 
C
      REAL * 8 VECNI ( 3 )  , ANTNI ( 3 , 3 )
C
      REAL * 8 VECZN ( 27 ) 
      REAL * 8 ANTZI ( 3 , 3 ) 
C
      REAL * 8 RIGNC ( 3  , 3  )
      REAL * 8 VRI ( 2601 ) 
C
C
C
C---- DECLARATIONS STANDARDS
C
      INTEGER IGEOM , ICONTR , IMATUU
C
      INTEGER LZI , LZR , JCARA
C
      INTEGER NB1 , NB2 
C
C
C
C
C---- DECLARATIONS PROPRES COQUE_3D
C
      INTEGER     INTE , INTSN 
C
      REAL * 8 EPAIS
C
      INTEGER     NPGE , NPGSN 
C
      REAL * 8 VECTA ( 9 , 2 , 3 )   
      REAL * 8 VECTN ( 9 , 3 ) , VECTPT ( 9 , 2 , 3 )
C
      REAL * 8 VECTG ( 2 , 3 ) , VECTT ( 3 , 3 )
C
      REAL * 8 JM1 ( 3 , 3 ) , DETJ 
C
      REAL * 8 HSTOUT ( 5 , 9 ) 
C
      REAL * 8 J1DN2 ( 9 , 51 )
      REAL * 8 J1DN3 ( 9 , 27 )
C
      REAL * 8 BTILD3 ( 5 , 27 )
C
      PARAMETER       ( NPGE = 3 )
      REAL * 8 EPSVAL ( NPGE ) , KSI3S2 , POIDS (NPGE)
C
C
C
C
C DEB
C
C---- LES NOMBRES
C

C       POIDS DES POINTS DE GAUSS DANS LA TRANCHE
     
      POIDS ( 1 ) = 0.33333333333333D0
      POIDS ( 2 ) = 1.33333333333333D0
      POIDS ( 3 ) = 0.33333333333333D0
C
C---- RECUPERATION DES POINTEURS ( L : LECTURE, E : ECRITURE )
C
C
C....... GEOMETRIE ( COORDONNEES DES NOEUDS )
C
         CALL JEVECH ( 'PGEOMER' , 'L' , IGEOM )
C
C....... CONTRAINTES DE CAUCHY ( CONFONDUES AVEC PK2 )
C
         CALL JEVECH ( 'PCONTRR' , 'L' , ICONTR )
C
C....... MATRICE SYMETRISEE DE RIGIDITE GEOMETRIQUE
C
         CALL JEVECH ( 'PMATUUR' , 'E' , IMATUU )
C
C
C
C
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
C
C------ CARACTERISTIQUES DE COQUE 
C
        CALL JEVECH ( 'PCACOQU' , 'L' , JCARA )
C
        EPAIS = ZR ( JCARA )

C       COORDONNEES DES POINTS DE GAUSS DANS LA TRANCHE
        EPSVAL ( 1 ) = ZR(LZR-1+1251)
        EPSVAL ( 2 ) = ZR(LZR-1+1252)
        EPSVAL ( 3 ) = ZR(LZR-1+1253)

C       POIDS DES POINTS DE GAUSS DANS LA TRANCHE
     
        POIDS ( 1 ) = 0.33333333333333D0
        POIDS ( 2 ) = 1.33333333333333D0
        POIDS ( 3 ) = 0.33333333333333D0
C
C
C
C
C---- VECTEURS DE BASE AUX NOEUDS
C
      CALL VECTAN(NB1,NB2, ZR(IGEOM) , ZR(LZR) ,VECTA,VECTN,VECTPT)
C
C
C
C        CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE 
C
C            RIG ( 6 * NB1 + 3 , 6 * NB1 + 3 ) 
C
C        DANS
C
C            VRI ( 6 * NB1 + 3 ) * ( 6 * NB1 + 3 ) )
C
C---- INITIALISATION DE LA MATRICE DE RIGIDITE GEOMETRIQUE
C
      CALL R8INIR ( 51 * 51 , 0.D0 , VRI , 1 )
C
C---- INITIALISATION DE VECZN AVANT INTEGRATION
C
      CALL R8INIR ( 27 , 0.D0 , VECZN , 1 )
C
C---- COMPTEUR DES POINTS D INTEGRATIONS ( EPAISSEUR * SURFACE )
C
      KPGS = 0
C
C---- BOUCLE SUR LES POINTS D INTEGRATION SUR L EPAISSEUR
C
      DO 600 INTE = 1 , NPGE
C
C------- COORDONNEE ISOPARAMETRIQUE SUR L EPAISSEUR  DIVISEE PAR DEUX
C
         KSI3S2 = EPSVAL ( INTE ) / 2.D0
C
C------- BOUCLE SUR LES POINTS D INTEGRATION SUR LA SURFACE MOYENNE
C
         DO 610 INTSN = 1 , NPGSN
C
            KPGS = KPGS + 1
C
C---------- VECTEUR 5 * 1 DES CONTRAINTES LOCALES
C
            SIGMTD ( 1 )=ZR ( ICONTR - 1 + ( KPGS - 1 ) * 6 + 1 )
            SIGMTD ( 2 )=ZR ( ICONTR - 1 + ( KPGS - 1 ) * 6 + 2 ) 
C
            SIGMTD ( 3 )=ZR ( ICONTR - 1 + ( KPGS - 1 ) * 6 + 4 )
C
            SIGMTD ( 4 )=ZR ( ICONTR - 1 + ( KPGS - 1 ) * 6 + 5 )
            SIGMTD ( 5 )=ZR ( ICONTR - 1 + ( KPGS - 1 ) * 6 + 6 )
C
C---------- TENSEUR 3 * 3 CONTRAINTES LOCALES TRIANGULAIRE SUPERIEURE
C
            CALL SIGVTE ( SIGMTD , SIGMT )
C
C---------- MATRICE ROTATION GLOBAL --> LOCAL AUX POINTS D INTEGRATION
C
C                              ( T_1 )
C           VECTT ( 3 , 3 ) =  ( T_2 )  = ( LAMDA0 ) T
C                              ( N   )
C
            CALL VECTGT ( 1 , NB1 , ZR ( IGEOM ) , KSI3S2 , INTSN , 
     &                    ZR ( LZR ) , EPAIS , VECTN , VECTG , VECTT )
C
C---------- ROTATION DU TENSEUR DES CONTRAINTES : LOCALES --> GLOBALES 
C
C           SIGMA =  ( VECTT ) T * SIGMT * VECTT
C
            CALL  BTKB ( 3 , 3 , 3 , SIGMT , VECTT , BID33 , SIGMA )
C
C
C
C
C
C---------- POUR LE TERME NON CLASSIQUE
C
C---------- CALCUL DE    HSTOUT ( 5 , 9 ) = H ( 5 , 6 )  * S ( 6 , 9 )
C
            CALL HSALL ( VECTT , HSTOUT )
C
C---------- CALCUL DE LA MATRICE JACOBIENNE INVERSE       JM1 ( 3, 3 )
C
            CALL JACBM1 ( EPAIS , VECTG , VECTT , BID33 , JM1 , DETJ )
C
C---------- CALCUL DE
C           J1DN3( 9 , 3 * NB2 )=JTILDM1( 9 , 9 )*DNDQSI3( 9 , 3 * NB2 )
C
            CALL JM1DN3 ( NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSN ,
     &                    JM1 , J1DN3 )
C
C---------- CALCUL DE
C           BTILD3 ( 5 , 27 ) = HSTOUT ( 5 , 9 ) * J1DN3 ( 9 , 3 * NB2 )
C
            CALL PROMAT ( HSTOUT  , 5 , 5 , 9       ,
     &                    J1DN3   , 9 , 9 , 3 * NB2 , 
     &                    BTILD3  )
C
C---------- VECZN ( 27 )  =     INTEGRALE  DE  
C           ( BTILD3 ( 5 , 27 ) ) T * SIGMTD ( 5 ) *
C           POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR 
C           VOIR ROUTINE INI080 , HSJ1F
C
            CALL BTSIG ( 3 * NB2 , 5 , 
     &           ZR (LZR - 1 + 127 + INTSN - 1) * DETJ * POIDS(INTE),
     &           BTILD3 , SIGMTD , VECZN )
C
C
C
C
C
C
C---------- POUR LE TERME CLASSIQUE
C
C---------- BARSIG   ( 9 , 9 )
C
            CALL SIGBAR ( SIGMA , BARSIG )
C
C---------- CALCUL DE
C           J1DN2 ( 9 , 6 * NB1 + 3 ) = 
C           JTILDM1 ( 9 , 9 ) * DNDQSI2 ( 9 , 6 * NB1 + 3 )
C
C           INDN = 1 INTEGRATION NORMALE        
C           INDC = 1 COMPLET
C
      CALL JM1DN2
     & ( 1 , 1 , NB1 , NB2 ,  ZR ( LZR ) , EPAIS , KSI3S2 , INTSN ,
     &                                             VECTN , JM1 , J1DN2 )
C
C---------- RIG  ( 6 * NB1 + 3 , 6 * NB1 + 3 )  = INTERALE
C           ( J1DN2 ( 9 , 6 * NB1 + 3 ) ) T * BARSIG ( 9 , 9 ) 
C           *                               J1DN2 ( 9 , 6 * NB1 + 3 ) *
C           POIDS SURFACE MOYENNE * DETJ * POIDS EPAISSEUR
C           VOIR ROUTINE INI080 , HSJ1F
C
            CALL  BTDBMA ( J1DN2 , BARSIG , 
     &            ZR (LZR - 1 + 127 + INTSN - 1) * DETJ * POIDS(INTE),
     &            9 , 6 * NB1 + 3 , VRI )
C
 610     CONTINUE
 600  CONTINUE
C
C---- PAS DE RIGIDITE DE ROTATION AUTOUR NORMALE
C
C
C
C---- RIGIDITE NON CLASSIQUE 
C
C---- BOULE SUR TOUS LES NOEUDS
C
      DO 500 IN = 1 , NB2
C
C------- MATRICE ANTISYMETRIQUE    ANTZI ( 3 , 3 ) AU NOEUD
C
         CALL ANTISY ( VECZN ( ( IN - 1 ) * 3 + 1 ) , 1.D0 , ANTZI )
C
C------- NORMALE INITIALE ET SA MATRICE ANTISYM AU NOEUD
C
         DO 520 II = 1 , 3
            VECNI ( II ) = VECTN ( IN , II )
 520     CONTINUE
C
         CALL ANTISY ( VECNI , 1.D0 , ANTNI )
C
C------- RIGIDITE ROTATION NON CLASSIQUE RIGN ( 3 , 3 ) NON SYMETRIQUE
C
         CALL PROMAT ( ANTZI , 3 , 3 , 3 , 
     &                 ANTNI , 3 , 3 , 3 , 
     &                 RIGNC )
C
C------- RAJOUT DE LA PARTIE SYMETRIQUE DE RIGN ( 3 , 3 ) 
C
         IF ( IN . LE . NB1 ) THEN 
C
C---------- NOEUDS DE SERENDIP
            DO 530 JJ = 1 , 3
               DO 540 II = 1 , 3
                  J = 6 * ( IN - 1 ) + JJ + 3 
                  I = 6 * ( IN - 1 ) + II + 3 
                  IRIG = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
                  VRI ( IRIG ) = VRI ( IRIG ) +
     &            ( RIGNC ( II , JJ ) + RIGNC ( JJ , II ) ) * 0.5D0
 540           CONTINUE
 530        CONTINUE
C
         ELSE 
C
C---------- SUPERNOEUD
            DO 531 JJ = 1 , 3
               DO 541 II = 1 , 3
                  J = 6 * NB1 + JJ
                  I = 6 * NB1 + II
                  IRIG = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
                  VRI ( IRIG ) = VRI ( IRIG ) +
     &            ( RIGNC ( II , JJ ) + RIGNC ( JJ , II ) ) * 0.5D0
 541           CONTINUE
 531        CONTINUE
C
         ENDIF
C
 500  CONTINUE
C
C
C
C
C______________________________________________________________________
C
C---- STOCKAGE DE LA PARTIE TRIANGULAIRE SUPERIEURE  DANS
C
C                       ZR ( IMATUU )
C
C______________________________________________________________________
C     JEU D INDICES I J POUR LA PARTIE TRIANGULAIRE SUPERIEURE
C     
C     ZR ( IMATUU ---> IMATUU + TAILLE  - 1 ) : TRIANGULAIRE SUP DE RIG
C
C     TAILLE = NDDLET * ( 1 + (NDDLET - 1)/2 ) : NDDLET = 6 * NB1 + 3
C
C     VOIR ROUTINE TRANLG 
C
C
C
C
C---- COMPTEUR DE POSITION
C
            KOMPT = 0 
C
      DO 100   J = 1 , 6 * NB1 + 3
C
         DO 110  I = 1 , J 
C
            KOMPT = KOMPT + 1 
C
            ZR ( IMATUU - 1 + KOMPT ) = 
     &         + VRI ( ( 6 * NB1 + 3 ) * ( J - 1 ) + I )
C
 110     CONTINUE
C
 100  CONTINUE   
C
C
C
C
C
C FIN
C
      END
