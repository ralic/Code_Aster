      SUBROUTINE FORNGR ( OPTION , NOMTE )
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
C
      IMPLICIT NONE
C
      CHARACTER*16        OPTION , NOMTE
C
C ......................................................................
C     FONCTION  :  FORC_NODA DES COQUE_3D
C                  GEOMETRIQUE AVEC GRANDES ROTATIONS
C
C     ARGUMENTS :
C     DONNEES   :      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C     OPTIONS   :     FORC_NODA      : FORCE INTERNE
C
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
C---- DECLARATIONS BIDONS
C
      REAL * 8 BID33 ( 3 , 3 )
C
C---- DECLARATIONS LOCALES
C
      INTEGER I  ,  J  , IN , II , NVAL,  KPGS
C
C---- DECLARATIONS RIGIDITE GEOMETRIQUE
C
      REAL * 8 STILD  ( 5 )
C
C---- DECLARATIONS STANDARDS
C
      INTEGER IGEOM , ICONTM , IVECTU
      INTEGER LZI , LZR , JCARA
      INTEGER NB1 , NB2
C
C---- DECLARATIONS PROPRES COQUE_3D NON LINEAIRE
C
      INTEGER     INTE , INTSR , INTSN
      REAL * 8 EPTOT
      INTEGER     NPGE      , NPGSR , NPGSN
      PARAMETER ( NPGE = 3 )
      REAL * 8 VECTA ( 9 , 2 , 3 )
      REAL * 8 VECTN  ( 9 , 3 ) , VECTPT ( 9 , 2 , 3 )
      REAL * 8 VECNPH ( 9 , 3 )
      REAL * 8 VECTG ( 2 , 3 ) , VECTT ( 3 , 3 )
      REAL * 8 JM1 ( 3 , 3 ) , DETJ
      REAL * 8 JDN1RI ( 9 , 51 ) , JDN1RC ( 9 , 51 )
      REAL * 8 JDN1NI ( 9 , 51 ) , JDN1NC ( 9 , 51 )
      REAL * 8                     JDN2RC ( 9 , 51 )
      REAL * 8                     JDN2NC ( 9 , 51 )
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
      INTEGER IUM
      REAL * 8 B1SU  ( 5 , 51 ) , B2SU  ( 5 , 51 )
      REAL * 8 B1SRC ( 2 , 51 , 4 )
      REAL * 8 B2SRC ( 2 , 51 , 4 )
      REAL * 8 B1MNC ( 3 , 51 ) , B1MNI ( 3 , 51 )
      REAL * 8 B2MNC ( 3 , 51 ) , B2MNI ( 3 , 51 )
      REAL * 8 B1MRI ( 3 , 51 , 4 )
      REAL * 8 B2MRI ( 3 , 51 , 4 )
      REAL * 8 DUDXRI ( 9 ) , DUDXNI ( 9 )
      REAL * 8 DUDXRC ( 9 ) , DUDXNC ( 9 )
      REAL * 8 VECU ( 8 , 3 ) , VECTHE ( 9 , 3 )
      REAL * 8 VECPE  ( 51 )
C    POUR_RESI_REFE_RELA
      REAL*8 SIGTMP(5),FTEMP(51),EFFINT(51)
C
C---- DECLARATIONS ROTATION GLOBAL LOCAL AU NOEUDS
C
      INTEGER IRIG,JNBSPI
C
      REAL * 8 BLAM ( 9 , 3 , 3 )
C
      REAL * 8 R8PREM
C
C DEB
C
C---- LE NOMBRE DE COUCHES
C
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
C------- CONTRAINTES DE CAUCHY AUX POINTS DE GAUSS
C
         IF (OPTION.EQ.'FORC_NODA') THEN

            CALL JEVECH ( 'PCONTMR' , 'L' , ICONTM )

         ELSEIF (OPTION.EQ.'REFE_FORC_NODA') THEN

            CALL JEVECH('PREFCO','L',ICONTM)

         ENDIF
C
C______________________________________________________________________
C
C---- RECUPERATION DES POINTEURS ( E : ECRITURE ) SELON OPTION
C______________________________________________________________________
C
C------- VECTEUR DES FORCES INTERNES
C
         CALL JEVECH ( 'PVECTUR' , 'E' , IVECTU )
C
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
C---- RECUPERATION DE L ADRESSE DES VARIABLES NODALES TOTALES
C
C---- A L INSTANT MOINS  ( PAS PRECEDENT )
C
      CALL JEVECH ( 'PDEPLMR' , 'L' , IUM    )
C
C______________________________________________________________________
C
C---- REPERE LOCAUX AUX NOEUDS SUR LA CONFIGURATION INITIALE
C
      CALL VECTAN(NB1,NB2, ZR(IGEOM) , ZR(LZR) ,VECTA,VECTN,VECTPT)
C
C---- DEPLACEMENT TOTAL AUX NOEUDS DE SERENDIP
C
      CALL R8INIR ( 8 * 3 , 0.D0 , VECU   , 1 )
C
      DO 101 IN = 1 , NB1
         DO 111 II = 1 , 3
C
           VECU   ( IN , II ) = ZR ( IUM - 1 + 6 * ( IN - 1 ) + II     )
C
 111     CONTINUE
 101  CONTINUE
C
C---- ROTATION TOTALE AUX NOEUDS
C
      CALL R8INIR ( 9 * 3 , 0.D0 , VECTHE , 1 )
C
C
C------- EN ACCORD AVEC LA MISE A JOUR DES GRANDES ROTATIONS AUFAURE
C
C------- NOEUDS DE SERENDIP
C
         DO 202 IN = 1 , NB1
            DO 212 II = 1 , 3
           VECTHE ( IN , II ) = ZR ( IUM - 1 + 6 * ( IN - 1 ) + II + 3 )
 212        CONTINUE
 202     CONTINUE
C
C--------- SUPERNOEUD
C
         DO 222 II = 1 , 3
           VECTHE ( NB2, II ) = ZR ( IUM - 1 + 6 * ( NB1    ) + II     )
 222     CONTINUE
C
C
C---- TRANSFORMEES NORMALES ET MATRICES DE ROTATION AUX NOEUDS
C
      CALL VECTRN ( NB2 , VECTPT , VECTN  , VECTHE , VECNPH , BLAM )
C
C---- VECTEUR PE DES VARIABLES NODALES TOTALES GENERALISEES
C
      CALL VECTPE ( NB1 , NB2 , VECU , VECTN , VECNPH , VECPE )
C
C______________________________________________________________________
C
C---- INITIALISATION DES OPERATEURS DE DEFORMATION A EXTRAPOLER
C
C---- MEMBRANE REDUIT INCOMPLET
C
      CALL R8INIR ( 3 * 51 * 4 , 0.D0 , B1MRI , 1 )
C
      CALL R8INIR ( 3 * 51 * 4 , 0.D0 , B2MRI , 1 )
C
C---- SHEAR    REDUIT   COMPLET
C
      CALL R8INIR ( 2 * 51 * 4 , 0.D0 , B1SRC , 1 )
C
      CALL R8INIR ( 2 * 51 * 4 , 0.D0 , B2SRC , 1 )

C POUR RESI_REFE_RELA

      IF  (OPTION.EQ.'REFE_FORC_NODA') THEN

         CALL R8INIR(51,0.D0,FTEMP,1)

      ENDIF
C
C---- COMPTEUR DES POINTS D INTEGRATIONS ( EPAISSEUR * SURFACE )
C
      KPGS  = 0
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
C
C+++++++++++++ B1MRI ( 3 , 51 , 4 ) MEMBRANE REDUIT INCOMPLET
C              B2MRI ( 3 , 51 , 4 )
C
               CALL MATBMR ( NB1   , VECTT , DUDXRI , INTSR , JDN1RI ,
     &                       B1MRI , B2MRI )
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
C
C------------- J1DN2RC ( 9 , 6 * NB1 + 3 ) INDN = 0 REDUIT
C                                          INDC = 1 COMPLET
C
               CALL JM1DN2
     & ( 0 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSR ,
     &                                          VECNPH , JM1 , JDN2RC )
C
C+++++++++++++ B1SRC ( 2 , 51 , 4 ) SHEAR REDUIT COMPLET
C              B2SRC ( 2 , 51 , 4 )
C
               CALL MATBSR ( NB1    , VECTT  , DUDXRC , INTSR ,
     &                       JDN1RC , JDN2RC ,
     &                       B1SRC  , B2SRC  )
C
C========== FIN 1 ERE BOUCLE NPGSR
C
 620        CONTINUE
C
C========== BOUCLE SUR POINTS INTEGRATION NORMALE SURFACE MOYENNE
C
            DO 630 INTSN = 1 , NPGSN
C
               KPGS = KPGS + 1
C
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
C
C------------- J1DN2NC ( 9 , 6 * NB1 + 3 ) INDN = 1 NORMAL
C                                          INDC = 1 COMPLET
C
               CALL JM1DN2
     & ( 1 , 1 , NB1 , NB2 , ZR ( LZR ) , EPAIS , KSI3S2 , INTSN ,
     &                                          VECNPH , JM1 , JDN2NC )
C
C+++++++++++++ B1MNC ( 3 , 51 ) MEMBRANE NORMAL COMPLET
C              B2MNC ( 3 , 51 )
C
               CALL MATBMN ( NB1   , VECTT , DUDXNC , JDN1NC , JDN2NC ,
     &                       B1MNC , B2MNC )
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
C
C+++++++++++++ B1MNI ( 3 , 51 ) MEMBRANE NORMAL INCOMPLET
C              B2MNI ( 3 , 51 )
C
               CALL MATBMN ( NB1   , VECTT , DUDXNI , JDN1NI , JDN1NI ,
     &                       B1MNI , B2MNI )
C
C============= B1SU ( 5 , 51 ) SUBSTITUTION TOTAL
C              B2SU ( 5 , 51 ) SUBSTITUTION DIFFERENTIEL
C
               CALL MATBSU ( NB1   , ZR ( LZR ) , NPGSR , INTSN ,
     &                       B1MNC , B2MNC      , B1MNI , B2MNI ,
     &                       B1MRI , B2MRI      , B1SRC , B2SRC ,
     &                       B1SU  , B2SU  )

               IF  (OPTION.EQ.'FORC_NODA') THEN

C
C------- CONTRAINTES DE CAUCHY = PK2 AUX POINTS DE GAUSS
C
                   STILD(1) = ZR ( ICONTM - 1 + ( KPGS - 1 ) * 6 + 1 )
                   STILD(2) = ZR ( ICONTM - 1 + ( KPGS - 1 ) * 6 + 2 )
                   STILD(3) = ZR ( ICONTM - 1 + ( KPGS - 1 ) * 6 + 4 )
                   STILD(4) = ZR ( ICONTM - 1 + ( KPGS - 1 ) * 6 + 5 )
                   STILD(5) = ZR ( ICONTM - 1 + ( KPGS - 1 ) * 6 + 6 )
C
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
C
               ELSEIF  (OPTION.EQ.'REFE_FORC_NODA') THEN

C            CALCUL DES FORCES NODALES DE REFERENCE EN AFFECTANT
C            LA VALEUR SIGM_REFE A CHAQUE CMP SUCCESSIVEMENT
C            POUR CHAQUE POINT D'INTEGRATION

                  CALL R8INIR(5,0.D0,SIGTMP,1)

                  DO 155 I=1,5

                    SIGTMP(I)=ZR(ICONTM)

                    CALL BTSIG ( 6 * NB1 + 3 , 5 ,
     &                   ZR (LZR - 1 + 127 + INTSN - 1) * DETJ * COEF ,
     &                   B2SU , SIGTMP , EFFINT )
C
                    SIGTMP(I)=0.D0

                    DO 156 J=1,51

                       FTEMP(J) = FTEMP(J)+ABS(EFFINT(J))

 156                CONTINUE

 155              CONTINUE

               ENDIF
C
C========== FIN BOUCLE NPGSN
C
 630        CONTINUE
C
C
C-------- FIN BOUCLE NPGE
C
 610      CONTINUE
C
C---- FIN BOUCLE NBCOU
C
 600  CONTINUE
C
C
C      ON PREND LA VALEUR MOYENNE DES FORCES NODALES DE REFERENCE

      IF  (OPTION.EQ.'REFE_FORC_NODA') THEN

         NVAL=NBCOU*NPGE*NPGSN*5

         CALL DAXPY(51,1.D0/NVAL,FTEMP,1,ZR ( IVECTU ) ,1)

      ENDIF
C
      END
