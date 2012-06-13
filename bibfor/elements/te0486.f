      SUBROUTINE TE0486 ( OPTION , NOMTE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT EN PRESSION SUIVEUSE
C          POUR LES PLAQUES ET COQUES
C          MATRICE DE RIGIDITE TANGENTE POUR LES COQUES 3D
C          (TOUJOURS EN PRESSION SUIVEUSE)
C
C          OPTIONS : 'CHAR_MECA_PRSU_R '
C                    'CHAR_MECA_SRCO3D '
C                    'RIGI_MECA_SRCO3D' (COQUES 3D SEULEMENT)
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
      CHARACTER*24 VALK
C ----------------------------------------------------------------------
      INTEGER       NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO
      INTEGER       IGEOM, IUM, IUP, IPRES, IRES, ICACO
      INTEGER       INO, LZI, LZR, IADZI, IAZK24, IRET
      INTEGER       I, J, IN, KN, II, KOMPTN, NB1, NB2
      INTEGER       IVECTU, IMATUN, INTSN, NPGSN
      INTEGER       IRCO3D, IFCO3D, ITEMPS, IERZ
      REAL*8        PRES, PRESNO(9), MADN(3,51), NKS1(3,51), NKS2(3,51)
      REAL*8        A1(3), A2(3), ANTA1(3,3), ANTA2(3,3), SURF(3)
      REAL*8        RIGNS(2601), PR
      REAL*8        VECTA(9,2,3), VECTN(9,3), VECTPT(9,2,3), VALPAR(4)
      LOGICAL       LOCAPR
      CHARACTER*8   NOMAIL, NOMPAR(4)
C     ------------------------------------------------------------------
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PDEPLMR', 'L', IUM )
      CALL JEVECH ( 'PDEPLPR', 'L', IUP )
C
      IF (NOMTE.NE.'MEC3QU9H' .AND. NOMTE.NE.'MEC3TR7H') THEN
C     --- AUTRES ELEMENTS QUE LA COQUE 3 D ---
C
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO)

         CALL TECACH ( 'NNN', 'PPRESSR', 1, IPRES,IRET )
C
         IF ( IPRES .EQ. 0 ) THEN
            CALL JEVECH ( 'PFRCO3D', 'L', IPRES )
            CALL JEVECH ( 'PCACOQU', 'L', ICACO )
         ENDIF
         CALL JEVECH ( 'PVECTUR', 'E', IRES  )
C
         DO 10 INO = 0 , NNO-1
            IF ( ZR(IPRES+INO) .NE. 0.D0 ) THEN
               CALL TECAEL ( IADZI, IAZK24 )
               NOMAIL = ZK24(IAZK24-1+3)(1:8)
               VALK = NOMAIL
               CALL U2MESG('F', 'ELEMENTS4_92',1,VALK,0,0,0,0.D0)
            ENDIF
 10      CONTINUE
C
      ELSE
C     --- ELEMENTS COQUES 3D ---
C
         CALL JEVETE ( '&INEL.'//NOMTE(1:8)//'.DESI' , ' ' , LZI )
C
C        --- NOMBRE DE NOEUDS ( NB1 : SERENDIP , NB2 : LAGRANGE )
         NB1   = ZI ( LZI - 1 + 1 )
         NB2   = ZI ( LZI - 1 + 2 )
C        --- NBRE POINTS INTEGRATIONS (NPGSN : NORMALE )
         NPGSN = ZI ( LZI - 1 + 4 )
C
C        ---  ( FONCTIONS DE FORMES, DERIVEES ET POIDS )
         CALL JEVETE ( '&INEL.'//NOMTE(1:8)//'.DESR' , ' ' , LZR )
C
C        ---- RECUPERATION DES POINTEURS ( E : ECRITURE ) SELON OPTION
         IF ( OPTION ( 1 : 16 ) . EQ . 'CHAR_MECA_SRCO3D' . OR .
     &        OPTION ( 1 : 16 ) . EQ . 'CHAR_MECA_SFCO3D'        ) THEN
C           --- VECTEUR DES FORCES INTERNES
            CALL JEVECH ( 'PVECTUR' , 'E' , IVECTU )
         ENDIF
C
         IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_SRCO3D' . OR .
     &        OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_SFCO3D'        ) THEN
C         --- MATRICE TANGENTE DE RIGIDITE ET INITIALISATION
C
C           CALL JEVECH ( 'PMATUUR' , 'E' , IMATUU )
C            POUR UNE MATRICE NON SYMETRIQUE (VOIR AUSSI MECGME)
            CALL JEVECH ( 'PMATUNS' , 'E' , IMATUN )
            CALL R8INIR ( 51 * 51 , 0.D0 , RIGNS , 1 )
         ENDIF
C
C        ---- RECUPERATION DE L ADRESSE DES VARIABLES NODALES TOTALES
         CALL JEVECH ( 'PDEPLMR' , 'L' , IUM    )
         CALL JEVECH ( 'PDEPLPR' , 'L' , IUP    )
C
C        ---  REACTUALISATION DE LA GEOMETRIE
C
         DO 30 IN = 1 , NB2-1
            DO 20 II = 1 , 3
               ZR ( IGEOM - 1 + 3 *  ( IN - 1 ) + II )
     &       = ZR ( IGEOM - 1 + 3 *  ( IN - 1 ) + II )
     &       + ZR ( IUM   - 1 +  6 * ( IN - 1 ) + II )
     &       + ZR ( IUP   - 1 +  6 * ( IN - 1 ) + II )
  20        CONTINUE
  30     CONTINUE
C
C        ---- RECUPERATION DU VECTEUR DE PRESSION NODALE A INTERPOLER
         IF      ( OPTION ( 10 : 16 ) . EQ . '_SRCO3D' ) THEN
            CALL JEVECH ( 'PFRCO3D' , 'L' , IRCO3D )
            DO 40 KN = 1 , NB2
               PRESNO ( KN ) = - ZR ( IRCO3D - 1 + ( KN - 1 ) * 7 + 3 )
  40        CONTINUE
C
         ELSE IF ( OPTION ( 10 : 16 ) . EQ . '_SFCO3D' ) THEN
            CALL JEVECH ( 'PFFCO3D' , 'L' , IFCO3D )
            CALL JEVECH ( 'PTEMPSR' , 'L' , ITEMPS )
            VALPAR ( 4 ) = ZR ( ITEMPS )
            NOMPAR ( 4 ) = 'INST'
            NOMPAR ( 1 ) = 'X'
            NOMPAR ( 2 ) = 'Y'
            NOMPAR ( 3 ) = 'Z'

            DO 55 I=1,NB2
               PRESNO(I)=0.D0
55          CONTINUE

C            GLOBAL = ZK8 ( IFCO3D + 6 ) .EQ. 'GLOBAL'
            LOCAPR = ZK8 ( IFCO3D + 6 ) .EQ. 'LOCAL_PR'
C
            IF ( LOCAPR ) THEN
C
C             IF (NOMTE.EQ.'MEC3QU9H') THEN
              DO 50 IN = 0, NB2-1
                 VALPAR ( 1 ) = ZR ( IGEOM + 3 * IN     )
                 VALPAR ( 2 ) = ZR ( IGEOM + 3 * IN + 1 )
                 VALPAR ( 3 ) = ZR ( IGEOM + 3 * IN + 2 )
C
                 CALL FOINTE ( 'FM', ZK8 ( IFCO3D + 2 ), 4, NOMPAR,
     &                        VALPAR , PR , IERZ )
                 PRESNO ( IN+1 ) = PR
                 IF ( IERZ . NE . 0 )  CALL U2MESS('F','ELEMENTS4_1')
C
  50          CONTINUE
C
C             ENDIF
            ENDIF
C
         ENDIF
C
C        ---- VECTEURS TANGENTS A1 ET A2 AUX NOEUDS NON NORMALISES
C
         CALL VECTAN(NB1,NB2, ZR(IGEOM) , ZR(LZR) ,VECTA,VECTN,VECTPT)
C
C        ---- BOUCLE SUR LES POINTS D INTEGRATION NORMALE
         DO 80 INTSN = 1 , NPGSN
C
C          ---- VECTEURS DE BASE AUX POINTS DE GAUSS A KSI3 = 0.D0
           CALL R8INIR ( 3 , 0.D0 , A1 , 1 )
           CALL R8INIR ( 3 , 0.D0 , A2 , 1 )
C
C          ---- INTERPOLATIONS PRESSION
C               VECTEURS TANGENTS NON NORMES A1 A2
           PRES = 0.D0
C
           DO 70 KN = 1 , NB2
C
             PRES = PRES + ZR ( LZR - 1 + 459 + 9 * ( INTSN - 1 ) + KN )
     &                    * PRESNO ( KN )
C
              DO 60 II = 1 , 3
C
                A1 ( II ) = A1 ( II ) +
     &                     ZR ( LZR - 1 + 459 + 9 * ( INTSN - 1 ) + KN )
     &                   * VECTA ( KN , 1 , II )
C
                A2 ( II ) = A2 ( II ) +
     &                     ZR ( LZR - 1 + 459 + 9 * ( INTSN - 1 ) + KN )
     &                   * VECTA ( KN , 2 , II )
C
  60         CONTINUE
C
  70       CONTINUE
C
C          ---- A1 VECTORIEL A2
           CALL PROVEC ( A1 , A2 , SURF )
C
C          --- MATRICE D INTERPOLATION POUR LES DEPLACEMENTS
           CALL MATDN ( NB1 , ZR (LZR) , INTSN , MADN , NKS1 , NKS2 )
C
           IF ( OPTION ( 1 : 16 ) . EQ . 'CHAR_MECA_SRCO3D' . OR .
     &          OPTION ( 1 : 16 ) . EQ . 'CHAR_MECA_SFCO3D'      )  THEN
C
C            --- FORCE EXTERNE NODALE AU SIGNE DU TE0423 ET NMPR3D
             CALL BTSIG ( 6 * NB1 + 3 , 3 ,
     &                    - PRES * ZR (LZR - 1 + 127 + INTSN - 1) ,
     &                    MADN , SURF , ZR ( IVECTU ) )
           ENDIF
C
           IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_SRCO3D' . OR .
     &          OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_SFCO3D'       ) THEN
C
C            --- MATRICE ANTISYM DE A1 ET DE A2
             CALL ANTISY ( A1 , 1.D0 , ANTA1 )
             CALL ANTISY ( A2 , 1.D0 , ANTA2 )
C
C            --- PREMIER TERME
             CALL B1TDB2 ( MADN , NKS2 , ANTA1 ,
     &                     PRES *  ZR (LZR - 1 + 127 + INTSN - 1)  ,
     &                     3 , 6 * NB1 + 3 , RIGNS )
C
C           --- DEUXIEME TERME
            CALL B1TDB2 ( MADN , NKS1 , ANTA2 ,
     &                    - PRES *  ZR (LZR - 1 + 127 + INTSN - 1)  ,
     &                     3 , 6 * NB1 + 3 , RIGNS )
           ENDIF
C
  80     CONTINUE
C
C
         IF ( OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_SRCO3D' . OR .
     &        OPTION ( 1 : 16 ) . EQ . 'RIGI_MECA_SFCO3D'        ) THEN
C
C        --- PARTIE SYMETRIQUE DE LA MATRICE TANGENTE
C
            KOMPTN = 0
C            KOMPTU = 0
            DO 110   J = 1 , 6 * NB1 + 3
C
C            POUR UNE MATRICE NON SYMETRIQUE (VOIR AUSSI MECGME)
               DO 90  I = 1 , 6*NB1+3
                  ZR ( IMATUN + KOMPTN ) = - RIGNS((6*NB1+3)*(I-1)+J)
                  KOMPTN = KOMPTN + 1
   90          CONTINUE
C
C              DO 100  I = 1 , J
C                 KOMPTU = KOMPTU + 1
C                 ZR ( IMATUU - 1 + KOMPTU ) = -0.5D0 *
C     &              (    RIGNS ( ( 6 * NB1 + 3 ) * ( J - 1 ) + I )
C     &              +  RIGNS ( ( 6 * NB1 + 3 ) * ( I - 1 ) + J )  )
C
C  100         CONTINUE
C
  110       CONTINUE
C
         ENDIF
C
      ENDIF
C
      END
