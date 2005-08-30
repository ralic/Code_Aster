      SUBROUTINE TE0401 ( OPTIOZ , NOMTZ )
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
C MODIF ELEMENTS  DATE 29/08/2005   AUTEUR A3BHHAE H.ANDRIAMBOLOLONA 
      IMPLICIT NONE
      CHARACTER*(*)   OPTIOZ , NOMTZ
      CHARACTER*16    OPTION , NOMTE
C     ----------------------------------------------------------------
C     CALCUL DES OPTIONS DES ELEMENTS DE COQUE : COQUE_3D
C     ----------------------------------------------------------------
C
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
      INTEGER       NB1 , NB2 , NDDLET 
      INTEGER       LZR 
      INTEGER       JGEOM , JENER , JVECT , JVAPR
      INTEGER       I , J , KOMPT
      INTEGER       IU , IMATUU
      REAL*8        MATLOC(51,51),PLG(9,3,3),MATP(51,51)
      REAL*8        VRS (1326) 
      REAL*8        BSIGTH(51), ENERTH
      LOGICAL       INDITH
C DEB
C
      OPTION = OPTIOZ
      NOMTE  = NOMTZ
C
      ENERTH = 0.0D0
C
      IF ( NOMTE(1:8) .EQ. 'MEGRC3Q9'  .OR.
     +     NOMTE(1:8) .EQ. 'MEGRC3T7'  ) THEN
         CALL UTMESS('F','TE0401','DEVELOPPEMENT NON REALISE')
      ENDIF
C
      CALL JEVECH ('PGEOMER' , 'L' , JGEOM)
C
      IF ( OPTION .EQ. 'RIGI_MECA'      )   
     &                     CALL JEVECH ( 'PMATUUR' , 'E' , IMATUU )
C
      IF ( OPTION .EQ. 'RIGI_MECA'      .OR. 
     &     OPTION .EQ. 'RIGI_MECA_SENSI' .OR.
     &     OPTION .EQ. 'RIGI_MECA_SENS_C' .OR.
     &     OPTION .EQ. 'EPOT_ELEM_DEPL'      ) THEN
C
         CALL VDXRIG (NOMTE,ZR(JGEOM),MATLOC,NB1 , 0 , 0 )
C
C     CONSTRUCTION DE LA MATRICE DE PASSAGE REPERE GLOBAL REPERE LOCAL
C
         CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ', LZR )
C
         NB2 = NB1 + 1
         CALL MATPGL ( NB2, ZR(LZR), PLG )
C
         CALL R8INIR ( 1326 , 0.D0 , VRS , 1 )
C
         NDDLET = 6*NB1 + 3
C
         CALL TRANLG ( NB1 , 51 , NDDLET , PLG , MATLOC , VRS )
C
      ELSE
         CALL UTMESS('F','TE0401','OPTION: '//OPTION//' NON IMPLANTE')
      ENDIF
C
C
      IF ( OPTION .EQ. 'RIGI_MECA'      ) THEN
C
C--------- STOCKAGE
C
C--------- COMPTEUR DE POSITION
C
           KOMPT = 0
C
           DO 100   J = 1 , 6 * NB1 + 3
              DO 110  I = 1 , J
                 KOMPT = KOMPT + 1
                 ZR ( IMATUU - 1 + KOMPT ) = VRS ( KOMPT )
 110          CONTINUE
 100       CONTINUE
C
      END IF
C
      IF (OPTION(11:14).EQ.'SENS') THEN
        IF (OPTION(15:16).EQ.'_C') THEN
          CALL JEVECH('PVECTUC','E',JVECT)
        ELSE
          CALL JEVECH('PVECTUR','E',JVECT)
        END IF
        CALL JEVECH('PVAPRIN','L',JVAPR)
C PASSAGE VECTEUR - MATRICE
        KOMPT = 0
        DO 220 J = 1,NDDLET
          DO 230 I = 1,J
             KOMPT = KOMPT+1
             MATP(J,I) = VRS(KOMPT)
             MATP(I,J) = VRS(KOMPT)
 230      CONTINUE
 220    CONTINUE
        DO 200 I = 1,NDDLET
          IF (OPTION(15:16).EQ.'_C') THEN
            ZC(JVECT-1+I) = DCMPLX(0.D0,0.D0)
          ELSE
            ZR(JVECT-1+I) = 0.D0
          END IF
          DO 210 J = 1,NDDLET
            IF (OPTION(15:16).EQ.'_C') THEN
              ZC(JVECT-1+I) = ZC(JVECT-1+I)
     &             - MATP(I,J)*ZC(JVAPR-1+J)
            ELSE
              ZR(JVECT-1+I) = ZR(JVECT-1+I)
     &             - MATP(I,J)*ZR(JVAPR-1+J)
            END IF
 210      CONTINUE
 200    CONTINUE
      END IF
C
C---- ENERGIES DE DEFORMATION ELASTIQUE
C
      IF ( OPTION .EQ. 'EPOT_ELEM_DEPL' ) THEN
C
C------- LECTURE DE L'ADRESSE
C
         CALL JEVECH ( 'PENERDR' , 'E' , JENER )
C
C------- ADRESSE DES DEPLACEMENTS
C
         CALL JEVECH ( 'PDEPLAR' , 'L' , IU    )
C
C
C------- ENERGIE DE DEFORMATION TOTALE
C
         CALL UTVTSV 
     &       ( 'ZERO' , 6 * NB1 + 3 , VRS , ZR ( IU ) , ZR ( JENER ) )
C
         ZR ( JENER ) = 0.5D0 * ZR ( JENER )
C
         CALL BSTHCO ( NOMTE(1:8), BSIGTH, INDITH )
C
         IF ( INDITH ) THEN
           DO 120 I = 1, 6 * NB1 + 3
             ENERTH = ENERTH + BSIGTH(I)*ZR(IU+I-1)
  120      CONTINUE
           ZR(JENER) = ZR(JENER) - ENERTH
         ENDIF
C
         IF ( ABS ( ZR ( JENER ) ) . GT . 1.D-6 ) THEN
C
C--------- ENERGIE DE DEFORMATION DE MEMBRANE
C
           CALL VDXRIG (NOMTE,ZR(JGEOM),MATLOC,NB1 , 1 , 0 )
C
           CALL R8INIR ( 1326 , 0.D0 , VRS , 1 )
C
           CALL TRANLG ( NB1 , 51 , NDDLET , PLG , MATLOC , VRS )
C
           CALL UTVTSV
     &     ( 'ZERO' , 6 * NB1 + 3 , VRS , ZR ( IU ) , ZR ( JENER + 1 ) )
C
           ZR ( JENER + 1 ) = 0.5D0 * ZR ( JENER + 1 )
C
C
C--------- ENERGIE DE DEFORMATION DE FLEXION
C
           CALL VDXRIG (NOMTE,ZR(JGEOM),MATLOC,NB1 , 0 , 1 )
C
           CALL R8INIR ( 1326 , 0.D0 , VRS , 1 )
C
           CALL TRANLG ( NB1 , 51 , NDDLET , PLG , MATLOC , VRS )
C
           CALL UTVTSV
     &     ( 'ZERO' , 6 * NB1 + 3 , VRS , ZR ( IU ) , ZR ( JENER + 2 ) )
C
           ZR ( JENER + 2 ) = 0.5D0 * ZR ( JENER + 2 )
C
C--------- VALEURS RELATIVES
C
           ZR ( JENER + 1 ) = ZR ( JENER + 1 ) / ZR ( JENER )
           ZR ( JENER + 2 ) = ZR ( JENER + 2 ) / ZR ( JENER )
C
         ELSE
C
           CALL R8INIR ( 2 , 0.D0 ,  ZR ( JENER + 1 ) , 1 )
C
         END IF
C
CCC         CALL UTMESS('A','TE0401',
CCC     &   'ENERGIE POTENTIELLE DE DEFORMATION ELASTIQUE DE
CCC     &    CISAILLEMENT TRANSVERSE NON NULLE')
C
      END IF
C
C
      END
