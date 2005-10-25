      SUBROUTINE TE0414 ( OPTIOZ , NOMTZ )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)   OPTIOZ , NOMTZ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/10/2005   AUTEUR CIBHHLV L.VIVAN 
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
C     ----------------------------------------------------------------
C     CALCUL DES OPTIONS DES ELEMENTS DE COQUE : COQUE_3D
C     ----------------------------------------------------------------
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
      INTEGER       NB1, JCRET, CODRET
      REAL*8        MATLOC(51,51), PLG(9,3,3), COEF
      LOGICAL       MATRIC
      CHARACTER*16  OPTION , NOMTE
      CHARACTER*24  DESI , DESR
C ----------------------------------------------------------------------
C
      OPTION = OPTIOZ
      NOMTE  = NOMTZ
C
      CALL JEVEUO ('&INEL.'//NOMTE(1:8)//'.DESI', 'L' , LZI )
      NB2 = ZI(LZI-1+2)
C
      MATRIC = ( OPTION(1:9) .EQ.'FULL_MECA'  .OR.
     &           OPTION(1:10).EQ.'RIGI_MECA_' )
C
      CALL JEVECH ('PGEOMER', 'L', JGEOM )
      CALL JEVECH ('PDEPLMR', 'L', IDEPLM)
      CALL JEVECH ('PDEPLPR', 'L', IDEPLP)
      CALL JEVECH ('PCOMPOR', 'L', ICOMPO)
C
      IF ( ZK16(ICOMPO+3)(1:9) .EQ. 'COMP_ELAS' ) THEN
C          ------------------------------------
C
C ------ HYPER-ELASTICITE
C
         IF ( ZK16 ( ICOMPO + 2 ) ( 1 : 8 ) . EQ . 'GREEN_GR' ) THEN
C
C --------- DEFORMATION DE GREEN 
C
            CALL VDGNLR ( OPTION , NOMTE ) 
C
            GO TO 9999
C
         ELSE
C
C --------- AUTRES MESURES DE DEFORMATIONS
C
           CALL UTMESS('F','TE0414',' DEFORMATION : '//ZK16(ICOMPO+2)// 
     &   ' NON IMPLANTEE SUR LES ELEMENTS COQUE_3D EN GRANDES ROTATIONS.
     &   DEFORMATION : GREEN_GR OBLIGATOIREMENT ' )
C
         ENDIF
C
      ELSEIF ( ZK16(ICOMPO+3)(1:9) .EQ. 'COMP_INCR' ) THEN
C              ------------------------------------

         IF ( ZK16(ICOMPO+2)(1:8) .EQ. 'GREEN_GR') THEN 
C
C --------- HYPO-ELASTICITE
C
            CALL VDPNLR ( OPTION , NOMTE, CODRET ) 
C
            GO TO 9999
C
         ELSE IF( ZK16(ICOMPO+2)(6:10) .EQ. '_REAC') THEN
C
            CALL UTMESS('A','TE0414',' LA REACTUALISATION DE LA '//
     +                  'GEOMETRIE (DEFORMATION : PETIT_REAC '//
     +                  'SOUS LE MOT CLE COMP_INCR) EST '//
     +                  'DECONSEILLEE POUR LES ELEMENTS DE COQUE_3D.')
C
            DO 90 I=1,NB2-1
               I1=3*(I-1)
               I2=6*(I-1)
               ZR(JGEOM+I1)   = ZR(JGEOM+I1)  +ZR(IDEPLM+I2)  
     &                                        +ZR(IDEPLP+I2)
               ZR(JGEOM+I1+1) = ZR(JGEOM+I1+1)+ZR(IDEPLM+I2+1)
     &                                        +ZR(IDEPLP+I2+1)
               ZR(JGEOM+I1+2) = ZR(JGEOM+I1+2)+ZR(IDEPLM+I2+2)
     &                                        +ZR(IDEPLP+I2+2)
 90         CONTINUE
         ENDIF
C
         CALL VDXNLR (OPTION,NOMTE,ZR(JGEOM),MATLOC,NB1,CODRET)

         IF ( MATRIC ) THEN
C
            CALL JEVECH ('PMATUUR' , 'E' , JMATR)
C
C --------- MATRICE DE PASSAGE REPERE GLOBAL REPERE LOCAL
C
            CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ', LZR )
            CALL MATPGL ( NB2, ZR(LZR), PLG )
C
C --------- OPERATION DE TRANFORMATION DE MATLOC DANS LE REPERE GLOBAL
C           ET STOCKAGE DANS ZR
C
            NDDLET = 6*NB1+3
            CALL TRANLG ( NB1 , 51 , NDDLET , PLG , MATLOC , ZR(JMATR) )
         ENDIF

      ENDIF
C
      IF ( OPTION(1:9) .EQ. 'RAPH_MECA'  .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA'  ) THEN
         CALL JEVECH ( 'PCODRET', 'E', JCRET )
         ZI(JCRET) = CODRET
      ENDIF
C
 9999 CONTINUE
C
      END
