      SUBROUTINE TE0487 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/05/2003   AUTEUR CIBHHPD D.NUNEZ 
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
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT EN PRESSION SUIVEUSE
C          POUR LES PLAQUES ET COQUES
C
C          OPTION : 'CHAR_MECA_PRSU_F '
C                   'CHAR_MECA_SFCO3D '
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ----------------------------------------------------------------------
      INTEGER       IGEOM, IDEPM, IDEPP, IPRES, ITEMP, IRES, ICACO
      INTEGER       NNO, INO, LZI, IADZI, IAZK24, IER,IRET
      REAL*8        VALPAR(4), PR
      CHARACTER*8   NOMAIL, NOMPAR(4),ELREFE
      CHARACTER*24  DESI
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)

      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PDEPLMR', 'L', IDEPM )
      CALL JEVECH ( 'PDEPLPR', 'L', IDEPP )
      CALL JEVECH ( 'PTEMPSR', 'L', ITEMP )
      CALL TECACH ( 'NNN', 'PPRESSF', 1, IPRES,IRET )
      IF ( IPRES .EQ. 0 ) THEN
         CALL JEVECH ( 'PFFCO3D', 'L', IPRES )
         CALL JEVECH ( 'PCACOQU', 'L', ICACO )
      ENDIF
      CALL JEVECH ( 'PVECTUR', 'E', IRES  )
C
      DESI = '&INEL.'//ELREFE//'.DESI'
      CALL JEVETE ( DESI, 'L', LZI )
      NNO = ZI(LZI)
C
      NOMPAR(1) = 'X'
      NOMPAR(2) = 'Y'
      NOMPAR(3) = 'Z'
      NOMPAR(4) = 'INST'
      VALPAR(4) = ZR(ITEMP)
C
      DO 10 INO = 0 , NNO-1
         VALPAR(1) = ZR(IGEOM+3*INO  )
         VALPAR(2) = ZR(IGEOM+3*INO+1)
         VALPAR(3) = ZR(IGEOM+3*INO+2)
         CALL FOINTE('FM',ZK8(IPRES),4,NOMPAR,VALPAR,PR,IER)
         IF ( PR .NE. 0.D0 ) THEN
            CALL TECAEL ( IADZI, IAZK24 )
            NOMAIL = ZK24(IAZK24-1+3)(1:8)
            CALL UTDEBM ( 'F', 'TE0487', 'LA PRESSION DOIT ETRE NULLE' )
            CALL UTIMPK ( 'S', ' POUR LA MAILLE ', 1, NOMAIL )
            CALL UTFINM
         ENDIF
 10   CONTINUE
C
      END
