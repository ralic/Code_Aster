      SUBROUTINE TE0050 ( OPTION , NOMTE )
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'AMOR_MECA'
C                                OU 'RIGI_MECA_HYST'
C        POUR TOUS LES TYPES D'ELEMENTS (SAUF LES ELEMENTS DISCRETS)
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER         ( NBRES=2 )
      PARAMETER         ( NBPAR=4 )
      CHARACTER*8        NOMRES(NBRES),NOMPAR(NBPAR)
      CHARACTER*2        BL2, CODRET(NBRES)
      REAL*8             VALRES(NBRES),VALPAR(NBPAR)
      INTEGER            IRESU,ITEMPE,IRIGEL,IMASEL,IGEOM,IMATE
      INTEGER            IDRESU(5),IDRIGI(2),IDMASS(2),IDGEO(5)
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
C     -- RECUPERATION DES CHAMPS PARAMETRES ET DE LEURS LONGUEURS:
C     ------------------------------------------------------------
      IF (OPTION.EQ.'AMOR_MECA') THEN
      CALL TECACH('ONN','PMATUUR',5,IDRESU,IRET)
      ELSE IF (OPTION.EQ.'RIGI_MECA_HYST') THEN
      CALL TECACH('ONN','PMATUUC',5,IDRESU,IRET)
      ELSE
        CALL UTMESS('F','TE0050','MESSAGE VIDE    ')
      END IF
      NBVAL= IDRESU(2)
C
      BL2 = '  '
      NOMPAR(1)='TEMP'
      NOMPAR(2)='X'
      NOMPAR(3)='Y'
      NOMPAR(4)='Z'
C
      CALL JEVECH('PTEMPER','L',ITEMPE)
      VALPAR(1)=ZR(ITEMPE)
C
      CALL TECACH('ONN','PGEOMER',5,IDGEO,IRET)
      IDIMGE=IDGEO(2)
      DO 5, K=1,IDIMGE
        VALPAR(1+K)=ZR(IDGEO(1)-1+K)
 5    CONTINUE
      NPARA=1+IDIMGE
C
      CALL JEVECH('PMATERC','L',IMATE)
      MATER=ZI(IMATE)
C
      CALL TECACH('ONN','PRIGIEL',2,IDRIGI,IRET)
      IF (IDRIGI(2).NE.NBVAL)
     + CALL UTMESS('F','TE0050','LONGUEURS DIFF. POUR RIGI ET AMOR')
C
C
C     -- RECUPERATION DES COEFFICIENTS FONCTIONS DE LA TEMPERATURE
C        ET DE LA GEOMETRIE :
C     -------------------------------------------------------------
C
      IF (OPTION.EQ.'AMOR_MECA') THEN
C     --------------------------------
      CALL TECACH('ONN','PMASSEL',2,IDMASS,IRET)
        IF (IDMASS(2).NE.NBVAL)
     + CALL UTMESS('F','TE0050','LONGUEURS DIFF. POUR MASS ET AMOR')
        NOMRES(1)='AMOR_ALPHA'
        NOMRES(2)='AMOR_BETA'
        VALRES(1) = 0.D0
        VALRES(2) = 0.D0
        CALL RCVALA ( MATER,'ELAS',NPARA,NOMPAR,VALPAR,
     +                2,NOMRES,VALRES,CODRET, BL2 )
        IF ( CODRET(1) .NE. 'OK' .OR. CODRET(2) .NE. 'OK') THEN
          CALL RCVALA ( MATER,'CABLE',NPARA,NOMPAR,VALPAR,
     +                2,NOMRES,VALRES,CODRET, BL2 )
          IF ( CODRET(1) .NE. 'OK' .OR. CODRET(2) .NE. 'OK') THEN
            VALRES(1) = 0.D0
            VALRES(2) = 0.D0
          ENDIF
        ENDIF
C        IF ( CODRET(2) .NE. 'OK' ) VALRES(2) = 0.D0
C
      ELSE IF (OPTION.EQ.'RIGI_MECA_HYST') THEN
C     ------------------------------------------
        NOMRES(1)='AMOR_HYST'
        CALL RCVALA ( MATER,'ELAS',NPARA,NOMPAR,VALPAR,
     +                1,NOMRES,VALRES,CODRET, 'FM' )
      ELSE
        CALL UTMESS('F','TE0050','MESSAGE VIDE    ')
      END IF
C
C
C     -- CALCUL PROPREMENT DIT :
C     --------------------------
      IRESU= IDRESU(1)
      IRIGI= IDRIGI(1)
      IF (OPTION.EQ.'AMOR_MECA') THEN
        ALPHA= VALRES(1)
        BETA = VALRES(2)
        IMASS= IDMASS(1)
C
        DO 1 I=1,NBVAL
          ZR(IRESU-1+I)=ALPHA*ZR(IRIGI-1+I)+BETA*ZR(IMASS-1+I)
 1      CONTINUE
      ELSE IF (OPTION.EQ.'RIGI_MECA_HYST') THEN
        ETA  = VALRES(1)
        DO 2 I=1,NBVAL
          ZC(IRESU-1+I)=DCMPLX(ZR(IRIGI-1+I),ETA*ZR(IRIGI-1+I))
 2      CONTINUE
      END IF
C
C
      END
