      SUBROUTINE NMORTH (NDIM,TYPMOD,IMATE,COMPOR,PHENOM,
     &                   TEMP,TREF,OPTION,EPS,SIG,DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/03/2002   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER            NDIM,IMATE
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       COMPOR(4),OPTION,PHENOM
      REAL*8             TEMP,TREF
      REAL*8             EPS(6),SIG(6),DSIDEP(6,6)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ORTHOTROPE ELASTIQUE
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
C IN  PHENOM  : TYPE D'ELASTICITE
C                 ELAS_ORTH : ORTHOTROPE
C                 ELAS_GITR : ISOTROPE TRANSVERSE
C IN  TEMP    : TEMPERATURE A L'INSTANT DU CALCUL
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  OPTION  : OPTION DEMANDEE :
C                 RIGI_MECA_TANG : SIG DSIDEP
C                 FULL_MECA      : SIG DSIDEP
C                 RAPH_MECA      : SIG
C IN  EPS     : INCREMENT DE DEFORMATION
C               SI C_PLAN EPS(3) EST EN FAIT CALCULE
C OUT SIG     : CONTRAINTES
C OUT DSIDEP  : RIGIDITE TANGENTE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      LOGICAL      CPLAN, ORTHO
      REAL*8       VALRES(12)
      REAL*8       COEF(3), EPSTH(6)
      REAL*8       E1 ,E2 ,E3 ,A11 ,A12 ,A13 ,A22 ,A23 ,A33 ,C1 ,DELTA
      REAL*8       NU12 ,NU21 ,NU13 ,NU31 ,NU23 ,NU32 ,G12 ,G13 ,G23
      REAL*8       AL1 ,AL2 ,AL3
      INTEGER      NDIMSI, NBV, I, J
      CHARACTER*2  BL2, FB2, CODRET(12)
      CHARACTER*8  NOMRES(12)
C
C
C - INITIALISATION
C
      BL2 = '  '
      FB2 = 'F '
C
      CPLAN  = TYPMOD(1) .EQ.'C_PLAN'
      NDIMSI = 2*NDIM
      IF (PHENOM.EQ.'ELAS_ORTH') THEN
        ORTHO = .TRUE.
        NOMRES(1)='E_L'
        NOMRES(2)='E_T'
        NOMRES(3)='E_N'
        NOMRES(4)='NU_LT'
        NOMRES(5)='NU_LN'
        NOMRES(6)='NU_TN'
        NOMRES(7)='G_LT'
        NOMRES(8)='G_LN'
        NOMRES(9)='G_TN'
        NOMRES(10)='ALPHA_L'
        NOMRES(11)='ALPHA_T'
        NOMRES(12)='ALPHA_N'
      ELSE IF (PHENOM .EQ. 'ELAS_ISTR') THEN
        ORTHO = .FALSE.
        NOMRES(1)='E_L'
        NOMRES(2)='E_N'
        NOMRES(3)='NU_LT'
        NOMRES(4)='NU_LN'
        NOMRES(5)='G_LN'
        NOMRES(6)='ALPHA_L'
        NOMRES(7)='ALPHA_N'
      ELSE
        CALL UTMESS('F','NMORTH_1','COMPORTEMENT ELASTIQUE INEXISTANT')
      ENDIF
C
C
C - RECUPERATION DES CARACTERISTIQUES ELASTIQUES
C
      IF (ORTHO) THEN
        CALL RCVALA ( IMATE,PHENOM,1,'TEMP',TEMP,9,
     +                             NOMRES,VALRES,CODRET, FB2 )
        CALL RCVALA ( IMATE,PHENOM,1,'TEMP',TEMP,3,
     +                      NOMRES(10),VALRES(10),CODRET(10), BL2 )
        IF (CODRET(10).NE.'OK') VALRES(10)=0.D0
        IF (CODRET(11).NE.'OK') VALRES(11)=0.D0
        IF (CODRET(12).NE.'OK') VALRES(12)=0.D0
        E1   = VALRES(1)
        E2   = VALRES(2)
        E3   = VALRES(3)
        NU12 = VALRES(4)
        NU13 = VALRES(5)
        NU23 = VALRES(6)
        NU21 = E2*NU12/E1
        NU31 = E1*NU13/E3
        NU32 = E2*NU23/E3
        DELTA= 1.D0-NU23*NU32-NU31*NU13-NU21*NU12-2.D0*NU23*NU31*NU21
        A11 = (1.D0 - NU23*NU32)*E1/DELTA
        A12 = (NU21 + NU13*NU32)*E1/DELTA
        A13 = (NU13 + NU21*NU23)*E1/DELTA
        A22 = (1.D0 - NU13*NU31)*E2/DELTA
        A23 = (NU23 + NU13*NU12)*E2/DELTA
        A33 = (1.D0 - NU21*NU12)*E3/DELTA
        G12 = VALRES(7)
        G13 = VALRES(8)
        G23 = VALRES(9)
        AL1 = VALRES(10)
        AL2 = VALRES(11)
        AL3 = VALRES(12)
      ELSE
        CALL RCVALA ( IMATE,PHENOM,1,'TEMP',TEMP,5,
     +                             NOMRES,VALRES,CODRET, FB2 )
        CALL RCVALA ( IMATE,PHENOM,1,'TEMP',TEMP,2,
     +                      NOMRES(6),VALRES(6),CODRET(6), BL2 )
        IF (CODRET(6).NE.'OK') VALRES(6)=0.D0
        IF (CODRET(7).NE.'OK') VALRES(7)=0.D0
        E1   = VALRES(1)
        E3   = VALRES(2)
        NU12 = VALRES(3)
        NU13 = VALRES(4)
        C1   = E1/(1.D0+NU12)
        DELTA = 1.D0 - NU12 - 2.D0*NU13*NU13*E1/E3
        A11 = (1.D0 - NU13*NU13*E1/E3)/DELTA
        A12 = C1*(A11 - 1.D0)
        A11 = C1* A11
        A13 = E1*NU13/DELTA
        A22 = A11
        A23 = A13
        A33 = E3*(1.D0 - NU12)/DELTA
        G12 = C1/2.D0
        G13 = VALRES(5)
        G23 = G13
        AL1 = VALRES(6)
        AL2 = AL1
        AL3 = VALRES(7)
      ENDIF
C
C
C - CALCUL DE LA PART MECANIQUE DES DEFORMATIONS
C
      COEF(1)=AL1*(TEMP-TREF)
      COEF(2)=AL2*(TEMP-TREF)
      COEF(3)=AL3*(TEMP-TREF)
      IF (CPLAN)
     &  EPS(3)=-(A13*(EPS(1)-COEF(1))+A23*(EPS(2)-COEF(2)))/A33
     &          + COEF(3)
      DO 10 I=1,3
        EPSTH(I)   = EPS(I) -COEF(I)
        EPSTH(I+3) = EPS(I+3)
 10   CONTINUE
C
C
C - CALCUL DES CONTRAINTES
C
      SIG(1)=A11*EPSTH(1)+A12*EPSTH(2)+A13*EPSTH(3)
      SIG(2)=A12*EPSTH(1)+A22*EPSTH(2)+A23*EPSTH(3)
      SIG(3)=A13*EPSTH(1)+A23*EPSTH(2)+A33*EPSTH(3)
      SIG(4)=2.D0*G12*EPSTH(4)
      IF (NDIMSI.EQ.6) THEN
        SIG(5)=2.D0*G13*EPSTH(5)
        SIG(6)=2.D0*G23*EPSTH(6)
      ENDIF
C
C
C - CALCUL DE LA RIGIDITE TANGENTE
C
      IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG' .OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA' ) THEN
        DO 20 I=1,NDIMSI
          DO 30 J=1,NDIMSI
            DSIDEP(I,J) = 0.D0
 30       CONTINUE
 20     CONTINUE
C
        DSIDEP(1,1) = A11
        DSIDEP(2,2) = A22
        DSIDEP(3,3) = A33
        DSIDEP(1,2) = A12
        DSIDEP(2,1) = A12
        DSIDEP(1,3) = A13
        DSIDEP(3,1) = A13
        DSIDEP(2,3) = A23
        DSIDEP(3,2) = A23
        DSIDEP(4,4) = 2.D0*G12
        IF (NDIMSI.EQ.6) THEN
          DSIDEP(5,5) = 2.D0*G13
          DSIDEP(6,6) = 2.D0*G23
        ENDIF
C
C      CORRECTION POUR LES CONTRAINTES PLANES :
        IF (CPLAN) THEN
          DO 40 I=1,NDIMSI
            IF (I.EQ.3) GOTO 40
            DO 50 J=1,NDIMSI
              IF (J.EQ.3) GOTO 50
              DSIDEP(I,J)=DSIDEP(I,J)
     &                   - 1.D0/DSIDEP(3,3)*DSIDEP(I,3)*DSIDEP(3,J)
 50         CONTINUE
 40       CONTINUE
        ENDIF
      ENDIF
      END
