      SUBROUTINE DMATHL(ORDI,EPI,EPAIS,DF,DM,DMF,NNO,PGL,R,MULTIC,ICOU,
     +                  INDITH,GRILLE)
      IMPLICIT   NONE
      INTEGER NNO,MULTIC,INDITH,ICOU
      REAL*8 DF(3,3),DM(3,3),DMF(3,3)
      REAL*8 PGL(3,3),R(*)
      REAL*8 ORDI,EPI,EPAIS
      LOGICAL GRILLE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C     ------------------------------------------------------------------
C     CALCUL DES MATRICES DE COEFFCIENTS THERMOELASTIQUES DE FLEXION,
C     MEMBRANE, COUPLAGE MEMBRANE-FLEXION POUR UN MATERIAU ISOTROPE OU
C     MULTICOUCHE
C     IN  GRILLE : .TRUE. => ELEMENT DE GRILLE (MEGRDKT)
C     OUT MULTIC :
C        1 POUR UN MATERIAU MULTICOUCHE SANS COUPLAGE MEMBRANE-FLEXION
C        2 POUR UN MATERIAU MULTICOUCHE AVEC COUPLAGE MEMBRANE-FLEXION
C        0 DANS LES AUTRES CAS
C     LA VARIABLE INDITH EST INITIALISEE A 0
C     DANS LE CAS OU LE COEFFICIENT DE DILATATION ALPHA N'A
C     PAS ETE DONNE, INDITH VAUT -1 ET ON  NE CALCULE PAS LES
C     CONTRAINTES THERMIQUES
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER IBID,JCOQU,JMATE,ITEMP,INDEP,ITAB(8),IRET
      INTEGER LT1VE,LT2VE,LT2EV,NBV,I,J,K,NBPAR,IER
      REAL*8 CDF,CDM,CDC,GCIS,VALRES(134),VALPU(2)
      REAL*8 YOUNG,NU,VALPAR
      REAL*8 XAB1(3,3),XAB2(2,2),DH(3,3),ROT(3,3)
      REAL*8 DX,DY,DZ,S,C,NORM
      REAL*8 PS,PJDX,PJDY,PJDZ,T,TINF,TSUP,TPG1,ALPHAT
      REAL*8 ALPHA,BETA,R8DGRD,R8PREM
      REAL*8 ANGLL,PCL,PCT,YOUNG1,YOUNG2,EPTOT
      CHARACTER*2 BL2,CODRET(134),VAL
      CHARACTER*3 NUM
      CHARACTER*8 NOMRES(134),NOMPAR,NOMPU(2)
      CHARACTER*10 PHENOM
C     ------------------------------------------------------------------

      CALL R8INIR(9,0.D0,DM,1)
      CALL R8INIR(9,0.D0,DF,1)
      CALL R8INIR(9,0.D0,DH,1)
      CALL R8INIR(9,0.D0,DMF,1)

      CALL JEVECH('PCACOQU','L',JCOQU)
      EPAIS = ZR(JCOQU)
      EPI = EPAIS
      ORDI = 0.D0
      ALPHA = ZR(JCOQU+1)*R8DGRD()
      BETA = ZR(JCOQU+2)*R8DGRD()

      IF (GRILLE) THEN
C        --- ANGLE EN DEGRES: 1IERE DIRECTION D'ARMATURES
        ANGLL = ZR(JCOQU+3)*R8DGRD()
C        --- POURCENTAGE D'ARMATURES 1 / UNITE DE LARGEUR
        PCL = ZR(JCOQU+4)
C        --- POURCENTAGE D'ARMATURES 2 / UNITE DE LARGEUR
        PCT = ZR(JCOQU+5)
      END IF

      DX = COS(BETA)*COS(ALPHA)
      DY = COS(BETA)*SIN(ALPHA)
      DZ = SIN(BETA)
      NORM = SQRT(DX*DX+DY*DY+DZ*DZ)
      DX = DX/NORM
      DY = DY/NORM
      DZ = DZ/NORM
      PS = DX*PGL(3,1) + DY*PGL(3,2) + DZ*PGL(3,3)
      PJDX = DX - PS*PGL(3,1)
      PJDY = DY - PS*PGL(3,2)
      PJDZ = DZ - PS*PGL(3,3)
      NORM = SQRT(PJDX*PJDX+PJDY*PJDY+PJDZ*PJDZ)
C     ------------------------------------------------
      INDITH = 0
      CALL JEVECH('PMATERC','L',JMATE)
      BL2 = '  '
      CALL RCCOMA(ZI(JMATE),'ELAS',PHENOM,CODRET)

      IF (NNO.EQ.3) THEN
C        8 + 3 * NPG + 2 * NNO + 5 * NC ( NNO = NPG = NC = 3 )
        LT1VE = 38
      ELSE IF (NNO.EQ.4) THEN
C        8 + 3 * NPG + 2 * NNO + 9 * NC + 4 ( NNO = NPG = NC = 4 )
        LT1VE = 68
      END IF
      LT2VE = LT1VE + 9
      LT2EV = LT2VE + 4

      IF (PHENOM.EQ.'ELAS_COQMU') THEN
        IF (NORM.LE.R8PREM()) THEN
          CALL UTMESS('F','DXMATH','L''AXE DE REFERENCE EST NORMAL A'//
     +                ' UN ELEMENT DE PLAQUE ANISOTROPE')
        END IF
C          CALCUL DES MATRICE T1VE ET T2VE DE PASSAGE D'UNE MATRICE
C          (3,3) ET (2,2) DU REPERE DE LA VARIETE AU REPERE ELEMENT
C          ET T2VE INVERSE DE T2EV
        PJDX = PJDX/NORM
        PJDY = PJDY/NORM
        PJDZ = PJDZ/NORM
        C = PJDX*PGL(1,1) + PJDY*PGL(1,2) + PJDZ*PGL(1,3)
        S = PJDX*PGL(2,1) + PJDY*PGL(2,2) + PJDZ*PGL(2,3)
        R(LT2EV) = C
        R(LT2EV+1) = S
        R(LT2EV+2) = -S
        R(LT2EV+3) = C
        R(LT2VE) = C
        R(LT2VE+1) = -S
        R(LT2VE+2) = S
        R(LT2VE+3) = C
        R(LT1VE) = C*C
        R(LT1VE+3) = S*S
        R(LT1VE+6) = C*S
        R(LT1VE+1) = R(LT1VE+3)
        R(LT1VE+4) = R(LT1VE)
        R(LT1VE+7) = -R(LT1VE+6)
        R(LT1VE+2) = -R(LT1VE+6) - R(LT1VE+6)
        R(LT1VE+5) = R(LT1VE+6) + R(LT1VE+6)
        R(LT1VE+8) = R(LT1VE) - R(LT1VE+3)
        NBV = 56
        DO 10 I = 1,NBV
          CALL CODENT(I,'G',NUM)
          NOMRES(I) = 'HOM_'//NUM
   10   CONTINUE
        CALL CODENT(ICOU,'G',NUM)
        DO 20 I = 1,78
          CALL CODENT(I,'G',VAL)
          NOMRES(56+I) = 'C'//NUM//'_V'//VAL
   20   CONTINUE

      ELSE IF (PHENOM.EQ.'ELAS') THEN
        IF (NORM.LE.R8PREM()) THEN
          CALL UTMESS('A','DXMATH','L''AXE DE REFERENCE EST NORMAL A'//
     +            ' UN ELEMENT DE PLAQUE. VOUS NE POURREZ CALCULER LES '
     +                //' LES CONTRAINTES.')
        END IF
        NBV = 3
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'

      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN
        IF (NORM.LE.R8PREM()) THEN
          CALL UTMESS('A','DXMATH','L''AXE DE REFERENCE EST NORMAL A'//
     +            ' UN ELEMENT DE PLAQUE. VOUS NE POURREZ CALCULER LES '
     +                //' LES CONTRAINTES.')
        END IF

C        CALCUL DES MATRICE T1VE ET T2VE DE PASSAGE D'UNE MATRICE
C        (3,3) ET (2,2) DU REPERE DE LA VARIETE AU REPERE ELEMENT
C        ET T2VE INVERSE DE T2EV
        PJDX = PJDX/NORM
        PJDY = PJDY/NORM
        PJDZ = PJDZ/NORM
        C = PJDX*PGL(1,1) + PJDY*PGL(1,2) + PJDZ*PGL(1,3)
        S = PJDX*PGL(2,1) + PJDY*PGL(2,2) + PJDZ*PGL(2,3)
        R(LT2EV) = C
        R(LT2EV+1) = S
        R(LT2EV+2) = -S
        R(LT2EV+3) = C
        R(LT2VE) = C
        R(LT2VE+1) = -S
        R(LT2VE+2) = S
        R(LT2VE+3) = C
        R(LT1VE) = C*C
        R(LT1VE+3) = S*S
        R(LT1VE+6) = C*S
        R(LT1VE+1) = R(LT1VE+3)
        R(LT1VE+4) = R(LT1VE)
        R(LT1VE+7) = -R(LT1VE+6)
        R(LT1VE+2) = -R(LT1VE+6) - R(LT1VE+6)
        R(LT1VE+5) = R(LT1VE+6) + R(LT1VE+6)
        R(LT1VE+8) = R(LT1VE) - R(LT1VE+3)

        NBV = 10
        NOMRES(1) = 'MEMB_L  '
        NOMRES(2) = 'MEMB_LT '
        NOMRES(3) = 'MEMB_T  '
        NOMRES(4) = 'MEMB_G_L'
        NOMRES(5) = 'FLEX_L  '
        NOMRES(6) = 'FLEX_LT '
        NOMRES(7) = 'FLEX_T  '
        NOMRES(8) = 'FLEX_G_L'
        NOMRES(9) = 'CISA_L  '
        NOMRES(10) = 'CISA_T  '
        NOMRES(11) = 'ALPHA   '
      ELSE
        CALL UTMESS('F','DXMATH','COMPORTEMENT MATERIAU NON ADMIS')
      END IF

C===============================================================
C     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:

C     -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
      CALL TECACH ('ONN','PTEMPER',8,ITAB,IRET)
      ITEMP=ITAB(1)
      IF (ITEMP.NE.0) THEN
        NBPAR = 1
        NOMPAR = 'TEMP'
        TPG1 = 0.D0
        DO 30 I = 1,NNO
          CALL DXTPIF(ZR(ITEMP+3*(I-1)),ZL(ITAB(8)+3*(I-1)))
          T = ZR(ITEMP+3* (I-1))
          TINF = ZR(ITEMP+1+3* (I-1))
          TSUP = ZR(ITEMP+2+3* (I-1))
          TPG1 = TPG1 + T + (TSUP+TINF-2*T)/6.D0
   30   CONTINUE
        VALPAR = TPG1/NNO
      ELSE

C     -- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS':
      CALL TECACH('ONN','PTEMPEF',1,ITEMP,IRET)
        IF (ITEMP.GT.0) THEN
          NBPAR = 1
          NOMPAR = 'TEMP'
          NOMPU(1) = 'INST'
          NOMPU(2) = 'EPAIS'
          CALL JEVECH('PTEMPSR','L',IBID)
          VALPU(1) = ZR(IBID)
          VALPU(2) = 0.D0
          CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,VALPAR,IER)

C     -- SI LA TEMPERATURE N'EST PAS DONNEE:
        ELSE
          NBPAR = 0
          NOMPAR = ' '
          VALPAR = 0.D0
        END IF
      END IF
C===============================================================

      IF (PHENOM.EQ.'ELAS') THEN
C        ------ MATERIAU ISOTROPE ------------------------------------

        MULTIC = 0

        CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,2,NOMRES,
     +              VALRES,CODRET,'FM')
        CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,2,
     +              NOMRES(3),VALRES(3),CODRET(3),BL2)
        IF (CODRET(3).NE.'OK') THEN
          INDITH = -1
          GO TO 70
        END IF
        YOUNG = VALRES(1)
        NU = VALRES(2)
        ALPHAT = VALRES(3)
        YOUNG = YOUNG*ALPHAT
C        ------------------------------------------------------------
        IF (GRILLE) THEN
C        ---- CALCUL DE LA MATRICE DE RIGIDITE ORTHOTROPE - GRILLES -
          YOUNG1 = YOUNG
          YOUNG2 = YOUNG*PCT/PCL
          DH(1,1) = YOUNG1
          DH(2,2) = YOUNG2
          DH(3,3) = YOUNG1*1.D-7

C   MATRICE PASSAGE DU REPERE D'ORTHOTROPIE VERS LE REPERE DE L'ELEMENT

          CALL GRIROT(ALPHA,BETA,ANGLL,PGL,ROT,C,S)

C   PASSAGE DU REPERE D'ORTHOTROPIE VERS LE REPERE DE L'ELEMENT

          CALL UTBTAB('ZERO',3,3,DH,ROT,XAB1,DH)

C        --- CALCUL DES MATRICES DE RIGIDITE EN MEMBRANE ET FLEXION --

          CDF = EPAIS*EPAIS*EPAIS/12.D0
          DO 50 J = 1,3
            DO 40 I = 1,3
              DM(I,J) = DH(I,J)*EPAIS
              DF(I,J) = DH(I,J)*CDF
   40       CONTINUE
   50     CONTINUE
        ELSE

C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
          CDF = YOUNG*EPAIS*EPAIS*EPAIS/12.D0/ (1.D0-NU*NU)
          DF(1,1) = CDF
          DF(1,2) = CDF*NU
          DF(2,1) = DF(1,2)
          DF(2,2) = DF(1,1)
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
          CDM = EPAIS*YOUNG/ (1.D0-NU*NU)
          DM(1,1) = CDM
          DM(1,2) = CDM*NU
          DM(2,1) = DM(1,2)
          DM(2,2) = DM(1,1)

        END IF
C        ---------------------------------------------------------------
      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN
        MULTIC = 0
        CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,NBV,NOMRES,
     +              VALRES,CODRET,'FM')
        CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,1,
     +            NOMRES(11),  VALRES(11),CODRET(11),BL2)
        IF (CODRET(11).NE.'OK') THEN
          INDITH = -1
          GO TO 70
        END IF
        ALPHAT = VALRES(11)
        IF (GRILLE) CALL UTMESS('F','DXMATH','STOP 1')

C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        DM(1,1) = VALRES(1)*ALPHAT
        DM(1,2) = VALRES(2)*ALPHAT
        DM(2,1) = DM(1,2)
        DM(2,2) = VALRES(3)*ALPHAT
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        DF(1,1) = VALRES(5)*ALPHAT
        DF(1,2) = VALRES(6)*ALPHAT
        DF(2,1) = DF(1,2)
        DF(2,2) = VALRES(7)*ALPHAT
C        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
        CALL UTBTAB('ZERO',3,3,DM,R(LT1VE),XAB1,DM)
        CALL UTBTAB('ZERO',3,3,DF,R(LT1VE),XAB1,DF)
        CALL UTBTAB('ZERO',3,3,DMF,R(LT1VE),XAB1,DMF)

      ELSE IF (PHENOM.EQ.'ELAS_COQMU') THEN
C        ------ MATERIAU MULTICOUCHE -----------------------------------
        CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,1,
     +             NOMRES(19), VALRES(19),CODRET(19),'FM')
        EPAIS = VALRES(19)
        CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,1,
     +             NOMRES(57), VALRES(57),CODRET(57),'FM')
        EPI = VALRES(57)
        CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,1,
     +              NOMRES(59),VALRES(59),CODRET(59),'FM')
        ORDI = VALRES(59)
        CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,27,
     +             NOMRES(102), VALRES(102),CODRET(102),'FM')
        DM(1,1) = VALRES(102)
        DM(1,2) = VALRES(103)
        DM(1,3) = VALRES(104)
        DM(2,1) = VALRES(105)
        DM(2,2) = VALRES(106)
        DM(2,3) = VALRES(107)
        DM(3,1) = VALRES(108)
        DM(3,2) = VALRES(109)
        DM(3,3) = VALRES(110)
        DMF(1,1) = VALRES(111)
        DMF(1,2) = VALRES(112)
        DMF(1,3) = VALRES(113)
        DMF(2,1) = VALRES(114)
        DMF(2,2) = VALRES(115)
        DMF(2,3) = VALRES(116)
        DMF(3,1) = VALRES(117)
        DMF(3,2) = VALRES(118)
        DMF(3,3) = VALRES(119)
        DF(1,1) = VALRES(120)
        DF(1,2) = VALRES(121)
        DF(1,3) = VALRES(122)
        DF(2,1) = VALRES(123)
        DF(2,2) = VALRES(124)
        DF(2,3) = VALRES(125)
        DF(3,1) = VALRES(126)
        DF(3,2) = VALRES(127)
        DF(3,3) = VALRES(128)

C        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --

        CALL UTBTAB('ZERO',3,3,DM,R(LT1VE),XAB1,DM)
        CALL UTBTAB('ZERO',3,3,DF,R(LT1VE),XAB1,DF)
        CALL UTBTAB('ZERO',3,3,DMF,R(LT1VE),XAB1,DMF)

        MULTIC = 1
        DO 60 K = 1,9
          IF (ABS(DMF(K,1)).GT.1.D-10) MULTIC = 2
   60   CONTINUE

      END IF
   70 CONTINUE
      END
