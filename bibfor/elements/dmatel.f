      SUBROUTINE DMATEL(DF,DM,DMF,DC,DCI,NNO,PGL,R,MULTIC,ICOU,GRILLE)
      IMPLICIT   NONE
C TOLE CRP_20
      INTEGER NNO,MULTIC,ICOU
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2)
      REAL*8 PGL(3,3),R(*)
      LOGICAL GRILLE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/01/2002   AUTEUR VABHHTS J.TESELET 
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
C     CALCUL DES MATRICES DE RIGIDITE DE FLEXION, MEMBRANE , COUPLAGE
C     MEMBRANE-FLEXION ET CISAILLEMENT POUR UN MATERIAU ISOTROPE OU
C     MULTICOUCHE
C     IN  GRILLE : .TRUE. => ELEMENT DE GRILLE (MEGRDKT)
C     OUT MULTIC :
C        1 POUR UN MATERIAU MULTICOUCHE SANS COUPLAGE MEMBRANE-FLEXION
C        2 POUR UN MATERIAU MULTICOUCHE AVEC COUPLAGE MEMBRANE-FLEXION
C        0 DANS LES AUTRES CAS
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
      INTEGER IBID,JCOQU,JMATE,ITEMP,INDEP,ELASCO,IADZI,IAZK24
      INTEGER LT1VE,LT2VE,LT2EV,NBV,I,J,K,NBPAR,IER,ITAB(8),IRET
      REAL*8 DMC(3,2),DFC(3,2),EXCENT
      REAL*8 KCIS,CDF,CDM,CDC,GCIS,VALRES(134),VALPU(2)
      REAL*8 YOUNG,NU,EPAIS,VALPAR
      REAL*8 XAB1(3,3),XAB2(2,2),XAB3(3,2),DH(3,3),ROT(3,3)
      REAL*8 DX,DY,DZ,S,C,NORM
      REAL*8 PS,PJDX,PJDY,PJDZ,T,TINF,TSUP,TPG1
      REAL*8 ALPHA,BETA,R8DGRD,R8PREM,ZERO,DET
      REAL*8 ANGLL,PCL,PCT,YOUNG1,YOUNG2
      CHARACTER*2 CODRET(134),VAL
      CHARACTER*3 NUM
      CHARACTER*8 NOMRES(134),NOMPAR,NOMPU(2),MOTCLE,NOMAIL
      CHARACTER*10 PHENOM
C     ------------------------------------------------------------------
C
      ZERO   = 0.0D0
      ELASCO = 0
      CALL R8INIR(6,ZERO,DMC,1)
      CALL R8INIR(6,ZERO,DFC,1)
C
      CALL JEVECH('PCACOQU','L',JCOQU)
      EPAIS = ZR(JCOQU)
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
      CALL JEVECH('PMATERC','L',JMATE)
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
          CALL UTMESS('F','DMATEL','L''AXE DE REFERENCE EST NORMAL A'//
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
          CALL UTMESS('A','DMATEL','L''AXE DE REFERENCE EST NORMAL A'//
     +            ' UN ELEMENT DE PLAQUE. VOUS NE POURREZ CALCULER LES '
     +                //' LES CONTRAINTES.')
        END IF
        IF (ICOU.GT.1) THEN
           CALL TECAEL(IADZI,IAZK24)
           NOMAIL = ZK24(IAZK24-1+3)(1:8)
           CALL UTDEBM ('F','DMATEL','NUMERO DE')
           CALL UTIMPI ( 'S', ' COUCHE ', 1, ICOU )
           CALL UTIMPK ( 'S', ' TROP GRAND PAR RAPPORT AU NOMBRE '//
     +               'DE COUCHES AUTORISE POUR LA MAILLE ', 1, NOMAIL )
           CALL UTFINM ()
        ENDIF
        NBV = 2
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'

      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN
        IF (NORM.LE.R8PREM()) THEN
          CALL UTMESS('A','DMATEL','L''AXE DE REFERENCE EST NORMAL A'//
     +            ' UN ELEMENT DE PLAQUE. VOUS NE POURREZ CALCULER LES '
     +                //' LES CONTRAINTES.')
        END IF
        IF (ICOU.GT.1) THEN
           CALL TECAEL(IADZI,IAZK24)
           NOMAIL = ZK24(IAZK24-1+3)(1:8)
           CALL UTDEBM ('F','DMATEL','NUMERO DE')
           CALL UTIMPI ( 'S', ' COUCHE ', 1, ICOU )
           CALL UTIMPK ( 'S', ' TROP GRAND PAR RAPPORT AU NOMBRE '//
     +               'DE COUCHES AUTORISE POUR LA MAILLE ', 1, NOMAIL )
           CALL UTFINM ()
        ENDIF

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
C
        CALL RCVALA(ZI(JMATE),PHENOM,0,' ',ZERO,1,'MEMB_L  ',
     +              VALRES(1),CODRET,' ')
        IF (CODRET(1).EQ.'NO') THEN
          CALL RCVALA(ZI(JMATE),PHENOM,0,' ',ZERO,1,'M_LLLL  ',
     +                VALRES(1),CODRET,' ')
          IF (CODRET(1).EQ.'NO') THEN
            CALL UTMESS('F','DMATEL','IMPOSSIBILITE : VOUS AVEZ UN '//
     +      'MATERIAU DE TYPE "ELAS_COQUE" ET VOUS N''AVEZ PAS '//
     +      'DEFINI LA RAIDEUR DE MEMBRANE, NI SOUS LA FORME '//
     +      '"MEMB_L", NI SOUS LA FORME "M_LLLL".')
          ELSE
            ELASCO = 2
          ENDIF
        ELSE
          ELASCO = 1
        ENDIF
        IF (ELASCO.EQ.1) THEN
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
        ELSEIF (ELASCO.EQ.2) THEN
          NBV        = 33
          MULTIC     =  2
          NOMRES(1)  = 'M_LLLL  '
          NOMRES(2)  = 'M_LLTT  '
          NOMRES(3)  = 'M_LLLT  '
          NOMRES(4)  = 'M_TTTT  '
          NOMRES(5)  = 'M_TTLT  '
          NOMRES(6)  = 'M_LTLT  '
          NOMRES(7)  = 'F_LLLL  '
          NOMRES(8)  = 'F_LLTT  '
          NOMRES(9)  = 'F_LLLT  '
          NOMRES(10) = 'F_TTTT  '
          NOMRES(11) = 'F_TTLT  '
          NOMRES(12) = 'F_LTLT  '
          NOMRES(13) = 'MF_LLLL '
          NOMRES(14) = 'MF_LLTT '
          NOMRES(15) = 'MF_LLLT '
          NOMRES(16) = 'MF_TTTT '
          NOMRES(17) = 'MF_TTLT '
          NOMRES(18) = 'MF_LTLT '
          NOMRES(19) = 'MC_LLLZ '
          NOMRES(20) = 'MC_LLTZ '
          NOMRES(21) = 'MC_TTLZ '
          NOMRES(22) = 'MC_TTTZ '
          NOMRES(23) = 'MC_LTLZ '
          NOMRES(24) = 'MC_LTTZ '
          NOMRES(25) = 'FC_LLLZ '
          NOMRES(26) = 'FC_LLTZ '
          NOMRES(27) = 'FC_TTLZ '
          NOMRES(28) = 'FC_TTTZ '
          NOMRES(29) = 'FC_LTLZ '
          NOMRES(30) = 'FC_LTTZ '
          NOMRES(31) = 'C_LZLZ  '
          NOMRES(32) = 'C_LZTZ  '
          NOMRES(33) = 'C_TZTZ  '
        ENDIF
      ELSE
        CALL UTMESS('F','DMATEL','COMPORTEMENT MATERIAU NON ADMIS')
      END IF

C===============================================================
C     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:

C     -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
      CALL TECAC2 ('ONN','PTEMPER',8,ITAB,IRET)
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
      CALL TECACH(.TRUE.,.FALSE.,'PTEMPEF',1,ITEMP)
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
        CALL RCVALA(ZI(JMATE),PHENOM,NBPAR,NOMPAR,VALPAR,NBV,NOMRES,
     +              VALRES,CODRET,'FM')

        MULTIC = 0

C        ------ MATERIAU ISOTROPE --------------------------------------
        YOUNG = VALRES(1)
        NU = VALRES(2)

        IF (GRILLE) THEN
C        ---- CALCUL DE LA MATRICE DE RIGIDITE ORTHOTROPE ------------
          YOUNG1 = YOUNG
          YOUNG2 = YOUNG*PCT/PCL
          CALL R8INIR(9,0.D0,DH,1)
          CALL R8INIR(9,0.D0,DMF,1)
          CALL R8INIR(4,0.D0,DC,1)
          CALL R8INIR(4,0.D0,DCI,1)
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

          KCIS = 5.D0/6.D0
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
          CDF = YOUNG*EPAIS*EPAIS*EPAIS/12.D0/ (1.D0-NU*NU)
          DO 60 K = 1,9
            DF(K,1) = 0.D0
            DMF(K,1) = 0.D0
   60     CONTINUE
          DF(1,1) = CDF
          DF(1,2) = CDF*NU
          DF(2,1) = DF(1,2)
          DF(2,2) = DF(1,1)
          DF(3,3) = CDF* (1.D0-NU)/2.D0
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
          CDM = EPAIS*YOUNG/ (1.D0-NU*NU)
          DO 70 K = 1,9
            DM(K,1) = 0.D0
   70     CONTINUE
          DM(1,1) = CDM
          DM(1,2) = CDM*NU
          DM(2,1) = DM(1,2)
          DM(2,2) = DM(1,1)
          DM(3,3) = CDM* (1.D0-NU)/2.D0
C        --- CALCUL DE LA MATRICE DE RIGIDITE EN CISAILLEMENT ----------
          GCIS = YOUNG/2.D0/ (1.D0+NU)
          CDC = GCIS*KCIS*EPAIS
          DC(1,1) = CDC
          DC(2,2) = DC(1,1)
          DC(1,2) = 0.D0
          DC(2,1) = 0.D0
C        --- CALCUL DE SON INVERSE ------------------------------------
          DCI(1,1) = 1/CDC
          DCI(2,2) = DCI(1,1)
          DCI(1,2) = 0.D0
          DCI(2,1) = 0.D0

        END IF

      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN
        CALL RCVALA(ZI(JMATE),PHENOM,NBPAR,NOMPAR,VALPAR,NBV,NOMRES,
     +              VALRES,CODRET,'FM')

        IF (ELASCO.EQ.1) THEN
          MULTIC = 0
          IF (GRILLE) CALL UTMESS('F','DMATEL','STOP 1')

C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
          DM(1,1) = VALRES(1)
          DM(1,2) = VALRES(2)
          DM(1,3) = 0.D0
          DM(2,1) = DM(1,2)
          DM(2,2) = VALRES(3)
          DM(2,3) = 0.D0
          DM(3,1) = 0.D0
          DM(3,2) = 0.D0
          DM(3,3) = VALRES(4)
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
          DF(1,1) = VALRES(5)
          DF(1,2) = VALRES(6)
          DF(1,3) = 0.D0
          DF(2,1) = DF(1,2)
          DF(2,2) = VALRES(7)
          DF(2,3) = 0.D0
          DF(3,1) = 0.D0
          DF(3,2) = 0.D0
          DF(3,3) = VALRES(8)
C        --- COUPLAGE  MEMBRANE FLEXION --------------------------------
          DMF(1,1) = 0.D0
          DMF(1,2) = 0.D0
          DMF(1,3) = 0.D0
          DMF(2,1) = 0.D0
          DMF(2,2) = 0.D0
          DMF(2,3) = 0.D0
          DMF(3,1) = 0.D0
          DMF(3,2) = 0.D0
          DMF(3,3) = 0.D0
C        --- CALCUL DE LA MATRICE DE RIGIDITE EN CISAILLEMENT ----------
          DC(1,1) = VALRES(9)
          DC(1,2) = 0.D0
          DC(2,1) = 0.D0
          DC(2,2) = VALRES(10)
C        --- CALCUL DE SON INVERSE -------------------------------------
          DCI(1,1) = 1/VALRES(9)
          DCI(1,2) = 0.D0
          DCI(2,1) = 0.D0
          DCI(2,2) = 1/VALRES(10)
C
        ELSEIF (ELASCO.EQ.2) THEN
          MULTIC = 2
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
          DM(1,1) = VALRES(1)
          DM(1,2) = VALRES(2)
          DM(1,3) = VALRES(3)
          DM(2,1) = DM(1,2)
          DM(2,2) = VALRES(4)
          DM(2,3) = VALRES(5)
          DM(3,1) = DM(1,3)
          DM(3,2) = DM(2,3)
          DM(3,3) = VALRES(6)
C        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
          DF(1,1) = VALRES(7)
          DF(1,2) = VALRES(8)
          DF(1,3) = VALRES(9)
          DF(2,1) = DF(1,2)
          DF(2,2) = VALRES(10)
          DF(2,3) = VALRES(11)
          DF(3,1) = DF(1,3)
          DF(3,2) = DF(2,3)
          DF(3,3) = VALRES(12)
C        --- COUPLAGE  MEMBRANE FLEXION --------------------------------
          DMF(1,1) = VALRES(13)
          DMF(1,2) = VALRES(14)
          DMF(1,3) = VALRES(15)
          DMF(2,1) = DMF(1,2)
          DMF(2,2) = VALRES(16)
          DMF(2,3) = VALRES(17)
          DMF(3,1) = DMF(1,3)
          DMF(3,2) = DMF(2,3)
          DMF(3,3) = VALRES(18)
C        --- COUPLAGE  MEMBRANE CISAILLEMENT ---------------------------
          DMC(1,1) = VALRES(19)
          DMC(1,2) = VALRES(20)
          DMC(2,1) = VALRES(21)
          DMC(2,2) = VALRES(22)
          DMC(3,1) = VALRES(23)
          DMC(3,2) = VALRES(24)
C        --- COUPLAGE  FLEXION CISAILLEMENT ---------------------------
          DFC(1,1) = VALRES(25)
          DFC(1,2) = VALRES(26)
          DFC(2,1) = VALRES(27)
          DFC(2,2) = VALRES(28)
          DFC(3,1) = VALRES(29)
          DFC(3,2) = VALRES(30)
C        --- CALCUL DE LA MATRICE DE RIGIDITE EN CISAILLEMENT ----------
          DC(1,1) = VALRES(31)
          DC(1,2) = VALRES(32)
          DC(2,1) = DC(1,2)
          DC(2,2) = VALRES(33)
C        --- CALCUL DE SON INVERSE -------------------------------------
          DET = DC(1,1)*DC(2,2) - DC(1,2)*DC(2,1)
          IF (DET.GT.R8PREM()) THEN
            DCI(1,1) =  DC(2,2)/DET
            DCI(1,2) = -DC(1,2)/DET
            DCI(2,1) = -DC(2,1)/DET
            DCI(2,2) =  DC(1,1)/DET
          ELSE
            CALL UTMESS('F','DMATEL','IMPOSSIBILITE : VOUS AVEZ UN '//
     +      'MATERIAU DE TYPE "ELAS_COQUE" ET LE DETERMINANT '//
     +      'DE LA SOUS-MATRICE DE HOOKE RELATIVE AU CISAILLEMENT '//
     +      'EST NUL.')
          ENDIF
        ENDIF
C
C        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
        CALL UTBTAB('ZERO',3,3,DM,R(LT1VE),XAB1,DM)
        CALL UTBTAB('ZERO',3,3,DF,R(LT1VE),XAB1,DF)
        CALL UTBTAB('ZERO',3,3,DMF,R(LT1VE),XAB1,DMF)
        CALL UTBTAB('ZERO',2,2,DC,R(LT2VE),XAB2,DC)
        CALL UTBTAB('ZERO',2,2,DCI,R(LT2VE),XAB2,DCI)
        IF (ELASCO.EQ.2) THEN
          CALL UTDTAB('ZERO',3,2,2,3,DMC,R(LT2VE),R(LT1VE),XAB3,DMC)
          CALL UTDTAB('ZERO',3,2,2,3,DFC,R(LT2VE),R(LT1VE),XAB3,DFC)
        ENDIF
      ELSE IF (PHENOM.EQ.'ELAS_COQMU') THEN
C        ------ MATERIAU MULTICOUCHE -----------------------------------
        CALL RCVALA(ZI(JMATE),PHENOM,NBPAR,NOMPAR,VALPAR,18,NOMRES(1),
     +              VALRES(1),CODRET(1),'  ')
        DM(1,1) = VALRES(1)
        DM(1,2) = VALRES(2)
        DM(1,3) = VALRES(3)
        DM(2,2) = VALRES(4)
        DM(2,3) = VALRES(5)
        DM(3,3) = VALRES(6)
        DM(2,1) = DM(1,2)
        DM(3,1) = DM(1,3)
        DM(3,2) = DM(2,3)
        DMF(1,1) = VALRES(7)
        DMF(1,2) = VALRES(8)
        DMF(1,3) = VALRES(9)
        DMF(2,2) = VALRES(10)
        DMF(2,3) = VALRES(11)
        DMF(3,3) = VALRES(12)
        DMF(2,1) = DMF(1,2)
        DMF(3,1) = DMF(1,3)
        DMF(3,2) = DMF(2,3)
        DF(1,1) = VALRES(13)
        DF(1,2) = VALRES(14)
        DF(1,3) = VALRES(15)
        DF(2,2) = VALRES(16)
        DF(2,3) = VALRES(17)
        DF(3,3) = VALRES(18)
        DF(2,1) = DF(1,2)
        DF(3,1) = DF(1,3)
        DF(3,2) = DF(2,3)
        CALL RCVALA(ZI(JMATE),PHENOM,NBPAR,NOMPAR,VALPAR,6,NOMRES(21),
     +              VALRES(21),CODRET(21),'FM')
        DCI(1,1) = VALRES(21)
        DCI(2,2) = VALRES(22)
        DCI(1,2) = VALRES(23)
        DCI(2,1) = DCI(1,2)
        DC(1,1) = VALRES(24)
        DC(2,2) = VALRES(25)
        DC(1,2) = VALRES(26)
        DC(2,1) = DC(1,2)
C
C        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
        CALL UTBTAB('ZERO',3,3,DM,R(LT1VE),XAB1,DM)
        CALL UTBTAB('ZERO',3,3,DF,R(LT1VE),XAB1,DF)
        CALL UTBTAB('ZERO',3,3,DMF,R(LT1VE),XAB1,DMF)
        CALL UTBTAB('ZERO',2,2,DC,R(LT2VE),XAB2,DC)
        CALL UTBTAB('ZERO',2,2,DCI,R(LT2VE),XAB2,DCI)

        MULTIC = 1
        DO 80 K = 1,9
          IF (ABS(DMF(K,1)).GT.1.D-10) MULTIC = 2
   80   CONTINUE

      END IF
      END
