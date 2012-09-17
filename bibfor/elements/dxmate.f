      SUBROUTINE DXMATE(FAMI,DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     &                  COUPMF,T2EV,T2VE,T1VE)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER NNO,MULTIC
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 PGL(3,3),T2EV(4),T2VE(4),T1VE(9)
      LOGICAL COUPMF
      CHARACTER*4 FAMI
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/09/2012   AUTEUR FLEJOU J-L.FLEJOU 
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
C TOLE CRP_20
C     ------------------------------------------------------------------
C     CALCUL DES MATRICES DE RIGIDITE DE FLEXION, MEMBRANE , COUPLAGE
C     MEMBRANE-FLEXION ET CISAILLEMENT POUR UN MATERIAU ISOTROPE OU
C     MULTICOUCHE
C     OUT MULTIC :
C        1 POUR UN MATERIAU MULTICOUCHE SANS COUPLAGE MEMBRANE-FLEXION
C        0 DANS LES AUTRES CAS
C     OUT COUPMF :
C        .TRUE. POUR UN MATERIAU AVEC COUPLAGE MEMBRANE-FLEXION
C  ------------------------------------------------------------------
      INTEGER      JCOQU,JMATE,NBV,I,J,K,NBPAR,ELASCO
      INTEGER      IAZI,IAZK24,NPG,JCOU,NCOU,IRET,NPGH,IRET1
      INTEGER      NDIM,NNOS,IPOIDS,IVF,IDFDE,JGANO
      REAL*8       KCIS,CDF,CDM,CDC,GCIS,VALRES(33)
      REAL*8       YOUNG,NU,EPAIS,VALPAR,EXCENT
      REAL*8       XAB1(3,3),XAB2(2,2),XAB3(3,2)
      REAL*8       DX,DY,DZ,S,C,NORM,PS,PJDX,PJDY,PJDZ
      REAL*8       ALPHA,BETA,R8DGRD,R8PREM,DET
      REAL*8       ZERO,DEUX
      INTEGER  ICODRE(33)
      CHARACTER*3  NUM
      CHARACTER*8  NOMRES(33),NOMPAR
      CHARACTER*10 PHENOM
      CHARACTER*16 NOMTE

C     ------------------------------------------------------------------

      ZERO   = 0.0D0
      DEUX   = 2.0D0
      ELASCO = 0
      COUPMF = .FALSE.
      CALL R8INIR(9,ZERO,DMF,1)
      CALL R8INIR(6,ZERO,DMC,1)
      CALL R8INIR(6,ZERO,DFC,1)
C
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,
     &                  IDFDE,JGANO)
      CALL TECAEL(IAZI,IAZK24)
      NOMTE  = ZK24(IAZK24-1+3+NNO+1)(1:16)
C
      CALL JEVECH('PCACOQU','L',JCOQU)
      EPAIS = ZR(JCOQU)
      ALPHA = ZR(JCOQU+1)*R8DGRD()
      BETA  = ZR(JCOQU+2)*R8DGRD()
      EXCENT= ZR(JCOQU+4)
      CALL TECACH('NNN','PNBSP_I',1,JCOU,IRET)
      IF (IRET.EQ.0 ) THEN
        NCOU=ZI(JCOU)
        NPGH=3
      ELSE
        NPGH=1
        NCOU=1
      ENDIF
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
      CALL TECACH('NNN','PMATERC',1,JMATE,IRET)
      IF (IRET.NE.0) THEN
        MULTIC = 0
        GOTO 999
      ENDIF
      CALL RCCOMA(ZI(JMATE),'ELAS',PHENOM,ICODRE)

      IF (PHENOM.EQ.'ELAS_COQMU') THEN
        IF (NORM.LE.R8PREM()) THEN
          CALL U2MESS('F','ELEMENTS_39')
        END IF
C          CALCUL DES MATRICE T1VE ET T2VE DE PASSAGE D'UNE MATRICE
C          (3,3) ET (2,2) DU REPERE DE LA VARIETE AU REPERE ELEMENT
C          ET T2VE INVERSE DE T2EV
        PJDX = PJDX/NORM
        PJDY = PJDY/NORM
        PJDZ = PJDZ/NORM
        C = PJDX*PGL(1,1) + PJDY*PGL(1,2) + PJDZ*PGL(1,3)
        S = PJDX*PGL(2,1) + PJDY*PGL(2,2) + PJDZ*PGL(2,3)
        T2EV(1) = C
        T2EV(2) = S
        T2EV(3) = -S
        T2EV(4) = C
        T2VE(1) = C
        T2VE(2) = -S
        T2VE(3) = S
        T2VE(4) = C
        T1VE(1) = C*C
        T1VE(4) = S*S
        T1VE(7) = C*S
        T1VE(2) =  T1VE(4)
        T1VE(5) =  T1VE(1)
        T1VE(8) = -T1VE(7)
        T1VE(3) = -T1VE(7) - T1VE(7)
        T1VE(6) =  T1VE(7) + T1VE(7)
        T1VE(9) =  T1VE(1) - T1VE(4)
        NBV = 26
        DO 10 I = 1,NBV
          CALL CODENT(I,'G',NUM)
          NOMRES(I) = 'HOM_'//NUM
   10   CONTINUE

      ELSE IF (PHENOM.EQ.'ELAS') THEN
        IF (NORM.LE.R8PREM()) THEN
          CALL U2MESS('A','ELEMENTS_40')
        END IF
        NBV = 2
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN
        IF (NORM.LE.R8PREM()) THEN
          CALL U2MESS('A','ELEMENTS_40')
        END IF

C        CALCUL DES MATRICE T1VE ET T2VE DE PASSAGE D'UNE MATRICE
C        (3,3) ET (2,2) DU REPERE DE LA VARIETE AU REPERE ELEMENT
C        ET T2VE INVERSE DE T2EV
        PJDX = PJDX/NORM
        PJDY = PJDY/NORM
        PJDZ = PJDZ/NORM
        C = PJDX*PGL(1,1) + PJDY*PGL(1,2) + PJDZ*PGL(1,3)
        S = PJDX*PGL(2,1) + PJDY*PGL(2,2) + PJDZ*PGL(2,3)
        T2EV(1) = C
        T2EV(2) = S
        T2EV(3) = -S
        T2EV(4) = C
        T2VE(1) = C
        T2VE(2) = -S
        T2VE(3) = S
        T2VE(4) = C
        T1VE(1) = C*C
        T1VE(4) = S*S
        T1VE(7) = C*S
        T1VE(2) =  T1VE(4)
        T1VE(5) =  T1VE(1)
        T1VE(8) = -T1VE(7)
        T1VE(3) = -T1VE(7) - T1VE(7)
        T1VE(6) =  T1VE(7) + T1VE(7)
        T1VE(9) =  T1VE(1) - T1VE(4)

        CALL RCVALA(ZI(JMATE),' ',PHENOM,0,' ',ZERO,1,'MEMB_L  ',
     &              VALRES(1),ICODRE,0)
        IF (ICODRE(1).EQ.1) THEN
          CALL RCVALA(ZI(JMATE),' ',PHENOM,0,' ',ZERO,1,'M_LLLL  ',
     &                VALRES(1),ICODRE,0)
          IF (ICODRE(1).EQ.1) THEN
            CALL U2MESS('F','ELEMENTS_41')
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
          COUPMF     =  .TRUE.
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
        CALL U2MESK('F','ELEMENTS_42',1,PHENOM)
      END IF
C
      CALL MOYTEM(FAMI,NPG,NPGH*NCOU,'+',VALPAR,IRET1)
      NBPAR = 1
      NOMPAR = 'TEMP'

C
      IF (PHENOM.EQ.'ELAS') THEN

C        ------ MATERIAU ISOTROPE --------------------------------------

        CALL RCVALB(FAMI,1,1,'+',ZI(JMATE),' ',
     &             PHENOM,NBPAR,NOMPAR,VALPAR,NBV,
     &             NOMRES, VALRES,ICODRE,1)

        YOUNG = VALRES(1)
        NU = VALRES(2)

        MULTIC = 0
        KCIS = 5.D0/6.D0

C      ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        CDF = YOUNG*EPAIS*EPAIS*EPAIS/12.D0/ (1.D0-NU*NU)
        DO 50 K = 1,9
           DF(K,1) = 0.D0
          DMF(K,1) = 0.D0
   50   CONTINUE
        DF(1,1) = CDF
        DF(1,2) = CDF*NU
        DF(2,1) = DF(1,2)
        DF(2,2) = DF(1,1)
        DF(3,3) = CDF* (1.D0-NU)/2.D0
C      ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        CDM = EPAIS*YOUNG/ (1.D0-NU*NU)
        DO 60 K = 1,9
          DM(K,1) = 0.D0
   60   CONTINUE
        DM(1,1) = CDM
        DM(1,2) = CDM*NU
        DM(2,1) = DM(1,2)
        DM(2,2) = DM(1,1)
        DM(3,3) = CDM* (1.D0-NU)/2.D0
C      --- CALCUL DE LA MATRICE DE RIGIDITE EN CISAILLEMENT ----------
        GCIS = YOUNG/2.D0/ (1.D0+NU)
        CDC = GCIS*KCIS*EPAIS
        DC(1,1) = CDC
        DC(2,2) = DC(1,1)
        DC(1,2) = 0.D0
        DC(2,1) = 0.D0
C      --- CALCUL DE SON INVERSE ------------------------------------
        DCI(1,1) = 1.D0/DC(1,1)
        DCI(2,2) = DCI(1,1)
        DCI(1,2) = 0.D0
        DCI(2,1) = 0.D0
C      --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
C      --- ET REACTUALISATION DE LA MATRICE DE FLEXION       --------
C      --- DANS LE CAS D'UN EXCENTREMENT                     --------
        DO 70 I = 1, 3
        DO 70 J = 1, 3
          DMF(I,J) =                  EXCENT*DM(I,J)
          DF (I,J) = DF(I,J) + EXCENT*EXCENT*DM(I,J)
  70    CONTINUE


      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN
        CALL RCVALB(FAMI,1,1,'+',ZI(JMATE),' ',
     &              PHENOM,NBPAR,NOMPAR,VALPAR,NBV,
     &              NOMRES,VALRES,ICODRE,1)
        IF (ELASCO.EQ.1) THEN
          MULTIC = 0

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
          MULTIC = 0
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
            CALL U2MESS('F','ELEMENTS_43')
          ENDIF
        ENDIF
C        --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
C        --- REACTUALISATION DE LA MATRICE DE FLEXION ET DE LA --------
C        --- MATRICE DE COUPLAGE FLEXION-CISAILLEMENT          --------
C        --- DANS LE CAS D'UN EXCENTREMENT                     --------
          DO 80 I = 1, 3
          DO 80 J = 1, 3
            DF(I,J) = DF(I,J)+DEUX*EXCENT*DMF(I,J)+EXCENT*EXCENT*DM(I,J)
            DMF(I,J)= DMF(I,J)                    +       EXCENT*DM(I,J)
  80      CONTINUE
C
          DO 90 I = 1, 3
          DO 90 J = 1, 2
            DFC(I,J) = DFC(I,J) + EXCENT*DMC(I,J)
  90      CONTINUE
C
        IF(NOMTE.NE.'MEDKQG4'.AND.NOMTE.NE.'MEDKTG3')THEN
C        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
          CALL UTBTAB('ZERO',3,3, DM,T1VE,XAB1, DM)
          CALL UTBTAB('ZERO',3,3, DF,T1VE,XAB1, DF)
          CALL UTBTAB('ZERO',3,3,DMF,T1VE,XAB1,DMF)
          CALL UTBTAB('ZERO',2,2 ,DC,T2VE,XAB2, DC)
          CALL UTBTAB('ZERO',2,2,DCI,T2VE,XAB2,DCI)
          IF (ELASCO.EQ.2) THEN
            CALL UTDTAB('ZERO',3,2,2,3,DMC,T2VE,T1VE,XAB3,DMC)
            CALL UTDTAB('ZERO',3,2,2,3,DFC,T2VE,T1VE,XAB3,DFC)
          ENDIF
        ENDIF
C
      ELSE IF (PHENOM.EQ.'ELAS_COQMU') THEN
C        ------ MATERIAU MULTICOUCHE -----------------------------------
        CALL RCVALB(FAMI,1,1,'+',ZI(JMATE),' ',
     &              PHENOM,NBPAR,NOMPAR,VALPAR,18,NOMRES,
     &              VALRES,ICODRE,1)
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
        CALL RCVALB(FAMI,1,1,'+',ZI(JMATE),' ',
     &             PHENOM,NBPAR,NOMPAR,VALPAR,6,
     &             NOMRES(21), VALRES(21),ICODRE(21),1)
        DCI(1,1) = VALRES(21)
        DCI(2,2) = VALRES(22)
        DCI(1,2) = VALRES(23)
        DCI(2,1) = DCI(1,2)
        DC(1,1) = VALRES(24)
        DC(2,2) = VALRES(25)
        DC(1,2) = VALRES(26)
        DC(2,1) = DC(1,2)
C
C        --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
C        --- REACTUALISATION DE LA MATRICE DE FLEXION          --------
C        --- DANS LE CAS D'UN EXCENTREMENT                     --------
        DO 100 I = 1, 3
          DO 100 J = 1, 3
            DF(I,J) = DF(I,J)+DEUX*EXCENT*DMF(I,J)+EXCENT*EXCENT*DM(I,J)
            DMF(I,J)= DMF(I,J)                    +EXCENT*DM(I,J)
  100   CONTINUE
C
C        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
        CALL UTBTAB('ZERO',3,3, DM,T1VE,XAB1, DM)
        CALL UTBTAB('ZERO',3,3, DF,T1VE,XAB1, DF)
        CALL UTBTAB('ZERO',3,3,DMF,T1VE,XAB1,DMF)
        CALL UTBTAB('ZERO',2,2, DC,T2VE,XAB2, DC)
        CALL UTBTAB('ZERO',2,2,DCI,T2VE,XAB2,DCI)
C
        MULTIC = 1
C
      END IF
C
      DO 110 K = 1,9
        IF (ABS(DMF(K,1)).GT.1.D-10) COUPMF = .TRUE.
 110  CONTINUE
C
 999  CONTINUE
C
      END
