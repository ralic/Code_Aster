      SUBROUTINE DXMAT1(FAMI,EPAIS,DF,DM,DMF,PGL,INDITH,
     &                  T2EV,T2VE,T1VE,NPG)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER INDITH,NPG
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DMC(3,2),DFC(3,2)
      REAL*8 PGL(3,3),T2EV(4),T2VE(4),T1VE(9)
      CHARACTER*4 FAMI
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/02/2013   AUTEUR DESROCHE X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ------------------------------------------------------------------
C     CALCUL DES MATRICES DE COEFFCIENTS THERMOELASTIQUES DE FLEXION,
C  MEMBRANE, COUPLAGE MEMBRANE-FLEXION POUR LE DKTG (MATERIAU ISOTROPE) 
C     LA VARIABLE INDITH EST INITIALISEE A 0
C     DANS LE CAS OU LE COEFFICIENT DE DILATATION ALPHA N'A
C     PAS ETE DONNE, INDITH VAUT -1 ET ON  NE CALCULE PAS LES
C     CONTRAINTES THERMIQUES
C     ------------------------------------------------------------------
      INTEGER JCOQU,JMATE,IRET
      INTEGER I,J,K,NBPAR,ELASCO,INDALF
      REAL*8 CDF,CDM,VALRES(56)
      REAL*8 YOUNG,NU,EPAIS,VALPAR
      REAL*8 XAB1(3,3),DH(3,3)
      REAL*8 DX,DY,DZ,S,C,NORM
      REAL*8 PS,PJDX,PJDY,PJDZ,ALPHAT
      REAL*8 ALPHA,BETA,R8DGRD,R8PREM
      INTEGER ICODRE(56)
      CHARACTER*3 NUM
      CHARACTER*8 NOMRES(56),NOMPAR
      CHARACTER*10 PHENOM
C     ------------------------------------------------------------------

      CALL R8INIR(9,0.D0,DM,1)
      CALL R8INIR(9,0.D0,DF,1)
      CALL R8INIR(9,0.D0,DH,1)
      CALL R8INIR(9,0.D0,DMF,1)
      CALL R8INIR(6,0.D0,DMC,1)
      CALL R8INIR(6,0.D0,DFC,1)

      CALL JEVECH('PCACOQU','L',JCOQU)
      EPAIS = ZR(JCOQU)
      ALPHA = ZR(JCOQU+1)*R8DGRD()
      BETA  = ZR(JCOQU+2)*R8DGRD()

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
      CALL RCCOMA(ZI(JMATE),'ELAS',1,PHENOM,ICODRE)

      IF (PHENOM.EQ.'ELAS') THEN
        IF (NORM.LE.R8PREM()) THEN
          CALL U2MESS('A','ELEMENTS_40')
        END IF
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'

      ELSE
        CALL U2MESS('F','ELEMENTS_42')
      END IF

C===============================================================
C     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:

      CALL MOYTE2(FAMI,NPG,'+',VALPAR,IRET)
      NBPAR = 1
      NOMPAR = 'TEMP'
C===============================================================

      IF (PHENOM.EQ.'ELAS') THEN
C        ------ MATERIAU ISOTROPE ------------------------------------

        CALL RCVALB(FAMI,1,1,'+',ZI(JMATE),' ',
     &              PHENOM,NBPAR,NOMPAR,VALPAR,2,NOMRES,
     &              VALRES,ICODRE,1)
        CALL RCVALB(FAMI,1,1,'+',ZI(JMATE),' ',
     &             PHENOM,NBPAR,NOMPAR,VALPAR,1,
     &             NOMRES(3), VALRES(3),ICODRE(3),0)
        IF ((ICODRE(3).NE.0).OR.(VALRES(3).EQ.0.D0)) THEN
          INDITH = -1
          GO TO 90
        END IF
        YOUNG = VALRES(1)
        NU = VALRES(2)
        ALPHAT = VALRES(3)
        YOUNG = YOUNG*ALPHAT

C      ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        CDF = YOUNG*EPAIS*EPAIS*EPAIS/12.D0/ (1.D0-NU*NU)
        DF(1,1) = CDF
        DF(1,2) = CDF*NU
        DF(2,1) = DF(1,2)
        DF(2,2) = DF(1,1)
C      ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        CDM = EPAIS*YOUNG/ (1.D0-NU*NU)
        DM(1,1) = CDM
        DM(1,2) = CDM*NU
        DM(2,1) = DM(1,2)
        DM(2,2) = DM(1,1)

      END IF
C
   90 CONTINUE
      END
