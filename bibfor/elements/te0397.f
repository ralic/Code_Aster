      SUBROUTINE TE0397(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/01/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          COQUE 1D
C                          OPTION : 'CHAR_MECA_PRES_R  '
C                          ELEMENT: MECXSE3,METCSE3,METDSE3
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

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

      INTEGER NNO,NNOS,JGANO,NDIM,NDDL,KP,NPG,IPOIDS,IVF,IDFDK,IGEOM
      INTEGER IVECTU,K,I,L,IPRES,IER,IADZI,IAZK24,ITEMPS
      REAL*8 VALPAR(4),POIDS,R,FX,FY,F3,NX,NY,COUR,DFDX(3),PR
      CHARACTER*8 NOMPAR(4),NOMAIL,ELREFE
C DEB ------------------------------------------------------------------

      CALL ELREF1(ELREFE)

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDK,JGANO)


      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVECTUR','E',IVECTU)
      NDDL = 3

      IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN
C          ------------------------------
        CALL JEVECH('PPRESSR','L',IPRES)

        DO 30 KP = 1,NPG
          K = (KP-1)*NNO
          CALL DFDM1D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDK+K),ZR(IGEOM),DFDX,
     &                COUR,POIDS,NX,NY)
          R = 0.D0
          FX = 0.D0
          FY = 0.D0
          DO 10 I = 1,NNO
            L = (KP-1)*NNO + I
C-----------------------------------------------------
C              LE SIGNE MOINS CORRESPOND A LA CONVENTION :
C                 UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
C-----------------------------------------------------
            F3 = -ZR(IPRES+I-1)
            FX = FX + NX*F3*ZR(IVF+L-1)
            FY = FY + NY*F3*ZR(IVF+L-1)
            R = R + ZR(IGEOM+2* (I-1))*ZR(IVF+L-1)
   10     CONTINUE
          IF (NOMTE(3:4).EQ.'CX') POIDS = POIDS*R
          DO 20 I = 1,NNO
            L = (KP-1)*NNO + I
            ZR(IVECTU+NDDL* (I-1)) = ZR(IVECTU+NDDL* (I-1)) +
     &                               FX*ZR(IVF+L-1)*POIDS
            ZR(IVECTU+NDDL* (I-1)+1) = ZR(IVECTU+NDDL* (I-1)+1) +
     &                                 FY*ZR(IVF+L-1)*POIDS
   20     CONTINUE
   30   CONTINUE

      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN
C              ------------------------------
        CALL JEVECH('PPRESSF','L',IPRES)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        VALPAR(4) = ZR(ITEMPS)
        NOMPAR(4) = 'INST'
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        DO 40 I = 0,NNO - 1
          VALPAR(1) = ZR(IGEOM+3*I)
          VALPAR(2) = ZR(IGEOM+3*I+1)
          VALPAR(3) = ZR(IGEOM+3*I+2)
          CALL FOINTE('FM',ZK8(IPRES+I),4,NOMPAR,VALPAR,PR,IER)
          IF (PR.NE.0.D0) THEN
            CALL TECAEL(IADZI,IAZK24)
            NOMAIL = ZK24(IAZK24-1+3) (1:8)
            CALL UTDEBM('F','TE0397','LA PRESSION DOIT ETRE NULLE')
            CALL UTIMPK('S',' POUR LA MAILLE ',1,NOMAIL)
            CALL UTFINM
          END IF
   40   CONTINUE

      END IF

      END
