      SUBROUTINE TE0404(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/10/2012   AUTEUR SELLENET N.SELLENET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C ----------------------------------------------------------------------
C FONCTION REALISEE:  CALCUL DU PAS DE TEMPS DE COURANT POUR L'ELEMENT

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT


      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE

      CHARACTER*4 FAMI
      INTEGER ICODRE
      INTEGER CODRES(2)
      CHARACTER*2 NOMRES(2)
      CHARACTER*8  CND
      CHARACTER*16 PHENOM
      INTEGER ICOUR,IMATE,IGEOM,ND,NDIM,NNO,NNOS,NPG
      INTEGER I,J,IPOIDS,IVF,IDFDE,JGANO,IER,IRET
      INTEGER JCOQU,MULTIC,IDFD2,ICOOPG
      REAL*8  DMIN,DISTIJ,XI,YI,ZII,XJ,YJ,ZJ
      REAL*8  E,NU,RHO,VITMAT,EPAIS
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 PGL(3,3),T2EV(4),T2VE(4),T1VE(9),VALRES(2)
      LOGICAL COUPMF
C DEB ------------------------------------------------------------------

      CALL JEVECH('PCOURAN','E',ICOUR)

C     RECUPERATION DES COORDONNEES DES NOEUDS
      CALL TEATTR(' ','S','DIM_COOR_MODELI',CND,IER)
      READ(CND,'(I8)')  ND
      CALL JEVECH('PGEOMER','L',IGEOM)
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C     CALCUL DE LA PLUS PETITE DISTANCE ENTRE LES NOEUDS SOMMETS
      DMIN = SQRT((ZR(IGEOM-1+ND*(2-1)+1)-ZR(IGEOM-1+1))**2
     &           +(ZR(IGEOM-1+ND*(2-1)+2)-ZR(IGEOM-1+2))**2
     &           +(ZR(IGEOM-1+ND*(2-1)+3)-ZR(IGEOM-1+3))**2)

      DO 10 I=1,NNOS-1
        DO 20 J=I+1,NNOS

         XI = ZR(IGEOM-1+ND*(I-1)+1)
         YI = ZR(IGEOM-1+ND*(I-1)+2)

         XJ = ZR(IGEOM-1+ND*(J-1)+1)
         YJ = ZR(IGEOM-1+ND*(J-1)+2)

         IF(ND.EQ.3) THEN
            ZII = ZR(IGEOM-1+ND*(I-1)+3)
            ZJ = ZR(IGEOM-1+ND*(J-1)+3)
         ELSE
            ZII = 0.D0
            ZJ = 0.D0
         ENDIF

         DISTIJ = SQRT((XJ-XI)**2+(YJ-YI)**2+(ZJ-ZII)**2)
         IF ((DISTIJ.LE.DMIN).AND.(DISTIJ.NE.0)) DMIN = DISTIJ

 20     CONTINUE
 10   CONTINUE

C     RECUPERATION DU MODULE D'YOUNG ET DE LA MASSE VOLUMIQUE
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)
      IF (PHENOM.EQ.'ELAS') THEN
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        CALL RCVALB(FAMI,1,1,'+',ZI(IMATE),' ','ELAS',0,' ',0.D0,2,
     &              NOMRES, VALRES, CODRES, 1)
        E = VALRES(1)
        NU = VALRES(2)
      ELSE IF (PHENOM.EQ.'ELAS_COQUE') THEN
        CALL ELREF5(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,
     &             IDFDE,IDFD2,JGANO)
        CALL JEVECH('PCACOQU','L',JCOQU)
        EPAIS = ZR(JCOQU)
        IF (NNO.EQ.3) THEN
          CALL DXTPGL(ZR(IGEOM),PGL)
        ELSE IF (NNO.EQ.4) THEN
          CALL DXQPGL(ZR(IGEOM),PGL,'S',IRET)
        END IF

        CALL DXMATE(FAMI,DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     &                  COUPMF,T2EV,T2VE,T1VE)
        NU = DM(1,2)/DM(1,1)
        E = (1.D0-NU**2)*DM(1,1)/EPAIS
      ENDIF

      CALL RCVALB(FAMI,1,1,'+',ZI(IMATE),' ',PHENOM,0,' ',0.D0,
     &            1,'RHO',RHO,ICODRE,1)

C     CALCUL DE LA CELERITE DES ONDES DANS LE MATERIAU

      VITMAT = SQRT(E/RHO)

C     CALCUL DU PAS DE TEMPS DE LA CONDITION DE COURANT

      ZR(ICOUR) = DMIN/VITMAT
C FIN ------------------------------------------------------------------
      END
