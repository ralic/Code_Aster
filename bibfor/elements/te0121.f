      SUBROUTINE TE0121(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/01/2011   AUTEUR SFAYOLLE S.FAYOLLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE SFAYOLLE S.FAYOLLE
      IMPLICIT NONE

      CHARACTER*16 OPTION,NOMTE

C ......................................................................
C    - FONCTION REALISEE: CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                POUR DES ELEMENTS QUASI-INCOMPRESSIBLES A 2 CHAMPS 
C                EN 2D ET AXI
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER NNO1, NNO2, NPG1, IMATUU, NNO1S, NNO2S
      INTEGER IPOIDS, IVF1, IVF2, IDFDE1, IGEOM, NDIM, JGANO1, JGANO2
      INTEGER IMATE, NPG2, IDFDE2, IPOI1, IPOI2
      INTEGER I, N, KK, M, JMAX, J

      REAL*8 KUU(3,20,3,20),KUA(3,20,1,8),KAA(1,8,1,8)

      CHARACTER*4 FAMI
      CHARACTER*8 ELREF2

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

C - FONCTIONS DE FORMES ET POINTS DE GAUSS
      IF (NOMTE(8:12).EQ.'TETRA') THEN
        ELREF2 = 'TE4'
      ELSE IF (NOMTE(8:11).EQ.'HEXA') THEN
        ELREF2 = 'HE8'
      ELSE IF (NOMTE(8:12).EQ.'PENTA') THEN
        ELREF2 = 'PE6'
      ELSE
        CALL U2MESK('F','DVP_4',1,NOMTE)
      END IF

      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO1,NNO1S,NPG1,IPOI1,IVF1,IDFDE1,
     &            JGANO1)

      CALL ELREF4(ELREF2,FAMI,NDIM,NNO2,NNO2S,NPG2,IPOI2,IVF2,IDFDE2,
     &            JGANO2)

C - PARAMETRES EN ENTREE
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)

C - PARAMETRES EN SORTIE
      IF (OPTION(1:10).EQ.'RIGI_MECA ') THEN
        CALL JEVECH('PMATUUR','E',IMATUU)
      END IF

      IF (NOMTE(8:13).EQ.'TETRA4') THEN
        CALL NIRM3B(NNO1,NNO2,NPG1,IPOIDS,IVF1,IVF2,IDFDE1,
     &              ZR(IGEOM),ZI(IMATE),KUU,KUA, KAA)
      ELSE
        CALL NIRM3C(NNO1,NNO2,NPG1,IPOIDS,IVF1,IVF2,IDFDE1,
     &              ZR(IGEOM),ZI(IMATE),KUU,KUA, KAA)
      END IF

      KK = 0
      DO 80 N = 1,NNO1
        DO 70 I = 1,4
          DO 60 M = 1,N
            IF (M.EQ.N) THEN
              JMAX = I
            ELSE
              JMAX = 4
            END IF
            DO 50 J = 1,JMAX
              IF (I.LE.3 .AND. J.LE.3) THEN
                ZR(IMATUU+KK) = KUU(I,N,J,M)
                KK = KK + 1
              END IF
              IF (I.GE.4 .AND. N.LE.NNO2 .AND. J.LE.3) THEN
                ZR(IMATUU+KK) = KUA(J,M,I-3,N)
                KK = KK + 1
              END IF
              IF (I.LE.3 .AND. M.LE.NNO2 .AND. J.GE.4) THEN
                ZR(IMATUU+KK) = KUA(I,N,J-3,M)
                KK = KK + 1
              END IF
              IF (I.GE.4 .AND. N.LE.NNO2 .AND. J.GE.4 .AND.
     &            M.LE.NNO2) THEN
                ZR(IMATUU+KK) = KAA(I-3,N,J-3,M)
                KK = KK + 1
              END IF
   50       CONTINUE
   60     CONTINUE
   70   CONTINUE
   80 CONTINUE

      END
