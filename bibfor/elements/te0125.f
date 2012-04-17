      SUBROUTINE TE0125(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/05/2011   AUTEUR SFAYOLLE S.FAYOLLE 
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
C                POUR DES ELEMENTS QUASI-INCOMPRESSIBLES A 3 CHAMPS
C                EN 2D ET AXI
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER NNO1,NNO2,NPG1,IMATUU
      INTEGER IPOID1,IPOID2,IVF1,IVF2,IDFDE1,IGEOM,NDIM,NNOS,JGANO
      INTEGER IMATE,NPG2,IDFDE2
      INTEGER I, N ,KK , M, JMAX, J, NTROU

      REAL*8 KUU(2,9,2,9), KUA(2,9,2,4), KAA(2,4,2,4)

      CHARACTER*4 FAMI
      CHARACTER*8 LIELRF(10)

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

      CALL ELREF2(NOMTE,10,LIELRF,NTROU)
      CALL ASSERT(NTROU.GE.3)

      FAMI = 'RIGI'
      CALL ELREF4(LIELRF(1),FAMI,NDIM,NNO1,NNOS,NPG1,IPOID1,IVF1,
     &                                              IDFDE1,JGANO)

      CALL ELREF4(LIELRF(2),FAMI,NDIM,NNO2,NNOS,NPG2,IPOID2,IVF2,
     &                                              IDFDE2,JGANO)

C - PARAMETRES EN ENTREE
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)

C - PARAMETRES EN SORTIE
      IF (OPTION(1:10).EQ.'RIGI_MECA ') THEN
        CALL JEVECH('PMATUUR','E',IMATUU)
      END IF

      CALL NIRM2D(NNO1,NNO2,NPG1,IPOID1,IVF1,IVF2,IDFDE1,
     &              IGEOM,IMATE,KUU,KUA, KAA)

        KK = 0
        DO 30 N = 1, NNO1
          DO 35 I = 1,4
            DO 40 M = 1,N
              IF (M.EQ.N) THEN
                JMAX = I
              ELSE
                JMAX = 4
              END IF
              DO 45 J = 1, JMAX
                IF (I.LE.2 .AND. J.LE.2) THEN
                  ZR(IMATUU+KK) = KUU(I,N,J,M)
                  KK = KK + 1
                END IF
                IF (I.GE.3 .AND. N.LE.NNO2 .AND. J.LE.2) THEN
                  ZR(IMATUU+KK) = KUA(J,M,I-2,N)
                  KK = KK + 1
                END IF
                IF (I.LE.2 .AND. M.LE.NNO2 .AND. J.GE.3) THEN
                  ZR(IMATUU+KK) = KUA(I,N,J-2,M)
                  KK = KK + 1
                END IF
                IF (I.GE.3 .AND. N.LE.NNO2 .AND.
     &              J.GE.3 .AND. M.LE.NNO2) THEN
                  ZR(IMATUU+KK) = KAA(I-2,N,J-2,M)
                  KK = KK + 1
                END IF
 45           CONTINUE
 40         CONTINUE
 35       CONTINUE
 30     CONTINUE

      END
