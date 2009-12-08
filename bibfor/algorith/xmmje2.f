      SUBROUTINE XMMJE2(NDIM,NORM,IGEOM,IDEPL,SINGE,SINGM,NDLS,
     &                RRE,RRM,NNE,NNES,NNM,FFES,FFMA,JEU)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
      IMPLICIT NONE
      INTEGER NDIM,NNE,NNES,NNM,IGEOM,IDEPL,SINGE,SINGM,NDLS
      REAL*8  NORM(3),RRE,RRM
      REAL*8  FFES(9),FFMA(9),JEU
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0363
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C CALCUL DES JEUX
C 
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NORM   : VALEUR DE LA NORMALE
C IN  GEOME  : COORDONNÉES RÉELES DU POINT DE CONTACT
C IN  GEOMM  : COORDONNÉES RÉELES DU PROJETE DU POINT DE CONTACT
C IN  DEPLE  : DEPLACEMENTS DU POINT DE CONTACT
C IN  DEPLM  : DEPLACEMENTS DU PROJETE DU POINT DE CONTACT
C IN  DEPLME : DEPLACEMENTS PRECEDENTS DU POINT DE CONTACT
C IN  DEPLMM : DEPLACEMENTS PRECEDENTS DU PROJETE DU POINT DE CONTACT
C OUT JEU    : VALEUR DU JEU POUR LES CONTRAINTES ACTIVES
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER I,J,K,PL
      REAL*8  POSE(NDIM),POSM(NDIM)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INNITIALISATION
C
      CALL VECINI(NDIM,0.D0,POSE)
      CALL VECINI(NDIM,0.D0,POSM)
      JEU  = 0.D0
C
      DO 10 I = 1,NDIM
C --- CALCUL DE LA POSITION COURRANTE DU POINT ESCLAVE
        DO 20 J = 1,NNES
          IF (NNM.NE.0) THEN
            PL = NDLS*(J-1) + I
            POSE(I) = POSE(I) + FFES(J)*(ZR(IGEOM-1+NDIM*(J-1)+I)
     &                                   +ZR(IDEPL-1+PL)
     &                                   -ZR(IDEPL-1+PL+NDIM))
          ENDIF
          DO 25 K = 1,SINGE
            PL = NDLS*J - 2*NDIM + I
            POSE(I) = POSE(I) - RRE*FFES(J)*ZR(IDEPL-1+PL)
 25       CONTINUE
 20     CONTINUE
C
C --- CALCUL DE LA POSITION COURRANTE DU POINT MAITRE
        DO 30 J = 1,NNM
          PL= NDLS*NNES + NDIM*(NNE-NNES) + (2+SINGM)*NDIM*(J-1) + I
          POSM(I) = POSM(I)+FFMA(J)*(ZR(IGEOM-1+NNE*NDIM+(J-1)*NDIM+I)
     &                                 +ZR(IDEPL-1+PL)
     &                                 +ZR(IDEPL-1+PL+NDIM))
          DO 40 K = 1,SINGM
            PL = PL + 2*NDIM
            POSM(I) = POSM(I) + RRM*FFMA(J)*ZR(IDEPL-1+PL)
 40       CONTINUE
 30     CONTINUE
 10   CONTINUE
C
C --- CALCUL DU JEU
      DO 70 I = 1,NDIM
        JEU = JEU + NORM(I)*(POSE(I)-POSM(I))
  70  CONTINUE
C  
      CALL JEDEMA()      
      END
