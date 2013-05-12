      SUBROUTINE TE0592(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2013   AUTEUR SFAYOLLE S.FAYOLLE 
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
C RESPONSABLE SFAYOLLE S.FAYOLLE
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16 OPTION,NOMTE

C ----------------------------------------------------------------------
C FONCTION REALISEE:  CALCUL DE LA RIGIDITE MECANIQUE POUR LES ELEMENTS
C                     INCOMPRESSIBLES A 3 CHAMPS UGP
C                     EN 3D/D_PLAN/AXI
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------

      LOGICAL      LTEATT
      INTEGER      NDIM,NNO1,NNO2,NNO3,NNOS,NPG,JGN,NTROU
      INTEGER      IW,IVF1,IVF2,IVF3,IDF1,IDF2,IDF3
      INTEGER      VU(3,27),VG(27),VP(27),VPI(3,27)
      INTEGER      IGEOM,IMATE,IMATUU
      CHARACTER*8  LIELRF(10),TYPMOD(2)
C ----------------------------------------------------------------------

C - FONCTIONS DE FORME
      CALL ELREF2(NOMTE,10,LIELRF,NTROU)
      CALL ASSERT(NTROU.GE.3)
      CALL ELREF4(LIELRF(3),'RIGI',NDIM,NNO3,NNOS,NPG,IW,IVF3,IDF3,JGN)
      CALL ELREF4(LIELRF(2),'RIGI',NDIM,NNO2,NNOS,NPG,IW,IVF2,IDF2,JGN)
      CALL ELREF4(LIELRF(1),'RIGI',NDIM,NNO1,NNOS,NPG,IW,IVF1,IDF1,JGN)

C - TYPE DE MODELISATION
      IF (NDIM.EQ.2 .AND. LTEATT(' ','AXIS','OUI')) THEN
        TYPMOD(1) = 'AXIS  '
      ELSE IF (NDIM.EQ.2 .AND. LTEATT(' ','D_PLAN','OUI')) THEN
        TYPMOD(1) = 'D_PLAN  '
      ELSE IF (NDIM .EQ. 3) THEN
        TYPMOD(1) = '3D'
      ELSE
        CALL U2MESK('F','ELEMENTS_34',1,NOMTE)
      END IF
      TYPMOD(2) = '        '

C - ACCES AUX COMPOSANTES DU VECTEUR DDL
      CALL NIINIT(NOMTE,TYPMOD,NDIM,NNO1,NNO2,NNO3,0,VU,VG,VP,VPI)

C - PARAMETRES EN ENTREE
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PMATUUR','E',IMATUU)

      CALL NIRMTD(NDIM,NNO1,NNO2,NNO3,NPG,IW,ZR(IVF2),ZR(IVF3),
     &            IVF1,IDF1,VU,VG,VP,IGEOM,ZI(IMATE),ZR(IMATUU))

      END
