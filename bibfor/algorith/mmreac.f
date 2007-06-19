      SUBROUTINE MMREAC ( NBDM, NDIM, NNE, NNM, IGEOM, IDEPM )
      IMPLICIT NONE
      INTEGER      NBDM,NDIM,NNE,NNM,IGEOM,IDEPM    
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ROUTINE APPELLEE PAR : TE0364/TE0365
C ----------------------------------------------------------------------
C
C CALCULE LES NOUVEAUX CHAMPS DE DEPLACEMENT POUR LES MAILLES MAITRES
C ET ESCLAVES
C  DEPL = DEPL_M + DEPL
C
C IN  NBDM   : NB DE DDL DE LA MAILLE ESCLAVE
C IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
C IN  NNE    : NOMBRE DE NOEUDS ESCLAVES
C IN  NNM    : NOMBRE DE NOEUDS MAITRES
C IN  IGEOM  : POINTEUR JEVEUX SUR GEOMETRIE INITIALE
C IN  IDEPM  : POINTEUR JEVEUX SUR CHAMP DE DEPLACEMENT A L'INSTANT
C              PRECEDENT
C ----------------------------------------------------------------------
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
      INTEGER   I, J
C ----------------------------------------------------------------------
C
C --- NOEUDS ESCLAVES
C
      DO 100 I = 1,NNE
         DO 110 J = 1,NDIM
            ZR(IGEOM+(I-1)*NDIM+J-1) = ZR(IGEOM+(I-1)*NDIM+J-1) +
     &                                 ZR(IDEPM+(I-1)*NBDM+J-1)
 110     CONTINUE
 100  CONTINUE
C
C --- NOEUDS MAITRES
C
      DO 120 I = 1,NNM
         DO 122 J = 1,NDIM
            ZR(IGEOM+NNE*NDIM+(I-1)*NDIM+J-1) =
     &      ZR(IGEOM+NNE*NDIM+(I-1)*NDIM+J-1)+
     &                              ZR(IDEPM+NNE*NBDM+(I-1)*NDIM+J-1)
 122     CONTINUE
 120  CONTINUE
C   
      END
