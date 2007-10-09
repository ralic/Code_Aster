      SUBROUTINE XMREAC(NDIM,NNES,NNM,
     &                  IGEOM,IDEPM)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/10/2007   AUTEUR NISTOR I.NISTOR 
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
      IMPLICIT NONE
      INTEGER      NDIM
      INTEGER      NNES
      INTEGER      NNM 
      INTEGER      IGEOM
      INTEGER      IDEPM    
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0366/TE0367
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C CALCULE LES NOUVELLES COORDONNÉES DES NOEUDS POUR LES MAILLES X-FEM
C MAITRES ET ESCLAVES
C                COORD = COORD + DEPL_M
C  ATTENTION!!! ON SORT AVEC DES CHAMPS FICTIFS POUR LES COORDONNES 
C  (VOIR LA DOC SUR LES GRANDS GLISSEMENTS AVEC X-FEM)
C
C IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
C IN  NNES    : NOMBRE DE NOEUDS ESCLAVES SOMMETS
C IN  NNM    : NOMBRE DE NOEUDS MAITRES
C IN/OUT  IGEOM  : POINTEUR JEVEUX SUR GEOMETRIE INITIALE
C IN  IDEPM  : POINTEUR JEVEUX SUR CHAMP DE DEPLACEMENT A L'INSTANT
C              PRECEDENT
C
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
      INTEGER I,J     
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DO 20 I = 1,NNES
        DO 10 J = 1,NDIM
          ZR(IGEOM+(I-1)*NDIM+J-1) = ZR(IGEOM+(I-1)*NDIM+J-1) +
     &    ZR(IDEPM+(I-1)*(3*NDIM)+J-1)-ZR(IDEPM+(I-1)*(3*NDIM)+J+2-1)
   10   CONTINUE
   20 CONTINUE

      DO 21 I = 1,NNM
        DO 11 J = 1,NDIM
           ZR(IGEOM+2*NNES*NDIM+(I-1)*NDIM+J-1)=
     &     ZR(IGEOM+2*NNES*NDIM+(I-1)*NDIM+J-1)+
     &     ZR(IDEPM+NNES*4*NDIM+(I-1)*(2*NDIM)+J-1)+
     &     ZR(IDEPM+NNES*4*NDIM+(I-1)*(2*NDIM)+J+2-1)
   11   CONTINUE
   21 CONTINUE
C
      CALL JEDEMA()
C   
      END
