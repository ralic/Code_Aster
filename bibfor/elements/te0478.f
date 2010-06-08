      SUBROUTINE TE0478 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/09/2006   AUTEUR VABHHTS J.PELLET 
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
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C     CALCUL DES COORDONNEES DES POINTS DE GAUSS
C     POUR LES ELEMENTS BARRE ET CABLE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER  NDIM,NNO,NNOS,NPG,JGANO,ICOPG,IDFDE,IPOIDS,IVF,IGEOM
      INTEGER  TAB(2),IRET,NDIM1
C ----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM1,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C     NDIM1 EST LA DIMENSION TOPOLOGIQUE. IL FAUT CALCULER LA
C     DIMENSION DE L'ESPACE NDIM (2 OU 3) :
      CALL TECACH('OOO','PGEOMER',2,TAB,IRET)
      NDIM  = TAB(2)/NNO

      IGEOM = TAB(1)
      CALL JEVECH('PCOORPG','E',ICOPG )

      CALL GEDISC(NDIM,NNO,NPG,ZR(IVF),ZR(IGEOM),ZR(ICOPG))
      END
