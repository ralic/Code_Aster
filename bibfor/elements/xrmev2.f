      SUBROUTINE XRMEV2(CPT,NPG,NDIM,IGEOM,JSIGSE,COORSE,TVOLSE)
      IMPLICIT NONE
      INTEGER CPT,NPG,NDIM,IGEOM,JSIGSE
      REAL*8  TVOLSE
      CHARACTER*24 COORSE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2009   AUTEUR GNICOLAS G.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
C  XFEM - ERREUR EN MECANIQUE - TERME VOLUMIQUE - DIMENSION 2
C  *       *        **                *                     *
C =====================================================================
C     
C     BUT:
C         CALCUL (AU SEIN DU SOUS-ELEMENT XFEM COURANT) DU CARRE DU 
C         TERME VOLUMIQUE DE L'INDICATEUR D'ERREUR EN RESIDU 
C         DANS LE CAS 2D
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   CPT    : NUMERO DU SOUS-ELEMENT COURANT 
C IN   NPG    : NOMBRE DE POINTS DE GAUSS DU SOUS-ELEMENT
C IN   NDIM   : DIMENSION
C IN   IGEOM  : ADRESSE DU CHAMP DE GEOMETRIE
C IN   JSIGSE : ADRESSE DU CHAMP DE CONTRAINTES AUX NOEUDS 
C               DU SOUS-ELEMENT
C IN   COORSE : ADRESSE DES COORDONNEES DES NOEUDS DU SOUS-ELEMENT 
C               DANS LE REPERE REEL 
C
C      SORTIE :
C-------------
C OUT  TVOLSE : CARRE DU TERME VOLUMIQUE DE L'INDICATEUR D'ERREUR
C
C ......................................................................
C            
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER NBCMP
      PARAMETER(NBCMP=4)

      REAL*8 RBID1,DFDX(3),DFDY(3),POIJAC,DSX,DSY,NORME
      INTEGER NDIMB,NNO,NNOS,NPGBIS,IPOIDS,JCOOPG,IVF,IDFDE,JDFD2,JGANO
      INTEGER JCOORS,KPG,N,IBID,ICMP,JSIGNO,IADPG
      CHARACTER*24 SIGNSE
C
C ----------------------------------------------------------------------
C      
      CALL ELREF5('TR3','XINT',NDIMB,NNO,NNOS,NPGBIS,IPOIDS,JCOOPG,IVF,
     &                  IDFDE,JDFD2,JGANO)
      CALL ASSERT(NPG.EQ.NPGBIS.AND.NDIM.EQ.NDIMB)
C
      TVOLSE=0.D0
C
C --- COORDONEES DU SOUS-ELEMENT ---------------------------------------
C
      CALL JEVEUO(COORSE,'L',JCOORS)
C
C --- ECRITURE POUR LE SOUS-ELEMENT COURANT D'UN TABLEAU DE CONTRAINTES
C --- AUX NOEUDS UTILISABLE PAR LA ROUTINE ERMEV2 
C
      SIGNSE='&&XRMEV2.SIGNSE'
      CALL WKVECT(SIGNSE,'V V R',NBCMP*NNO,JSIGNO)
      DO 100 N=1,NNO
        DO 110 ICMP=1,NBCMP
          ZR(JSIGNO-1+NBCMP*(N-1)+ICMP)=
     &                   ZR(JSIGSE-1+NBCMP*NNO*(CPT-1)+NBCMP*(N-1)+ICMP)
 110    CONTINUE
 100  CONTINUE
C
C ----------------------------------------------------------------------
C --------- BOUCLE SUR LES POINTS DE GAUSS DU SOUS ELEMENT -------------
C ----------------------------------------------------------------------
C
      DO 200 KPG=1,NPG
C        
C --- CALCUL DES DERIVEES DES FONCTIONS DE FORME DU SOUS-ELEMENT -------
C --- AU POINT DE GAUSS COURANT DANS LE REPERE REEL -------------------
C
        CALL DFDM2D(NNO,KPG,IPOIDS,IDFDE,ZR(JCOORS),DFDX,DFDY,POIJAC)
C        
C --- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA ------------------
C
        IADPG=IVF+(KPG-1)*NNO
        IBID = 1
        CALL ERMEV2(NNO,IGEOM,ZR(IADPG),JSIGNO,NBCMP,DFDX,DFDY,
     &              POIJAC,IBID,
     &              DSX,DSY,NORME)
C
C --- CALCUL DU TERME VOLUMIQUE AVEC INTEGRATION DE GAUSS --------------
C
        TVOLSE=TVOLSE+(DSX**2+DSY**2)*POIJAC

 200  CONTINUE 

      CALL JEDETR(SIGNSE)

      END
