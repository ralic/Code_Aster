      SUBROUTINE       XPLMAT(NDIM,DDLH,NFE,DDLC,NNO,NNOM,N,PL)

      IMPLICIT NONE
      INTEGER          NDIM,DDLH,NFE,DDLC,NNO,NNOM,N,PL

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/09/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

C    CADRE : X-FEM ET CONTACT CONTINU
C             CALCULE LA PLACE DU LAMBDA(N) NORMAL DANS LA MATRICE
C             DE RAIDEUR DUE AU CONTACT 
C
C IN  NDIM    : DIMENSION (=3)
C IN  DDLH    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
C IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES
C IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
C IN  NNO     : NOMBRE DE NOEUDS SOMMET
C IN  NNOM    : NOMBRE DE NOEUDS MILIEU
C IN  N       : NUMÉRO DU NOEUD PORTANT LE LAMBDA
C
C OUT PL      : PLACE DU LMBDA DANS LA MATRICE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     
      INTEGER       DDLS      
C ----------------------------------------------------------------------

      CALL ASSERT(N.LE.(NNO+NNOM))

C     NOMBRE DE DDL PAR NOEUD SOMMET 
      DDLS=NDIM+DDLH+NFE*NDIM+DDLC
      
      IF (N.LE.NNO) THEN
        PL=DDLS*(N-1)+NDIM+DDLH+NFE*NDIM+1
      ELSE
        PL=DDLS*NNO+DDLC*(N-NNO-1)+1
      ENDIF

      END
