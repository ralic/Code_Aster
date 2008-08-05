      SUBROUTINE XMVEF0(NDIM,NNC,NNE,NNES,NFAES,IAINES,
     &                  HPG,FFPC,JACOBI,DEPLE,CFACE,
     &                  ESQ,TAU1,TAU2,VTMP)     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/08/2008   AUTEUR MAZET S.MAZET 
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
      INTEGER  NDIM,NNC,NNE,NNES,NFAES,IAINES,CFACE(5,3)
      REAL*8   HPG,FFPC(9),JACOBI  
      REAL*8   DEPLE(6)    
      REAL*8   TAU1(3),TAU2(3)     
      REAL*8   VTMP(81)
      CHARACTER*8  ESQ  
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0367
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C CALCUL DU SECOND MEMBRE POUR LE FROTTEMENT
C CAS SANS CONTACT (XFEM)
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFPC   : FONCTIONS DE FORME DU POINT DE CONTACT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  DEPLE  : DEPLACEMENTS DE LA SURFACE ESCLAVE
C IN  TAU1   : PREMIER VECTEUR TANGENT
C IN  TAU2   : DEUXIEME VECTEUR TANGENT
C I/O VTMP   : VECTEUR SECOND MEMBRE ELEMENTAIRE DE CONTACT/FROTTEMENT
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
      INTEGER I,K,L,II,IN,PL,XOULA
      REAL*8  TT(3)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      DO 100 I = 1,3
        TT(I) = 0.D0
 100  CONTINUE           
C
C --- MATRICE 
C
      IF (NDIM.EQ.2) THEN
        DO 200 K = 1,NDIM
          TT(1) = TAU1(K)*TAU1(K) +TT(1)
 200    CONTINUE
        TT(1) = DEPLE(2*NDIM+1+1)*TT(1)
        TT(2) = 0.D0
      ELSE IF (NDIM.EQ.3) THEN
        DO 300 K = 1,NDIM
          TT(1) = (DEPLE(2*NDIM+1+1)*TAU1(K)+DEPLE(2*NDIM+1+2)
     +             *TAU2(K))*TAU1(K)+TT(1)
     
 300     CONTINUE
        DO 400 K = 1,NDIM
          TT(2) = (DEPLE(2*NDIM+1+1)*TAU1(K)+DEPLE(2*NDIM+1+2)
     +             *TAU2(K))*TAU2(K)+TT(2)
     
 400     CONTINUE
      END IF
      DO 500 I=1,NNC
        DO 600 L=1,NDIM-1
          IN=XOULA(CFACE,NFAES,I,IAINES,ESQ)
          CALL XPLMA2(NDIM,NNE,NNES,IN,PL)
          II = PL+L
          VTMP(II)=-JACOBI*HPG*FFPC(I)*TT(L)
  600   CONTINUE
  500 CONTINUE
C
      CALL JEDEMA()      
      END
