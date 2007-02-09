      SUBROUTINE MMMAA0(NDIM,NNE,
     &                  HPG,FFPC,JACOBI,
     &                  TYALGC,COEFCA,COEFCS,COEFCP,
     &                  MMAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2007   AUTEUR TORKHANI M.TORKHANI 
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
      INTEGER  NDIM,NNE,TYALGC
      REAL*8   MMAT(81,81)
      REAL*8   HPG,FFPC(9),JACOBI
      REAL*8   COEFCA,COEFCS,COEFCP

C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0364
C ----------------------------------------------------------------------
C
C CALCUL DE A ET DE AT POUR LE CONTACT METHODE CONTINUE
C CAS SANS CONTACT
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFPC   : FONCTIONS DE FORME DU POINT DE CONTACT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFCA : COEF_REGU_CONT
C IN  COEFFS : COEF_STAB_CONT
C IN  COEFFP : COEF_PENA_CONT
C IN  TYALGC : TYPE D'ALGORITHME DE CONTACT
C I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
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
      INTEGER I,J,II,JJ
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DO 61 I = 1,NNE
        DO 51 J = 1,NNE
          II = (2*NDIM)*(I-1)+NDIM+1
          JJ = (J-1)*(2*NDIM)+NDIM+1            
          IF (TYALGC .EQ. 1) THEN
            MMAT(II,JJ) = -HPG*FFPC(J)*FFPC(I)*JACOBI/COEFCA
          ELSE
            MMAT(II,JJ) = -HPG*FFPC(J)*FFPC(I)*JACOBI/COEFCS
          END IF 
 51     CONTINUE
 61   CONTINUE
C
      CALL JEDEMA()      
      END
