      SUBROUTINE PCCOEF(N,IN,IP,AC,ICPL,ICPC,ACPC,CX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRP_4

C   ENTREE
C   N          : TAILLE DE A
C   IN,IP,AC   : MATRICE D'ENTREE FORMAT SYMETRIQUE
C   CX      : TRAVAIL
C   ICPL       : IDEM IN POUR LA MATRICE DE PRECOND.
C   ICPC       : IDEM IP POUR LA MATRICE DE PRECOND.

C   SORTIE
C   ACPC       : COEFS DE LA MATRICE DE PRECOND.
C--------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      REAL*8 AC(*)
      INTEGER IN(N)
      INTEGER*4 IP(*),ICPC(*)
      REAL*8 ACPC(*),CX(N)
      INTEGER ICPL(0:N)
C----------------------------------------------------------------------
C----------------------------------------------------------------------



C AC ---> ACPC
C ==========================
C   MISE A 0 DE ACPC
      KK2 = ICPL(N-1)
      DO 10 KK = 1,KK2
        ACPC(KK) = 0.D0
   10 CONTINUE
      CALL WKVECT('&&PCCOEF.IND','V V I',N,JIND)

      ACPC(1) = AC(1)
      DO 40 I = 2,N
C  LIGNE CREUSE I DE AC --> LIGNE PLEINE IND-CX
C                          (ICPL(I-1)=FIN LIGNE I)
        K1 = IN(I-1) + 1
        K2 = IN(I)
        DO 20 K = K1,K2 - 1
          J = IP(K)
          ZI(JIND-1+J) = I
          CX(J) = AC(K)
   20   CONTINUE
        KK1 = ICPL(I-2) + 1
        KK2 = ICPL(I-1)
        DO 30 KK = KK1,KK2 - 1
          J = ICPC(KK)
          IF (ZI(JIND-1+J).EQ.I) ACPC(KK) = CX(J)
   30   CONTINUE
        ACPC(KK2) = AC(K2)
   40 CONTINUE

      IMP = 0
      IF (IMP.EQ.1) THEN
C       WRITE (6,*) ' FIN DU S-P PCCOEF'
      END IF
      CALL JEDETR('&&PCCOEF.IND')

      END
