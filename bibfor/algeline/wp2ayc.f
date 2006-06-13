      SUBROUTINE WP2AYC(LMATRA,LMASSE,LAMOR,SIGMA,LBLOQ,YH,YB,
     +                                      ZH,ZB,U1,U2,U3,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16  SIGMA, U1(*),U2(*),U3(*),YH(*),YB(*),
     +            ZH(*),ZB(*)
      INTEGER     LMATRA,LMASSE,LAMOR,N,LBLOQ(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 18/11/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C                   T               T
C     CALCUL (ZH,ZB)   = A * (YH,YB)
C
C     OU A EST L' OPERATEUR (COMPLEXE) DONT ON CHERCHE UNE 
C     APPROXIMATION DES ELEMENTS PROPRES
C     ------------------------------------------------------------------
C IN  LMATRA : I : FACTORISEE LDLT (DANS C) DE LA MATRICE DYNAMIQUE
C IN  LMASSE : I : MATRICE DE MASSE
C IN  LAMOR  : I : MATRICE D' AMORTISSEMENT
C IN  SIGMA  : C : VALEUR DU SHIFT
C IN  YH     : C : PARTIE SUPERIEUR DE Y
C IN  YB     : C : PARTIE INFERIEURE DE Y
C IN  N      : I : DIMENSION DE MATRICE
C IN  LBLOQ  : I : TYPE DES DDL (LBOLOQ(I) = 0 <=> DDL(I) = BLOQUE)
C OUT ZH     : C : PARTIE SUPERIEURE DU RESULTAT
C OUT ZB     : C : PARTIE INFERIEURE DU RESULTAT
C VAR U1     : C : VECTEUR DE TRAVAIL, EN SORTIE VAUT AMOR *YH
C VAR U2     : C : VECTEUR DE TRAVAIL, EN SORTIE VAUT MASSE*YB
C VAR U3     : C : VECTEUR DE TRAVAIL, EN SORTIE VAUT MASSE*YH
C VAR V      : C : VECTEUR DE TRAVAIL
C     ------------------------------------------------------------------
      INTEGER   I
      REAL*8    RBID
C     ------------------------------------------------------------------
C
      CALL MCMULT('ZERO',LAMOR,YH,'C',U1,1)
      CALL MCMULT('ZERO',LMASSE,YB,'C',U2,1)
      CALL MCMULT('ZERO',LMASSE,YH,'C',U3,1)
C-RM-DEB
C     LA BOUCLE 5 REALISE LE PRODUIT PAR MASSE*INV(MASSE_REG)*MASSR
C     OR CETTE MATRICE EST EGALE A MASSE
C---> VOIR CE QUI SE PASSE QUAND LA BOUCLE EST SUPPRIMEE
      DO 5, I = 1, N, 1
         U3(I) = U3(I)*LBLOQ(I)
         U2(I) = U2(I)*LBLOQ(I)
 5    CONTINUE
C-RM-FIN
      DO 10, I = 1, N, 1
         U1(I) = U1(I) + SIGMA*U3(I) + U2(I)
10    CONTINUE
      CALL RLDLGG(LMATRA,RBID,U1,1)
      DO 20, I = 1, N, 1
         ZH(I) = - U1(I)
         ZB(I) = (YH(I) - SIGMA*U1(I))*LBLOQ(I)
20    CONTINUE
      END
