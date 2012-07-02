      SUBROUTINE SH8EPS(XETEMP,XIDEPP,DEPLOC,PROPEL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C               ELEMENT SHB8
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER LAG
      REAL*8 XE(24),XIDEPP(*)
      REAL*8 XXG5(5),XCOQ(3,4),BKSIP(3,8,5),B(3,8)
      REAL*8 XCENT(3),PPP(3,3)
      REAL*8 XL(3,4),XXX(3),YYY(3)
      REAL*8 DEPS(6),UE(3,8)
      REAL*8 DEPSLO(6),DEPLOC(*),PROPEL(*),RR2(3,3)
      REAL*8 XETEMP(*),RR12(3,3),DUSX(9),PPPT(3,3)
C
C
C ON DEFINIT LES POINTS DE GAUSS ET LES POIDS
C
C-----------------------------------------------------------------------
      INTEGER I ,IP ,J 
      REAL*8 AJAC ,RBID ,ZETA ,ZLAMB 
C-----------------------------------------------------------------------
      XXG5(1) = -0.906179845938664D0
      XXG5(2) = -0.538469310105683D0
      XXG5(3) = 0.D0
      XXG5(4) = 0.538469310105683D0
      XXG5(5) = 0.906179845938664D0
C
C -----------------------------------------------------
C ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
C SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
C -----------------------------------------------------
C
C     ON FAIT UNE COPIE DE XETEMP DANS XE
      DO 10 I = 1,24
         XE(I) = XETEMP(I)
   10 CONTINUE
C
C UE: INCREMENT DE DEPLACEMENT NODAL, REPERE GLOBAL
C
C XE: DEBUT DU PAS
      DO 30 J = 1,8
         DO 20 I = 1,3
            UE(I,J) = XIDEPP((J-1)*3+I)
   20    CONTINUE
   30 CONTINUE
C
C CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
C      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
C      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
C      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
C
      CALL SHBKSI(5,XXG5,BKSIP)
C
      DO 120 IP = 1,5
C
C DEFINITION DES 4 POINTS  COQUES
C
         ZETA = XXG5(IP)
         ZLAMB = 0.5D0*(1.D0-ZETA)
         DO 50 I = 1,4
            DO 40 J = 1,3
              XCOQ(J,I) = ZLAMB*XE((I-1)*3+J) +
     &              (1.D0-ZLAMB)*XE((I-1+4)*3+J)
   40       CONTINUE
   50    CONTINUE
C
C CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
C XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
C
         CALL RLOSHB(XCOQ,XCENT,PPP,XL,XXX,YYY,RBID)
C
C CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
C
         CALL SHCALB(BKSIP(1,1,IP),XE,B,AJAC)
C
C CALCUL DE EPS DANS LE REPERE GLOBAL: 1 POUR DEFORMATIONS LINEAIRES
C                                     2 POUR TERMES CARRES EN PLUS
         DO 60 I = 1,6
            DEPS(I) = 0.D0
   60    CONTINUE
         LAG = 0
         IF (LAG.EQ.1) THEN
C ON AJOUTE LA PARTIE NON-LINEAIRE DE EPS
            CALL DSDX3D(2,B,UE,DEPS,DUSX,8)
         ELSE
            CALL DSDX3D(1,B,UE,DEPS,DUSX,8)
         END IF
         DO 80 I = 1,3
           DO 70 J = 1,3
             PPPT(J,I) = PPP(I,J)
   70      CONTINUE
   80    CONTINUE
         RR12(1,1) = DUSX(1)
         RR12(1,2) = DUSX(2)
         RR12(1,3) = DUSX(3)
         RR12(2,1) = DUSX(4)
         RR12(2,2) = DUSX(5)
         RR12(2,3) = DUSX(6)
         RR12(3,1) = DUSX(7)
         RR12(3,2) = DUSX(8)
         RR12(3,3) = DUSX(9)
         CALL MULMAT(3,3,3,PPPT,RR12,RR2)
         CALL MULMAT(3,3,3,RR2,PPP,RR12)
         DUSX(1) = RR12(1,1)
         DUSX(2) = RR12(1,2)
         DUSX(3) = RR12(1,3)
         DUSX(4) = RR12(2,1)
         DUSX(5) = RR12(2,2)
         DUSX(6) = RR12(2,3)
         DUSX(7) = RR12(3,1)
         DUSX(8) = RR12(3,2)
         DUSX(9) = RR12(3,3)
         DO 90 I = 1,9
           PROPEL(I+ (IP-1)*9) = DUSX(I)
   90    CONTINUE
C
         DO 100 I = 1,6
           DEPSLO(I) = 0.D0
  100    CONTINUE
         CALL CHRP3D(PPP,DEPS,DEPSLO,2)
C
C ATTENTION !!! DEFORMATION ECRITES SOUS LA FORME:
C               [DEPS] = [E_XX, E_YY, E_ZZ, 2*E_XY, 2*E_YZ, 2*E_XZ]
         DO 110 I = 1,6
C
C ON LAISSE LES DEFORMATIONS DANS LE REPERE LOCAL POUR LA PLASTICITE
C
            DEPLOC((IP-1)*6+I) = DEPSLO(I)
  110    CONTINUE
  120 CONTINUE
C
      END
