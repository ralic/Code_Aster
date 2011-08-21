      SUBROUTINE VDEFR2 ( NP , NBSP, MATEV , TENSEL , TENLOC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/08/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      VDEFR2   -- PASSAGE DU VECTEUR DES EFFORTS GENERALISES
C                  OU DU VECTEUR DES DEFORMATIONS-COURBURES
C                  DU REPERE INTRINSEQUE AUX NOEUDS
C                  OU AUX POINTS D'INTEGRATION DE L'ELEMENT
C                  AU REPERE UTILISATEUR POUR LES ELEMENTS DE
C                  COQUE EPAISSE 3D .
C
C                 CETTE ROUTINE EST ANALOGUE A DXEFRO QUI EST
C                 OPERATIONELLE POUR LES ELEMENTS DE PLAQUE
C                 A L'EXCEPTION DES MATRICES DE PASSAGE QUI
C                 SONT DEFINIES EN DES POINTS DE L'ELEMENT.
C
C   ARGUMENT        E/S   TYPE         ROLE
C    NP             IN     I        NOMBRE DE POINTS OU SONT CALCULES
C                                   LES TENSEURS (I.E. IL S'AGIT DES
C                                   NOEUDS OU DES POINTS D'INTEGRATION
C                                   DE L'ELEMENT)
C    NBSP           IN     I        NOMBRE DE SOUS-POINTS A TRAITER
C    MATEV(2,2,10)  IN     R        MATRICES DE PASSAGE DES REPERES
C                                   INTRINSEQUES AUX POINTS  DE
C                                   L'ELEMENT AU REPERE UTILISATEUR
C    TENSEL(1)      IN     R        VECTEUR DES EFFORTS GENERALISES
C                                   OU DES DEFORMATIONS-COURBURES
C                                   DANS LE REPERE INTRINSEQUE A
C                                   L'ELEMENT I.E.
C                                       NXX NYY NXY MXX MYY MXY VX VY
C                                   OU  EXX EYY EXY KXX KYY KXY GAX GAY
C    TENLOC(1)      OUT    R        VECTEUR DES EFFORTS GENERALISES
C                                   OU DES DEFORMATIONS-COURBURES
C                                   DANS LE REPERE UTILISATEUR
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           REAL*8            MATEV(2,2,1), TENSEL(1), TENLOC(1)
C -----  VARIABLES LOCALES
           REAL*8            NELEM(4),  MELEM(4), XAB(2,2)
           REAL*8            NLOCAL(4), MLOCAL(4)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- BOUCLE SUR LES POINTS OU SONT CALCULES LES VECTEURS
C --- (I.E. LES NOEUDS OU LES POINTS D'INTEGRATION) :
C     ============================================
      DO 100 ISP = 1 , NBSP
       DO 101 I = 1 , NP
         IDEC = 8*(ISP-1)+8*NBSP*(I-1)
C
         NELEM(1) = TENSEL(1+IDEC)
         NELEM(2) = TENSEL(3+IDEC)
         NELEM(3) = TENSEL(3+IDEC)
         NELEM(4) = TENSEL(2+IDEC)
C
         MELEM(1) = TENSEL(4+IDEC)
         MELEM(2) = TENSEL(6+IDEC)
         MELEM(3) = TENSEL(6+IDEC)
         MELEM(4) = TENSEL(5+IDEC)
C
         CALL UTBTAB ('ZERO',2,2,NELEM,MATEV(1,1,I),XAB,NLOCAL)
         CALL UTBTAB ('ZERO',2,2,MELEM,MATEV(1,1,I),XAB,MLOCAL)
C
         TENLOC(1+IDEC) = NLOCAL(1)
         TENLOC(2+IDEC) = NLOCAL(4)
         TENLOC(3+IDEC) = NLOCAL(2)
C
         TENLOC(4+IDEC) = MLOCAL(1)
         TENLOC(5+IDEC) = MLOCAL(4)
         TENLOC(6+IDEC) = MLOCAL(2)
C
         TENLOC(7+IDEC) = TENSEL(7+IDEC) * MATEV(1,1,I) +
     +                       TENSEL(8+IDEC) * MATEV(2,1,I)
         TENLOC(8+IDEC) = TENSEL(7+IDEC) * MATEV(1,2,I) +
     +                       TENSEL(8+IDEC) * MATEV(2,2,I)
C
  101  CONTINUE
 100  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
