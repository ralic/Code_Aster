      SUBROUTINE BURAFI(VIN,NVI,MATERD,MATERF,
     &                  NMAT,TIMED,TIMEF,AFI,BFI,CFI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2011   AUTEUR FOUCAULT A.FOUCAULT 
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C=======================================================================
C
C ROUTINE QUI CALCULE LES MATRICES DE DEFORMATION 
C  DE FLUAGE PROPRE SPHERIQUE ET DEVIATORIQUE IRREVERSIBLE LINEARISEE
C
C IN  VIN      : VARIABLES INTERNES INITIALES
C     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
C     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
C     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
C     NMAT     : DIMENSION DE CMAT
C     TIMED    : INSTANT T
C     TIMEF    : INSTANT T+DT
C OUT AFI      : VECTEUR LIE A LA DEFOR. IRREV. DE FLUAGE PROPRE 
C     BFI      : MATRICE LIEE A LA DEFOR. IRREV. DE FLUAGE PROPRE 
C     CFI      : MATRICE LIEE A LA DEFOR. IRREV. DE FLUAGE PROPRE 
C=======================================================================
      IMPLICIT NONE
      INTEGER         NVI,NMAT,NDT,NDI,I,J
      REAL*8          VIN(*)
      REAL*8          MATERD(NMAT,2),MATERF(NMAT,2)
      REAL*8          TIMED,TIMEF
      REAL*8          AFI(6),BFI(6,6),CFI(6,6)
      REAL*8          BFIS,CFIS
      REAL*8          BFID,CFID

      COMMON /TDIM/   NDT,NDI

C === =================================================================
C INITIALISATION DES VARIABLES
C === =================================================================
      DO 1 I=1,NDT
        AFI(I)  = 0.D0
        DO 2 J = 1, NDT
          BFI(I,J) = 0.D0
          CFI(I,J) = 0.D0
 2      CONTINUE
 1    CONTINUE
C === =================================================================
C CALCUL DE LA MATRICE DES DEFORMATIONS IRREVERSIBLES LINEARISEE
C          DE FLUAGE PROPRE SPHERIQUE INCREMENTALES
C === =================================================================
      CALL BURAIL(VIN,NVI,MATERD,MATERF,NMAT,
     &            TIMED,TIMEF,'SPH',BFIS,CFIS)
C === =================================================================
C CALCUL DE LA MATRICE DES DEFORMATIONS IRREVERSIBLES LINEARISEE
C          DE FLUAGE PROPRE DEVIATORIQUE INCREMENTALES
C === =================================================================
      CALL BURAIL(VIN,NVI,MATERD,MATERF,NMAT,
     &            TIMED,TIMEF,'DEV',BFID,CFID)
C === =================================================================
C CONSTRUCTION DE LA MATRICE DES DEFORMATIONS IRREVERSIBLES 
C          DE FLUAGE PROPRE INCREMENTALES
C === =================================================================
      DO 3 I = 1, NDI 
        BFI(I,I) = (BFIS+2.D0*BFID)/3.D0
        CFI(I,I) = (CFIS+2.D0*CFID)/3.D0
        BFI(I+NDI,I+NDI) = BFID
        CFI(I+NDI,I+NDI) = CFID
 3    CONTINUE 
      BFI(1,2) = (BFIS-BFID)/3
      CFI(1,2) = (CFIS-CFID)/3
      BFI(2,1) = BFI(1,2)
      CFI(2,1) = CFI(1,2)
      BFI(3,1) = BFI(1,2)
      CFI(3,1) = CFI(1,2)
      BFI(1,3) = BFI(1,2)
      CFI(1,3) = CFI(1,2)
      BFI(2,3) = BFI(1,2)
      CFI(2,3) = CFI(1,2)
      BFI(3,2) = BFI(1,2)
      CFI(3,2) = CFI(1,2)
      
      END
