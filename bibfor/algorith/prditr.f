      SUBROUTINE PRDITR(LAMOLD,TOLEIN,ARETE,NOEUD)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/03/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8    LAMOLD(3)
      REAL*8    TOLEIN
      INTEGER   ARETE(4)
      INTEGER   NOEUD(4)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL - TRI)
C
C DIAGNOSTIC GEOMETRIQUE FIN LORS DE LA PROJECTION SUR UN TRIANGLE
C
C ----------------------------------------------------------------------
C
C
C IN  LAMOLD : COORDONNEES PARAMETRIQUES DE LA "PROJECTION" M AVANT
C                RABATTEMENT DANS LA MAILLE SI ON A DEPASSE
C IN  TOLEIN : TOLERANCE <IN> POUR LA PROJECTION GEOMETRIQUE
C              DETERMINE SI PROJECTION SUR ARETE OU NOEUD
C OUT ARETE  : DETECTION DE PROJECTION SUR ARETE
C                 (1: SUR L'ARETE, 0: NON)
C              ARETE(1) : SEGMENT AB
C              ARETE(2) : SEGMENT BC
C              ARETE(3) : SEGMENT AC
C OUT NOEUD  : DETECTION DE PROJECTION SUR NOEUD 
C                 (1: SUR LE NOEUD, 0: NON)
C              NOEUD(1) : NOEUD A
C              NOEUD(2) : NOEUD B
C              NOEUD(3) : NOEUD C
C
C ----------------------------------------------------------------------
C
      REAL*8  KSI1P,KSI2P,KSI3P
      INTEGER K
C
C ----------------------------------------------------------------------
C

C
C --- INITIALISATIONS
C      
      DO 1 K = 1,3
        ARETE(K) = 0
        NOEUD(K) = 0
1     CONTINUE  
      KSI1P    = LAMOLD(1)
      KSI2P    = LAMOLD(2)
      KSI3P    = LAMOLD(3)
C
C --- PROJECTION SUR ARETES
C      
      IF (ABS(KSI1P).LE.TOLEIN) THEN
        ARETE(3) = 1    
      END IF
      IF (ABS(KSI2P).LE.TOLEIN) THEN
        ARETE(1) = 1    
      END IF
      IF (ABS(KSI3P).LE.TOLEIN) THEN
        ARETE(2) = 1    
      END IF
C
C --- PROJECTION SUR NOEUDS
C        
      IF ((ARETE(1).EQ.1).AND.
     &    (ARETE(2).EQ.1)) THEN
        NOEUD(1) = 1    
      END IF
      IF ((ARETE(2).EQ.1).AND.
     &    (ARETE(3).EQ.1)) THEN
        NOEUD(2) = 1    
      END IF
      IF ((ARETE(1).EQ.1).AND.
     &    (ARETE(3).EQ.1)) THEN
        NOEUD(3) = 1    
      END IF

      END
