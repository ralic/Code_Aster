      SUBROUTINE PRDIMA(MATYP ,LAMBDA,LAMOLD,TOLEIN,
     &                  ARETE ,NOEUD)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/09/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT     NONE
      CHARACTER*4  MATYP
      REAL*8       LAMOLD(3) 
      REAL*8       LAMBDA(3)            
      REAL*8       TOLEIN
      INTEGER      ARETE(4)
      INTEGER      NOEUD(4)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL)
C
C VERIFIE LES PROJECTIONS SUIVANTS ENTITES GEOMETRIQUES SI DIAGNOSTIC
C FIN
C
C ----------------------------------------------------------------------
C
C
C NB: POUR LE QUADRANGLE, LE TEST  SE FAIT DANS PROJQU DIRECTEMENT
C
C IN  MATYP  : TYPE DE LA MAILLE MAITRE
C                -> SEG2,SEG3,TRI3,TRI6,QUA4,QUA8,QUA9
C IN  LAMOLD : COORDONNEES PARAMETRIQUES DE LA "PROJECTION" M AVANT
C                RABATTEMENT DANS LA MAILLE SI ON A DEPASSE
C IN  LAMBDA : COORDONNEES PARAMETRIQUES DE LA PROJECTION M APRES
C                RABATTEMENT DANS LA MAILLE SI ON A DEPASSE
C IN  TOLEIN : TOLERANCE <IN> POUR LA PROJECTION GEOMETRIQUE
C              DETERMINE SI PROJECTION SUR ARETE OU NOEUD
C OUT NOEUD  : DETECTION DE PROJECTION SUR NOEUD 
C                 (1: SUR LE NOEUD, 0: NON)
C               SEGMENT AB   -  NOEUD(1) : NOEUD A
C                               NOEUD(2) : NOEUD B
C               TRIANGLE ABC -  NOEUD(1) : NOEUD A
C                               NOEUD(2) : NOEUD B
C                               NOEUD(3) : NOEUD C
C               QUAD    ABCD -  NOEUD(1) : NOEUD A
C                               NOEUD(2) : NOEUD B
C                               NOEUD(3) : NOEUD C
C                               NOEUD(4) : NOEUD D
C OUT ARETE  : DETECTION DE PROJECTION SUR ARETE
C                 (1: SUR L'ARETE, 0: NON)
C               TRIANGLE ABC -  ARETE(1) : SEGMENT AB
C                               ARETE(2) : SEGMENT BC
C                               ARETE(3) : SEGMENT AC
C               QUAD    ABCD -  ARETE(1) : SEGMENT AB
C                               ARETE(2) : SEGMENT BC
C                               ARETE(3) : SEGMENT CD
C                               ARETE(3) : SEGMENT DA
C
C ----------------------------------------------------------------------
C
      INTEGER K
C
C ----------------------------------------------------------------------
C
      DO 1 K = 1,4
        ARETE(K) = 0
        NOEUD(K) = 0
   1  CONTINUE 
C
      IF (MATYP(1:3).EQ.'SEG') THEN
        CALL PRDISE(LAMBDA,TOLEIN,NOEUD)
      ELSE IF (MATYP(1:3).EQ.'TRI') THEN
        CALL PRDITR(LAMOLD,TOLEIN,ARETE,NOEUD)
      ELSE IF (MATYP(1:3).EQ.'QUA') THEN
C
C --- FAIT DANS LA ROUTINE PROJQU DIRECTEMENT
C        
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C

      END
