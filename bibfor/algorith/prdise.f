      SUBROUTINE PRDISE(LAMBDA,
     &                  TOLEIN,NOEUD)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/10/2004   AUTEUR MABBAS M.ABBAS 
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
      IMPLICIT NONE
C
      REAL*8  LAMBDA

      REAL*8    TOLEIN
      INTEGER   NOEUD(2)
 



C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : PROJSE
C ----------------------------------------------------------------------
C
C DIAGNOSTIC GEOMETRIQUE FIN POUR LA PROJECTION SUR UN SEGMENT
C
C IN  LAMBDA : PARAMETRE DE PROJECTION SUR LE SEGMENT
C IN  TOLEIN : TOLERANCE <IN> POUR LA PROJECTION GEOMETRIQUE
C              DETERMINE SI PROJECTION SUR ARETE OU NOEUD
C OUT NOEUD  : DETECTION DE PROJECTION SUR NOEUD 
C                 (1: SUR LE NOEUD, 0: NON)
C              NOEUD(1) : NOEUD A
C              NOEUD(2) : NOEUD B
C
C ----------------------------------------------------------------------
C

        NOEUD(1) = 0
        NOEUD(2) = 0

        IF (LAMBDA.LE.TOLEIN) THEN
            NOEUD(1) = 1 
        END IF
        IF (LAMBDA.GE.(1.D0-TOLEIN))    THEN
            NOEUD(2) = 1    
        END IF


      END
