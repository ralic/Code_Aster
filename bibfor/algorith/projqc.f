      SUBROUTINE PROJQC(DIAG,JEUPMJ,ITRIA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/03/2007   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      DIAG(2)      
      REAL*8       JEUPMJ(4)
      INTEGER      ITRIA       
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL - QUA)
C
C CHOIX DU TRIANGLE QUAND ON DECOUPE UN QUADRANGLE
C
C ----------------------------------------------------------------------
C
C
C IN  DIAG   : DETECTION SUR PSEUDO-DIAGONALE
C                 (1: SUR DIAGONALE, 0: NON)
C IN  JEUPMJ : DISTANCE SUR PM ENTRE NOEUD ESCALVE P ET TRAINGLE MAITRE
C IN  ITRIA  : TRIANGLE LE PLUS PROCHE DU NOEUD ESCLAVE
C
C ----------------------------------------------------------------------
C
      REAL*8  JEUMIN,ECAN,R8GAEM
      INTEGER K
C
C ----------------------------------------------------------------------
C

C
C --- ON CHOISIT LE TRIANGLE REALISANT LA PLUS PETITE DISTANCE
C --- AVANT CORRECTION. LA RELATION DE NON PENETRATION EST
C --- ECRITE ENTRE LE NOEUD ESCLAVE ET LES 3 SOMMETS DE CE TRIANGLE.
C
        JEUMIN = R8GAEM()
        DO 20 K = 1,4
          IF ((JEUPMJ(K).LE.JEUMIN).AND.
     &        (ABS(JEUPMJ(K)-JEUMIN).GT.1D-15)) THEN

           ITRIA = K
C
C --- GESTION DES CONFLITS EN CAS DE PROJECTION SUR DIAGONALES
C
           IF ((ITRIA.EQ.1).AND.DIAG(1).EQ.1) THEN
             IF (JEUPMJ(3).LT.JEUPMJ(4)) THEN
               ECAN = ABS((JEUPMJ(3)-JEUPMJ(1)))
               IF (ECAN.LE.1D-15) THEN
                 ITRIA = 3
               ELSE
                 ITRIA = 1
               ENDIF
             ELSE
               ECAN = ABS((JEUPMJ(4)-JEUPMJ(1)))
               IF (ECAN.LE.1D-15) THEN
                 ITRIA = 4
               ELSE
                 ITRIA = 1
               ENDIF
             ENDIF
           ENDIF

           IF ((ITRIA.EQ.2).AND.DIAG(1).EQ.1) THEN
             IF (JEUPMJ(3).LT.JEUPMJ(4)) THEN
               ECAN = ABS((JEUPMJ(3)-JEUPMJ(2)))
               IF (ECAN.LE.1D-15) THEN
                 ITRIA = 3
               ELSE
                 ITRIA = 2
               ENDIF
             ELSE
               ECAN = ABS((JEUPMJ(4)-JEUPMJ(2)))
               IF (ECAN.LE.1D-15) THEN
                 ITRIA = 4
               ELSE
                 ITRIA = 2
               ENDIF
             ENDIF
           ENDIF


           IF ((ITRIA.EQ.3).AND.DIAG(2).EQ.1) THEN
             IF (JEUPMJ(1).LT.JEUPMJ(2)) THEN
               ECAN = ABS((JEUPMJ(1)-JEUPMJ(3)))
               IF (ECAN.LE.1D-15) THEN
                 ITRIA = 1
               ELSE
                 ITRIA = 3
               ENDIF
             ELSE
               ECAN = ABS((JEUPMJ(2)-JEUPMJ(3)))
               IF (ECAN.LE.1D-15) THEN
                 ITRIA = 2
               ELSE
                 ITRIA = 3
               ENDIF
             ENDIF
           ENDIF

           IF ((ITRIA.EQ.4).AND.DIAG(2).EQ.1) THEN
             IF (JEUPMJ(1).LT.JEUPMJ(2)) THEN
               ECAN = ABS((JEUPMJ(1)-JEUPMJ(4)))
               IF (ECAN.LE.1D-15) THEN
                 ITRIA = 1
               ELSE
                 ITRIA = 4
               ENDIF
             ELSE
               ECAN = ABS((JEUPMJ(2)-JEUPMJ(4)))
               IF (ECAN.LE.1D-15) THEN
                 ITRIA = 2
               ELSE
                 ITRIA = 4
               ENDIF
             ENDIF
           ENDIF
           IF ((DIAG(1).EQ.1).AND.(DIAG(2).EQ.1)) THEN
             ITRIA = 1
           ENDIF
           JEUMIN = JEUPMJ(ITRIA)
          END IF
 20     CONTINUE
C 
        IF (ITRIA.EQ.0) THEN
          CALL U2MESS('F','CONTACT_31')
        ENDIF      
C
      END
