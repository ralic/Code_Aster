      SUBROUTINE CGNOP0 (NBNOE, COOR, X0, VECNOR, PREC, NBNO, LISNOE )
      IMPLICIT   NONE
C.=====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 31/05/2000   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C.========================= DEBUT DES DECLARATIONS ====================
C
C -----  ARGUMENTS
      INTEGER       NBNOE, NBNO, LISNOE(*)
      REAL*8        COOR(*), X0(*), VECNOR(*), PREC
C
C --------- VARIABLES LOCALES ---------------------------
      INTEGER       INO
      REAL*8        X(3), XX0(3), D
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- PARCOURS DES NOEUDS DU MAILLAGE :
C     --------------------------------
      NBNO = 0
      DO 10 INO = 1, NBNOE
C
C ---     COORDONNEES DU NOEUD :
C         --------------------
           X(1) =  COOR(3*(INO-1)+1)
           X(2) =  COOR(3*(INO-1)+2)
           X(3) =  COOR(3*(INO-1)+3)
C
           XX0(1) = X(1) - X0(1)
           XX0(2) = X(2) - X0(2)
           XX0(3) = X(3) - X0(3)
C
C ---     CALCUL DE LA DISTANCE DU NOEUD COURANT AU PLAN OU A
C ---     LA DROITE :
C         ---------
           D    = XX0(1)*VECNOR(1) + XX0(2)*VECNOR(2) +
     +            XX0(3)*VECNOR(3)
C
C ---     SI LE NOEUD COURANT EST SITUE DANS LE PLAN OU LA DROITE,
C ---     ON L'AFFECTE A LA LISTE DE NOEUDS QUI SERA AFFECTEE
C ---     AU GROUP_NO :
C         ----------
           IF ( ABS(D) .LE. PREC ) THEN
               NBNO = NBNO + 1
               LISNOE(NBNO) = INO
           ENDIF
C
 10   CONTINUE
C.============================ FIN DE LA ROUTINE ======================
      END
