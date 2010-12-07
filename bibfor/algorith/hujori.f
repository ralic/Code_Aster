      SUBROUTINE  HUJORI(SENS, NMAT, REORIE, ANGL, VEC, MAT)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/05/2008   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C======================================================================
C
C      DPASSA  -- CALCUL DE LA MATRICE DE PASSAGE DU REPERE
C                 D'ORTHOTROPIE AU REPERE GLOBAL POUR LE 
C                 TENSEUR D'ELASTICITE
C                 CETTE MATRICE EST CONSTRUITE EN PARTANT
C                 DE LA CONSIDERATION QUE L'ENERGIE DE DEFORMATION
C                 EXPRIMEE DANS LE REPERE GLOBAL EST EGALE A
C                 L'ENERGIE DE DEFORMATION EXPRIMEE DANS LE REPERE
C                 D'ORTHOTROPIE
C
C   ARGUMENT        E/S  TYPE         ROLE
C    XYZGAU(3)      IN     R        COORDONNEES DU POINT D'INTEGRATION
C                                   COURANT
C    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    IREP           OUT    I        = 0 
C                                     SI LE CHANGEMENT DE REPERE EST
C                                     TRIVIAL (I.E. PASSAG = IDENTITE)
C                                   = 1 SINON
C    PASSAG(6,6)    OUT    R        MATRICE DE PASSAGE DU REPERE
C                                   D'ORTHOTROPIE AU REPERE GLOBAL
C                                   POUR LE TENSEUR D'ELASTICITE
C
C========================= DEBUT DES DECLARATIONS ====================
      INTEGER     I, J, NMAT
      REAL*8      ZERO, DEUX, DSQR, ISQR
      REAL*8      ANGL(3), P(3,3), PASSAG(6,6), PASSAL(6,6)
      REAL*8      VEC1(6), VEC(6), MAT(6,6), MAT1(6,6), WORK(6,6)
      CHARACTER*5 SENS
      LOGICAL     REORIE
      
      DATA   ZERO  / 0.D0 /
      DATA   DEUX  / 2.D0 /
      DATA   DSQR  / 1.41421356237D0 /
      DATA   ISQR  / .707106781187D0 /

      IF (.NOT.REORIE) GOTO 9999
      
      DO 20 I = 1, 3
      DO 20 J = 1, 3
        P(I,J) = ZERO
 20     CONTINUE
 
      DO 21 I = 1, 6
        VEC1(I) = ZERO
 21     CONTINUE


C ----   CONSTRUCTION DE LA MATRICE DE PASSAGE (POUR DES VECTEURS)
C ----   DU REPERE D'ORTHOTROPIE AU REPERE GLOBAL
C        ----------------------------------------
      CALL MATROT(ANGL, P)


C calcul de PASSAGT * SIG *PASSAG et PASSAGT * DEPS *PASSAG
      IF (NMAT.EQ.1) THEN
      
        IF (SENS.EQ.'LOCAL') THEN
        
          PASSAL(1,1) = P(1,1)*P(1,1)
          PASSAL(1,2) = P(1,2)*P(1,2)
          PASSAL(1,3) = P(1,3)*P(1,3)
          PASSAL(1,4) = DEUX*ISQR*P(1,1)*P(1,2)
          PASSAL(1,5) = DEUX*ISQR*P(1,1)*P(1,3)
          PASSAL(1,6) = DEUX*ISQR*P(1,2)*P(1,3)

          PASSAL(2,1) = P(2,1)*P(2,1)
          PASSAL(2,2) = P(2,2)*P(2,2)
          PASSAL(2,3) = P(2,3)*P(2,3)
          PASSAL(2,4) = DEUX*ISQR*P(2,1)*P(2,2)
          PASSAL(2,5) = DEUX*ISQR*P(2,1)*P(2,3)
          PASSAL(2,6) = DEUX*ISQR*P(2,2)*P(2,3)

          PASSAL(3,1) = P(3,1)*P(3,1)
          PASSAL(3,2) = P(3,2)*P(3,2)
          PASSAL(3,3) = P(3,3)*P(3,3)
          PASSAL(3,4) = DEUX*ISQR*P(3,1)*P(3,2)
          PASSAL(3,5) = DEUX*ISQR*P(3,1)*P(3,3)
          PASSAL(3,6) = DEUX*ISQR*P(3,2)*P(3,3)

          PASSAL(4,1) = DSQR*P(1,1)*P(2,1)
          PASSAL(4,2) = DSQR*P(1,2)*P(2,2)
          PASSAL(4,3) = DSQR*P(1,3)*P(2,3)
          PASSAL(4,4) = P(1,1)*P(2,2) + P(1,2)*P(2,1)
          PASSAL(4,5) = P(1,1)*P(2,3) + P(1,3)*P(2,1)
          PASSAL(4,6) = P(1,2)*P(2,3) + P(1,3)*P(2,2)

          PASSAL(5,1) = DSQR*P(1,1)*P(3,1)
          PASSAL(5,2) = DSQR*P(1,2)*P(3,2)
          PASSAL(5,3) = DSQR*P(1,3)*P(3,3)
          PASSAL(5,4) = P(1,1)*P(3,2) + P(1,2)*P(3,1)
          PASSAL(5,5) = P(1,1)*P(3,3) + P(1,3)*P(3,1)
          PASSAL(5,6) = P(1,2)*P(3,3) + P(1,3)*P(3,2)

          PASSAL(6,1) = DSQR*P(2,1)*P(3,1)
          PASSAL(6,2) = DSQR*P(2,2)*P(3,2)
          PASSAL(6,3) = DSQR*P(2,3)*P(3,3)
          PASSAL(6,4) = P(2,1)*P(3,2) + P(2,2)*P(3,1)
          PASSAL(6,5) = P(2,1)*P(3,3) + P(2,3)*P(3,1)
          PASSAL(6,6) = P(2,2)*P(3,3) + P(2,3)*P(3,2)

          DO 22 I = 1,6
            DO 22 J = 1,6
              VEC1(I) = VEC1(I) + PASSAL(I,J)*VEC(J)
 22           CONTINUE
 
        ELSEIF (SENS.EQ.'GLOBA') THEN
        
          PASSAG(1,1) = P(1,1)*P(1,1)
          PASSAG(1,2) = P(2,1)*P(2,1)
          PASSAG(1,3) = P(3,1)*P(3,1)
          PASSAG(1,4) = DEUX*ISQR*P(1,1)*P(2,1)
          PASSAG(1,5) = DEUX*ISQR*P(1,1)*P(3,1)
          PASSAG(1,6) = DEUX*ISQR*P(2,1)*P(3,1)

          PASSAG(2,1) = P(1,2)*P(1,2)
          PASSAG(2,2) = P(2,2)*P(2,2)
          PASSAG(2,3) = P(3,2)*P(3,2)
          PASSAG(2,4) = DEUX*ISQR*P(1,2)*P(2,2)
          PASSAG(2,5) = DEUX*ISQR*P(1,2)*P(3,2)
          PASSAG(2,6) = DEUX*ISQR*P(2,2)*P(3,2)

          PASSAG(3,1) = P(1,3)*P(1,3)
          PASSAG(3,2) = P(2,3)*P(2,3)
          PASSAG(3,3) = P(3,3)*P(3,3)
          PASSAG(3,4) = DEUX*ISQR*P(1,3)*P(2,3)
          PASSAG(3,5) = DEUX*ISQR*P(1,3)*P(3,3)
          PASSAG(3,6) = DEUX*ISQR*P(2,3)*P(3,3)

          PASSAG(4,1) = DSQR*P(1,1)*P(1,2)
          PASSAG(4,2) = DSQR*P(2,1)*P(2,2)
          PASSAG(4,3) = DSQR*P(3,1)*P(3,2)
          PASSAG(4,4) = P(1,1)*P(2,2) + P(2,1)*P(1,2)
          PASSAG(4,5) = P(1,1)*P(3,2) + P(3,1)*P(1,2)
          PASSAG(4,6) = P(2,1)*P(3,2) + P(3,1)*P(2,2)

          PASSAG(5,1) = DSQR*P(1,1)*P(1,3)
          PASSAG(5,2) = DSQR*P(2,1)*P(2,3)
          PASSAG(5,3) = DSQR*P(3,1)*P(3,3)
          PASSAG(5,4) = P(1,1)*P(2,3) + P(2,1)*P(1,3)
          PASSAG(5,5) = P(1,1)*P(3,3) + P(3,1)*P(1,3)
          PASSAG(5,6) = P(2,1)*P(3,3) + P(3,1)*P(2,3)

          PASSAG(6,1) = DSQR*P(1,2)*P(1,3)
          PASSAG(6,2) = DSQR*P(2,2)*P(2,3)
          PASSAG(6,3) = DSQR*P(3,2)*P(3,3)
          PASSAG(6,4) = P(1,2)*P(2,3) + P(2,2)*P(1,3)
          PASSAG(6,5) = P(1,2)*P(3,3) + P(3,2)*P(1,3)
          PASSAG(6,6) = P(2,2)*P(3,3) + P(3,2)*P(2,3)

          DO 23 I = 1,6
            DO 23 J = 1,6
              VEC1(I) = VEC1(I) + PASSAG(I,J)*VEC(J)
 23           CONTINUE

        ENDIF

        DO 25 I = 1,6
          VEC(I) = VEC1(I)
 25       CONTINUE
        

C calcul de PASSAG * DSDE *PASSAL et PASSAG * DEPS *PASSAL
      ELSEIF (NMAT.EQ.2) THEN
      
        PASSAL(1,1) = P(1,1)*P(1,1)
        PASSAL(1,2) = P(1,2)*P(1,2)
        PASSAL(1,3) = P(1,3)*P(1,3)
        PASSAL(1,4) = DEUX*ISQR*P(1,1)*P(1,2)
        PASSAL(1,5) = DEUX*ISQR*P(1,1)*P(1,3)
        PASSAL(1,6) = DEUX*ISQR*P(1,2)*P(1,3)

        PASSAL(2,1) = P(2,1)*P(2,1)
        PASSAL(2,2) = P(2,2)*P(2,2)
        PASSAL(2,3) = P(2,3)*P(2,3)
        PASSAL(2,4) = DEUX*ISQR*P(2,1)*P(2,2)
        PASSAL(2,5) = DEUX*ISQR*P(2,1)*P(2,3)
        PASSAL(2,6) = DEUX*ISQR*P(2,2)*P(2,3)

        PASSAL(3,1) = P(3,1)*P(3,1)
        PASSAL(3,2) = P(3,2)*P(3,2)
        PASSAL(3,3) = P(3,3)*P(3,3)
        PASSAL(3,4) = DEUX*ISQR*P(3,1)*P(3,2)
        PASSAL(3,5) = DEUX*ISQR*P(3,1)*P(3,3)
        PASSAL(3,6) = DEUX*ISQR*P(3,2)*P(3,3)

        PASSAL(4,1) = DSQR*P(1,1)*P(2,1)
        PASSAL(4,2) = DSQR*P(1,2)*P(2,2)
        PASSAL(4,3) = DSQR*P(1,3)*P(2,3)
        PASSAL(4,4) = P(1,1)*P(2,2) + P(1,2)*P(2,1)
        PASSAL(4,5) = P(1,1)*P(2,3) + P(1,3)*P(2,1)
        PASSAL(4,6) = P(1,2)*P(2,3) + P(1,3)*P(2,2)

        PASSAL(5,1) = DSQR*P(1,1)*P(3,1)
        PASSAL(5,2) = DSQR*P(1,2)*P(3,2)
        PASSAL(5,3) = DSQR*P(1,3)*P(3,3)
        PASSAL(5,4) = P(1,1)*P(3,2) + P(1,2)*P(3,1)
        PASSAL(5,5) = P(1,1)*P(3,3) + P(1,3)*P(3,1)
        PASSAL(5,6) = P(1,2)*P(3,3) + P(1,3)*P(3,2)

        PASSAL(6,1) = DSQR*P(2,1)*P(3,1)
        PASSAL(6,2) = DSQR*P(2,2)*P(3,2)
        PASSAL(6,3) = DSQR*P(2,3)*P(3,3)
        PASSAL(6,4) = P(2,1)*P(3,2) + P(2,2)*P(3,1)
        PASSAL(6,5) = P(2,1)*P(3,3) + P(2,3)*P(3,1)
        PASSAL(6,6) = P(2,2)*P(3,3) + P(2,3)*P(3,2)
        
        PASSAG(1,1) = P(1,1)*P(1,1)
        PASSAG(1,2) = P(2,1)*P(2,1)
        PASSAG(1,3) = P(3,1)*P(3,1)
        PASSAG(1,4) = DEUX*ISQR*P(1,1)*P(2,1)
        PASSAG(1,5) = DEUX*ISQR*P(1,1)*P(3,1)
        PASSAG(1,6) = DEUX*ISQR*P(2,1)*P(3,1)
        
        PASSAG(2,1) = P(1,2)*P(1,2)
        PASSAG(2,2) = P(2,2)*P(2,2)
        PASSAG(2,3) = P(3,2)*P(3,2)
        PASSAG(2,4) = DEUX*ISQR*P(1,2)*P(2,2)
        PASSAG(2,5) = DEUX*ISQR*P(1,2)*P(3,2)
        PASSAG(2,6) = DEUX*ISQR*P(2,2)*P(3,2)
        
        PASSAG(3,1) = P(1,3)*P(1,3)
        PASSAG(3,2) = P(2,3)*P(2,3)
        PASSAG(3,3) = P(3,3)*P(3,3)
        PASSAG(3,4) = DEUX*ISQR*P(1,3)*P(2,3)
        PASSAG(3,5) = DEUX*ISQR*P(1,3)*P(3,3)
        PASSAG(3,6) = DEUX*ISQR*P(2,3)*P(3,3)
        
        PASSAG(4,1) = DSQR*P(1,1)*P(1,2)
        PASSAG(4,2) = DSQR*P(2,1)*P(2,2)
        PASSAG(4,3) = DSQR*P(3,1)*P(3,2)
        PASSAG(4,4) = P(1,1)*P(2,2) + P(2,1)*P(1,2)
        PASSAG(4,5) = P(1,1)*P(3,2) + P(3,1)*P(1,2)
        PASSAG(4,6) = P(2,1)*P(3,2) + P(3,1)*P(2,2)
        
        PASSAG(5,1) = DSQR*P(1,1)*P(1,3)
        PASSAG(5,2) = DSQR*P(2,1)*P(2,3)
        PASSAG(5,3) = DSQR*P(3,1)*P(3,3)
        PASSAG(5,4) = P(1,1)*P(2,3) + P(2,1)*P(1,3)
        PASSAG(5,5) = P(1,1)*P(3,3) + P(3,1)*P(1,3)
        PASSAG(5,6) = P(2,1)*P(3,3) + P(3,1)*P(2,3)
        
        PASSAG(6,1) = DSQR*P(1,2)*P(1,3)
        PASSAG(6,2) = DSQR*P(2,2)*P(2,3)
        PASSAG(6,3) = DSQR*P(3,2)*P(3,3)
        PASSAG(6,4) = P(1,2)*P(2,3) + P(2,2)*P(1,3)
        PASSAG(6,5) = P(1,2)*P(3,3) + P(3,2)*P(1,3)
        PASSAG(6,6) = P(2,2)*P(3,3) + P(3,2)*P(2,3)
      
        IF (SENS.EQ.'LOCAL') THEN

          CALL PMAT(6,MAT,PASSAG,WORK)
          CALL PMAT(6,PASSAL,WORK,MAT1)
          
        ELSEIF (SENS.EQ.'GLOBA') THEN

          CALL PMAT(6,MAT,PASSAL,WORK)
          CALL PMAT(6,PASSAG,WORK,MAT1)
          
        ENDIF
        
        DO 30 J = 1,6
          DO 30 I = 1,6
            MAT(I,J) = MAT1(I,J)
 30         CONTINUE
 
      ENDIF

 9999 CONTINUE
      END
