      SUBROUTINE LCEIBT (NDIMSI,EPS,EPSF,DEP,INVN,CN,DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/12/2003   AUTEUR PBADEL P.BADEL 
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
      IMPLICIT NONE
      REAL*8         EPS(6),EPSF(6),DEP(6,12),DSIDEP(6,6),INVN(6,6)
      REAL*8         CN(6,6)
      INTEGER        NDIMSI
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDO_ISOT_BETON - TERME COMPLEMENTAIRE DE
C                                           LA MATRICE TANGENTE POUR
C                                           LES LOIS COUPLES
C ----------------------------------------------------------------------
      INTEGER I,J,K,L
      REAL*8  SIGEL(6),SIGME(6),TEMP1(6,6)
      
      CALL R8INIR(NDIMSI,0.D0,SIGEL,1)
      CALL R8INIR(NDIMSI,0.D0,SIGME,1)
      CALL R8INIR(36,0.D0,TEMP1,1)
      
      DO 30 I=1,NDIMSI
        TEMP1(I,I)=TEMP1(I,I)+1.D0
        DO 30 J=1,NDIMSI
          DO 30 K=1,NDIMSI
            DO 30 L=1,NDIMSI
              TEMP1(I,J)=TEMP1(I,J)-DEP(I,K)*INVN(K,L)*CN(L,J)
30    CONTINUE
      
      DO 10 I=1,NDIMSI
        DO 10 J=1,NDIMSI
            SIGEL(I) = SIGEL(I) + DEP(I,J+6)*EPS(J)
            SIGME(I) = SIGME(I) + DEP(I,J+6)*(EPS(J)-EPSF(J))
10    CONTINUE

      DO 20 I=1,NDIMSI
        DO 20 J=1,NDIMSI
          DO 20 K=1,NDIMSI
          DSIDEP(I,J)=DSIDEP(I,J)-TEMP1(I,K)*SIGME(K)*SIGEL(J)
20    CONTINUE

      END
