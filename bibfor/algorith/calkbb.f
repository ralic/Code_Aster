      SUBROUTINE CALKBB(NNO,NDIM,W,DEF,DSIDEP,KBB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/02/2013   AUTEUR SFAYOLLE S.FAYOLLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE SFAYOLLE S.FAYOLLE
C TOLE CRS_1404
      IMPLICIT NONE

      INTEGER      NDIM,NNO
      REAL*8       W,DEF(2*NDIM,NNO,NDIM)
      REAL*8       KBB(NDIM,NDIM),DSIDEP(2*NDIM,2*NDIM)
C-----------------------------------------------------------------------
C     BUT:  CALCUL DE LA MATRICE DE RAIDEUR LIEE A LA BULLE KBB
C-----------------------------------------------------------------------
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  W      : POIDS DU POINT DE GAUSS
C IN  DEF    : MATRICE B
C IN  DSIDEP : MATRICE TANGENTE COHERENTE POUR LA PARTIE BULLE
C OUT KBB    : MATRICE KBB
C-----------------------------------------------------------------------

      INTEGER      IA,JA,NA,KL,PQ
      REAL*8       T1
      REAL*8       PBULLE
      REAL*8       DEVD(2*NDIM,2*NDIM)
      REAL*8       DDDEV(2*NDIM,2*NDIM)
      REAL*8       IDEV(6,6),IDEV2(4,4)

      DATA         IDEV2/ 2.D0,-1.D0,-1.D0, 0.D0,
     &                   -1.D0, 2.D0,-1.D0, 0.D0,
     &                   -1.D0,-1.D0, 2.D0, 0.D0,
     &                    0.D0, 0.D0, 0.D0, 3.D0/
      DATA         IDEV / 2.D0,-1.D0,-1.D0, 0.D0, 0.D0, 0.D0,
     &                   -1.D0, 2.D0,-1.D0, 0.D0, 0.D0, 0.D0,
     &                   -1.D0,-1.D0, 2.D0, 0.D0, 0.D0, 0.D0,
     &                    0.D0, 0.D0, 0.D0, 3.D0, 0.D0, 0.D0,
     &                    0.D0, 0.D0, 0.D0, 0.D0, 3.D0, 0.D0,
     &                    0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 3.D0/
C-----------------------------------------------------------------------

C - INITIALISATION
      CALL R8INIR(NDIM*NDIM,0.D0,KBB,1)

C - CAS 2D
      IF(NDIM .EQ. 3)THEN
        PBULLE = 4.D0
        CALL PMAT(6,IDEV/3.D0,DSIDEP,DEVD)
        CALL PMAT(6,DEVD,IDEV/3.D0,DDDEV)
      ELSEIF(NDIM .EQ. 2)THEN
        PBULLE = 3.D0
        CALL PMAT(4,IDEV2/3.D0,DSIDEP,DEVD)
        CALL PMAT(4,DEVD,IDEV2/3.D0,DDDEV)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

C - CALCUL DE LA MATRICE KBB
      DO 105 NA = 1,NNO
        DO 104 IA = 1,NDIM
          DO 102 JA = 1,NDIM
            T1 = 0.D0
            DO 101 KL = 1,2*NDIM
              DO 100 PQ = 1,2*NDIM
                T1 = T1 + DEF(KL,NA,IA)*DDDEV(KL,PQ)*DEF(PQ,NA,JA)
 100          CONTINUE
 101        CONTINUE
            KBB(IA,JA) = KBB(IA,JA) + W*T1*PBULLE
 102      CONTINUE
 104    CONTINUE
 105  CONTINUE

      END
