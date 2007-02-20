      SUBROUTINE NIINIT(NOMTE,TYPMOD,NDIM,NNO1,NNO2,NNO3,VU,VG,VP)
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR MICHEL S.MICHEL 
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
      IMPLICIT NONE
      
      CHARACTER*8  TYPMOD(*)
      CHARACTER*16 NOMTE,ALIAS
      INTEGER NDIM,NNO1,NNO2,NNO3,VU(3,27),VG(27),VP(27),IRET
C ----------------------------------------------------------------------
C        INITIALISATION POUR LES ELEMENTS QUASI-INCOMPRESSIBLES
C ----------------------------------------------------------------------
C IN  NOMTE     NOM DE L'ELEMENT
C IN  TYPMOD    TYPE DE MODELISATION  
C IN  NDIM      DIMENSION DE L'ESPACE
C IN  NNO1      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES DEPLACEMENTS
C IN  NNO2      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES GONFELEMENTS
C IN  NNO3      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES PRESSIONS
C OUT VU
C OUT VG
C OUT VP
C ----------------------------------------------------------------------
      INTEGER N,I,OS
C ----------------------------------------------------------------------

      CHARACTER*11 CHOIX


C    RECUPERATION DU TYPE D'ELEMENT VIA L'ALIAS      
      CALL TEATTR(' ','S','ALIAS8',ALIAS,IRET)
      CHOIX = ' '
      IF (ALIAS(6:8).EQ.'T10')  CHOIX = '3D-P2.P1.P2'
      IF (ALIAS(6:8).EQ.'H20')  CHOIX = '3D-P2.P1.P2'
      IF (ALIAS(6:8).EQ.'TR6')  CHOIX = '2D-P2.P1.P2'
      IF (ALIAS(6:8).EQ.'QU8')  CHOIX = '2D-P2.P1.P2'
C      IF (ALIAS(6:8).EQ.'T14')  CHOIX = '3D-P2.P1.CR'
C      IF (ALIAS(6:8).EQ.'TR7')  CHOIX = '2D-P2.P0.P0'
      
      IF (CHOIX.EQ.'3D-P2.P1.P2') THEN
        DO 201 N = 1,NNO2
          VU(1,N) = 1 + (N-1)*5
          VU(2,N) = 2 + (N-1)*5
          VU(3,N) = 3 + (N-1)*5          
          VP(N)   = 4 + (N-1)*5
          VG(N)   = 5 + (N-1)*5
 201    CONTINUE
        OS = 5*NNO2
        DO 204 N = 1,NNO1-NNO2
          VU(1,N+NNO2) = 1 + (N-1)*4 + OS
          VU(2,N+NNO2) = 2 + (N-1)*4 + OS
          VU(3,N+NNO2) = 3 + (N-1)*4 + OS
          VP(  N+NNO2) = 4 + (N-1)*4 + OS
 204    CONTINUE
        GOTO 1000
      END IF
      
      IF (CHOIX.EQ.'2D-P2.P1.P2') THEN
        DO 110 N = 1,NNO2
          VU(1,N) = 1 + (N-1)*4
          VU(2,N) = 2 + (N-1)*4
          VU(3,N) = 0
          VP(N)   = 3 + (N-1)*4
          VG(N)   = 4 + (N-1)*4
 110    CONTINUE
        OS = 4*NNO2
        DO 140 N = 1,NNO1-NNO2
          VU(1,N+NNO2) = 1 + (N-1)*3 + OS
          VU(2,N+NNO2) = 2 + (N-1)*3 + OS
          VU(3,N) = 0
          VP(N+NNO2)   = 3 + (N-1)*3 + OS
 140    CONTINUE
        GOTO 1000
      END IF

C       IF (CHOIX.EQ.'2D-P2.P1.P1') THEN
C         DO 10 N = 1,NNO2
C           VU(1,N) = 1 + (N-1)*4
C           VU(2,N) = 2 + (N-1)*4
C           VP(N)   = 3 + (N-1)*4
C           VG(N)   = 4 + (N-1)*4
C  10     CONTINUE
C         OS = 4*NNO2
C         DO 40 N = 1,NNO1-NNO2
C           VU(1,N+NNO2) = 1 + (N-1)*2 + OS
C           VU(2,N+NNO2) = 2 + (N-1)*2 + OS
C  40     CONTINUE
C         GOTO 1000
C       END IF
            
C       IF (CHOIX.EQ.'3D-P2.P1.CR') THEN
C         DO 1 N = 1,NNO2
C           DO 2 I = 1,NDIM
C             VU(I,N) = I + (N-1)*(NDIM+1)
C  2        CONTINUE
C           VG(N) = 1 + NDIM + (N-1)*(NDIM+1)
C  1      CONTINUE
C         OS = (1+NDIM)*NNO2
C         DO 4 N = 1,NNO1-NNO2
C           DO 5 I = 1,NDIM
C             VU(I,N+NNO2) = I + (N-1)*(NDIM) + OS
C  5        CONTINUE
C  4      CONTINUE
C         OS = NNO1*NDIM + NNO2
C         DO 6 N = 1,NNO3
C           VP(N) = OS + N
C  6      CONTINUE      
C         GOTO 1000
C       END IF

      
C       IF (CHOIX.EQ.'2D-P2.P1.CR') THEN
C         DO 11 N = 1,NNO2
C           VU(1,N) = 1 + (N-1)*3
C           VU(2,N) = 2 + (N-1)*3
C           VG(N)   = 3 + (N-1)*3
C  11     CONTINUE
C         OS = 3*NNO2
C         DO 14 N = 1,NNO1-NNO2
C           VU(1,N+NNO2) = 1 + (N-1)*3 + OS
C           VU(2,N+NNO2) = 2 + (N-1)*3 + OS
C           VP(N)        = 3 + (N-1)*3 + OS
C  14     CONTINUE
C         GOTO 1000
C       END IF
      

C       IF (CHOIX.EQ.'2D-P2.P0.P0') THEN
C         DO 300 N = 1,NNO1
C           VU(1,N) = 1 + (N-1)*2
C           VU(2,N) = 2 + (N-1)*2
C  300    CONTINUE
C         OS = 2*NNO1
C         VP(1) = OS+1
C         VG(1) = OS+2
C         GOTO 1000
C       END IF

      CALL U2MESK('F','ALGORITH4_46',1,NOMTE)
      
 1000 CONTINUE 
      IF (TYPMOD(1).EQ.'AXIS') THEN
        DO 60 N = 1,NNO1
          VU(3,N) = VU(1,N)
 60     CONTINUE
      END IF
    
      END     
