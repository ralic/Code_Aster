      SUBROUTINE CALDFE(DF,NR,VIND,DFPDS,FE,DFPDBS,MSDGDT,DRDY)

      IMPLICIT NONE      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2011   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE PROIX J-M.PROIX
C     ----------------------------------------------------------------
C     
C     MONOCRISTAL : calcul des derivees de Fe en GDEF      
C     IN  DF     :  GRADIENT DF
C         NR     :  DIMENSION DECLAREE DRDY
C         VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C         DFPDS  :  DERIVEE DE FP PAR RAPPORT A S
C         FE     :  GRADIENT ELASTIQUE FE
C         DFPDBS :  DERIVEE DE FP PAR RAPPORT A BETA_S
C         MSDGDT :  SOMME DES MUS(I)*MUS(J)*DGAMMAS/DTAUS
C       OUT DRDY :  BLOC ((1-6),(7-NS)) JACOBIEN DU SYSTEME NON LINEAIRE

      INTEGER NR,NDT,NDI,NS,I,J,K,L,M,IND(3,3)
      REAL*8 FE(3,3),DF(3,3),DFPDS(3,3,3,3),MSDGDT(6,6),DFEFDT(3,3,3,3)
      REAL*8 VIND(*),DFEDS(3,3,3,3),DFEFDS(3,3,3,3),DFFE(3,3),FEM(3,3)
      REAL*8 ID(3,3),DRDY(NR,NR)
      REAL*8 DFPDBS(3,3,24),DFEDBS(3,3,24),DFEFDB(3,3,24)
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
      DATA ID/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
      DATA IND/1,4,5,4,2,6,5,6,3/
C     ----------------------------------------------------------------

      NS=NR-NDT
      CALL DCOPY(9,VIND(6+3*NS+1),1,FEM,1)
      CALL DAXPY(9,1.D0,ID,1,FEM,1)
      CALL PMAT(3,DF,FEM,DFFE)

C     on calcule dFe/dS
      CALL R8INIR ( 81, 0.D0 , DFEDS, 1 )
      DO 1004 I=1,3
      DO 1004 J=1,3
      DO 1004 K=1,3
      DO 1004 L=1,3
      DO 1004 M=1,3
        DFEDS(I,J,K,L)=DFEDS(I,J,K,L)+DFFE(I,M)*DFPDS(M,J,K,L)
 1004 CONTINUE
 
      CALL R8INIR ( 81, 0.D0 , DFEFDS, 1 )
      DO 1005 I=1,3
      DO 1005 J=1,3
      DO 1005 K=1,3
      DO 1005 L=1,3
      DO 1005 M=1,3
        DFEFDS(I,J,K,L)=DFEFDS(I,J,K,L)+DFEDS(M,I,K,L)*FE(M,J)
 1005 CONTINUE
 
      CALL R8INIR ( 81, 0.D0 , DFEFDT, 1 )
      DO 1006 I=1,3
      DO 1006 J=1,3
      DO 1006 K=1,3
      DO 1006 L=1,3
      DO 1006 M=1,3
        DFEFDT(I,J,K,L)=DFEFDT(I,J,K,L)+DFEDS(M,J,K,L)*FE(M,I)
 1006 CONTINUE

      CALL DAXPY(81,1.D0,DFEFDS,1,DFEFDT,1)
      CALL DSCAL(81,-0.5D0,DFEFDT,1)
 
      DO 1007 I=1,3
      DO 1007 J=1,3
      DO 1007 K=1,3
      DO 1007 L=1,3
         MSDGDT(IND(I,J),IND(K,L))=DFEFDT(I,J,K,L)
 1007 CONTINUE


C     on calcule dFe/dbetas
      CALL R8INIR ( 3*3*24, 0.D0 , DFEDBS, 1 )
      DO 1014 I=1,3
      DO 1014 J=1,3
      DO 1014 K=1,24
      DO 1014 M=1,3
        DFEDBS(I,J,K)=DFEDBS(I,J,K)+DFFE(I,M)*DFPDBS(M,J,K)
 1014 CONTINUE
 
      CALL R8INIR ( 3*3*24, 0.D0 , DFEFDB, 1 )
      DO 1015 I=1,3
      DO 1015 J=1,3
      DO 1015 K=1,24
      DO 1015 M=1,3
        DFEFDB(I,J,K)=DFEFDB(I,J,K)+DFEDBS(M,I,K)*FE(M,J)
     &                             +DFEDBS(M,J,K)*FE(M,I)
 1015 CONTINUE

      CALL DSCAL(3*3*24,-0.5D0,DFEFDB,1)
 
      DO 1018 I=1,3
      DO 1018 J=1,3
      DO 1018 K=1,12
         DRDY(IND(I,J),6+K)=DFEFDB(I,J,K)
 1018 CONTINUE
      END
