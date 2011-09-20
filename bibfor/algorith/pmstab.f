      SUBROUTINE PMSTAB(SIGM,SIGP,EPSM,DEPS,NBVARI,VIM,VIP,IFORTA,
     & INSTAM,INSTAP,ITER,NBPAR,NOMPAR,TABLE,VR,IGRAD,VALIMP,
     & IMPTGT,DSIDEP,NOMVI,NBVITA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/09/2011   AUTEUR PROIX J-M.PROIX 
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
C TOLE CRP_21

      IMPLICIT NONE
C-----------------------------------------------------------------------
C           OPERATEUR    CALC_POINT_MAT STOCKAGE DANS LA TBLE RESULTAT
C-----------------------------------------------------------------------
      INTEGER      NBVARI,NBPAR,I,ITER,IFORTA,IGRAD,NCMP,IMPTGT,NBVITA
      CHARACTER*4  NOMEPS(6),NOMSIG(6),NOMGRD(9)
      CHARACTER*8  K8B,TABLE,VK8(2),NOMVI(*)
      CHARACTER*16 NOMPAR(*)
      REAL*8       DEPS(9),SIGM(6),SIGP(6),EPSP(9),EPSM(9),EPST(9)
      REAL*8       VIM(*),VIP(*),VR(*),EQUI(17),VALIMP(9),SIGT(6)
      REAL*8       RAC2,INSTAM,INSTAP,DSIDEP(*)
      COMPLEX*16 CBID
      DATA NOMEPS/'EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ'/
      DATA NOMSIG/'SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ'/
      DATA NOMGRD/'F11','F12','F13','F21','F22','F23','F31','F32','F33'/

      RAC2=SQRT(2.D0)
      IF (IGRAD.NE.0) THEN
         NCMP=9
      ELSE
         NCMP=6
      ENDIF

C     STOCKAGE DE LA SOLUTION DANS LA TABLE
      IF (NCMP.EQ.6) THEN
         CALL DCOPY(NCMP,EPSM,1,EPSP,1)
         CALL DAXPY(NCMP,1.D0,DEPS,1,EPSP,1)
         CALL DCOPY(NCMP,EPSP,1,EPSM,1)
         CALL DCOPY(NCMP,EPSP,1,EPST,1)
         CALL DSCAL(3,1.D0/RAC2,EPST(4),1)
      ELSE
         CALL DCOPY(NCMP,VALIMP,1,EPST,1)
         CALL DCOPY(NCMP,VALIMP,1,EPSM,1)      
      ENDIF
      CALL DCOPY(NBVARI,VIP,1,VIM,1)
      INSTAM=INSTAP
      CALL DCOPY(6,SIGP,1,SIGM,1)
      CALL DCOPY(6,SIGP,1,SIGT,1)
      CALL DSCAL(3,1.D0/RAC2,SIGT(4),1)
      CALL FGEQUI (SIGT, 'SIGM_DIR', 3, EQUI)
      
      IF (IFORTA.EQ.0) THEN
      
         CALL DCOPY(NCMP,EPST,1,VR(2),1)
         CALL DCOPY(6,SIGT,1,VR(NCMP+2),1) 
         VR(NCMP+8)=EQUI(16)
         VR(NCMP+9)=EQUI(1) 
         CALL DCOPY(NBVITA,VIP,1,VR(1+NCMP+6+2+1),1) 
         VR(1)=INSTAP
         VR(NBPAR)=ITER 
C        ajout KTGT
         IF (IMPTGT.EQ.1) THEN
            CALL DCOPY(36,DSIDEP,1,VR(1+6+6+3+NBVARI),1)
         ENDIF
         CALL TBAJLI(TABLE,NBPAR,NOMPAR,I,VR,CBID,K8B,0)
      
      ELSE
      
         VR(1)=INSTAP
         VK8(1)='EPSI'
         DO 551 I=1,NCMP
            VR(2)=EPST(I)
            IF (IGRAD.EQ.0) THEN
               VK8(2)=NOMEPS(I)
            ELSE
               VK8(2)=NOMGRD(I)
            ENDIF
            CALL TBAJLI(TABLE,NBPAR,NOMPAR,0,VR,CBID,VK8,0)
            
 551     CONTINUE
         VK8(1)='SIGM'
         DO 552 I=1,6
            VR(2)=SIGT(I)
            VK8(2)=NOMSIG(I)
            CALL TBAJLI(TABLE,NBPAR,NOMPAR,0,VR,CBID,VK8,0)
            
 552     CONTINUE
         VK8(1)='SIEQ'
         VR(2)=EQUI(1)
         VK8(2)='VMIS'
         CALL TBAJLI(TABLE,NBPAR,NOMPAR,0,VR,CBID,VK8,0)
         
         VR(2)=EQUI(16)
         VK8(2)='TRACE'
         CALL TBAJLI(TABLE,NBPAR,NOMPAR,0,VR,CBID,VK8,0)
         
         VK8(1)='VARI'
         DO 553 I=1,NBVITA
            VR(2)=VIP(I)
            VK8(2)=NOMVI(I)
C            VK8(2)(1:1)='V'
C            CALL CODENT(I,'G',VK8(2)(2:8))
            CALL TBAJLI(TABLE,NBPAR,NOMPAR,0,VR,CBID,VK8,0)
 553     CONTINUE
      
      ENDIF
      

      END
