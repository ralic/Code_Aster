      SUBROUTINE PMSTA1(SIGM,SIGP,DEPS,VIM,VIP,NBVARI,NBVITA,
     &                  NBPAR,NOMPAR,TABINC,VR,IGRAD)
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

      IMPLICIT NONE
C-----------------------------------------------------------------------
C           OPERATEUR    CALC_POINT_MAT STOCKAGE DANS LA TBLE RESULTAT
C-----------------------------------------------------------------------
      INTEGER      NBPAR,I,NBVARI,IGRAD,NCMP,NBVITA
      CHARACTER*8  K8B
      CHARACTER*16 NOMPAR(*)
      CHARACTER*19 TABINC
      COMPLEX*16 CBID
      REAL*8 DEPS(9),SIGM(6),SIGP(6),VIM(*),VIP(*),VR(*),RAC2,DSIG(6)
      REAL*8 DEPST(9)

      RAC2=SQRT(2.D0)
      IF (IGRAD.NE.0) THEN
         NCMP=9
      ELSE
         NCMP=6
      ENDIF
      
      CALL DAXPY(NCMP,1.D0,DEPS,1,DEPST,1)
      IF (IGRAD.EQ.0) CALL DSCAL(3,1.D0/RAC2,DEPST(4),1)
      
      CALL DCOPY(6,SIGP,1,DSIG,1)
      CALL DAXPY(6,-1.D0,SIGM,1,DSIG,1)
      CALL DSCAL(3,1.D0/RAC2,DSIG,1)

C     a ce niveau, VR contient l'accroissement de variables internes
      CALL DCOPY(NBVITA,VIP,1,VR(1+NCMP+6+3),1)
      CALL DAXPY(NBVITA,-1.D0,VIM,1,VR(1+NCMP+6+3),1)

C     STOCKAGE DES INCREMENTS POUR NMEVDR

         CALL DCOPY(NCMP,DEPST,1,VR(2),1)
 
         CALL DCOPY(6,DSIG,1,VR(NCMP+2),1)

         CALL TBAJLI(TABINC,NBPAR,NOMPAR,I,VR,CBID,K8B,0)


      END
