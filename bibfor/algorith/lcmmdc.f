        SUBROUTINE LCMMDC(COEFT,IFA,NMAT,NBCOMM,ALPHAP,IS,CEFF,DCDALS)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3),IS
        REAL*8 COEFT(*),ALPHAP(12),CEFF,DCDALS

C TOLE CRP_21
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/09/2010   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C  CALCUL DE LA FONCTION Ceffectif POUR LA LOI D'ECOULEMENT  DD-CFC
C       IN  COEFT   :  PARAMETRES MATERIAU
C           IFA     :  NUMERO DE FAMILLE
C           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NMAT    :  NOMBRE DE MATERIAUX
C           ALPHAP  :  ALPHA =RHO*B**2 (TOTAL) A T+DT
C     OUT:
C           CEFF    :  Coefficeint pour passer de A_ij a Aeff_ij
C     ----------------------------------------------------------------
      REAL*8 ALPHA,BETA,RHOREF,OMEGAT
      INTEGER IEI,I
C     ----------------------------------------------------------------

      IEI=NBCOMM(IFA,3)
      ALPHA =COEFT(IEI+1)
      BETA  =COEFT(IEI+2)
      RHOREF=COEFT(IEI+3)
      CEFF=1.D0
      DCDALS=0.D0
      IF (ALPHA.GT.0.D0) THEN
         OMEGAT=0.D0
            DO 10 I=1,12
               IF (ALPHAP(I).GT.0.D0) THEN
                  OMEGAT=OMEGAT+ALPHAP(I)
               ENDIF
 10         CONTINUE
C        PARTIE POSITIVE
         IF (OMEGAT.GT.0.D0) THEN
            CEFF=0.2D0+0.8D0*LOG(ALPHA*SQRT(OMEGAT))/
     &                       LOG(ALPHA*BETA*SQRT(RHOREF))
            IF (ALPHAP(IS).GT.0.D0) THEN
            DCDALS=0.8D0/2.D0/LOG(ALPHA*BETA*SQRT(RHOREF))/OMEGAT
            ENDIF
         ENDIF
      ENDIF
      
      END
