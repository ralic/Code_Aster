      SUBROUTINE LCMMJG(COMP,NMAT,NBCOMM,CPMONO,HSR,DT,NVI,VIND,YD,DY,
     &               ITMAX,TOLER,MATERF,SIGF,FKOOH,NFS,NSG,TOUTMS,PGL,
     &               MSNST,GAMSNS,DFPDGA,IRET)
      IMPLICIT NONE
C TOLE CRP_21 CRS_1404
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J.M.PROIX
C       ----------------------------------------------------------------
C     MONOCRISTAL : POUR LE CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE
C                   CALCUL EN GDEF DE Gamma.Ms*Ns et dFp/Dgamma
C       IN 
C           COMP   :  NOM COMPORTEMENT
C           NMAT   :  DIMENSION MATER
C           NBCOMM :  INCIDES DES COEF MATERIAU
C           CPMONO :  NOM DES COMPORTEMENTS
C           HSR    :  MATRICE D'INTERACTION
C           DT     :  DELTA T
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C           YD     :  VARIABLES A T      
C           DY     :  SOLUTION          
C           ITMAX  :  ITER_INTE_MAXI
C           TOLER  :  RESI_INTE_RELA
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           SIGF   :  CONTRAINTES A T+DT
C           FKOOH  :  INVERSE TENSEUR HOOKE
C           PGL    :  MATRICE DE PASSAGE
C           TOUTMS :  TENSEURS D'ORIENTATION
C       OUT MSNST  :  Ms*Ns pour chaque systele de glissement
C           GAMSNS :  Somme de GammaS*MS*NS
C           DFPDGA :  derivee de Fp / dGamma_S pour tous les systemes S
C       OUT IRET   :  CODE RETOUR
C       ----------------------------------------------------------------
      INTEGER NVI,NMAT,NBFSYS,NSFA,NSFV,NBSYS,IS,NFS,NSG
      INTEGER NBCOMM(NMAT,3),IFA,IRET,ITMAX
      REAL*8 VIND(*),DY(*),MATERF(NMAT*2)
      REAL*8 PGL(3,3),TOUTMS(NFS,NSG,6),HSR(NSG,NSG),GAMSNS(3,3)
      REAL*8 DT,FKOOH(6,6),SIGF(6),TOLER,TAUS,DP,CRIT,SGNS,RP
      REAL*8 Q(3,3),MUS(6),NS(3),MS(3),DFPDGA(3,3,NSG)
      REAL*8 EXPBP(NSG),YD(*),MSNST(3,3,NSG),DALPHA,DGAMMA
      CHARACTER*16 NOMFAM,COMP(*)
      CHARACTER*24 CPMONO(5*NMAT+1)
C     ----------------------------------------------------------------

C     NSFA : debut de la famille IFA dans DY et YD, YF
      NSFA=6
C     NSFV : debut de la famille IFA dans les variables internes
      NSFV=6
      NBFSYS=NBCOMM(NMAT,2)
C     PROGRAMMATION VALABLE POUR UNE SEULE FAMILLE DE SYSTEMES     
      CALL ASSERT(NBFSYS.EQ.1)
      DO 16 IFA=1,NBFSYS
C        Calcul preliminaire de somme(dgamma*ms*ns)      
         CALL R8INIR(9,0.D0,GAMSNS,1)
         NOMFAM=CPMONO(5*(IFA-1)+1)
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MUS,NS,MS,0,Q)
         DO 17 IS=1,NBSYS
           CALL CALTAU(COMP,IFA,IS,SIGF,FKOOH,NFS,NSG,TOUTMS,
     &                 TAUS,MUS,MSNST(1,1,IS))
           CALL LCMMLC(NMAT,NBCOMM,CPMONO,NFS,NSG,HSR,NSFV,NSFA,IFA,
     &     NBSYS,IS,DT,NVI,VIND,YD,DY,ITMAX,TOLER,MATERF,EXPBP,TAUS,
     &               DALPHA,DGAMMA,DP,CRIT,SGNS,RP,IRET)
           CALL DAXPY(9,DGAMMA,MSNST(1,1,IS),1,GAMSNS,1)
 17      CONTINUE
         DO 19 IS=1,NBSYS
            CALL CALDFP(MSNST(1,1,IS),GAMSNS,DFPDGA(1,1,IS),IRET)
 19      CONTINUE
         NSFA=NSFA+NBSYS
         NSFV=NSFV+NBSYS*3
 16   CONTINUE
      END
