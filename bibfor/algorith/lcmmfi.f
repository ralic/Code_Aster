        SUBROUTINE LCMMFI( COEFT,IFA,NMAT,NBCOMM,NECRIS,
     &                  IS,NBSYS,VIND,DY,HSR,IEXP,EXPBP,RP)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3),NBSYS,IS,IEXP
        REAL*8 COEFT(NMAT),DY(*),VIND(*),HSR(5,24,24),SQ,EXPBP(*)
        CHARACTER*16 NECRIS
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2007   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE JMBHH01 J.M.PROIX
C  COMPORTEMENT MONOCRISTALLIN : ECROUISSAGE ISOTROPE 
C     IN  COEFT   :  PARAMETRES MATERIAU 
C         IFA     :  NUMERO DE FAMILLE
C         NMAT    :  NOMBRE MAXI DE MATERIAUX
C         NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C         NECRIS  :  NOM DE LA LOI D'ECROUISSAGE ISTROPE
C         IS      :  NUMERO DU SYSTEME DE GLISSEMET EN COURS 
C         NBCOMM  :  INCIDES DES COEF MATERIAU
C         NBSYS   :  NOMBRE DE SYSTEMES DE GLISSEMENT DE LA FAMILLE
C         VIND    :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C         DY      :  SOLUTION
C         HSR     :  MATRICE D'INTERACTION
C         IEXP    :  Indice pour recalculer EXPBP (0 si deja calcule)
C         EXPBP   :  TERMES 1.-EXP(-BPr)  pour tous les systemes Ir
C     OUT:
C         RP      :  R(P)
C ======================================================================

C     ----------------------------------------------------------------
      REAL*8 C,P,R0,Q,B,RP,K,N,FTAU,CRIT,B1,B2,Q1,Q2,A,GAMMA0,V,D
      REAL*8 TPERD,TABS,PR,DRDP,B1P
      INTEGER IEI,TNS,NS,IR,NBCOEF,NUEISO
      INTEGER NUMHSR
C     ----------------------------------------------------------------

      IEI=NBCOMM(IFA,3)
      NUEISO=COEFT(IEI)

C--------------------------------------------------------------------
C     POUR UN NOUVEAU TYPE D'ECROUISSAGE ISOTROPE, AJOUTER UN BLOC IF
C--------------------------------------------------------------------
C      IF (NECRIS.EQ.'ECRO_ISOT1') THEN
      IF (NUEISO.EQ.1) THEN

         R0    =COEFT(IEI+1)
         Q     =COEFT(IEI+2)
         B     =COEFT(IEI+3)
         NUMHSR=COEFT(IEI+4)
         
         IF (IEXP.EQ.1) THEN
           DO 10 IR = 1, NBSYS                              
            PR=VIND(3*(IR-1)+3)+ABS(DY(IR))
            EXPBP(IR) = (1.D0-EXP(-B*PR))      
  10      CONTINUE                                          
         ENDIF
         
C       VIND commence en fait au début de systemes de glissement
C      de LA famille courante;         
         SQ=0.D0                                            
           DO 11 IR = 1, NBSYS                              
            PR=VIND(3*(IR-1)+3)+ABS(DY(IR))
            SQ = SQ + HSR(NUMHSR,IS,IR)*EXPBP(IR)      
  11      CONTINUE                                          
            RP=R0+Q*SQ                                      

C      ELSEIF (NECRIS.EQ.'ECRO_ISOT2') THEN
      ELSEIF (NUEISO.EQ.2) THEN

         R0=COEFT(IEI+1)
         Q1=COEFT(IEI+2)
         B1=COEFT(IEI+3)
         Q2=COEFT(IEI+5)
         B2=COEFT(IEI+6)
         NUMHSR=COEFT(IEI+7)


C        VIND COMMENCE EN FAIT AU DÉBUT DE SYSTEMES DE GLISSEMENT
C        DE LA FAMILLE COURANTE;         

         SQ=0.D0                                            
         DO 12 IR = 1, NBSYS                              
            PR=VIND(3*(IR-1)+3)+ABS(DY(IR))
            SQ = SQ + HSR(NUMHSR,IS,IR)*(1.D0-EXP(-B1*PR))      
  12     CONTINUE       
         P=VIND(3*(IS-1)+3)+ABS(DY(IS))
         RP=R0+Q1*SQ+Q2*(1.D0-EXP(-B2*P))
         
      ELSE
          CALL U2MESS('F','COMPOR1_21')          
      ENDIF
           
      END
