         SUBROUTINE VFCFKS(CONT,TANGE,MAXFA,NFACE,
     >                 UK,DUKP1,DUKP2,UFA,
     >                 DUFA1,DUFA2,C,
     >                 PESA,RHO,DRHO1,
     >                 DRHO2,XK,XFA,MAXDIM,NDIM,
     >                 FKS,DFKS1,DFKS2)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
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
C TOLE CRP_21
C TOLE CRP_21
C
C
C     CETTE SUBROUTINE PERMET DE CALCULER LES FLUX SURFACIQUES 
C     (I-E F_K,SIGMA L' APPROXIMATION DU FLUX )
C IN
C     UK              : UNE VALEUR AU CENTRE
C     DUKP1           : DERIVEE DE UK PAR RAP A LA PREMIERE INCONNUE
C                       AU CENTRE
C     DUKP2           : DERIVEE DE UK PAR RAP A LA DEUXIEME INCONNUE
C                       AU CENTRE
C
C     UFA(IFA)        : UNE VALEUR SUR FACE IFA
C     DUFA1(IFA)      : DERIVEE DE U_FA(IFA) PAR RAP A LA PREMIERE
C                      INCONNUE SUR IFA
C     DUFA2S(IFA)     : DERIVEE DE U_FA(IFA) PAR RAP A LA DEUXIEME
C                       INCONNUE SUR IFA
C
C OUT
C     FKS(IFA)        : FLUX SURFACIQUE SUR IFA
C     DFKS1(1,IFA)    : DERIVEE DE FKS(IFA) PAR RAP A LA PREMIERE
C                       INCONNUE AU CENTRE
C     DFKS1(JFA+1,IFA): DERIVEE DE FKS(IFA) PAR RAP A LA PREMIERE
C                      INCONNUE FACE
C     DFKS2(1,IFA)    : DERIVEE DE FKS(IFA) PAR RAP A LA DEUXIEME
C                       INCONNUE AU CENTRE
C     DFKS2(JFA+1,IFA): DERIVEE DE FKS(IFA) PAR RAP A LA DEUXIEME
C                      INCONNUE FACE
      IMPLICIT NONE
C      
      LOGICAL       CONT,TANGE
      INTEGER       MAXFA,MAXDIM
      INTEGER       NFACE,NDIM
      REAL*8        FKS(NFACE)
      REAL*8        UK,DUKP1,DUKP2
      REAL*8        DUFA1(1:NFACE),DUFA2(1:NFACE),C(1:MAXFA,1:NFACE)
      REAL*8        UFA(1:NFACE)
      REAL*8        DFKS1(1+MAXFA,NFACE), DFKS2(1+MAXFA,NFACE)
      REAL*8        PESA(NDIM),RHO,DRHO1,DRHO2
      REAL*8        XK(NDIM),XFA(1:MAXDIM,1:NFACE)
      REAL*8        GRAVI(NFACE)
      INTEGER       IFA,JFA,KFA,IDIM
C
      DO 4 IFA = 1 , NFACE        
         DO 41 JFA = 1 , NFACE
            GRAVI(JFA)=0.D0
            IF ( CONT ) THEN
               DO 42 IDIM = 1 , NDIM
                  GRAVI(JFA)=GRAVI(JFA)+PESA(IDIM)*
     >                             (XK(IDIM)-XFA(IDIM,JFA))
 42            CONTINUE  
               FKS(IFA)  = FKS(IFA) + C(IFA,JFA)*
     >                     (UK-UFA(JFA)-RHO*GRAVI(JFA) )
            ENDIF
            IF ( TANGE) THEN 
               KFA=JFA+1
               DFKS1(1,IFA)= DFKS1(1,IFA) + C(IFA,JFA)*(DUKP1
     >          -DRHO1*GRAVI(JFA))
               DFKS1(KFA,IFA)=DFKS1(KFA,IFA)-C(IFA,JFA)*DUFA1(JFA)
               DFKS2(1,IFA)= DFKS2(1,IFA) + C(IFA,JFA) *(DUKP2 
     >          -DRHO2*GRAVI(JFA))          
               DFKS2(KFA,IFA)=DFKS2(KFA,IFA)-C(IFA,JFA)*DUFA2(JFA)
            ENDIF
 41      CONTINUE
 4    CONTINUE
      END
