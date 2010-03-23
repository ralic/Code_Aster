      SUBROUTINE CAFMES(IFA,CONT,TANGE,MAXFA,NFACE,
     &                  FKSS,DFKS1,DFKS2,MOBFAS,
     &                  DMOB1,DMOB2,DMOB1F,DMOB2F,
     &                  FMW,FM1W,FM2W)
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
C=======================================================================
C  CETTE SUBROUTINE PERMET DE CALCULER D UNE MANIERE GENERIQUE LE FLUX
C  MASSIQUE
C=======================================================================
C ****IN :
C     IFA              : ARETE EXTERNE QUE L ON CONSIDERE DANS ASSESU
C     FKSS             : FLUX SUR L ARETE EXTERNE 
C                             QUE L ON CONSIDERE DS ASSESU
C     DFKS1(1,IFA)     : DERIVEE DE FKS(IFA) % A LA PREMIERE 
C                                              INCONNUE AU CENTRE
C     DFKS1(JFA+1,IFA) : DERIVEE DE FKS(IFA) % A LA PREMIERE 
C                                              INCONNUE FACE
C     DFKS2(1,IFA)     : DERIVEE DE FKS(IFA) % A LA DEUXIEME 
C                                              INCONNUE AU CENTRE
C     DFKS2(JFA+1,IFA) : DERIVEE DE FKS(IFA) % A LA DEUXIEME 
C                                              INCONNUE FACE
C     MOBFAS           : MOBILITE SUR L ARETE EXTERNE QUE L ON 
C                                              CONSIDERE DS ASSESU
C     DMOB1            : DERIVEE DE MOBFAS % A LA PREMIERE 
C                                              VARIABLE AU CENTRE
C     DMOB2            : DERIVEE DE MOBFAS % A LA SECONDE 
C                                              VARIABLE AU CENTRE
C     DMOB1F           : DERIVEE DE MOBFAS % A LA PREMIERE 
C                                              VARIABLE A L ARETE
C     DMOB2F           : DERIVEE DE MOBFAS % A LA SECONDE 
C                                              VARIABLE A L ARETE
   
C ****IN-OUT :  
C     FMW              : FLUX MASSIQUE 
C     FM1W             : DERIVEE DU FLUX % A LA PREMIERE VARIABLE
C     FM2W             : DERIVEE DU FLUX % A LA SECONDE VARIABLE
C ================================================
C     FMW =  MOB * F_{K,SIGMA}
C================================================     
      IMPLICIT NONE
      LOGICAL      CONT,TANGE
      INTEGER      MAXFA
      INTEGER      NFACE
      REAL*8       FMW(1:MAXFA)
      REAL*8       FKSS
      REAL*8       MOBFAS
      REAL*8       DMOB1(1:MAXFA),DMOB2(1:MAXFA),
     >             DMOB1F(1:MAXFA),DMOB2F(1:MAXFA)
      REAL*8       FM1W(1+MAXFA,MAXFA),FM2W(1+MAXFA,MAXFA)
      REAL*8       DFKS1(1+MAXFA,MAXFA), DFKS2(1+MAXFA,MAXFA)     
      INTEGER      IFA,JFA
C     
      IF ( CONT ) THEN
         FMW(IFA) = FMW(IFA)+ MOBFAS * FKSS
      ENDIF     
      IF ( TANGE) THEN
         FM1W(1,IFA) = FM1W(1,IFA) + DMOB1(IFA) * FKSS
     >                                   + MOBFAS * DFKS1(1,IFA)
C
         FM2W(1,IFA) = FM2W(1,IFA) + DMOB2(IFA) * FKSS
     >                                   + MOBFAS * DFKS2(1,IFA)
         DO 4 JFA=2,NFACE+1
            FM1W(JFA,IFA)= FM1W(JFA,IFA) + MOBFAS * DFKS1(JFA,IFA)
            FM2W(JFA,IFA)= FM2W(JFA,IFA) + MOBFAS * DFKS2(JFA,IFA)
    4    CONTINUE  
         FM1W(1+IFA,IFA)= FM1W(1+IFA,IFA) + DMOB1F(IFA) * FKSS
         FM2W(1+IFA,IFA)= FM2W(1+IFA,IFA) + DMOB2F(IFA) * FKSS
      ENDIF
      END
