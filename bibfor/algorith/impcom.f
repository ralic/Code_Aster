      SUBROUTINE IMPCOM(INODA ,NOMDDL,CHAINE)    
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/09/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  NOMDDL
      INTEGER      INODA
      CHARACTER*16 CHAINE
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (AFFICHAGE - UTILITAIRE)
C
C RETOURNE UNE CHAINE FORMATEE K16 POUR LES INFOS SUR UNE COMPOSANTE
C POUR LE CAS D UN RESIDU RESI_COMP_RELA 
C      
C ----------------------------------------------------------------------
C      
C 
C IN  INODA  : NUMERO DU NOEUD
C IN  NOMDDL : CHAINE DU COMPOSANT 
C OUT CHAINE : CHAINE DU NOM DU NOEUD OU 'LIAISON_DDL'
C              CHAINE DU NOM DE LA CMP OU NOM DU LIGREL DE CHARGE
C
C
C ----------------------------------------------------------------------
C
      CHARACTER*8   CHNOD
      INTEGER       I,K
      INTEGER       NCHAIN
      PARAMETER    (NCHAIN = 7)
C
C ----------------------------------------------------------------------
C
      CHAINE = ' '
      CHNOD =  '  '
      IF (INODA.EQ.0) GOTO 99
C
      WRITE(CHNOD(1:NCHAIN),'(I7.7)') INODA      
      K = 1
C      
      DO 10 I=1,NCHAIN
        IF(CHNOD(I:I).EQ.'0')  THEN
          K=K+1
        ELSE
          CHAINE(1:1) = 'N'
          GOTO 20
        ENDIF
 10   CONTINUE
 20   CONTINUE  

      CHAINE(2:NCHAIN-K+2) = CHNOD(K:NCHAIN)
      
      CHAINE(9:16) = NOMDDL
 99   CONTINUE
      END
