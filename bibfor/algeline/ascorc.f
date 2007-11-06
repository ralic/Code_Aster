      SUBROUTINE ASCORC ( TYPCMO, NBSUP, N2SUP, NSUPP, NEQ, TCOSUP,
     +                    NBMODE, REPMOD, AMORT, MODAL, ID, TEMPS,
     +                    RECMOD, TABS,COMBSU )
      IMPLICIT  NONE
      INTEGER           NBSUP, N2SUP, NSUPP(*), NEQ, NBMODE, ID,
     +                  TCOSUP(NBSUP,*)
      REAL*8            COMBSU(NBMODE,NBSUP,NEQ,*)
      REAL*8            REPMOD(NBMODE,NBSUP,NEQ,*)
      REAL*8            MODAL(NBMODE,*), TABS(NBSUP,*), AMORT(*)
      REAL*8            TEMPS, RECMOD(NBSUP,NEQ,*)
      CHARACTER*(*)     TYPCMO
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 05/11/2007   AUTEUR VIVAN L.VIVAN 
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
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL
C        RECOMBINAISON DES REPONSES MODALES
C        POUR LE MULTI_APPUI, CAS DES EXCITATIONS CORRELEES
C     ------------------------------------------------------------------
C IN  : TYPCMO : TYPE DE COMBINAISON
C IN  : NBSUP  : NOMBRE DE SUPPORT
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBMODE : NOMBRE DE MODES
C IN  : REPMOD : VECTEUR DES REPONSES MODALES
C IN  : AMORT  : VECTEUR DES AMORTISSEMENTS MODAUX
C IN  : MODAL  : VECTEUR DES PARAMETRES MODAUX
C IN  : ID     : DIRECTION
C IN  : TEMPS  : DUREE DE LA PARTIE FORTE SU SEISME (TYPCMO='DSC')
C OUT : RECMOD : VECTEUR DES COMBINAISONS DES REPONSES DES MODES
C     ------------------------------------------------------------------
      INTEGER  IS, IM, IN
      REAL*8   XXX
      LOGICAL  MONOAP
C     ------------------------------------------------------------------
C
C --- ON RECOMBINE LES SUPPORTS 
C
      DO 100 IS = 1,NSUPP(ID)
         IF (TCOSUP(IS,ID).EQ.1) THEN
C           --- COMBINAISON QUADRATIQUE ---
            DO 110 IM = 1,NBMODE
               DO 112 IN = 1,NEQ
                  XXX = REPMOD(IM,IS,IN,ID)
                  COMBSU(IM,1,IN,ID)= COMBSU(IM,1,IN,ID)+ SQRT(XXX*XXX)
 112           CONTINUE
 110        CONTINUE
C
         ELSEIF (TCOSUP(IS,ID).EQ.2) THEN
C           --- COMBINAISON LINEAIRE ---
            DO 120 IM = 1,NBMODE
               DO 122 IN = 1,NEQ
                  XXX = REPMOD(IM,IS,IN,ID)
                  COMBSU(IM,1,IN,ID)= COMBSU(IM,1,IN,ID)+ XXX
 122           CONTINUE
 120        CONTINUE
C
         ELSE
C           --- COMBINAISON VALEUR ABSOLUE ---
            DO 130 IM = 1,NBMODE
               DO 132 IN = 1,NEQ
                  XXX = ABS( REPMOD(IM,IS,IN,ID) )
                  COMBSU(IM,1,IN,ID)= COMBSU(IM,1,IN,ID)+ XXX
 132           CONTINUE
 130        CONTINUE
         ENDIF
 100  CONTINUE
C
C --- PUIS LES MODES 
C
      MONOAP = .TRUE.
      CALL ASCORM ( MONOAP, TYPCMO, NBSUP, NSUPP, NEQ, NBMODE,
     +              COMBSU, AMORT, MODAL, ID, TEMPS, RECMOD, TABS )
C
      END
