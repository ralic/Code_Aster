      SUBROUTINE PSRENC ( NOSIMP, NOPASE, NOCOMP, IRET )
C
C     PARAMETRES SENSIBLES - RECUPERATION DU NOM COMPOSE
C     *          *           **              *   *
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 01/07/2003   AUTEUR GNICOLAS G.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GNICOLAS G.NICOLAS
C ----------------------------------------------------------------------
C     RECUPERE LE NOM COMPOSE BATI SUR UN COUPLE :
C      ( STRUCTURE DE BASE , PARAMETRE DE SENSIBILITE )
C     ------------------------------------------------------------------
C IN  NOSIMP  : NOM DE LA SD DE BASE
C IN  NOPASE  : NOM DU PARAMETRE DE SENSIBILITE
C OUT NOCOMP  : NOM DE LA SD DERIVEE
C OUT IRET    : CODE_RETOUR :
C                     0 -> TOUT S'EST BIEN PASSE
C                     1 -> LE COUPLE (NOSIMP,NOPASE) N'EST PAS RENSEIGNE
C                     2 -> LA STRUCTURE DE MEMORISATION EST INCONNUE
C                     3 -> LA STRUCTURE COMPOSEE LIEE AU COUPLE
C                          (NOSIMP,NOPASE) N'EST PAS LA BONNE
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      CHARACTER*(*) NOSIMP, NOPASE, NOCOMP
      INTEGER IRET
C
C 0.2. ==> COMMUNS
C 0.3. ==> VARIABLES LOCALES
C
      INTEGER IAUX
      CHARACTER*8 SAUX08
C
C      CHARACTER*6 NOMPRO
C      PARAMETER ( NOMPRO = 'PSRENC' )
C
C     ------------------------------------------------------------------
C====
C 1. APPEL DU PROGRAMME GENERIQUE
C====
C
      CALL SEMECO ( 'RENC', NOSIMP, NOPASE,
     >              SAUX08,
     >              NOCOMP, IAUX, SAUX08, SAUX08, SAUX08,
     >              IRET )
C
      END
