      SUBROUTINE PSREMC ( NOSIMP, NOPASE,
     >                    NBMOCL, LIMOCL, LIVALE, LIMOFA, IRET )
C
C     PARAMETRES SENSIBLES - RECUPERATION DES MOTS-CLES
C     *          *           **               *    *
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
C     RECUPERE LES MOTS-CLES ASSOCIES A UN COUPLE :
C      ( STRUCTURE DE BASE , PARAMETRE DE SENSIBILITE )
C     ------------------------------------------------------------------
C IN  NOSIMP  : NOM DE LA SD DE BASE
C IN  NOPASE  : NOM DU PARAMETRE DE SENSIBILITE
C OUT NBMOCL  : NOMBRE DE MOTS-CLES ASSOCIES A (NOSIMP,NOPASE)
C OUT LIMOCL  : LA STRUCTURE K80 CONTENANT LES MOTS-CLES CONCERNES
C OUT LIVALE  : LA STRUCTURE K80 CONTENANT LES VALEURS CONCERNEES
C OUT LIMOFA  : LA STRUCTURE K80 CONTENANT LES MOTS-CLES FACTEURS
C OUT IRET    : CODE_RETOUR :
C                     0 -> TOUT S'EST BIEN PASSE
C                     1 -> LE COUPLE (NOSIMP,NOPASE) N'EST PAS RENSEIGNE
C                     2 -> LA STRUCTURE DE MEMORISATION EST INCONNUE
C                     3 -> LA STRUCTURE COMPOSEE LIEE AU COUPLE
C                          (NOSIMP,NOPASE) N'EST PAS LA BONNE
C  REMARQUE : LES STRUCTURES LIMOCL ET LIVALE NE SONT ALLOUEES PAR
C             LE PROGRAMME SEMECO QUE S'IL Y A AU MOINS UN MOT-CLE
C     ------------------------------------------------------------------
C     ARBORESCENCE DE LA GESTION DES PARAMETRES SENSIBLES :
C  NTTYSE --!
C  METYSE --> PSTYSE --> PSTYPR --> SEGICO
C
C  NTTYSE --!
C  METYSE --> PSTYSS --> PSREMC --> SEMECO
C                    --> PSTYST
C  NTDOTH --!
C  NMDOME --> PSTYPA --> SEGICO
C                    --> PSREMC --> SEMECO
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      CHARACTER*(*) NOSIMP, NOPASE, LIMOCL, LIVALE, LIMOFA
      INTEGER NBMOCL
      INTEGER IRET
C
C 0.2. ==> COMMUNS
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*8 SAUX08
C
C      CHARACTER*6 NOMPRO
C      PARAMETER ( NOMPRO = 'PSREMC' )
C
C     ------------------------------------------------------------------
C====
C 1. APPEL DU PROGRAMME GENERIQUE
C====
C
      CALL SEMECO ( 'REMC', NOSIMP, NOPASE,
     >              SAUX08,
     >              SAUX08, NBMOCL, LIMOCL, LIVALE, LIMOFA,
     >              IRET )
C
      END
