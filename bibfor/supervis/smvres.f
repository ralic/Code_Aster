      SUBROUTINE SMVRES(ICMD,ICODE,NOMRES,IER,IERUSR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER ICODE,ICMD,IER,IERUSR
      CHARACTER*(*) NOMRES
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 12/01/2000   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     VERIFICATION SUR LE NOM UTILISATEUR
C     ------------------------------------------------------------------
C IN  ICODE  : IS  : CODE D'ENTREE
C           = 1    : NOUVEAU CONCEPT
C           = 2    : CONCEPT REENTRANT (&NOM_DE_CONCEPT)
C OUT IER    : IS  : CODE RETOUR
C           = 0  ==>  RESULTAT NON TROUVE
C           > 0  ==>  RESULTAT DE LA IER-IEME COMMANDE
C VAR IERUSR : IS  : CODE RETOUR D'ERREUR (INCREMENTE)
C     ------------------------------------------------------------------
C FIN SMVRES
C     ------------------------------------------------------------------
      PARAMETER (LENRES=8)
      CHARACTER*8 RESUSR,RESUS3
      CHARACTER*16 RESUS2
      INTEGER LGR
C     ------------------------------------------------------------------
      RESUSR = NOMRES(ICODE:)
      LGR = LXLGUT(RESUSR)
      IF (LEN(NOMRES(ICODE:)).GT.LENRES) THEN
        RESUS2 = NOMRES(ICODE:)
        CALL UTMESS('A','ANALYSE SEMANTIQUE (ALARME 03)',
     &              'LE NOM DU RESULTAT "'//RESUS2//
     &              '" EST TROP LONG : '//'IL EST TRONQUE EN "'//
     &              RESUSR//'"')
      END IF

C     --- RECHERCHE DE LA COMMANDE DE CREATION ---
      CALL GCUCON(ICMD,RESUSR,' ',IER)
      IF (ICODE.EQ.1) THEN
        IF (IER.NE.0) THEN
          IERUSR = IERUSR + 1
          RESUS3 = RESUSR(ICODE:LGR)
          CALL UTMESS('E','ANALYSE SEMANTIQUE (ERREUR 02)',
     &                'LE NOM DU RESULTAT "'//RESUS3//
     &                '" EST DEJA ATTRIBUE')
        END IF
      ELSE IF (ICODE.EQ.2) THEN
        IF (IER.EQ.0) THEN
          IERUSR = IERUSR + 1
          RESUS3 = RESUSR(ICODE:LGR)
          CALL UTMESS('E','ANALYSE SEMANTIQUE (ERREUR 03)',
     &                'LE NOM DU RESULTAT "'//RESUS3//
     &                '" DOIT DEJA ETRE '//
     &                'DEFINI AVANT RE-EMPLOI PAR "&".')
        END IF
      ELSE
        CALL UTMESS('F','SMVRES','ICODE = 1 OU 2 SEULEMENT')
      END IF
      END
