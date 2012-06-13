      SUBROUTINE RSCHEX ( NORESZ, NOMSYM, CODRET )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER CODRET
      CHARACTER*(*)       NORESZ, NOMSYM
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C      RESULTAT - NOMSYM EXISTE-T-IL ?
C
C     ENTREES:
C        NORESZ : NOM DU RESULTAT A EXAMINER
C        NOMSYM : NOM SYMBOLIQUE DU CHAMP
C     SORTIES:
C        CODRET : CODE D'EXISTENCE
C                 = 0 N'EXISTE PAS
C                 /= 0 EXISTE
C
C     ------------------------------------------------------------------
C
      INTEGER      LXLGUT, IAUX, NBTONO, JORDR, JTACH
      CHARACTER*1  K1BID
      CHARACTER*19 NORESU
C     ------------------------------------------------------------------
C
      CODRET = 0
C
      IAUX = LXLGUT(NORESZ)
C               1234567890123456789
      NORESU = '                   '
      NORESU(1:IAUX) = NORESZ(1:IAUX)
C
      CALL JELIRA ( NORESU//'.ORDR', 'LONUTI', NBTONO, K1BID )
      CALL JEVEUO ( NORESU//'.ORDR', 'L', JORDR )
      CALL JENONU ( JEXNOM(NORESU//'.DESC',NOMSYM), IAUX )
      CALL JEVEUO ( JEXNUM(NORESU//'.TACH',IAUX), 'L', JTACH )
C
C --- ON PARCOURT TOUS LES NUMEROS D'ORDRE DE LA STRUCTURE RESULTAT
C     QUAND ON TROUVE UN CHAMP ENREGISTRE, ON SORT
C
      DO 10 , IAUX = 0 , NBTONO - 1
         IF ( ZK24(JTACH+IAUX) .NE. ' ' ) THEN
            CODRET = 7
            GOTO 9999
         ENDIF
 10   CONTINUE
C
 9999 CONTINUE
C
      END
