      SUBROUTINE OP0042 ( IER )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            IER
C     ------------------------------------------------------------------
C     COMMANDE D'AIDE
C     ------------------------------------------------------------------
      CHARACTER*16 CBID, CONCEP, NOMCMD
      CHARACTER*24  FICHIE
C     ------------------------------------------------------------------
C
      CALL INFMAJ()
      CALL GETRES( CBID , CONCEP , NOMCMD )
C
C     ---------- FICHIER D'ECRITURE ------------------------------------
      CALL GETVTX(' ','FICHIER',0,1,1,FICHIE,L)
      IF ( L .EQ. 0 ) THEN
         FICHIE = 'RESULTAT'
      ENDIF
      IMPR = IUNIFI(FICHIE)
      IF ( IMPR .EQ. 0 ) THEN
         CALL UTMESS('I','AIDE','LE FICHIER "'//FICHIE(1:7)//
     +                                          '" N''EST PAS OUVERT.')
         GOTO 9999
      ENDIF
C
C     ---------- TRAITEMENT DE CE QUI EST RELATIF AUX CONCEPTS ------
      CALL GETFAC( 'CONCEPT', NBOCC )
      IF ( NBOCC .GT. 0 ) CALL AIDCON( NBOCC , IMPR, IER )
C     ------------------------------------------------------------------
C
C     ---------- TRAITEMENT DE CE QUI EST RELATIF A LA COMPLETUDE ---
      CALL GETFAC( 'TYPE_ELEM', NBOCC )
      IF ( NBOCC .GT. 0 ) CALL AIDTYP( NBOCC , IMPR, IER )
C     ------------------------------------------------------------------
C
 9999 CONTINUE
      END
