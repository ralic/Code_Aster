      SUBROUTINE VERIS3()
      IMPLICIT NONE
      CHARACTER*16 SDP, SDU,NOMCMD
      CHARACTER*8 NOMSD
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C    POUR VERIFIER LA SD PRODUITE PAR LA COMMANDE COURANTE
C ----------------------------------------------------------------------
      NOMSD=' '
      SDU=' '
      NOMCMD=' '
      CALL GETRES(NOMSD,SDU,NOMCMD)

C     -- SI LA COMMANDE PRODUIT UN RESULTAT :
      IF (NOMSD.NE.' ') THEN
         CALL SDUSDP(SDU,SDP)
C        CALL VERISD(SDP,NOMSD)
      ENDIF
      END
