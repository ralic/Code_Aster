      SUBROUTINE FONIMP(RESU)

      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8    RESU

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C
C     ----------------------------------------------------------------
C FONCTION REALISEE:
C
C     OPERATEUR DEFI_FOND_FISS :
C          ROUTINE D'IMPRESSION EN INFO=2
C
C     ------------------------------------------------------------------
C ENTREE:
C        RESU   : NOM DE LA SD_FOND_FISS
C
C     ------------------------------------------------------------------
C
C
      CHARACTER*8  K8B
      CHARACTER*24 FONOEU,FONDFI
      INTEGER      JNOE,JFON
      INTEGER      LNOFF,I
C
C
      CALL JEMARQ()

C
      FONOEU = RESU//'.FOND.NOEU'
      CALL JELIRA(FONOEU,'LONMAX',LNOFF,K8B)
      CALL JEVEUO(FONOEU,'L',JNOE)

      FONDFI = RESU//'.FONDFISS'
      CALL JEVEUO(FONDFI,'L',JFON)


      DO 100 I=1,LNOFF
C        WRITE(6,*)'NOEUD ',ZK8(JNOE),ZR(JFON-1+4*(I-1)+4)
 100  CONTINUE
 
      CALL JEDEMA()
      END
