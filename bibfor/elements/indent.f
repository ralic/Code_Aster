      SUBROUTINE INDENT(I,DDLS,DDLM,NNOS,IN)
      IMPLICIT   NONE
      INTEGER    I,DDLS,DDLM,NNOS,IN

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/06/2010   AUTEUR CARON A.CARON 
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
C.......................................................................
C
C         CALCUL DU SAUT DE DDL LORSQUE LE NOMBRE DE DDL EST DIFFERENT
C              SUR LES NOEUDS SOMMETS ET LES NOEUDS MILIEUX
C
C  ENTREES  
C
C  DDLS    : NOMBRE DE DDL SUR NOEUDS SOMMETS
C  DDLM    : NOMBRE DE DDL SUR NOEUDS MILIEU
C  NNOS    : NOMBRE DE NOEUDS SOMMETS DE L'ELEMENT
C
C  SORTIE
C  IN      : SAUT DE DDL
C......................................................................

      INTEGER     NNOI,DDLI     
C......................................................................

      CALL JEMARQ()

      IF (I.LE.NNOS) THEN
         NNOI=0
         DDLI=DDLS
      ELSEIF (I.GT.NNOS) THEN
         NNOI=NNOS
         DDLI=DDLM
      ENDIF
 
      IN = DDLS*NNOI+DDLI*(I-NNOI-1)

      CALL JEDEMA()
      END
