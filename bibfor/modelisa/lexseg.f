      LOGICAL FUNCTION LEXSEG(CONNEX,TYPMAI,NBRMA,N1,N2)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C-----------------------------------------------------------------------
C     FONCTION BOOLEENNE INDIQUANT L'EXISTENCE DANS LE MAILLAGE D'UNE
C     MAILLE SEGMENT DE NOEUD ORIGINE DE NUMERO N1 ET NOEUD EXTREMITE
C     DE NUMERO N2
C     APPELANT : FENEXC
C-----------------------------------------------------------------------
C IN : CONNEX : CHARACTER*24 , NOM DE L'OBJET CONNECTIVITE
C IN : TYPMAI : CHARACTER*24 , NOM DE L'OBJET CONTENANT LES TYPES
C               DES MAILLES
C IN : NBRMA  : INTEGER , NOMBRE DE MAILLES DU MAILLAGE
C IN : N1     : INTEGER , NUMERO DU NOEUD ORIGINE
C IN : N2     : INTEGER , NUMERO DU NOEUD EXTREMITE
C-----------------------------------------------------------------------
      CHARACTER*24  CONNEX, TYPMAI
      INTEGER       NBRMA, N1, N2
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER MI ,NUEXTR ,NUORIG 
C-----------------------------------------------------------------------
      LEXSEG = .FALSE.
      DO 10 MI = 1, NBRMA
         CALL I2EXTF(MI,1,CONNEX(1:15),TYPMAI(1:16),NUORIG,NUEXTR)
         IF ( NUORIG.EQ.N1 .AND. NUEXTR.EQ.N2 ) THEN
            LEXSEG = .TRUE.
            GO TO 11
         ENDIF
  10  CONTINUE
  11  CONTINUE
      END
