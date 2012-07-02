      SUBROUTINE I2EXTF (M,F,CONEC,TYPE,N1,N2)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C**********************************************************************
C
C     SAISIE DES EXTREMITES D' UNE FACE D' UNE MAILLE EN 2D
C
C       M     (IN)  : NUMERO DE LA MAILLE
C
C       F     (IN)  : NUMERO DE LA FACE
C
C       CONEC (IN)  : NOM DE L' OBJET CONECTIVITE
C
C       TYPE  (IN)  : NOM DE L' OBJET CONTENANT LES TYPES DES MAILLES
C
C       N1    (OUT) : NUMERO DU NOEUD ORIGINE
C
C       N2    (OUT) : NUMERO DU NOEUD EXTREMITE
C
C**********************************************************************
C
      INCLUDE 'jeveux.h'
      CHARACTER*(*) CONEC
      CHARACTER*(*) TYPE
      INTEGER      M,F,N1,N2
C
      INTEGER      ADRM,ATYPM,NBN,NBF
      CHARACTER*8  TYPM
C
C
C
      CHARACTER*1 K1BID
C
C-----------------------------------------------------------------------
      INTEGER IATYMA 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      NBN   = 0
      NBF   = 0
      ADRM  = 0
      ATYPM = 0
      TYPM  = ' '
C
      CALL JEVEUO (JEXNUM(CONEC,M),'L',ADRM)
      CALL JEVEUO(TYPE,'L',IATYMA)
      ATYPM=IATYMA-1+M
      CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ZI(ATYPM)),TYPM)
C
      IF ( (TYPM .EQ. 'SEG2') .OR. (TYPM .EQ. 'SEG3') ) THEN
C
         N1 = ZI(ADRM)
         N2 = ZI(ADRM + 1)
C
      ELSE
C
         CALL JELIRA (JEXNUM(CONEC,M),'LONMAX',NBN,K1BID)
         CALL I2NBRF (NBN,NBF)
C
         N1 = ZI(ADRM + F-1)
C
         IF ( F .EQ. NBF ) THEN
C
            N2 = ZI(ADRM)
C
         ELSE
C
            N2 = ZI(ADRM + F)
C
         ENDIF
C
      ENDIF
C
      CALL JEDEMA()
      END
