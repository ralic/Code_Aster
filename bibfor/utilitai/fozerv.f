      SUBROUTINE FOZERV(NOMFON)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     NOMFON
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     CREATION D'UN OBJET DE TYPE FONCTION CONSTANTE DE VALEUR NULLE
C     ------------------------------------------------------------------
C IN  NOM DE LA FONCTION CONSTANTE A CREER
C     ------------------------------------------------------------------
C     OBJETS SIMPLES CREES:
C        NOMFON//'.PROL'
C        NOMFON//'.VALE
C     ------------------------------------------------------------------
C
      CHARACTER*19 NOMF
      CHARACTER*24 CHPRO, CHVAL
C-----------------------------------------------------------------------
      INTEGER IRET ,JPRO ,LVAL ,LXLGUT 
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
      NOMF  = NOMFON
      CHPRO = NOMF//'.PROL'
      CALL JEEXIN(CHPRO,IRET)
      IF (IRET.NE.0) GOTO 9999
C
      CALL ASSERT(LXLGUT(NOMF).LE.24)
      CALL WKVECT(CHPRO,'V V K24',6,JPRO)
      ZK24(JPRO)   = 'CONSTANT'
      ZK24(JPRO+1) = 'LIN LIN '
      ZK24(JPRO+2) = 'TOUTPARA'
      ZK24(JPRO+4) = 'CC      '
      ZK24(JPRO+5) = NOMF
C
C     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE ---
      CHVAL(1:19)  = NOMF
      CHVAL(20:24) = '.VALE'
      CALL WKVECT(CHVAL,'V V R',2,LVAL)
      ZR(LVAL)   = 1.0D0
      ZR(LVAL+1) = 0.D0
C
C     --- LIBERATIONS ---
9999  CONTINUE
      CALL JEDEMA()
      END
