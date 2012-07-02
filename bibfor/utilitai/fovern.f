      SUBROUTINE FOVERN(VECNOM,NBFONC,VECPRO,IER)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER                  NBFONC,       IER
      CHARACTER*(*)     VECNOM(NBFONC),    VECPRO(*)
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
C     VERIFICATION DE L'HOMOGENEITE DES PARAMETRES DES FONCTIONS
C     COMPOSANT UNE NAPPE
C     STOCKAGE DE CE PARAMETRE UNIQUE ET DES TYPES DE PROLONGEMENTS
C     ET D'INTERPOLATION DE CHAQUE FONCTION
C     ------------------------------------------------------------------
C IN  VECNOM: VECTEUR DES NOMS DES FONCTIONS
C IN  NBFONC: NOMBRE DE FONCTIONS
C OUT    VECPRO: VECTEUR DESCRIPTEUR DE LA NAPPE
C     ------------------------------------------------------------------
C     OBJETS SIMPLES LUS
C        CHNOM=VECNOM(I)//'.PROL'
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
      INTEGER      I,JPROF,NBPF
      INTEGER VALI
      CHARACTER*24 CHNOM
      CHARACTER*24 VALK(3)
      CHARACTER*16 PROLGD,INTERP,TYPFON,NOMPF(10)
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CHNOM(20:24) = '.PROL'
      DO 1 I=1,NBFONC
         CHNOM(1:19) = VECNOM(I)
         CALL JEVEUO(CHNOM,'L',JPROF)
         CALL FOPRO1(ZK24(JPROF),0,PROLGD,INTERP)
         CALL FONBPA(CHNOM(1:19),ZK24(JPROF),TYPFON,10,NBPF,NOMPF)
         CALL JELIBE(CHNOM)
         IF (NOMPF(1).NE.'TOUTPARA') THEN
            VECPRO(7)=NOMPF(1)
            GO TO 2
         END IF
    1 CONTINUE
      VALI = NBFONC
      CALL U2MESG('E','UTILITAI8_1',0,' ',1,VALI,0,0.D0)
      IER=IER+1
    2 CONTINUE
      DO 3 I=1,NBFONC
         CHNOM(1:19) = VECNOM(I)
         CALL JEVEUO(CHNOM,'L',JPROF)
         CALL FOPRO1(ZK24(JPROF),0,PROLGD,INTERP)
         CALL FONBPA(CHNOM(1:19),ZK24(JPROF),TYPFON,10,NBPF,NOMPF)
         CALL JELIBE(CHNOM)
         IF (NOMPF(1).NE.VECPRO(7).AND.NOMPF(1).NE.'TOUTPARA') THEN
            VALK (1) = VECNOM(I)
            VALK (2) = NOMPF(1)
            VALK (3) = VECPRO(7)
            CALL U2MESG('E','UTILITAI8_2',3,VALK,0,0,0,0.D0)
            IER=IER+1
         END IF
         VECPRO(7+2*I-1) = INTERP
         VECPRO(7+2*I  ) = PROLGD
    3 CONTINUE
      CALL JEDEMA()
      END
