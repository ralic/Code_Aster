      SUBROUTINE FONBPA ( NOMF, VEC, TYPFON, MXPF, NBPF, NOMPF )
      IMPLICIT NONE
      INTEGER             MXPF, NBPF
      CHARACTER*(*)       NOMF, VEC(*), TYPFON, NOMPF(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C     NOMBRE DE PARAMETRE D'UNE FONCTION ET NOMS DE CES PARAMETRES
C     ON PASSE VEC, DESCRIPTEUR D'UN OBJET FONCTION (OU NAPPE)
C     ------------------------------------------------------------------
C IN  NOMF  : NOM DE LA FONCTION
C IN  VEC   : VECTEUR DESCRIPTEUR DE L'OBJET FONCTION
C OUT TYPFON: =VEC(1) QUELLE SPECIF ...
C             TYPE DE FONCTION (CONSTANT, LINEAIRE, OU NAPPE)
C IN  MXPF  : NOMBRE MAXIMUM DE PARAMETRE DE LA FONCTION
C OUT NBPF  :NOMBRE DE PARAMETRES
C             0 POUR 'C', 1 POUR 'F',2 POUR 'N',  N POUR 'I'
C OUT NOMPF :NOMS DE CES PARAMETRES
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       LNOVA, IPA
      INTEGER VALI(2)
      CHARACTER*8   K8B
      CHARACTER*24 VALK
      CHARACTER*19  NOMFON
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      TYPFON = VEC(1)
C
      IF (VEC(1)(1:8).EQ.'CONSTANT') THEN
         NBPF = 0
         NOMPF(1) = VEC(3)
C
      ELSE IF (VEC(1)(1:8).EQ.'FONCTION') THEN
         NBPF = 1
         NOMPF(1) = VEC(3)
C
      ELSE IF (VEC(1)(1:7).EQ.'FONCT_C') THEN
         NBPF = 1
         NOMPF(1) = VEC(3)
C
      ELSE IF (VEC(1)(1:5).EQ.'NAPPE') THEN
         NBPF = 2
         NOMPF(1) = VEC(3)
         NOMPF(2) = VEC(7)
C
      ELSE IF (VEC(1)(1:8).EQ.'INTERPRE') THEN
         NOMFON = NOMF
         CALL JELIRA ( NOMFON//'.NOVA', 'LONUTI', NBPF, K8B )
         CALL JEVEUO ( NOMFON//'.NOVA', 'L', LNOVA )
         DO 12 IPA = 1, NBPF
            NOMPF(IPA) = ZK8(LNOVA+IPA-1)
   12    CONTINUE
C
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
C
      IF ( NBPF .GT. MXPF ) THEN
         NOMFON = NOMF
         VALK = NOMFON
         VALI (1) = NBPF
         VALI (2) = MXPF
         CALL U2MESG('F', 'UTILITAI6_37',1,VALK,2,VALI,0,0.D0)
      ENDIF
C
      CALL JEDEMA()
      END
