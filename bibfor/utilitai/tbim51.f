      SUBROUTINE TBIM51 ( CHAINE, IDEB, IFIN, TYPVAL, IADR, FORMR,
     +                    IR, J )
      IMPLICIT   NONE
      INTEGER             IDEB, IFIN, IADR, IR, J
      CHARACTER*16        FORMR
      CHARACTER*(*)       CHAINE, TYPVAL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/12/98   AUTEUR CIBHHLV L.VIVAN 
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
C      IMPRESSION DE LA TABLE AVEC PAGINATION
C ----------------------------------------------------------------------
C IN  : TABLE  : NOM DE LA STRUCTURE "TABLE"
C ----------------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ----------------------------------------------------------------------
      REAL*8        DBLE, DIMAG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IF     ( TYPVAL(1:1) .EQ. 'I'   ) THEN
         IFIN = IDEB + 12 - 1
         WRITE(CHAINE(IDEB:IFIN),'(I12)') ZI(IADR+J-1)
         IDEB = IFIN + 2
      ELSEIF ( TYPVAL(1:1) .EQ. 'R'   ) THEN
         IFIN = IDEB + IR - 1
         WRITE(CHAINE(IDEB:IFIN),FORMR) ZR(IADR+J-1)
         IDEB = IFIN + 2
      ELSEIF ( TYPVAL(1:1) .EQ. 'C' ) THEN
         IFIN = IDEB + IR - 1
         WRITE(CHAINE(IDEB:IFIN),FORMR) DBLE( ZC(IADR+J-1) )
         IDEB = IFIN + 2
         IFIN = IDEB + IR - 1
         WRITE(CHAINE(IDEB:IFIN),FORMR) DIMAG( ZC(IADR+J-1) )
         IDEB = IFIN + 2
      ELSEIF ( TYPVAL(1:3) .EQ. 'K80' ) THEN
         IFIN = IDEB + 80 - 1
         CHAINE(IDEB:IFIN) = ZK80(IADR+J-1)
         IDEB = IFIN + 2
      ELSEIF ( TYPVAL(1:3) .EQ. 'K32' ) THEN
         IFIN = IDEB + 32 - 1
         CHAINE(IDEB:IFIN) = ZK32(IADR+J-1)
         IDEB = IFIN + 2
      ELSEIF ( TYPVAL(1:3) .EQ. 'K24' ) THEN
         IFIN = IDEB + 24 - 1
         CHAINE(IDEB:IFIN) = ZK24(IADR+J-1)
         IDEB = IFIN + 2
      ELSEIF ( TYPVAL(1:3) .EQ. 'K16' ) THEN
         IFIN = IDEB + 16 - 1
         CHAINE(IDEB:IFIN) = ZK16(IADR+J-1)
         IDEB = IFIN + 2
      ELSEIF ( TYPVAL(1:2) .EQ. 'K8' ) THEN
         IFIN = IDEB + 8 - 1
         CHAINE(IDEB:IFIN) = ZK8(IADR+J-1)
         IDEB = IFIN + 2
      ENDIF
C
      CALL JEDEMA()
C
      END
