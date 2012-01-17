      SUBROUTINE DDLLAG(NUME,IDDL,NEQ,LAGR1,LAGR2)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                IDDL,NEQ,LAGR1,LAGR2
      CHARACTER*(*)      NUME
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 16/01/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C ----------------------------------------------------------------------
C
C     RECHERCHE LES DEUX LAGRANGES ASSOCIES AU DDL IDDL.
C     CE IDDL DDL EST BLOQUE ET ON NE LE VERIFIE PAS.
C     DANS LE CAS OU IDDL N'EST PAS BLOQUE, LAGR1=LAGR2=0
C
C IN  : NUME   : NOM D'UN NUME_DDL
C IN  : IDDL   : NUMERO D'UN DDL BLOQUE
C IN  : NEQ    : NOMBRE D'EQUATIONS
C OUT : LAGR1  : PREMIER LAGRANGE ASSOCIE
C OUT : LAGR2  : DEUXIEME LAGRANGE ASSOCIE
C ----------------------------------------------------------------------
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNOM
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C ---------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------
      CHARACTER*24 NOMNU
C ----------------------------------------------------------------------
      CALL JEMARQ()
      LAGR1 = 0
      LAGR2 = 0
      NOMNU(1:14)  = NUME
      NOMNU(15:19) = '.NUME'
      CALL JEVEUO(NOMNU(1:19)//'.DEEQ','L',IADEEQ)
C
      INOE = ZI(IADEEQ + (2*(IDDL-1)) + 1 - 1 )
      ICMP = -ZI(IADEEQ + (2*(IDDL-1)) + 2 - 1 )
      ICAS = 1
      DO 10 I = 1,NEQ
         NN = ZI(IADEEQ + (2*(I-1)) + 1 - 1 )
         NC = ZI(IADEEQ + (2*(I-1)) + 2 - 1 )
         IF (NN.EQ.INOE .AND. NC.EQ.ICMP) THEN
            IF (ICAS.EQ.1) THEN
               LAGR1 = I
               ICAS = 2
            ELSE
               LAGR2 = I
               GOTO 9999
            ENDIF
         ENDIF
 10   CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
      END
