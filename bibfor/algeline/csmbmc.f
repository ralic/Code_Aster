      SUBROUTINE CSMBMC(NOMMAT,NEQ,VSMB)
      IMPLICIT NONE
      CHARACTER*(*) NOMMAT
      COMPLEX*16 VSMB(*)
      INTEGER NEQ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C BUT :
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     COMMUNS JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
C     VARIABLES LOCALES
C-----------------------------------------------------------------------
      INTEGER JCCID,IEQ,JREFA,JNUGL,ICCID
      CHARACTER*14 NU
      CHARACTER*19 MAT
C-----------------------------------------------------------------------
C     DEBUT
      CALL JEMARQ()
C-----------------------------------------------------------------------
      MAT = NOMMAT

      CALL JEVEUO(MAT//'.REFA','L',JREFA)
      IF (ZK24(JREFA-1+11).EQ.'MATR_DISTR') THEN
        NU = ZK24(JREFA-1+2)(1:14)
        CALL JEVEUO(NU//'.NUML.NUGL','L',JNUGL)

        CALL JEEXIN(MAT//'.CCID',ICCID)

        IF ( ICCID.NE.0 ) THEN
        CALL JEVEUO(MAT//'.CCID','L',JCCID)
        DO 10 IEQ = 1,NEQ
C         SI LE DDL NE N'APPARTIENT PAS AU PROC COURANT ET QU'IL Y A
C         UNE CHARGE CINEMATIQUE DESSUS, ON MET LE SECOND MEMBRE A ZERO
C         SUR LE PROC COURANT POUR EVITER DES INTERFERENCES AVEC
C         LE PROC QUI POSSEDE EFFECTIVEMENT LE DDL BLOQUE
          IF ( (ZI(JNUGL+IEQ-1).EQ.0).AND.(ZI(JCCID-1+IEQ).EQ.1) ) THEN
            VSMB(IEQ) = 0.D0
          END IF
   10   CONTINUE
        ENDIF
      ENDIF

      CALL JEDEMA()
      END
