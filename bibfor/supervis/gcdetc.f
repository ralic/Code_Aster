      SUBROUTINE GCDETC(CONCEP)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CONCEP
C
C     ------------------------------------------------------------------
C     MARQUAGE A &DETRUIT DU CONCEPT : CONCEP DANS LES
C     DECLARATIONS PRECEDENTES
C     ------------------------------------------------------------------
C OUT IER   : IS   : CODE RETOUR
C          > 0  NUMERO DE LA COMMANDE AYANT CREE LE COUPLE
C          = 0  RESUL ABSENT
C          < 0  RESUL PRESENT MAIS PAS DU BON CONCEPT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 07/05/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN GCDETC
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      INTEGER         ICMD
C     ------------------------------------------------------------------
C
C     --- VARIABLES LOCALES --------------------------------------------
C     ------------------------------------------------------------------
C
C     --- ALLOCATION DE LA TABLE DES RESULTATS ---
      CALL JEMARQ()
      CALL GCUOPR( -1 ,ICMD)
C
C     DESCRIPTION DE '&&SYS.KRESU' :
C          ZK80(JCMD)( 1: 8) = NOM UTILISATEUR DU RESULTAT
C          ZK80(JCMD)( 9:24) = NOM DU CONCEPT DU RESULTAT
C          ZK80(JCMD)(25:40) = NOM DE L'OPERATEUR
C          ZK80(JCMD)(41:48) = STATUT DE L'OBJET
C
C     --- NOM UTILISATEUR DU RESULTAT ---
      CALL JEVEUO ('&&SYS.KRESU','E',LGRESU)
      DO 10 I = 1 , ICMD
         IF(ZK80(LGRESU-1+I)(1:8) .EQ. CONCEP ) THEN
            ZK80(LGRESU-1+I)(41:48) = '&DETRUIT'
         ENDIF
  10  CONTINUE
C
C     --- DESALLOCATION DE LA TABLE DES RESULTATS ---
C
      CALL JEDEMA()
      END
