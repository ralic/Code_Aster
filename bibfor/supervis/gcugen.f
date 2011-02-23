      SUBROUTINE GCUGEN ( ICODE , SRESUL, SCON, SCMD, ICMDCT )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             ICODE ,                     ICMDCT
      CHARACTER*(*)               SRESUL, SCON, SCMD
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 22/02/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C     ------------------------------------------------------------------
C     ACTUALISATION DU TABLEAU DES COMMANDES UTILISATEURS
C     ------------------------------------------------------------------
C IN  ICODE  : IS : CODE D'ENTREE/SORTIE
C         ICODE = -1  INTERROGATION SUR LE NOMBRE DE COMMANDE DEJA EXEC.
C         ICODE =  0  ON ATTAQUE UN NOUVEL OPERATEUR
C         ICODE =  1  ON SORT SANS ERREUR D'UN OPERATEUR
C         ICODE =  2  ON SORT EN ERREUR D'UN OPERATEUR, CONCEPT VALIDE
C OUT ICMDCT : IS : NUMERO DE LA COMMANDE COURANTE
C         ICODE = -1 NOMBRE DE COMMANDE DEJA EXECUTEE
C         ICODE =  0 C'EST LE NUMERO DE LA COMMANDE EXECUTEE
C         ICODE =  1 C'EST LE NUMERO DE LA COMMANDE QUI VIENT DE S'EXEC.
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN GCUOPR
C     ------------------------------------------------------------------
C
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
C     --- VARIABLES GLOBALES -------------------------------------------
      CHARACTER*8     CBID
C     ------- COMMUN DEBUG SUPERVISEUR ---------------------------------
      LOGICAL         LDBG
      INTEGER                IFV
      COMMON /CXSU00/ LDBG , IFV
C     ------------------------------------------------------------------
C
C     DESCRIPTION DE '&&SYS.KRESU' : '&&SYS   RESULT.USER    '
C          ZK80(ICMD)( 1: 8) = NOM UTILISATEUR DU RESULTAT
C          ZK80(ICMD)( 9:24) = NOM DU CONCEPT DU RESULTAT
C          ZK80(ICMD)(25:40) = NOM DE L'OPERATEUR
C          ZK80(ICMD)(41:48) = STATUT DE L'OBJET
C              STATUT = '&A FAIRE' : VALEUR INITIALE
C                       '&ATTENTE' : OPERATEUR DECODE, NON CHARGER
C                       '&ENCOURS' : OPERATEUR EN COURS D'EXECUTION
C                       '&EXECUTE' : OPERATEUR TERMINER CORRECTEMENT
C
      INTEGER          LONMAX
      CHARACTER*80     INIT
      CHARACTER*8                 RESUL
      CHARACTER*16                       CONCEP , CMD
      RESUL  = SRESUL
      CONCEP = SCON
      CMD    = SCMD
      INIT   = '&ABSENT &PAS DE CONCEPT &PAS DE COMMANDE&A FAIRE'
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL JELIRA ('&&SYS.KRESU','LONUTI',LONUTI,CBID)
      CALL JEVEUO ('&&SYS.KRESU','E',LGRESU)
      CALL JELIRA ('&&SYS.KRESU','LONMAX',LONMAX,CBID)
      IF ( LONUTI .GE. LONMAX ) THEN
        LONMAX = LONMAX + 500
        CALL JUVECA ('&&SYS.KRESU',LONMAX)
        CALL JEVEUO ('&&SYS.KRESU','E',LGRESU)
        DO 20 IK=LONUTI,LONMAX-1
          ZK80(LGRESU+IK) = INIT
20      CONTINUE
      ENDIF
C
      IF ( ICODE .EQ. -1 ) THEN
         ICMDCT = LONUTI
      ELSEIF ( ICODE .EQ. 0 ) THEN
         ICMDCT = LONUTI+1
         ZK80(LGRESU+ICMDCT-1)( 1: 8) = RESUL
         ZK80(LGRESU+ICMDCT-1)( 9:24) = CONCEP
         ZK80(LGRESU+ICMDCT-1)(25:40) = CMD
         ZK80(LGRESU+ICMDCT-1)(41:48) =    '&ENCOURS'
         IF (LDBG)   THEN
           WRITE(IFV,*) ' <GCUOPR> '
           DO 101 I = 1 , ICMDCT
             WRITE(IFV,*) ZK80(LGRESU+I-1)
101        CONTINUE
         ENDIF
C
      ELSEIF ( ICODE .EQ. 1 ) THEN
         LONUTI = LONUTI + 1
         ICMDCT = LONUTI
         ZK80(LGRESU+ICMDCT-1)(41:52) =    '&EXECUTE    '
         CALL JEECRA ('&&SYS.KRESU','LONUTI',ICMDCT,CBID)
      ELSEIF ( ICODE .EQ. 2 ) THEN
         LONUTI = LONUTI + 1
         ICMDCT = LONUTI
         ZK80(LGRESU+ICMDCT-1)(41:52) =    '&EXECUTESPVR'
         CALL JEECRA ('&&SYS.KRESU','LONUTI',ICMDCT,CBID)
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
      CALL JEDEMA()
      END
