      SUBROUTINE IBTLOT ( LOT , IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL             LOT
      INTEGER                   IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 03/07/96   AUTEUR CIBHHLV L.VIVAN 
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
C     TRAITEMENT PAR LOT OU COMMANDE PAR COMMANDE.
C     ------------------------------------------------------------------
C OUT IER  : CODE RETOUR
C            0 TOUT C'EST BIEN PASSE
C            1 ERREUR DANS LA LECTURE DE LA COMMANDE
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         UTMESS
C     ROUTINE(S) FORTRAN     :
C         -
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
      CHARACTER*3  REPONS
      CHARACTER*16 NOMCMD,CBID
C
C     --- OPTIONS PAR DEFAUT ---

      CALL JEMARQ()
      INTER=ISINTE(-1)
      IF (INTER .GT. 0) THEN
C        EXECUTION AVEC L'OPTION INTERACTIVE
         REPONS = 'NON'
      ELSE
C        EXECUTION SANS L'OPTION INTERACTIVE
         REPONS = 'OUI'
      ENDIF

      IER    = 0
C
      CALL GETVTX(' ','PAR_LOT',1,1,1,REPONS,L)
      CALL GETRES(CBID,CBID,NOMCMD)
      ILGCMD = LXLGUT(NOMCMD)
      IF ( REPONS.NE.'OUI' .AND. REPONS.NE.'NON') THEN
         IER = IER + 1
         CALL UTDEBM('E',NOMCMD,'ARGUMENT ERRONE POUR LE MOT '//
     +                      'CLE "PAR_LOT" ')
         CALL UTIMPK('S',':',1,REPONS)
         CALL UTIMPK('L','LES ARGUMENTS AUTORISES SONT',1,'OUI')
         CALL UTIMPK('S',',',1,'NON')
         CALL UTFINM()
      ENDIF
      IF ( REPONS.EQ.'NON') THEN
         CALL UTMESS('I',NOMCMD(:ILGCMD),'TRAITEMENT COMMANDE PAR '//
     +                                             'COMMANDE DEMANDE.')
      ELSEIF ( REPONS.EQ.'OUI') THEN
         CALL UTMESS('I',NOMCMD(:ILGCMD),'TRAITEMENT DES COMMANDES '//
     +                                             'PAR LOT DEMANDE.')
      ENDIF
      CALL WKVECT('&&SYS   .PAR_LOT','L V K8',1,LLOT)
      ZK8(LLOT) = REPONS
      LOT       = REPONS .EQ. 'OUI'
C
      CALL JEDEMA()
      END
