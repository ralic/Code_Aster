      SUBROUTINE PRAFFM ( MCF, SDLIEU, SDEVAL, SDMOY, OPTION, SM,
     +                    SNORIG, SNEXTR, SNFLEX, ICHEF )
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8              SM, SNORIG(*), SNEXTR(*), SNFLEX(*)
      CHARACTER*16        OPTION
      CHARACTER*19        SDEVAL
      CHARACTER*24        SDLIEU, SDMOY
      CHARACTER*(*)       MCF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     AFFICHAGE MOYENNE DE LA COMMANDE POST_RCCM
C     ------------------------------------------------------------------
C IN  SDLIEU : K : SD DU LIEU TRAITEE
C IN  SDEVAL : K : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
C IN  SDMOY  : K : SD DES MOYENNES
C IN  OPTION : K : NOM DE L' OPTION   TRAITEE
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      ANOCP, NBCP, IOC, NBPT, NBOC, ASDMO
      INTEGER      I, ANOMND, NBCO, NBSP, IAD
      REAL*8       XA, XB, YA, YB, AX, ZA, ZB
      CHARACTER*4  DOCUL
      CHARACTER*24 NPCMP, NABSC, NNOCP
      CHARACTER*1  K1BID
C======================================================================
C
      CALL JEMARQ()
      NNOCP = SDEVAL//'.NOCP'
      NABSC = SDLIEU(1:19)//'.ABSC'
      CALL JELIRA ( SDLIEU(1:19)//'.REFE', 'DOCU', I, DOCUL )
      CALL JEVEUO ( SDLIEU(1:19)//'.DESC', 'L', ANOMND)
      CALL JELIRA ( NABSC,'NMAXOC', NBOC, K1BID )
      CALL JELIRA ( NNOCP,'LONMAX', NBCP, K1BID )
      CALL JEVEUO ( NNOCP,'L',ANOCP)
      CALL JEVEUO ( SDEVAL//'.PNCO', 'L', IAD )
      NBCO = ZI(IAD)
      CALL JEVEUO ( SDEVAL//'.PNSP', 'L', IAD )
      NBSP = ZI(IAD)
      IF ( NBOC .GT. 1 ) CALL UTMESS('F','PRAFFM',
     &                   'LE CHEMIN EST DEFINI PAR PLUSIEURS SEGMENT'//
     &                   'S, MODIFIER LA TOLERANCE DANS INTE_MAIL.')
      DO 100, IOC = 1, NBOC, 1
         CALL JELIRA(JEXNUM(NABSC ,IOC),'LONMAX',NBPT,K1BID)
         CALL JEVEUO(JEXNUM(SDMOY ,IOC),'L',ASDMO)
         IF ( MCF(1:5) .EQ. 'PM_PB' ) THEN
            CALL PRNIV0 ( ZR(ASDMO), NBCP, NBCO, NBSP, SNORIG )
         ELSEIF ( MCF(1:2) .EQ. 'SN' ) THEN
            CALL PRNIV1 ( ZR(ASDMO), ZK8(ANOCP), NBCP, NBCO, NBSP,
     +                    SNORIG, SNEXTR, SNFLEX )
         ENDIF
100   CONTINUE
C
      CALL JEDEMA()
      END
