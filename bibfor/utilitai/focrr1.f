      SUBROUTINE FOCRR1 ( NOMFOZ, RESZ, BASZ, NOMCHZ,
     +                    MAILLZ, NOEUZ, CMPZ, NPOINT, NUSP )
      IMPLICIT   NONE
      INTEGER             NPOINT, NUSP
      CHARACTER*1         BASE
      CHARACTER*8         MAILLE, NOEUD, CMP
      CHARACTER*16        NOMCHA
      CHARACTER*(*)       NOMFOZ, RESZ,NOMCHZ,CMPZ,MAILLZ,NOEUZ,BASZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 28/08/2002   AUTEUR MCOURTOI M.COURTOIS 
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
C BUT : FABRIQUER UNE FONCTION EN RELEVANT LES VALEURS D'UNE CMP
C       D'UN CHAMP POUR TOUS LES NUMEROS D'ORDRE D'UNS SD_RESULTAT
C     ------------------------------------------------------------------
C VAR : NOMFOZ : NOM DE LA FONCTION
C IN  : RESZ   : NOM DE LA STRUCTURE RESULTAT
C IN  : BASZ   : BASE OU L'ON CREE LA FONCTION
C IN  : NOMCHZ : NOM DU CHAMP
C IN  : NOEUZ  : NOM DU NOEUD
C IN  : MAILLZ : NOM DE LA MAILE
C IN  : CMPZ   : NON DE LA COMPOSANTE A CHERCHER
C IN  : NPOINT : NUMERO DU POINT ( CAS DES CHAM_ELEMS )
C IN  : NUSP   : NUMERO DU SOUS-POINT ( CAS DES CHAM_ELEMS )
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       IBID, NBORDR, LORDR, IVARI
      REAL*8        R8B
      COMPLEX*16    C16B
      CHARACTER*8   K8B, INTERP
      CHARACTER*19  KNUME,RESU,NOMFON
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      RESU=RESZ
      NOMFON=NOMFOZ
      NOMCHA=NOMCHZ
      CMP=CMPZ
      MAILLE=MAILLZ
      NOEUD=NOEUZ
      BASE=BASZ
C
      KNUME = '&&FOCRR1.NUME_ORDR'
C
      CALL RSORAC (RESU, 'LONUTI', IBID,R8B,K8B,C16B,R8B,K8B,
     +                                                 NBORDR,1,IBID)
      CALL WKVECT ( KNUME , 'V V I', NBORDR, LORDR )
      CALL RSORAC ( RESU, 'TOUT_ORDRE', IBID,R8B,K8B,C16B,R8B,K8B,
     +                                        ZI(LORDR),NBORDR,IBID)
C
      INTERP = 'LIN LIN '
C
      IVARI=0
      CALL FOCRR0 ( NOMFON, INTERP, BASE, RESU, NOMCHA,
     +                    MAILLE, NOEUD, CMP ,NPOINT, NUSP, IVARI,
     +                    NBORDR, ZI(LORDR) )
C
      CALL JEDETR( KNUME )
C
      CALL JEDEMA()
      END
