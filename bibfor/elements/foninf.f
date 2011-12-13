      SUBROUTINE FONINF (RESU)

      IMPLICIT NONE
      CHARACTER*8         RESU

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/12/2011   AUTEUR GENIAUT S.GENIAUT 
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
C
C       ---------------------------------------------------------------
C       STOCKAGE D'INFOS UTILES DANS LA SD EN SORTIE DE DEFI_FOND_FISS
C       ---------------------------------------------------------------
C
C IN/OUT
C       RESU   : NOM DE LA SD EN SORTIE DE DEFI_FOND_FISS

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
      CHARACTER*32     JEXNOM, JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      IARG,IBID,JINFO
      CHARACTER*8  SYME,CONFIN

C     -----------------------------------------------------------------
C           
      CALL JEMARQ() 
C
C     RECUPERATION DU MOT-CLE SYME
      CALL GETVTX (' ', 'SYME',0,IARG,1,SYME,IBID)
      CALL ASSERT(SYME.EQ.'OUI'.OR.SYME.EQ.'NON')

C     RECUPERATION DU MOT-CLE CONFIG_INIT
      CALL GETVTX (' ', 'CONFIG_INIT',0,IARG,1,CONFIN,IBID)
      CALL ASSERT(CONFIN.EQ.'DECOLLEE'.OR.CONFIN.EQ.'COLLEE')

C     CREATION DE L'OBJET .INFO DANS LA SD FOND_FISS
      CALL WKVECT(RESU//'.INFO','G V K8',2,JINFO)

C     STOCKAGE DU MOT-CLE SYME
      ZK8(JINFO-1+1) = SYME

C     STOCKAGE DU MOT-CLE CONFIG_INIT
      ZK8(JINFO-1+2) = CONFIN

      CALL JEDEMA()
      END
