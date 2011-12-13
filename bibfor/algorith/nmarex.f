      SUBROUTINE NMAREX(MOTFAC,SDARCH)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*19 SDARCH
      CHARACTER*16 MOTFAC
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (ARCHIVAGE)
C
C CONSTRUCTION CHAMPS EXCLUS DE L'ARCHIVAGE
C
C ----------------------------------------------------------------------
C
C
C IN  MOTFAC : MOT-FACTEUR POUR LIRE <CHAM_EXCL>
C IN  SDARCH : NOM DE LA SD ARCHIVAGE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IBID
      INTEGER      NB
      CHARACTER*16 K16BID
      CHARACTER*24 ARCEXC
      INTEGER      JAREXC
      INTEGER      IARG
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- NOM DE LA SD
C
      ARCEXC = SDARCH(1:19)//'.AEXC' 
C
C --- CONSTRUCTION CHAMPS EXCLUS DE L'ARCHIVAGE
C
      CALL GETVTX(MOTFAC,'CHAM_EXCLU',1,IARG,0,K16BID,NB    )
      NB     = -NB
      IF (NB .NE. 0) THEN
        CALL WKVECT(ARCEXC,'V V K16',NB,JAREXC)
        CALL GETVTX(MOTFAC,'CHAM_EXCLU',1,IARG,NB,ZK16(JAREXC),IBID)
      ENDIF  
C
      CALL JEDEMA()

      END
