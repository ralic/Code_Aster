      SUBROUTINE TBIMTR ( TABLE, NEWTAB )
      IMPLICIT   NONE
      CHARACTER*19        TABLE, NEWTAB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 27/09/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     OPERATEUR  IMPR_TABLE , TRAITEMENT DU MOT CLE FACTEUR "TRI"
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IRET, LTITR, JTITR, ITITR, L, NPATRI, JTRPA, JTROR,
     +             LONMAX
      REAL*8       PREC
      CHARACTER*8  K8B, CRIT
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETVTX ( 'TRI', 'NOM_PARA' , 1,1,0, K8B, L )
      NPATRI = -L
C
      CALL WKVECT ( '&&TBIMTR.TRI_NP', 'V V K16', NPATRI, JTRPA )
      CALL WKVECT ( '&&TBIMTR.TRI_CR', 'V V K8' , NPATRI, JTROR )
C
      CALL GETVTX ( 'TRI', 'NOM_PARA', 1,1,NPATRI, ZK16(JTRPA), L )
      CALL GETVTX ( 'TRI', 'ORDRE'   , 1,1,NPATRI,  ZK8(JTROR), L )
      CALL GETVR8 ( 'TRI', 'PRECISION',1,1,1,            PREC , L )
      CALL GETVTX ( 'TRI', 'CRITERE'  ,1,1,1,            CRIT , L )
C
      CALL TBTRTB ( TABLE,'V', NEWTAB, NPATRI,ZK16(JTRPA),ZK8(JTROR),
     &              PREC, CRIT )
C
       CALL JEEXIN ( TABLE//'.TITR', IRET )
       IF ( IRET .NE. 0 ) THEN
          CALL JEVEUO ( TABLE//'.TITR', 'L', LTITR )
          CALL JELIRA ( TABLE//'.TITR', 'LONMAX', LONMAX, K8B )
          CALL WKVECT ( NEWTAB//'.TITR', 'V V K80', LONMAX, JTITR )
          DO 10 ITITR = 1 , LONMAX
             ZK80(JTITR+ITITR-1) = ZK80(LTITR+ITITR-1)
 10       CONTINUE
       ENDIF
C
      CALL JEDETR ( '&&TBIMTR.TRI_NP' )
      CALL JEDETR ( '&&TBIMTR.TRI_CR' )
C
      CALL JEDEMA()
      END
