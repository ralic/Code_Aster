      SUBROUTINE FOECGV (NOMFON,IUL,IND,FONRES,IPS )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                   IUL,IND,       IPS
      CHARACTER*19       NOMFON,        FONRES
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/06/2004   AUTEUR DURAND C.DURAND 
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
C     IMPRESSION DES VALEURS DE LA FONCTION AU FORMAT "GNUPLOT"
C     ------------------------------------------------------------------
C IN  : NOMFON : NOM DE LA FONCTION OU DE LA NAPPE A IMPRIMER
C IN  : IUL    : NUMERO D'UNITE LOGIQUE
C IN  : IND    : > 0 , IMPRESSION SUIVANT UNE LISTE DE PARAMETRES
C IN  : FONRES : LISTE DES PARAMETRES A IMPRIMER
C IN  : IPS    : = 1 , SORTIE POSCRIPT
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
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8         VALPU(8)
      CHARACTER*8    K8B
      CHARACTER*16   NOMPU(2)
      CHARACTER*19   NOMF1
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IF (IPS.EQ.1) THEN
         NOMF1 = '%%%'
      ELSE
         NOMF1 = NOMFON
      ENDIF
      LF = LXLGUT(NOMF1)
C
      CALL JEVEUO(NOMFON//'.PROL','L',LPRO)
      IF ( ZK16(LPRO) .EQ. 'INTERPRE' )  THEN
         CALL JEVEUO(NOMFON//'.NOVA','L',LNOVA)
         CALL JEVEUO(FONRES//'.VALE','L',JVAL)
         CALL JELIRA(FONRES//'.VALE','LONUTI',NBVAL,K8B)
         WRITE(IUL,1000) NOMF1
         DO 10 IVAL = 0, NBVAL-1
          CALL FOINTE('F ',NOMFON,1,ZK8(LNOVA),ZR(JVAL+IVAL),XRESU,IRET)
            WRITE(IUL,1040) ZR(JVAL+IVAL), XRESU
 10      CONTINUE
C
      ELSEIF ( ZK16(LPRO) .EQ. 'FONCTION' )  THEN
         WRITE(IUL,1000) NOMF1
         IF ( IND.NE.0 ) THEN
            NOMPU(1) = ZK16(LPRO+2)
            CALL JEVEUO(FONRES//'.VALE','L',JVAL)
            CALL JELIRA(FONRES//'.VALE','LONUTI',NBVAL,K8B)
            DO 20 IVAL = 0, NBVAL-1
            CALL FOINTE('F ',NOMFON,1,NOMPU(1),ZR(JVAL+IVAL),XRESU,IRET)
               WRITE(IUL,1040) ZR(JVAL+IVAL), XRESU
 20         CONTINUE
         ELSE
            CALL JELIRA(NOMFON//'.VALE','LONMAX',NBVAL,K8B)
            CALL JEVEUO(NOMFON//'.VALE','L',JVAL)
            NBVAL = NBVAL / 2
            JFON  = JVAL + NBVAL
            DO 22 IVAL = 0,NBVAL-1
               WRITE(IUL,1040) ZR(JVAL+IVAL) , ZR(JFON+IVAL)
 22         CONTINUE
         ENDIF
C
      ELSEIF ( ZK16(LPRO) .EQ. 'NAPPE' )  THEN
         CALL JEVEUO(NOMFON//'.PARA','L',JPAR)
         CALL JELIRA(NOMFON//'.PARA','LONMAX',NBAMOR,K8B)
         NOMPU(1) = ZK16(LPRO+2)
         NOMPU(2) = ZK16(LPRO+5)
         LA = LXLGUT(NOMPU(1))
         IF ( IND.NE.0 ) THEN
            CALL JEVEUO(FONRES//'.VALE','L',JVAL)
            CALL JELIRA(FONRES//'.VALE','LONUTI',NBVAL,K8B)
            DO 30 IA = 1, NBAMOR
               VALPU(1) = ZR(JPAR+IA-1)
               WRITE(IUL,1020) NOMF1(1:LF), NOMPU(1)(1:LA), VALPU(1)
               DO 32 IVAL = 0, NBVAL-1
                  VALPU(2) = ZR(JVAL+IVAL)
                  CALL FOINTE('F ',NOMFON,2,NOMPU,VALPU,XRESU,IRET)
                  WRITE(IUL,1040) VALPU(2) , XRESU
 32            CONTINUE
 30         CONTINUE
         ELSE
            DO 40 IA = 1, NBAMOR
              CALL JELIRA(JEXNUM(NOMFON//'.VALE',IA),'LONMAX',NBVAL,K8B)
               CALL JEVEUO(JEXNUM(NOMFON//'.VALE',IA),'L',JVAL)
               NBVAL = NBVAL / 2
               JFON = JVAL + NBVAL
               WRITE(IUL,1020) NOMF1(1:LF),NOMPU(1)(1:LA),ZR(JPAR+IA-1)
               DO 42 IVAL = 0, NBVAL-1
                  WRITE(IUL,1040) ZR(JVAL+IVAL), ZR(JFON+IVAL)
 42            CONTINUE
 40         CONTINUE
         ENDIF
      ENDIF
C
 1000 FORMAT('#NOM DE LA FONCTION: ',A)
 1020 FORMAT('#NOM DE LA NAPPE: ',A,' , ',A,':',F8.4)
 1040 FORMAT(1X,1P,E12.5,1X,E12.5)
C
      CALL JEDEMA()
      END
