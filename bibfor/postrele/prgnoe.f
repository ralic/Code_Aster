      SUBROUTINE PRGNOE ( COURBE, PARASG, NOMA, MCF, IOCC, LSTNAC )
      IMPLICIT     NONE
      INTEGER             IOCC
      CHARACTER*8         NOMA, COURBE, PARASG
      CHARACTER*24        LSTNAC
      CHARACTER*(*)       MCF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 19/03/2002   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32    JEXNOM, JEXNUM
C     ------------------------------------------------------------------
      INTEGER         N1, INO, NBNO, JVALE, JNOEU, JLSTN, IRET
      REAL*8          PREC
      CHARACTER*3     K3B
      CHARACTER*8     K8B, GRN, CRIT
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETVID ( MCF, 'NOEUD', IOCC,1,0, K8B, N1 )
      IF ( N1 .NE. 0 ) THEN
         CALL CODENT(  IOCC, 'D0' , K3B  )
         COURBE = '&L_NO'//K3B
         PARASG = 'NOEUD'
         NBNO = -N1
         CALL WKVECT ( '&&PRGNOE_NOEUD', 'V V K8', NBNO, JNOEU )
         CALL GETVEM ( NOMA, 'NOEUD', MCF, 'NOEUD', IOCC,1,NBNO,
     +                                              ZK8(JNOEU), N1 )
C
         CALL WKVECT ( LSTNAC, 'V V I', NBNO, JLSTN )
         DO 10 INO = 1 , NBNO
            CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(JNOEU-1+INO)),
     +                                       ZI(JLSTN-1+INO) )
 10      CONTINUE
         CALL JEDETR ( '&&PRGNOE_NOEUD' )
      ENDIF
C
      CALL GETVID ( MCF, 'GROUP_NO', IOCC,1,0, K8B, N1 )
      IF ( N1 .NE. 0 ) THEN
         PARASG = 'GROUP_NO'
         CALL GETVEM(NOMA,'GROUP_NO',MCF,'GROUP_NO',IOCC,1,1,GRN,N1)
         COURBE = GRN
         CALL JELIRA(JEXNOM(NOMA//'.GROUPENO',GRN),'LONMAX',NBNO,K8B)
         CALL JEVEUO(JEXNOM(NOMA//'.GROUPENO',GRN),'L',JNOEU)
C
         CALL WKVECT ( LSTNAC, 'V V I', NBNO, JLSTN )
         DO 12 INO = 1 , NBNO
            ZI(JLSTN-1+INO) = ZI(JNOEU-1+INO)
 12      CONTINUE
      ENDIF
C
      CALL GETVR8 ( MCF, 'PRECISION', IOCC,1,1, PREC, N1 )
      CALL GETVTX ( MCF, 'CRITERE'  , IOCC,1,1, CRIT, N1 )
C
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', JVALE )
C
      CALL OREINO ( NOMA, ZI(JLSTN), NBNO, ZI(JLSTN), ZI(JLSTN+NBNO-1),
     +              ZR(JVALE), CRIT, PREC, IRET )
      IF ( IRET .NE. 0 ) CALL UTMESS('F','PRGNOE','ARRET SUR ERREURS')
C
      CALL JEDEMA()
      END
