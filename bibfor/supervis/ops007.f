      SUBROUTINE OPS007 ( ICMD, ICOND, IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER ICMD,ICOND,IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     OPERATEUR DESTRUCTION DE CONCEPT
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
C
C     ------------------------------------------------------------------
      CHARACTER*8 KBID
      INTEGER LBID
      CALL JEMARQ()
      IF(ICOND.EQ.-1) THEN
         CALL GETFAC('CONCEPT',NBOCC)
         DO 1 IOCC = 1,NBOCC
            CALL GETVID('CONCEPT','NOM',IOCC,1,0,KBID,NCON)
            NCON = -NCON
            CALL WKVECT('&&OPS007.NOMCON','V V K8',NCON,LCON)
            CALL GETVID('CONCEPT','NOM',IOCC,1,NCON,ZK8(LCON),LBID)
            DO 100 II =1,NCON
               CALL GCDETC(ICMD,ZK8(LCON-1+II))
 100        CONTINUE
            CALL JEDETR('&&OPS007.NOMCON')
 1       CONTINUE
      ELSEIF(ICOND.EQ.1) THEN
C ON NE VERIFIE RIEN
      ELSE
         CALL GETFAC('CONCEPT',NBOCC)
         DO 3 IOCC = 1,NBOCC
            CALL GETVID('CONCEPT','NOM',IOCC,1,0,KBID,NCON)
            NCON = -NCON
            CALL WKVECT('&&OPS007.NOMCON','V V K8',NCON,LCON)
            CALL GETVID('CONCEPT','NOM',IOCC,1,NCON,ZK8(LCON),LBID)
            DO 300 II=1,NCON
               CALL JEDETC('G',ZK8(LCON-1+II),1)
               CALL GCDETC(ICMD,ZK8(LCON-1+II))
 300        CONTINUE
            CALL JEDETR('&&OPS007.NOMCON')
 3       CONTINUE
      ENDIF
      CALL JEDEMA()
      END
