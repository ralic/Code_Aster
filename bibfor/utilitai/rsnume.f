      SUBROUTINE RSNUME(RESU,NOMSY,NU)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      CHARACTER*(*) NU,RESU,NOMSY
C ----------------------------------------------------------------------
C BUT : RECUPERER  UN NUME_DDL DANS UNE SD_RESULTAT
C ----------------------------------------------------------------------
C IN   K8   RESU    : NOM DE LA SD_RESULAT
C IN   K16  NOMSY   : NOM SYMBOLIQUE DU CHAM_NO : 'DEPL','TEMP', ...
C OUT  K14  NU      : NOM DU NUME_DDL  TROUVE (OU ' ' SINON)
C ----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      INTEGER DERNIE ,IBID,ICODE,IRET,LUTI,IRET2,IAREFE
      REAL*8 RBID
      COMPLEX*16 CBID
      CHARACTER*8 K8BID
      CHARACTER*19 CHAMNO ,RESU2

C DEB ------------------------------------------------------------------

      NU=' '
      RESU2=RESU
      CALL JEEXIN(RESU2//'.ORDR',IRET)
      IF (IRET.GT.0) THEN
         CALL JELIRA(RESU2//'.ORDR','LONUTI',LUTI,K8BID)
         IF (LUTI.EQ.0) GO TO 9999
         CALL RSORAC(RESU,'DERNIER',IBID,RBID,K8BID,CBID,RBID,
     &              'ABSOLU',DERNIE,1,IBID)

         CALL RSEXCH(RESU,NOMSY,DERNIE,CHAMNO,ICODE)

         IF (ICODE.EQ.0) THEN
            CALL JEVEUO(CHAMNO//'.REFE','L',IAREFE)
            CALL JEEXIN(ZK24(IAREFE+1)(1:19)//'.NEQU',IRET2)
            IF (IRET2.GT.0) NU=ZK24(IAREFE+1)(1:14)
         END IF
      END IF
9999  CONTINUE

      END
