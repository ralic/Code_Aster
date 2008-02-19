      SUBROUTINE GVERFO(CHARG,IER)
      IMPLICIT NONE
      CHARACTER*19      CHARG
      INTEGER                 IER
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     BUT : VERIFIE SI LE CHARGMENT FONCTION EST DE TYPE 'FORMULE'
C           ROUTINE APPELEE PAR GCHARG (OPERATEUR CALC_G)
C   
C
C     IN :    CHARG   : NOM DE LA CHARGE (ex: NOMCHA//'.CHME.F3D3D')
C
C     OUT:    IER     : =1 : PRESENCE D'UN CHARGMENT 'FORMULE'
C                       =0 : SINON

C ======================================================================
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ----------------------------------------------------------------------
      INTEGER  IVAL,NBVALE,IN,IPROL
      CHARACTER*8  K8B
      CHARACTER*19 NCH19

      CALL JEMARQ()

      CALL JEVEUO(CHARG//'.VALE','L',IVAL)
      CALL JELIRA(CHARG//'.VALE','LONMAX',NBVALE,K8B)

      IER=0
      DO 10 IN=1,NBVALE
          IF(ZK8(IVAL+IN-1)(1:7).NE.'&FOZERO' .AND.
     &       ZK8(IVAL+IN-1)(1:7).NE.'       ' .AND.
     &       ZK8(IVAL+IN-1)(1:6).NE.'GLOBAL' )THEN
            NCH19=ZK8(IVAL+IN-1)
            CALL JEVEUO(NCH19//'.PROL','L',IPROL)
            IF(ZK24(IPROL)(1:8).EQ.'INTERPRE')THEN 
               IER=1
               GOTO 9999
            ENDIF
          ENDIF
 10   CONTINUE

 9999 CONTINUE

      CALL JEDEMA()

      END
