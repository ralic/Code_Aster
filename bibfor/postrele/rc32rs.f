      SUBROUTINE RC32RS
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 01/10/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
C
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IBID, NPARA, IM, JVALE
      PARAMETER    ( NPARA = 7 )
      COMPLEX*16    C16B
      CHARACTER*4   LIEU(2)
      CHARACTER*8   NOMRES, TYPARA(NPARA)
      CHARACTER*16  CONCEP, NOMCMD, NOPARA(NPARA)
C     ------------------------------------------------------------------
      DATA NOPARA / 'LIEU' , 'SM' , 'SN/3SM' , 'SN_MAX' , 'SP_MAX' ,
     +              'SALT_MAX' , 'FACT_USAGE_CUMU' /
      DATA TYPARA / 'K8', 'R', 'R', 'R', 'R', 'R', 'R' /
      DATA LIEU   / 'ORIG' , 'EXTR' /
C DEB ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL TBCRSD ( NOMRES, 'G' )
      CALL TBAJPA ( NOMRES, NPARA, NOPARA, TYPARA )
C
      DO 10 IM = 1 , 2
C
         CALL JEVEUO ( '&&RC3200.RESULTAT  .'//LIEU(IM), 'L', JVALE )
C
         CALL TBAJLI ( NOMRES, NPARA, NOPARA, IBID, ZR(JVALE),
     +                                              C16B, LIEU(IM), 0 )
C
 10   CONTINUE
C
      END
