      SUBROUTINE RC32CM
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     LECTURE DU MOT CLE FACTEUR "CHAR_MECA"
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
      CHARACTER*32     JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      N1, N1T, IOCC, NDIM, NBCHAR, NUME, JCHAR
      REAL*8       R8B
      CHARACTER*8  KNUMEC
      CHARACTER*16 MOTCLF
      INTEGER      IARG
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'CHAR_MECA'
      CALL GETFAC ( MOTCLF, NBCHAR )
C
      NDIM = 0
      DO 10, IOCC = 1, NBCHAR, 1
         CALL GETVIS ( MOTCLF, 'NUME_CHAR', IOCC,IARG,1, NUME, N1 )
         NDIM = MAX (NDIM, NUME)
 10   CONTINUE
C
      CALL JECREC ('&&RC3200.VALE_CHAR', 'V V R'  , 'NO',
     +                               'DISPERSE', 'VARIABLE', NBCHAR )
C
      DO 20, IOCC = 1, NBCHAR, 1
C
         CALL GETVIS ( MOTCLF, 'NUME_CHAR', IOCC,IARG,1, NUME, N1 )
C
         KNUMEC = 'C       '
         CALL CODENT ( NUME , 'D0' , KNUMEC(2:8)  )
C
C
         CALL JECROC (JEXNOM('&&RC3200.VALE_CHAR',KNUMEC))
         CALL JEECRA (JEXNOM('&&RC3200.VALE_CHAR',KNUMEC),
     +                                              'LONMAX',12,' ')
         CALL JEECRA (JEXNOM('&&RC3200.VALE_CHAR',KNUMEC),
     +                                              'LONUTI',12,' ')
         CALL JEVEUO (JEXNOM('&&RC3200.VALE_CHAR',KNUMEC),'E',JCHAR)
C
C ------ UN SEUL TENSEUR OU 2 ?
C
         CALL GETVR8 ( MOTCLF, 'MX', IOCC,IARG,0, R8B, N1T )
C
         IF ( N1T .NE. 0 ) THEN
            CALL GETVR8 ( MOTCLF, 'FX', IOCC,IARG,1, ZR(JCHAR-1+1), N1 )
            CALL GETVR8 ( MOTCLF, 'FY', IOCC,IARG,1, ZR(JCHAR-1+2), N1 )
            CALL GETVR8 ( MOTCLF, 'FZ', IOCC,IARG,1, ZR(JCHAR-1+3), N1 )
            CALL GETVR8 ( MOTCLF, 'MX', IOCC,IARG,1, ZR(JCHAR-1+4), N1 )
            CALL GETVR8 ( MOTCLF, 'MY', IOCC,IARG,1, ZR(JCHAR-1+5), N1 )
            CALL GETVR8 ( MOTCLF, 'MZ', IOCC,IARG,1, ZR(JCHAR-1+6), N1 )
C
         ELSE
            CALL GETVR8 (MOTCLF,'FX_TUBU',IOCC,IARG,1,
     &                   ZR(JCHAR-1+1),N1)
            CALL GETVR8 (MOTCLF,'FY_TUBU',IOCC,IARG,1,
     &                   ZR(JCHAR-1+2),N1)
            CALL GETVR8 (MOTCLF,'FZ_TUBU',IOCC,IARG,1,
     &                   ZR(JCHAR-1+3),N1)
            CALL GETVR8 (MOTCLF,'MX_TUBU',IOCC,IARG,1,
     &                   ZR(JCHAR-1+4),N1)
            CALL GETVR8 (MOTCLF,'MY_TUBU',IOCC,IARG,1,
     &                   ZR(JCHAR-1+5),N1)
            CALL GETVR8 (MOTCLF,'MZ_TUBU',IOCC,IARG,1,
     &                   ZR(JCHAR-1+6),N1)
C
            CALL GETVR8 (MOTCLF,'FX_CORP',IOCC,IARG,1,
     &                   ZR(JCHAR-1+7),N1)
            CALL GETVR8 (MOTCLF,'FY_CORP',IOCC,IARG,1,
     &                   ZR(JCHAR-1+8),N1)
            CALL GETVR8 (MOTCLF,'FZ_CORP',IOCC,IARG,1,
     &                   ZR(JCHAR-1+9),N1)
            CALL GETVR8 (MOTCLF,'MX_CORP',IOCC,IARG,1,
     &                   ZR(JCHAR-1+10),N1)
            CALL GETVR8 (MOTCLF,'MY_CORP',IOCC,IARG,1,
     &                   ZR(JCHAR-1+11),N1)
            CALL GETVR8 (MOTCLF,'MZ_CORP',IOCC,IARG,1,
     &                   ZR(JCHAR-1+12),N1)
         ENDIF
C
 20   CONTINUE
C
      CALL JEDEMA( )
      END
