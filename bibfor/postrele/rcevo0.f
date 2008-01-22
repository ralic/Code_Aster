      SUBROUTINE RCEVO0 ( INTITU, NBINTI, LSN, LFATIG, NBTRAN )
      IMPLICIT      NONE
      INTEGER       NBINTI, NBTRAN
      LOGICAL       LSN, LFATIG
      CHARACTER*24  INTITU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 22/01/2008   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
C     DETERMINE LE NOMBRE DE SEGMENT A POST-TRAITER
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
      INTEGER      N1, JINTI, NBINT0, I, J, JINT0, IOCC, N2, N3
      LOGICAL      EXIST
      CHARACTER*8  K8B, TABLE,TABFL0,TABPR0
      CHARACTER*16 MOTCLF
      CHARACTER*24 INTIT0
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      NBINTI = 0
      MOTCLF = 'TRANSITOIRE'
      CALL GETFAC ( MOTCLF, NBTRAN )
      IF (NBTRAN.EQ.0) GOTO 9999

      DO 100, IOCC = 1, NBTRAN

        CALL GETVID ( MOTCLF, 'TABL_RESU_MECA', IOCC,1,1, TABLE , N1 )
        CALL GETVID ( MOTCLF, 'TABL_SIGM_THER', IOCC,1,1, TABFL0, N2 )
        CALL GETVID ( MOTCLF, 'TABL_RESU_PRES', IOCC,1,1, TABPR0, N3 )
C
C       VERIFICATION DE L'ORDRE DES NOEUDS DANS LES TABLES
        CALL RCVERI(TABLE)
        IF(N2.NE.0) CALL RCVER1('MECANIQUE',TABLE,TABFL0)
        IF(N3.NE.0) CALL RCVER1('MECANIQUE',TABLE,TABPR0)

        IF(IOCC.EQ.1)THEN
          CALL TBEXIP ( TABLE, 'INTITULE', EXIST, K8B )
          IF ( EXIST ) THEN
            CALL TBEXV1 ( TABLE, 'INTITULE', INTITU, 'V', NBINTI, K8B)
          ELSE
            CALL WKVECT ( INTITU, 'V V K16', 1, JINTI )
            ZK16(JINTI) = ' '
          ENDIF
        ENDIF

 100  CONTINUE
C
      IF ( LSN .AND. .NOT.LFATIG .AND. NBTRAN.GT.1 ) THEN
         NBINT0 = NBTRAN * NBINTI
         INTIT0 = '&&RCEVO0.INTITULE' 
         CALL JEVEUO ( INTITU, 'L', JINTI )
         CALL WKVECT ( INTIT0, 'V V K16', 1, JINT0 )
         DO 10 I = 1 , NBINTI
            ZK16(JINT0+I-1) = ZK16(JINTI+I-1)
 10      CONTINUE
         CALL JEDETR ( INTITU )
         CALL WKVECT ( INTITU, 'V V K16', NBINT0, JINTI )
         DO 20 I = 1 , NBINTI
            DO 22 J = 1 , NBTRAN
               ZK16(JINTI-1+NBTRAN*(I-1)+J) = ZK16(JINT0+I-1)
 22         CONTINUE
 20      CONTINUE
         CALL JEDETR ( INTIT0 )
      ELSE
         NBTRAN = 1
      ENDIF
C
9999  CONTINUE
      CALL JEDEMA( )
      END
