      SUBROUTINE DYOBS3 ( NBOCC, NBPAS, JOBSE, LISINS, NBOBSE )
      IMPLICIT   NONE
      INTEGER             NBOCC, NBPAS, JOBSE(*), NBOBSE
      CHARACTER*(*)       LISINS
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2001   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C     SAISIE DU MOT CLE FACTEUR "OBSERVATION"
C ----------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
      INTEGER      IOCC, N1, N2, N3, N4, N5, N6, I, JNUM, LNUM, JINSC, 
     +             IPACH, NUME, IBID
      REAL*8       EPSI
      CHARACTER*8  K8B, RELA
      CHARACTER*19 NUMOBS
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DO 10 IOCC = 1 , NBOCC
C
         CALL GETVID ( 'OBSERVATION', 'LIST_ARCH',IOCC,1,0, K8B , N1 )
         CALL GETVID ( 'OBSERVATION', 'LIST_INST',IOCC,1,0, K8B , N2 )
         CALL GETVR8 ( 'OBSERVATION', 'INST'     ,IOCC,1,0, EPSI, N3 )
         CALL GETVIS ( 'OBSERVATION', 'PAS_OBSE' ,IOCC,1,0, IBID, N4 )
C
         IF ( IOCC .EQ. 1 ) THEN
            IF ( N1+N2+N3+N4 .EQ. 0 ) THEN
               CALL UTMESS ('F','DYOBS3' ,'IL FAUT DEFINIR "LIST_'//
     &               'ARCH" OU "LIST_INST" OU "INST" OU "PAS_OBSE" '//
     &               'AU PREMIER MOT CLE FACTEUR "OBSERVATION"')
            ENDIF
         ELSE
            IF ( N1+N2+N3+N4 .NE. 0 ) THEN
               CALL UTMESS ('A','DYOBS3' ,'SEULE LA VALEUR DE "LIST_'//
     &        'ARCH" OU "LIST_INST" OU "INST" OU "PAS_OBSE" DU PREMI'//
     &        'ER MOT CLE FACTEUR "OBSERVATION" EST PRISE EN COMPTE')
            ENDIF
            GOTO 10
         ENDIF
C
         CALL GETVR8 ( 'OBSERVATION', 'PRECISION', IOCC,1,1, EPSI, N5 )
         CALL GETVTX ( 'OBSERVATION', 'CRITERE'  , IOCC,1,1, RELA, N6 )
C
         IF ( N1 .NE. 0 ) THEN
            CALL GETVID ('OBSERVATION','LIST_ARCH',IOCC,1,1, NUMOBS,N1)
            CALL JEVEUO ( NUMOBS//'.VALE', 'L', JNUM )
            CALL JELIRA ( NUMOBS//'.VALE', 'LONUTI', LNUM, K8B )
            DO 20 I = 1 , LNUM
               NUME = ZI(JNUM+I-1)
               IF ( NUME .LE. 0 ) THEN
                  GOTO 20 
               ELSEIF ( NUME .GT. NBPAS ) THEN
                  GOTO 22
               ELSE
                  JOBSE(NUME) = 1
               ENDIF
 20         CONTINUE
 22         CONTINUE
            JOBSE(NBPAS) = 1
            GOTO 10
         ENDIF
C
         IF ( LISINS(1:1) .NE.  ' ' ) THEN
            CALL JEVEUO ( LISINS(1:19)//'.VALE', 'L', JINSC )
            IF ( N2 .NE. 0 ) THEN
               CALL GETVID('OBSERVATION','LIST_INST',IOCC,1,1,NUMOBS,N2)
               CALL JEVEUO ( NUMOBS//'.VALE', 'L', JNUM )
               CALL JELIRA ( NUMOBS//'.VALE', 'LONUTI', LNUM, K8B )
               CALL DYARC1 ( ZR(JINSC), NBPAS, ZR(JNUM), LNUM, JOBSE,
     +                       EPSI, RELA  )
               GOTO 10
            ENDIF
            IF ( N3 .NE. 0 ) THEN
               CALL GETVR8 ( 'OBSERVATION', 'INST', IOCC,1,0, EPSI,N3)
               LNUM = -N3
               CALL WKVECT ( '&&DYOBS3.VALE_INST', 'V V R', LNUM, JNUM )
               CALL GETVR8 ( 'OBSERVATION', 'INST', IOCC,1,LNUM,
     &                                              ZR(JNUM), N3 )
               CALL DYARC1 ( ZR(JINSC), NBPAS, ZR(JNUM), LNUM, JOBSE,
     +                       EPSI, RELA  )
               CALL JEDETR ( '&&DYOBS3.VALE_INST' )
               GOTO 10
            ENDIF
         ENDIF
C
         CALL GETVIS ( 'OBSERVATION', 'PAS_OBSE', IOCC,1,1, IPACH, N4 )
         IF ( N4 .EQ. 0 ) IPACH = 1
         DO 30 I = IPACH , NBPAS , IPACH
            JOBSE(I) = 1
 30      CONTINUE
         JOBSE(NBPAS) = 1
C
 10   CONTINUE
C
C      NBOBSE = 1
      NBOBSE = 0
      DO 40 I = 1 , NBPAS
         NBOBSE = NBOBSE + JOBSE(I)
 40   CONTINUE
C
      CALL JEDEMA()
C
      END
