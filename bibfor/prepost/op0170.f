      SUBROUTINE OP0170 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
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
C
C     CALCUL FATIGUE ALEATOIRE
C
C     -----------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER       IBID, NBTAB, NBMOM, N1, NBPFAT, IVMOM, I, ILIGN,
     +              NBL0, INBL0, NBL2, INBL2, NBL4, INBL4, IRET
      PARAMETER     ( NBPFAT = 5 )
      REAL*8        XM0, XM2, XM4, R8VIDE, RUNDF, DOM, RDUREE, VALER(5),
     +              R8B
      COMPLEX*16    C16B
      CHARACTER*8   K8B, NOMRES, TABLE, TYPFAT(NBPFAT)
      CHARACTER*16  NOMCMD, CONCEP, NOPFAT(NBPFAT), NOPFA2(4)
      CHARACTER*24  NOMOB1, NOMOB2, NOMOB3
C     -----------------------------------------------------------------
      DATA  NOPFAT / 'MOMENT_SPEC_0' , 'MOMENT_SPEC_2' ,
     +               'MOMENT_SPEC_4' ,
     +               'DUREE'         , 'DOMMAGE'       /
      DATA  NOPFA2 / 'MOMENT_SPEC_0' , 'MOMENT_SPEC_2' ,
     +               'DUREE'         , 'DOMMAGE'       /
      DATA  TYPFAT / 'R' , 'R' , 'R' , 'R' , 'R' /
C     -----------------------------------------------------------------
C
      CALL INFMAJ()
      RUNDF = R8VIDE()
      XM4   = RUNDF
      IVMOM = 0
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL GETVR8 ( ' ', 'DUREE'   , 1,1,1, RDUREE, N1 )
C
      CALL GETVID ( ' ', 'TABL_POST_ALEA', 1,1,0, TABLE , NBTAB )
C
      IF ( NBTAB .NE. 0 ) THEN
        CALL GETVID ( ' ', 'TABL_POST_ALEA', 1,1,1, TABLE, N1 )
        CALL TBLIVA ( TABLE, 0, K8B, IBID, R8B, C16B, K8B, K8B, R8B, 
     +                'GRANDEUR', K8B, IBID, R8B, C16B, K8B, IRET )
        IF ( IRET.NE.0 .AND. IRET.NE.3 ) 
     +                CALL UTMESS('F','OP0170','Y A UN BUG 1')
        IF ( K8B .NE. 'DSP_SIPO' .OR. K8B .NE. 'DSP_SIGM' .OR.
     +       K8B .NE. 'DSP_EFGE'  ) THEN
              CALL UTMESS('A','POST_FATI_ALEA','ATTENTION : LE CALCUL '
     +      //'DU DOMMAGE N''A DE SENS QUE POUR DES DSP EN CONTRAINTES')
        ENDIF
        NOMOB1 = '&&OP0170.LAMBDA_0'
        CALL TBEXVE ( TABLE, 'LAMBDA_00', NOMOB1, 'V', NBL0, K8B )
        CALL JEVEUO ( NOMOB1, 'L', INBL0 )
        NOMOB2 = '&&OP0170.LAMBDA_2'
        CALL TBEXVE ( TABLE, 'LAMBDA_02', NOMOB2, 'V', NBL2, K8B )
        IF ( NBL2.NE.NBL0 )CALL UTMESS('F','OP0170','Y A UN BUG 2')
        CALL JEVEUO ( NOMOB2, 'L', INBL2 )
        NOMOB3 = '&&OP0170.LAMBDA_4'
        CALL TBEXVE ( TABLE, 'LAMBDA_04', NOMOB3, 'V', NBL4, K8B )
        IF ( NBL4.NE.NBL0 )CALL UTMESS('F','OP0170','Y A UN BUG 3')
        CALL JEVEUO ( NOMOB3, 'L', INBL4 )
        NBMOM = NBL0
        CALL WKVECT ( '&&OP0170.MOMENT', 'V V R', 3*NBMOM, IVMOM )
        DO 10 I = 1 , NBL0
           ZR(IVMOM+(I-1)*3  ) = ZR(INBL0+I-1)
           ZR(IVMOM+(I-1)*3+1) = ZR(INBL2+I-1)
           ZR(IVMOM+(I-1)*3+2) = ZR(INBL4+I-1)
 10     CONTINUE
        CALL JEDETR ( NOMOB1 )
        CALL JEDETR ( NOMOB2 )
        CALL JEDETR ( NOMOB3 )
C
      ELSE
C
        CALL GETVR8 ( ' ', 'MOMENT_SPEC_0', 1,1,1, XM0, N1 )
        CALL GETVR8 ( ' ', 'MOMENT_SPEC_2', 1,1,1, XM2, N1 )
        CALL GETVR8 ( ' ', 'MOMENT_SPEC_4', 1,1,1, XM4, N1 )
        NBMOM = 1
        CALL WKVECT ( '&&OP0170.MOMENT', 'V V R', 3*NBMOM, IVMOM )
        ZR(IVMOM  ) = XM0
        ZR(IVMOM+1) = XM2
        ZR(IVMOM+2) = XM4
C
      ENDIF
C
      IF ( NBMOM .EQ. 0 )  CALL UTMESS('A','POST_FATI_ALEA',
     +                         'AUCUNE VALEUR DE MOMENT PRESENTE')
C
      CALL TBCRSD ( NOMRES, 'G' )
      CALL TBAJPA ( NOMRES, NBPFAT, NOPFAT, TYPFAT )
C
      ILIGN = 0
      DO 20 I = 1 , NBMOM 
         XM0 = ZR(IVMOM+(I-1)*3  )
         XM2 = ZR(IVMOM+(I-1)*3+1)
         XM4 = ZR(IVMOM+(I-1)*3+2)
         CALL PDADOM ( XM0, XM2, XM4, DOM )
         DOM = DOM * RDUREE
         IF ( XM4 .EQ. RUNDF ) THEN
            VALER(1) = XM0
            VALER(2) = XM2
            VALER(3) = RDUREE
            VALER(4) = DOM
            CALL TBAJLI ( NOMRES,4,NOPFA2,IBID,VALER,C16B,K8B,ILIGN)
         ELSE
            VALER(1) = XM0
            VALER(2) = XM2
            VALER(3) = XM4
            VALER(4) = RDUREE
            VALER(5) = DOM
            CALL TBAJLI (NOMRES,NBPFAT,NOPFAT,IBID,VALER,C16B,K8B,ILIGN)
         ENDIF
 20   CONTINUE
C
      CALL TITRE
C
      IF ( IVMOM .NE. 0 ) CALL JEDETR ( '&&OP0170.MOMENT' )
C
      END
