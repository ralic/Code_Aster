      SUBROUTINE GILIG2 ( NFIC, NBNONO, NIV )
      IMPLICIT   NONE
      INTEGER    NFIC, NBNONO, NIV
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C     BUT: LIRE LES N LIGNES DES POINTS DU MAILLAGE GIBI :
C                 ( PROCEDURE SAUVER)
C
C     IN: NFIC   : UNITE DE LECTURE
C         NBNONO : NOMBRE D'OBJETS NOMMES
C         NIV    : NIVEAU GIBI
C
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
C
      INTEGER      NBNOM, NBNUM, IAPTNO, IAPTNU, NBFOIS, NBREST, ICOJ,
     +             I, J
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C     -- ON LIT LES NOEUDS NOMMES:
C
C
      IF ( NBNONO .GT. 0 ) THEN
C
         IF( NIV .EQ. 3 ) THEN
            NBNOM =  8
            NBNUM = 16
         ELSE
            NBNOM =  8
            NBNUM = 10
         ENDIF
C
C     -- ON CREE LES 2 OBJETS QUI CONTIENDRONT LES NOMS ET NUMEROS
C        DES POINTS NOMMES:
C
        CALL WKVECT ( '&&GILIRE.POINT_NOM', 'V V K8', NBNONO, IAPTNO )
        CALL WKVECT ( '&&GILIRE.POINT_NUM', 'V V I' , NBNONO, IAPTNU )
C
        NBFOIS = NBNONO / NBNOM
        NBREST = NBNONO - NBNOM*NBFOIS
        ICOJ = 0
        DO 10 I = 1 , NBFOIS
           READ(NFIC,1007) (ZK8(IAPTNO-1+J),J=ICOJ+1,ICOJ+NBNOM)
           ICOJ = ICOJ + NBNOM
 10     CONTINUE
        IF ( NBREST .GT. 0 ) THEN
           READ(NFIC,1007) (ZK8(IAPTNO-1+J),J=ICOJ+1,ICOJ+NBREST)
        ENDIF
C
        NBFOIS = NBNONO / NBNUM
        NBREST = NBNONO - NBNUM*NBFOIS
        ICOJ = 0
        DO 12 I = 1 , NBFOIS
           IF ( NIV .EQ. 3 ) THEN
              READ(NFIC,1009) (ZI(IAPTNU-1+J),J=ICOJ+1,ICOJ+NBNUM)
              ICOJ = ICOJ + NBNUM
           ELSE
              READ(NFIC,1008) (ZI(IAPTNU-1+J),J=ICOJ+1,ICOJ+NBNUM)
              ICOJ = ICOJ + NBNUM
           ENDIF
 12     CONTINUE
        IF ( NBREST .GT. 0 ) THEN
           IF ( NIV .EQ. 3 ) THEN
              READ(NFIC,1009) (ZI(IAPTNU-1+J),J=ICOJ+1,ICOJ+NBREST)
           ELSE
              READ(NFIC,1008) (ZI(IAPTNU-1+J),J=ICOJ+1,ICOJ+NBREST)
           ENDIF
        ENDIF
      ENDIF
C
 1007  FORMAT( 8(1X,A8) )
 1008  FORMAT( 10(I8) )
 1009  FORMAT( 16(I5) )
C
      CALL JEDEMA()
C
      END
