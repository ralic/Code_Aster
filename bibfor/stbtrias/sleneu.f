      SUBROUTINE SLENEU(NBNODE,AMA,BMA,CMA,AMI,BMI,CMI,MIX,MAN,ITES,
     &                  DATSET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 10/02/2004   AUTEUR NICOLAS O.NICOLAS 
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
C TOLE CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
C     ==============================================================
CA PRESUPER
C
C     ==============================================================
C     !                                                            !
C     !  FONCTION:LECTURE SUR LE FICHIER UNIVERSEL ISSU DE SUPER-  !
C     !           TAB I-DEAS 4.0, 6.0 OU 7.0   DES COORDONNEES DES !
C     !           DES NOEUDS ET STOCKAGE DANS OBJETS JEVEUX        !
C     !                                                            !
C     ==============================================================
C     !                                                            !
C     !  ROUTINES APPELES : CODENT                                 !
C     !                          : IUNIFI (FONCTION)               !
C     !                                                            !
C     !  ROUTINE APPELANTE : PRESUP                                !
C     !                                                            !
C     ==============================================================
C     !                                                            !
C     !                  **************                            !
C     !                  *  ARGUMENT  *                            !
C     !                  **************                            !
C     !                                                            !
C     !  ********************************************************  !
C     !  *   NOM    *  TYPE * MODE *ALTERE *      ROLE          *  !
C     !  ********************************************************  !
C     !  *          *       *      *       *                    *  !
C     !  * NBNODE   *INTEGER*SORTIE* NON   *NBRE TOTAL DE NOEUDS*  !
C     !  *          *       *      *       *                    *  !
C     !  * AMA      *D.PRECI*SORTIE* NON   * X(MAXIMUM)         *  !
C     !  *          *       *      *       *                    *  !
C     !  * BMA      *D.PRECI*SORTIE* NON   * Y(MAXIMUM)         *  !
C     !  *          *       *      *       *                    *  !
C     !  * CMA      *D.PRECI*SORTIE* NON   * Z(MAXIMUM)         *  !
C     !  *          *       *      *       *                    *  !
C     !  * AMI      *D.PRECI*SORTIE* NON   * X(MINIMUM)         *  !
C     !  *          *       *      *       *                    *  !
C     !  * BMI      *D.PRECI*SORTIE* NON   * Y(MINIMUM)         *  !
C     !  *          *       *      *       *                    *  !
C     !  * CMI      *D.PRECI*SORTIE* NON   * Z(MINIMUM)         *  !
C     !  *          *       *      *       *                    *  !
C     !  * MAN      *INTEGER*SORTIE* NON   * N DE NOEUD MAXI    *  !
C     !  *          *       *      *       *                    *  !
C     !  * MIX      *INTEGER*SORTIE* NON   * N DE NOEUD MINI    *  !
C     !  *          *       *      *       *                    *  !
C     !  * ITES     *INTEGER*SORTIE* NON   * INDIQUE S'IL EXISTE*  !
C     !  *          *       *      *       * AU MOINS DE SYST.  *  !
C     !  *          *       *      *       * DE COORDONNEES     *  !
C     !  *          *       *      *       *                    *  !
C     !  * DATSET   *INTEGER*ENTREE* NON   * NUMERO DU DATASET  *  !
C     !  *          *       *      *       * TRAITE             *  !
C     !  *          *       *      *       *                    *  !
C     !  ********************************************************  !
C     !                                                            !
C     ==============================================================
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER      ZI
      REAL*8       ZR
      COMPLEX*16   ZC
      LOGICAL      ZL,EXISDG
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM,JEXNOM,JEXATR
      CHARACTER*80 ZK80
C  --> DECLARATION DES ARGUMENTS
      INTEGER NBNODE,MIX,MAN,ITES,DATSET
      REAL* 8 AMA,BMA,CMA,AMI,BMI,CMI
C  --> DECLARATION DES VARIABLES LOCALES
      CHARACTER*80 CBUF
      INTEGER NODE,I,ICNODE,IND,J
      REAL*8  X,Y,Z
C
C  ---------- FIN DECLARATION -----------
C
      CALL JEMARQ()
      NBNODE=0
      ITES=0
C
C  --> N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
      IUNV = IUNIFI('IDEAS')
      IMES=IUNIFI('MESSAGE')
C
      NTAIL = 1000
      NITER = 1000
      NDECA = 0
      CALL JEEXIN ( '&&PRESUP.INFO.NOEUDS', IRE1 )
      IF ( IRE1 .NE. 0 ) CALL JEDETR( '&&PRESUP.INFO.NOEUDS' )
      CALL WKVECT('&&PRESUP.INFO.NOEUDS','V V I',3*NTAIL,JINFO)
      CALL JEEXIN ( '&&PRESUP.COOR.NOEUDS', IRE2 )
      IF ( IRE2 .NE. 0 ) CALL JEDETR( '&&PRESUP.COOR.NOEUDS' )
      CALL WKVECT('&&PRESUP.COOR.NOEUDS','V V R',3*NTAIL,JCOOR)
  1   CONTINUE
      DO 10 ITER=1,NITER
        READ (IUNV,'(A)') CBUF
        READ (UNIT=CBUF,FMT='(4X,I2)') IND
        IF (IND.EQ.-1) GO TO 99
C
C --> LES COORDONNEES DES NOEUDS SONT EN SIMPLE PRECISION (SUPERTAB 4
C     OU 6) OU EN REAL*8 (SUPERTAB 6 OU 7)
C
        IF(DATSET.EQ.15) THEN
          READ (CBUF,'(4I10,3E13.6)') NODE,I,J,ICNODE,X,Y,Z
        ELSE IF (DATSET.EQ.781.OR.DATSET.EQ.2411) THEN
          READ (CBUF,'(4I10)') NODE,I,J,ICNODE
          READ (IUNV,'(3E25.16)') X,Y,Z
        ENDIF
C
        IF ((DATSET.EQ.15.OR.DATSET.EQ.781).AND.I.NE.0) THEN
          CALL UTMESS('A','PRESUP',' ATTENTION SYSTEME DE COORDONNES'
     &      //' AUTRE QUE CARTESIEN NON RELU DANS ASTER.')
C          ITES=1
        ELSE IF (DATSET.EQ.2411) THEN
          CALL JEEXIN('&&IDEAS.SYST',IRET)
          IF (IRET.EQ.0) THEN
          CALL UTMESS('F','PRESUP',' ATTENTION AUCUN SYSTEME DE '
     &      //'COORDONNES N''EST DEFINI')
          ENDIF
          CALL JEVEUO('&&IDEAS.SYST','L',JSYS)
          ISYST = ZI(JSYS-1+I)
          IF(ISYST.NE.0) THEN
          CALL UTMESS('F','PRESUP',' ATTENTION SYSTEME DE COORDONNES'
     &      //' AUTRE QUE CARTESIEN NON RELU DANS ASTER.')
          ENDIF
        ENDIF
C
C  --> INITIALISATION POUR LA RECHERCHE DES MINI ET MAXI
        IF (NBNODE.EQ.0) THEN
          AMA=X
          BMA=Y
          CMA=Z
          AMI=X
          BMI=Y
          CMI=Z
        ELSE
          AMA=MAX(AMA,X)
          BMA=MAX(BMA,Y)
          CMA=MAX(CMA,Z)
          AMI=MIN(AMI,X)
          BMI=MIN(BMI,Y)
          CMI=MIN(CMI,Z)
        ENDIF
C
        IF (NBNODE.EQ.0) THEN
          MIX=NODE
        ELSE
          MAN=MAX(MIX,NODE)
        ENDIF
C
        NBNODE = NBNODE + 1
C
        ZI(JINFO-1+NDECA+(ITER-1)*3+1) = NODE
        ZI(JINFO-1+NDECA+(ITER-1)*3+2) = I
        ZI(JINFO-1+NDECA+(ITER-1)*3+3) = ICNODE
        ZR(JCOOR-1+NDECA+(ITER-1)*3+1) = X
        ZR(JCOOR-1+NDECA+(ITER-1)*3+2) = Y
        ZR(JCOOR-1+NDECA+(ITER-1)*3+3) = Z
   10 CONTINUE
      NTAIL = NTAIL + NITER
      NDECA = NDECA + 3000
      CALL JUVECA('&&PRESUP.INFO.NOEUDS',3*NTAIL)
      CALL JEVEUO('&&PRESUP.INFO.NOEUDS','E',JINFO)
      CALL JUVECA('&&PRESUP.COOR.NOEUDS',3*NTAIL)
      CALL JEVEUO('&&PRESUP.COOR.NOEUDS','E',JCOOR)
      GO TO 1
   99 CONTINUE
C
      WRITE (IMES,*) 'NOMBRE DE NOEUDS :',NBNODE
      CALL JEDEMA()
      END
