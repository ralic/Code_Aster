      SUBROUTINE SLENEU(IUNV,NBNODE,AMA,BMA,CMA,AMI,BMI,CMI,MIX,
     &                  MAN,ITES,DATSET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C  --> DECLARATION DES ARGUMENTS
      INCLUDE 'jeveux.h'
      INTEGER NBNODE,MIX,MAN,ITES,DATSET
      REAL* 8 AMA,BMA,CMA,AMI,BMI,CMI
C  --> DECLARATION DES VARIABLES LOCALES
      CHARACTER*80 CBUF
      INTEGER NODE,I,ICNODE,IND,J,ITMP,INUS,JSYS
      REAL*8  X,Y,Z
C
C  ---------- FIN DECLARATION -----------
C
      CALL JEMARQ()
      NBNODE=0
      ITES=0
C
C  --> N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
      IMES=IUNIFI('MESSAGE')
C
      NTAIL = 1000
      NITER = 1000
      NDECA = 0
      ITMP = -6
      INUS  = 10

      CALL JEEXIN ( '&&PRESUP.INFO.NOEUDS', IRE1 )
      IF ( IRE1 .NE. 0 ) CALL JEDETR( '&&PRESUP.INFO.NOEUDS' )
      CALL WKVECT('&&PRESUP.INFO.NOEUDS','V V I',3*NTAIL,JINFO)
      CALL JEEXIN ( '&&PRESUP.COOR.NOEUDS', IRE2 )
      IF ( IRE2 .NE. 0 ) CALL JEDETR( '&&PRESUP.COOR.NOEUDS' )
      CALL WKVECT('&&PRESUP.COOR.NOEUDS','V V R',3*NTAIL,JCOOR)
C
C -->  GESTION DES SYSTEMES DE COORDONNEES - PREMIERE PARTIE
C -->  ON TESTE L'EXISTENCE D'UN SYSTEME DE COORDONNEES (DATASET 2420)
C
      CALL JEEXIN('&&IDEAS.SYST',IRET)
      IF (IRET.EQ.0) THEN
        CALL U2MESS('I','STBTRIAS_9')
C     Il n'y a pas de sys de coord defini dans le fichier, pour ne pas
C     planter on en cree un bidon ici qu'on declare comme cartesien
         ISYST = 0
      ENDIF

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
C -->  GESTION DES SYSTEMES DE COORDONNEES - BIS
C -->  SI UN SYSTEME EST DEFINI, ON LE RECUPERE
C
        CALL JEEXIN('&&IDEAS.SYST',IRET)
        IF (IRET.NE.0) THEN
           CALL JEVEUO('&&IDEAS.SYST','L',JSYS)
           ISYST = ZI(JSYS-1+I)
        ENDIF

C        On ne teste ici que si le systeme de coordonnne est cartesien,
C        cylindrique ou autre
        IF(ISYST.NE.0) THEN
          CALL U2MESS('F','STBTRIAS_10')
        ENDIF
C        On ne teste ici que si les noeuds font reference a plusieurs
C        systeme de coordonnne
C        On ne teste pas si ces systemes sont identiques juste si leur
C        label est different
        IF ((I.NE.ITMP).AND.(ITMP.NE.-6)) THEN
          CALL U2MESS('A','STBTRIAS_11')
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
