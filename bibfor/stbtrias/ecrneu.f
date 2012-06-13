      SUBROUTINE ECRNEU(IMOD,NBNODE,AMA,BMA,CMA,AMI,BMI,
     +                  CMI,MIN,MAN,ITES)
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
      IMPLICIT REAL*8 (A-H,O-Z)
C     ==============================================================
CA PRESUPER
C
C   ================================================================
C   !                                                              !
C   !  FONCTION: ECRITURE DES COORDONNEES DES NOEUDS SUR LE FICHIER!
C   !            MODELE A PARTIR DU FICHIER BUFFER (NOEUDS-BUFFER) !
C   !                                                              !
C   ================================================================
C   !                                                              !
C   !  ROUTINES APPELES: CODENT                                    !
C   !                         : IUNIFI (FONCTION)                  !
C   !                         : JJMMAA                             !
C   !                                                              !
C   !                                                              !
C   !  ROUTINE APPELANTE : PRESUP                                  !
C   !                                                              !
C   ================================================================
C   !                                                              !
C   !                 ***************                              !
C   !                 *  ARGUMENTS  *                              !
C   !                 ***************                              !
C   !                                                              !
C   !  **********************************************************  !
C   !  *   NOM    * TYPE  *  MODE  *ALTERE *       ROLE         *  !
C   !  **********************************************************  !
C   !  *          *       *        *       *                    *  !
C   !  * NBNODE   *INTEGER*ENTREE  * NON   *NBRE DE NOEUDS TOTAL*  !
C   !  *          *       *        *       *                    *  !
C   !  * AMA      *D.PRECI*ENTREE  * NON   * X(MAXIMUM)         *  !
C   !  *          *       *        *       *                    *  !
C   !  * BMA      *D.PRECI*ENTREE  * NON   * Y(MAXIMUM)         *  !
C   !  *          *       *        *       *                    *  !
C   !  * CMA      *D.PRECI*ENTREE  * NON   * Z(MAXIMUM)         *  !
C   !  *          *       *        *       *                    *  !
C   !  * AMI      *D.PRECI*ENTREE  * NON   * X(MINIMUM)         *  !
C   !  *          *       *        *       *                    *  !
C   !  * BMI      *D.PRECI*ENTREE  * NON   * Y(MINIMUM)         *  !
C   !  *          *       *        *       *                    *  !
C   !  * CMI      *D.PRECI*ENTREE  * NON   * Z(MINIMUM)         *  !
C   !  *          *       *        *       *                    *  !
C   !  * MAN      *INTEGER*ENTREE  * NON   * N DE NOEUD MAXI    *  !
C   !  *          *       *        *       *                    *  !
C   !  * MIN      *INTEGER*ENTREE  * NON   * N DE NOEUD MINI    *  !
C   !  *          *       *        *       *                    *  !
C   !  * ITES     *INTEGER*ENTREE  * NON   * INDIQUE L'EXISTENCE*  !
C   !  *          *       *        *       * D'AU MOINS DEUX    *  !
C   !  *          *       *        *       * SYSTEMES DE COORD. *  !
C   !  *          *       *        *       *                    *  !
C   !  **********************************************************  !
C   !                                                              !
C   ================================================================
C
C
C
C
C ---> DECLARATION DES VARIABLES POUR LE TYPE D'ECRITURE
C
      INCLUDE 'jeveux.h'
      CHARACTER*1 PRFNOE,PRFNSY
C
C
C  --> DECLARATION DES ARGUMENTS
C
      INTEGER NBNODE,MIN,MAN,ITES
      REAL*8 AMA,BMA,CMA,AMI,BMI,CMI
C
C  --> DECLARATION DES VARIABLES LOCALES
C
      CHARACTER*4  CT(3)
      CHARACTER*8  CHNODE,CHSC
      CHARACTER*12 CHENTI,CHNOMI,CHNOMA,AUT
      CHARACTER*13 CHLIGN,CHLIGE
      CHARACTER*80 CHFONE(4)
      REAL*8       X,Y,Z
      INTEGER      NBLIT,NBLIE,NBLIF
      INTEGER      NODE,ISC
C
C  --------- FIN DECLARATION ---------
C
C  --> N  D'UNITE LOGIQUE ASSOCIE AU FICHIER
      CALL JEMARQ()
C
      PRFNOE='N'
      PRFNSY=' '
      CHFONE(1)='%FORMAT=(1*NOM_DE_NOEUD,3*COORD)'
      CHFONE(2)='%FORMAT=(1*NOM_DE_NOEUD,3*COORD,1*NOM_DE_SYSTEME)'
      CHFONE(3)='%FORMAT=(1*NOM_DE_NOEUD,2*COORD)'
      CHFONE(4)='%FORMAT=(1*NOM_DE_NOEUD,2*COORD,1*NOM_DE_SYSTEME)'
      CHENTI='NBOBJ=      '
      CHLIGN='NBLIGT=      '
      CHNODE='        '
      CHLIGE='NBLIGE=      '
      CHNOMI='NUMIN=      '
      CHNOMA='NUMAX=      '
      CHSC='        '
C
      NBLIF=1
C
C --- RECUPERATION DES VECTEURS DE TRAVAIL :
C     ------------------------------------
      CALL JEVEUO('&&PRESUP.INFO.NOEUDS','L',JINFO)
      CALL JEVEUO('&&PRESUP.COOR.NOEUDS','L',JCOOR)
C
      CALL CODENT(NBNODE,'G',CHENTI(7:12))
      CALL CODENT(MIN,'G',CHNOMI(7:12))
      CALL CODENT(MAN,'G',CHNOMA(7:12))
C
      IF (ITES.EQ.0) THEN
        NBLIE=5
        NBLIT=NBNODE+NBLIE+NBLIF+1
        CALL CODENT(NBLIE,'G',CHLIGE(8:13))
        CALL CODENT(NBLIT,'G',CHLIGN(8:13))
      ELSE
        NBLIE=3
        NBLIT=NBNODE+NBLIE+NBLIF+1
        CALL CODENT(NBLIE,'G',CHLIGE(8:13))
        CALL CODENT(NBLIT,'G',CHLIGN(8:13))
      ENDIF
C
      CALL JJMMAA(CT,AUT)
C
      WRITE (IMOD,'(A,4X,A,4X,A,3X,A,3X,A)')'COOR_3D','NOM=INDEFINI',
     &         CHENTI ,CHLIGE,CHLIGN
C
      WRITE(IMOD,'(11X,A,19X,A)') CHNOMI,CHNOMA
      WRITE(IMOD,'(11X,2A,12X,A,A2,A,A2,A,A4)')'AUTEUR=',AUT,'DATE=',
     &          CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
      IF (ITES.EQ.0) THEN
          WRITE(IMOD,'(A,7X,3(A,E15.8,1X))') '%','XMAX=',AMA,'YMAX=',
     &          BMA,'ZMAX=',CMA
          WRITE(IMOD,'(A,7X,3(A,E15.8,1X))') '%','XMIN=',AMI,'YMIN=',
     &          BMI,'ZMIN=',CMI
      ENDIF
C
      IF (ITES.EQ.0) THEN
          WRITE(IMOD,'(A)') CHFONE(1)
      ELSE
          WRITE(IMOD,'(A)') CHFONE(2)
      ENDIF
C
      DO 15 I=1,NBNODE
        NODE   = ZI(JINFO-1+(I-1)*3+1)
        ISC    = ZI(JINFO-1+(I-1)*3+2)
        X      = ZR(JCOOR-1+(I-1)*3+1)
        Y      = ZR(JCOOR-1+(I-1)*3+2)
        Z      = ZR(JCOOR-1+(I-1)*3+3)
C
        CALL CODNOP(CHNODE,PRFNOE,1,1)
        CALL CODENT(NODE,'G',CHNODE(2:8))
C
        IF (ITES.EQ.0) THEN
            WRITE (IMOD,'(2X,A,2X,3(1PE21.14,1X))') CHNODE,X,Y,Z
        ELSE
          CALL CODNOP(CHSC,PRFNSY,1,1)
C
C ---> RENUMEROTATION DES SYSTEMES DE COORDONNEES C.A.D. ON
C      INCREMENTE DE 1 LES NUMEROS DONNEES PAR SUPERTAB
C
          ISC=ISC+1
C
          CALL CODENT(ISC,'G',CHSC(2:8))
          WRITE (IMOD,'(2X,A,2X,3(1PE21.14,1X),A)') CHNODE,X,Y,Z,CHSC
        ENDIF
   15 CONTINUE
      WRITE (IMOD,'(A)') 'FINSF'
      WRITE (IMOD,'(A)') '%'
      CALL JEDEMA()
      END
