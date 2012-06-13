      SUBROUTINE ECRELT(IMOD,MAXNOD,NBTYMA,NOMAIL,NBMAIL,MINT,MANT,
     &                  LIMAIL,NBMTOT)
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
C     ===============================================================
CA PRESUPER
C
C  ==================================================================
C  !                                                                !
C  ! FONCTION: ECRITURE DES MAILLES DANS LE FICHIER MODELE          !
C  !                                                                !
C  ==================================================================
C  !                                                                !
C  !  ROUTINE APPELES : CODENT                                      !
C  !                         : IUNIFI (FONCTION)                    !
C  !                         : JJMMAA                               !
C  !                                                                !
C  !  ROUTINE APPELANTE : PRESUP                                    !
C  !                                                                !
C  ==================================================================
C  !                                                                !
C  !                 ***************                                !
C  !                 *  ARGUMENTS  *                                !
C  !                 ***************                                !
C  !                                                                !
C  !  ************************************************************* !
C  !  *   NOM   *  TYPE * MODE *ALTERE *        ROLE              * !
C  !  ************************************************************* !
C  !  *         *       *      *       *                          * !
C  !  * MAXNOD  *INTEGER*ENTREE* NON   * NBRE MAXI DE NOEUDS POUR * !
C  !  *         *       *      *       *  UNE MAILLE SUPERTAB     * !
C  !  *         *       *      *       *                          * !
C  !  * NBTYMA  *INTEGER*ENTREE* NON   * NBRE DE TYPES DE MAILLES * !
C  !  *         *       *      *       *     SUPERTAB             * !
C  !  *         *       *      *       *                          * !
C  !  * NOMAIL  *CHARACT*ENTREE* NON   * NOMS DES DIFFERENTS TYPES* !
C  !  *         *       *      *       * DE MAILLES LUS/LE FICHIER* !
C  !  *         *       *      *       *                          * !
C  !  * NBMAIL  *INTEGER*ENTREE* NON   * NBRE DE MAILLES DE CHAQUE* !
C  !  *         *       *      *       *      TYPE                * !
C  !  *         *       *      *       *                          * !
C  !  * MINT    *INTEGER*ENTREE* NON   * TABLEAU CONTENANT LE N DE* !
C  !  *         *       *      *       * MAILLE MIN POUR CHAQUE   * !
C  !  *         *       *      *       * TYPE DE MAILLE           * !
C  !  *         *       *      *       *                          * !
C  !  * MANT    *INTEGER*ENTREE* NON   * TABLEAU CONTENANT LE N DE* !
C  !  *         *       *      *       * MAILLE MAX POUR CHAQUE   * !
C  !  *         *       *      *       * TYPE DE MAILLE           * !
C  !  *         *       *      *       *                          * !
C  !  * LIMAIL  *INTEGER*ENTREE* NON   * TABLEAU CONTENANT LE NBRE* !
C  !  *         *       *      *       * DE LIGNE POUR L'ECRITURE * !
C  !  *         *       *      *       * DE CHAQUE TYPE DE MAILLE * !
C  !  *         *       *      *       *                          * !
C  !  ************************************************************* !
C  !                                                                !
C  ==================================================================
C
C
C  --> DECLARATION DES ARGUMENTS
C
      INCLUDE 'jeveux.h'
      INTEGER MAXNOD,NBTYMA
      CHARACTER*1 PRFNOE,PRFMAI
      CHARACTER*8 NOMAIL(NBTYMA)
      INTEGER NBMAIL(NBTYMA)
      INTEGER MINT(NBTYMA),MANT(NBTYMA),LIMAIL(NBTYMA)
C
C  --> DECLARATION DES VARIABLES LOCALES
C
      CHARACTER*4   CT(3)
      CHARACTER*8   CHMAIL,CHTAB(32)
      CHARACTER*12  CHNOMI,CHNOMA,CHENTI,AUT
      CHARACTER*13  CHLIGN,CHLIGE
      CHARACTER*80  CHFOMA(35)
      INTEGER IREST,IDIV,NBLIT,NBLIE,NBLIF
      INTEGER NEU2(32),INUM,ITYP,NEUL
      INTEGER L
C
C  --> DECLARATION DES INDICES DE BOUCLES
C
      INTEGER IN,NTE
C
C  ------------- FIN DECLARATION ------------
C
C
      CALL JEMARQ()
      PRFNOE='N'
      PRFMAI='M'
      DO 1 IFO=1,35
        CHFOMA(IFO)=' '
 1    CONTINUE
C
      CHFOMA(1)='%FORMAT=(1*NOM_DE_MAILLE,2*NOM_DE_NOEUD)'
      CHFOMA(2)='%FORMAT=(1*NOM_DE_MAILLE,3*NOM_DE_NOEUD)'
      CHFOMA(3)='%FORMAT=(1*NOM_DE_MAILLE,6*NOM_DE_NOEUD)'
      CHFOMA(4)='%FORMAT=(1*NOM_DE_MAIILE,9*NOM_DE_NOEUD)'
      CHFOMA(5)='%FORMAT=(1*NOM_DE_MAILLE,4*NOM_DE_NOEUD)'
      CHFOMA(6)='%FORMAT=(1*NOM_DE_MAILLE,8*NOM_DE_NOEUD)'
      CHFOMA(7)='%FORMAT=(1*NOM_DE_MAILLE,12*NOM_DE_NOEUD)'
      CHFOMA(8)='%FORMAT=(1*NOM_DE_MAILLE,6*NOM_DE_NOEUD)'
      CHFOMA(9)='%FORMAT=(1*NOM_DE_MAILLE,12*NOM_DE_NOEUD)'
      CHFOMA(10)='%FORMAT=(1*NOM_DE_MAILLE,18*NOM_DE_NOEUD)'
      CHFOMA(11)='%FORMAT=(1*NOM_DE_MAILLE,8*NOM_DE_NOEUD)'
      CHFOMA(12)='%FORMAT=(1*NOM_DE_MAILLE,16*NOM_DE_NOEUD)'
      CHFOMA(13)='%FORMAT=(1*NOM_DE_MAILLE,24*NOM_DE_NOEUD)'
      CHFOMA(14)='%FORMAT=(1*NOM_DE_MAILLE,4*NOM_DE_NOEUD)'
      CHFOMA(15)='%FORMAT=(1*NOM_DE_MAILLE,10*NOM_DE_NOEUD)'
      CHFOMA(16)='%FORMAT=(1*NOM_DE_MAILLE,6*NOM_DE_NOEUD)'
      CHFOMA(17)='%FORMAT=(1*NOM_DE_MAILLE,15*NOM_DE_NOEUD)'
      CHFOMA(18)='%FORMAT=(1*NOM_DE_MAILLE,24*NOM_DE_NOEUD)'
      CHFOMA(19)='%FORMAT=(1*NOM_DE_MAILLE,8*NOM_DE_NOEUD)'
      CHFOMA(20)='%FORMAT=(1*NOM_DE_MAILLE,20*NOM_DE_NOEUD)'
      CHFOMA(21)='%FORMAT=(1*NOM_DE_MAILLE,32*NOM_DE_NOEUD)'
      CHFOMA(29)='%FORMAT=(1*NOM_DE_MAILLE,2*NOM_DE_NOEUD)'
      CHFOMA(30)='%FORMAT=(1*NOM_DE_MAILLE,1*NOM_DE_NOEUD)'
      CHFOMA(31)='%FORMAT=(1*NOM_DE_MAILLE,2*NOM_DE_NOEUD)'
      CHFOMA(32)='%FORMAT=(1*NOM_DE_MAILLE,1*NOM_DE_NOEUD)'
      CHFOMA(33)='%FORMAT=(1*NOM_DE_MAILLE,1*NOM_DE_NOEUD)'
      CHFOMA(34)='%FORMAT=(1*NOM_DE_MAILLE,2*NOM_DE_NOEUD)'
      CHFOMA(35)='%FORMAT=(1*NOM_DE_MAILLE,3*NOM_DE_NOEUD)'
C
C     ECRITURE DES MAILLES
C     ====================
C
      CHMAIL = '        '
      CHENTI='NBOBJ=      '
      CHLIGN='NBLIGT=      '
      CHLIGE='NBLIGE=      '
      CHNOMI='NUMIN=      '
      CHNOMA='NUMAX=      '
C
C  --> N  D'UNITES LOGIQUES DES FICHIERS
C
      CALL JEVEUO('&&PRESUP.INFO.MAILLE','L',JINFO)
      CALL JEVEUO('&&PRESUP.CONN.MAILLE','L',JCONN)
      DO 2 IN = 1,MAXNOD
         CHTAB(IN) = '        '
    2 CONTINUE
C
      DO 10 NTE = 1,NBTYMA
       IPOS = 0
       IF (NBMAIL(NTE).EQ.0) GO TO 10
         NBLIE=3
         NBLIF=1
         NBLIT=NBMAIL(NTE)*LIMAIL(NTE)+NBLIE+NBLIF+1
C
         CALL CODENT (NBMAIL(NTE),'G',CHENTI(7:12))
         CALL CODENT (NBLIT,'G',CHLIGN(8:13))
         CALL CODENT (NBLIE,'G',CHLIGE(8:13))
         CALL CODENT (MINT(NTE),'G',CHNOMI(7:12))
         CALL CODENT (MANT(NTE),'G',CHNOMA(7:12))
C
C
C  --> ECRITURE DE LA DATE (IBM&CRAY)
         CALL JJMMAA(CT,AUT)
C
C  --> ECRITURE DE L'ENTETE DANS LE FICHIER NEUTRE
         WRITE (UNIT=IMOD,FMT='(A,3X,A,3X,A,3X,A,3X,A,3X,A)')
     &    NOMAIL(NTE),'NOM=INDEFINI',CHENTI,CHLIGE,CHLIGN
         WRITE(IMOD,'(11X,A,18X,A)') CHNOMI,CHNOMA
         WRITE (IMOD,'(11X,2A,11X,A,A2,A,A2,A,A4)')'AUTEUR=',AUT,
     &         'DATE=',CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
C
C  --> ECRITURE DU FORMAT DANS LE FICHIER NEUTRE
         WRITE(IMOD,'(A)') CHFOMA(NTE)
C
         DO 100 IMA=1,NBMTOT
           ITYP = ZI(JINFO-1+(IMA-1)*4+2)
           NEUL = ZI(JINFO-1+(IMA-1)*4+3)
           IF (ITYP.EQ.NTE) THEN
C
             INUM = ZI(JINFO-1+(IMA-1)*4+1)
             CALL CODNOP(CHMAIL,PRFMAI,1,1)
             CALL CODENT(INUM,'G',CHMAIL(2:8))
C
             DO 12 IN = 1,NEUL
                NEU2(IN) = ZI(JCONN-1+IPOS+IN)
                CALL CODNOP(CHTAB(IN),PRFNOE,1,1)
                CALL CODENT(NEU2(IN),'G',CHTAB(IN)(2:8))
   12        CONTINUE
C
             IDIV=INT(NEUL/8)
             IREST=MOD(NEUL,8)
             IF (IREST.NE.0) THEN
               WRITE (IMOD,202) CHMAIL, (CHTAB(I),I=1,NEUL)
             ELSE
               DO 1000 K=1,IDIV
                  L=8*(K-1)
                  IF (IDIV.EQ.1) THEN
                   WRITE(IMOD,'(A,8(1X,A))')  CHMAIL,
     &                                   (CHTAB(I),I=1,8)
                  ELSE
                   WRITE(IMOD,'(8X,8(1X,A))') (CHTAB(I),I=1+L,8+L)
                  ENDIF
 1000          CONTINUE
             ENDIF
           END IF
           IPOS = IPOS + NEUL
  100    CONTINUE
         WRITE (IMOD,'(A)') 'FINSF'
         WRITE(IMOD,'(A)') '%'
   10 CONTINUE
  202 FORMAT (A,8 (1X,A),/, (8X,8 (1X,A)))
      CALL JEDEMA()
      END
