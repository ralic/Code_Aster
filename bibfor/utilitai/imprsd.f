      SUBROUTINE IMPRSD(TYPESD,NOMSD,IFIC,TITRE)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*(*) TYPESD,NOMSD,TITRE
      INTEGER IFIC
C ----------------------------------------------------------------------
C MODIF UTILITAI  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C RESPONSABLE PELLET J.PELLET
C ----------------------------------------------------------------------
C  BUT : IMPRIMER UNE STRUCTURE DE DONNEE DONT ON CONNAIT LE TYPE


C TYPESD  IN K*   : TYPE DE LA STRUCTURE DE DONNEE A IMPRIMER
C                      'CHAMP' :   /CHAM_NO/CHAM_ELEM/RESUELEM/CARTE
C                                  /CHAM_NO_S/CHAM_ELEM_S
C                      'MATRICE' : MATR_ASSE/MATR_GENE
C NOMSD   IN K*  : NOM DE LA STRUCTURE DE DONNEES A IMPRIMER
C IFIC    IN I   : NUMERO LOGIQUE DU FICHIER ASCII POUR L'IMPRESSION
C TITRE   IN K*  : CHAINE DE CARACTERES IMPRIMEE EN TETE

C ----------------------------------------------------------------------

      INTEGER      IBID,I1,I2,I3,I4,I5,I6,IB
      INTEGER      J1,J2,J3,K,NPARA
      CHARACTER*16 TYP2SD
      CHARACTER*19 CH,CHS,MATR
      CHARACTER*17 TABLE

C -DEB------------------------------------------------------------------

      CALL JEMARQ()
      TYP2SD = TYPESD

      CALL ASSERT((IFIC.NE.0) .AND. (IFIC.LE.100))
      IBID = 0

C     1. ECRITURE DU TITRE :
C     ----------------------
      WRITE (IFIC,*) ' '
      WRITE (IFIC,*) '-----------------------------------------------'
      WRITE (IFIC,*) TITRE


C     2. APPEL A LA BONNE ROUTINE :
C     ------------------------------

      IF ((TYP2SD.EQ.'CHAMP') .OR. (TYP2SD.EQ.'CHAMP_GD') .OR.
     &    (TYP2SD.EQ.'CHAMP_S')) THEN
C     ------------------------------------
        CH = NOMSD
        CHS = '&&IMPRSD.CHS'

        CALL EXISD('CHAM_NO_S',CH,I1)
        CALL EXISD('CHAM_ELEM_S',CH,I2)
        CALL EXISD('CHAM_NO',CH,I3)
        CALL EXISD('CHAM_ELEM',CH,I4)
        CALL EXISD('CARTE',CH,I5)
        CALL EXISD('RESUELEM',CH,I6)


        IF (I1.GT.0) CALL CNSIMP(CH,IFIC)
        IF (I2.GT.0) CALL CESIMP(CH,IFIC,IBID,IBID)

        IF (I3.GT.0) THEN
          CALL CNOCNS(CH,'V',CHS)
          CALL CNSIMP(CHS,IFIC)
          CALL DETRSD('CHAM_NO_S',CHS)
        END IF

        IF (I4.GT.0) THEN
          CALL CELCES(CH,'V',CHS)
          CALL CESIMP(CHS,IFIC,IBID,IBID)
          CALL DETRSD('CHAM_ELEM_S',CHS)
        END IF

        IF (I5.GT.0) THEN
          CALL CARCES(CH,'ELEM',' ','V',CHS,'A',IB)
          CALL CESIMP(CHS,IFIC,IBID,IBID)
          CALL DETRSD('CHAM_ELEM_S',CHS)
        END IF

        IF (I6.GT.0) WRITE (IFIC,*) 'TYPE : RESUELEM NON TRAITE.'


      ELSE IF (TYP2SD.EQ.'TABLE') THEN
C     --------------------------------------
        TABLE=NOMSD
        CALL JEVEUO(TABLE//'  .TBNP','L',J1)
        CALL JEVEUO(TABLE//'  .TBLP','L',J2)
        NPARA=ZI(J1)
        CALL WKVECT('&&IMPRSD.LIPARA','V V K16',NPARA,J3)
        DO 1, K=1,NPARA
          ZK16(J3-1+K)=ZK24(J2-1+4*(K-1)+1)
1       CONTINUE
        CALL TBIMPR(TABLE,'ASTER',IFIC,NPARA,ZK16(J3),0,'1PE12.5')
        CALL JEDETR('&&IMPRSD.LIPARA')


      ELSE IF (TYP2SD.EQ.'MATRICE') THEN
C     --------------------------------------
        MATR=NOMSD
        CALL MATIMP(MATR,IFIC,'ASTER')


      ELSE
C     --------------------------------------
        CALL U2MESK('F','UTILITAI_47',1,TYP2SD)
      END IF

      CALL JEDEMA()
      END
