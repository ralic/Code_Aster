      SUBROUTINE SLETIT
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 10/12/2001   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C     =================
CA PRESUPER
C
C     ================================================================
C     !                                                              !
C     !  FONCTION: LECTURE DU TITRE DANS LE FICHIER UNIVERSEL ISSU DE!
C     !            SUPERTAB I-DEAS 4.0, 6.0 OU 7.0   PUIS ECRITURE   !
C     !            D'UNE PARTIE DE CELUI-CI DANS LE FICHIER MODELE   !
C     !                                                              !
C     ================================================================
C     !                                                              !
C     ! ROUTINE APPELES : CODENT                                     !
C     !                        : IUNIFI (FONCTION)                   !
C     !                        : JJMMAA                              !
C     !                                                              !
C     ! ROUTINE APPELANTE : PRESUP                                   !
C     !                                                              !
C     ================================================================
C
C
C
C  ---> DECLARATION DES VARIABLES LOCALES
C
      CHARACTER*1 CHEXT
      CHARACTER*4 CT(3)
      CHARACTER*6 MOINS1
      CHARACTER*13 CHLIGN,CHLIGE
      CHARACTER*12 CHENTI
      CHARACTER*12 AUT
      CHARACTER*15 AUT1
      CHARACTER*72 CTITR(7),IBLA
      CHARACTER*80 CHFOTE
      INTEGER IUNV,IMES
      INTEGER NBLIE,NBLIT,NBLIF,I
C
C ---> DECLARATION DES INDICES DE BOUCLES
C
      INTEGER ICAR
C
C  ----------- FIN DECLARATION -------------
C
C     LECTURE  DU  TITRE
C     ------------------
C
      CHFOTE='%FORMAT=(1*LIGNE_DE_TEXTE)'
      CHLIGN='NBLIGT=      '
      CHENTI='NBOBJ=      '
      CHLIGE='NBLIGE=      '
      MOINS1 = '    -1'
      CHEXT=' '
C
      I = 0
C
    1 CONTINUE
      I = I + 1
C
C  --> N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
      IUNV = IUNIFI('UNIVERSEL')
      IMES = IUNIFI('MESSAGE')
C
      READ (UNIT=IUNV,FMT='(A)') CTITR(I)
      IF (CTITR(I) (1:6).NE.MOINS1 .AND. I.LT.7) GO TO 1
      IBLA = CTITR(I)
    2 CONTINUE
      IF (IBLA(1:6).NE.MOINS1) THEN
         READ (UNIT=IUNV,FMT='(A)') IBLA
         GO TO 2
      END IF
C
C     ECRITURE  DU  TITRE
C     ------------------
C
C  --> ECRITURE DE LA DATE DU JOUR (IBM&CRAY)
      CALL JJMMAA(CT,AUT)
C
      NBLIE=5
      NBLIF=1
      NBLIT=NBLIE+NBLIF+2+1
C
      CALL CODENT(NBLIT,'G',CHLIGN(8:13))
      CALL CODENT(NBLIE,'G',CHLIGE(8:13))
      CALL CODENT(2,'G',CHENTI(7:12))
C
      IMOD = IUNIFI('FICHIER-MODELE')
      WRITE (IMOD,'(A,4X,A,8X,A,4X,A,4X,A)')'TITRE','NOM=INDEFINI',
     &      CHENTI,CHLIGE,CHLIGN
      AUT1 = 'INTERFACE_IDEAS'
      WRITE(IMOD,'(9X,2A,17X,A,A2,A,A2,A,A4)')'AUTEUR=',AUT1,'DATE=',
     &                        CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
      WRITE(IMOD,'(2A)') '%     LES INFORMATIONS QUI SUIVENT',
     &    ' CONCERNENT :'
      WRITE(IMOD,'(2A)') '%     1-LA DESCRIPTION DU FICHIER MODELE'
      WRITE(IMOD,'(2A)') '%     2-LA DATE D''ECRITURE DU FICHIER',
     &    ' UNIVERSEL'
C
C ---> ECRITURE DU FORMAT
C
      WRITE(IMOD,'(A)') CHFOTE
C
C ---> ECRITURE DES INFORMATIONS EXTRAITES
C
      DO 10 ICAR=1,72
        CHEXT=CTITR(7)(ICAR:ICAR)
C
        IF (CHEXT.EQ.'-') THEN
          CTITR(7)(ICAR:ICAR)=' '
        ENDIF
C
 10   CONTINUE
C
      WRITE (IMOD,'(2X,A)') CTITR(2)
      WRITE (IMOD,'(2X,A)') CTITR(7)
C
C ---> ECRITURE DE LA MARQUE DE FIN DE SOUS FICHIER
C
      WRITE(IMOD,'(2A)') '% ATTENTION: SI AUTEUR=INTERFACE_IDEAS',
     &  ' TRAITEMENT SPECIAL DANS IMPR_RESU'
      WRITE (IMOD,'(A)') 'FINSF'
      WRITE (IMOD,'(A)') '%'
      WRITE (IMES,*) 'ECRITURE DU TITRE'
      END
