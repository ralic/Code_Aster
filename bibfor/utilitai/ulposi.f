      SUBROUTINE ULPOSI ( UNIT, POSI, IERR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 27/05/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE D6BHHJP J.P.LEFEBVRE
      IMPLICIT   NONE 
      CHARACTER*(*)             POSI
      INTEGER             UNIT,       IERR
C     ------------------------------------------------------------------
C
C     POSITIONNEMENT DANS UN FICHIER ET VERIFICATION DE L'ETAT
C     APRES OPEN
C
C IN  : UNIT   : NUMERO D'UNITE LOGIQUE
C       POSI   : N = LE FICHIER EST ECRASE (NEW)
C                O = ON NE FAIT RIEN (ASSIS/OLD)
C                A = ON SE PLACE EN FIN DE FICHIER (APPEND)
C OUT : IERR   : CODE RETOUR D'ERREUR (OK  = 0)
C
C     ------------------------------------------------------------------
      CHARACTER*16     KACC
      CHARACTER*4      K4B
      CHARACTER*1      K1
      INTEGER          IOS,IEND
      LOGICAL          LOP,LNOM
C     ------------------------------------------------------------------
C
      IERR = 100
      K1   = POSI
      WRITE(K4B,'(I2)') UNIT
C
      INQUIRE(UNIT=UNIT, OPENED=LOP, NAMED=LNOM, ACCESS=KACC)
      IF ( LOP ) THEN
        IF ( KACC .NE. 'SEQUENTIAL' ) THEN
          IERR=101 
          CALL UTMESS('E','ULPOSI','TYPE D''ACCES INCONNU "'
     &                  //KACC//'", UNITE '//K4B )
        ELSE
          IF ( .NOT. LNOM ) THEN
            IERR=102
            CALL UTMESS('E','ULPOSI','FICHIER NON NOMME, UNITE '//K4B)
          ENDIF
        ENDIF
      ELSE
        IERR=103
        CALL UTMESS('E','ULPOSI','FICHIER NON OUVERT, UNITE '//K4B)
      ENDIF
C
      IF ( POSI .EQ. 'N' ) THEN
        REWIND (UNIT=UNIT, IOSTAT=IOS)
        IF ( IOS .EQ. 0) THEN
          IERR = 0 
        ELSE
          IERR = 104
          CALL UTMESS('E','ULPOSI','REWIND IMPOSSIBLE, UNITE '//K4B)
        ENDIF    
      ELSE IF ( POSI .EQ. 'O' ) THEN
        IERR = 0 
      ELSE IF ( POSI .EQ. 'A' ) THEN
C       POSITIONNEMENT EN FIN DE FICHIER 
C
 201    CONTINUE  
        IEND=0   
        IF ( IEND .LE. 0 ) THEN
          READ (UNIT,*,END=301)
          GOTO 201
        ENDIF   
 301    CONTINUE   
        IERR = 0 
      ELSE
        IERR = 105
        CALL UTMESS('E','ULPOSI','POSITIONNEMENT INCONNU "'//K1
     &              //'", UNITE '//K4B )
      ENDIF
C
 9999 CONTINUE
      END
