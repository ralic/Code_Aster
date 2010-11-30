      SUBROUTINE INSLRI(NBX,NBN,LISTER,LISTEI,VALR,VALI)
      IMPLICIT NONE
      INTEGER NBX,NBN
      INTEGER LISTEI(NBX),VALI
      REAL*8  LISTER(NBX),VALR

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 12/05/2009   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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

C  Insere VALR dans LISTER
C     LISTER est classe du plus grand au plus petit
C     LISTEI contient les VALI
C
C  IN
C     NBX    : nombre maximum de valeur dans les listes
C     VALR   : valeur reelle a inserer dans LISTER
C     VALI   : valeur entiere
C
C  OUT/IN
C     NBN    : nombre actualise de valeur dans les listes
C              Doit etre initialise a 0 avant l'appel
C     LISTER : liste actualisee des reels
C     LISTEI : liste actualisee des entiers

      INTEGER II,INDX

      IF ( NBN .EQ. 0) THEN
         NBN = 1
         LISTEI(1) = VALI
         LISTER(1) = VALR
      ELSE
         INDX = NBN+1
         DO 71,II = NBN,1,-1
            IF ( VALR .GT. LISTER(II) ) INDX = II
71       CONTINUE
         IF ( INDX .LE. NBX ) THEN
            DO 72,II = NBX,INDX,-1
               LISTEI(II) = LISTEI(II-1)
               LISTER(II) = LISTER(II-1)
72          CONTINUE
            LISTEI(INDX) = VALI
            LISTER(INDX) = VALR
         ENDIF
         IF ( NBN .LT. NBX ) NBN = NBN + 1
      ENDIF
      END
