      SUBROUTINE UTFK16(LK16,NBK16,K16,IPOS)
      IMPLICIT NONE
      CHARACTER*16   LK16(*),K16   
      INTEGER       NBK16,IPOS 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/11/95   AUTEUR D5BAXRM R.MICHEL 
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
C     ==================================================================
C     ! UTILITAIRE - RECHERCHE DE LA POSITION D'UNE CHAINE K16 DANS UNE!
C     ! LISTE DE CHAINE K16                                          RM!
C     ==================================================================
C     !                                                                !
C     !    RECHERCHE DE LA PREMIERE OCCURENCE DE LA CHAINE K16 DANS LA !
C     !    LISTE DE K16 DONT ON CONNAIT LA LONGUEUR.                   !
C     !    LA ROUTINE RENVOIT LA POSITION DE LA CHAINE DANS LA LISTE   !
C     !    SI LA CHAINE EST ELEMENT DE LA LISTYE ET 0 SINON            !
C     !                                                                !
C     ==================================================================
C IN  ! LK16   ! K16 ! LISTE DE K16                                    !
C IN  ! NBK16  ! IS  ! TAILLE DE LA LISTE LK16                         !
C IN  ! K16    ! K16 ! CHAINE A POSITIONNER DANS LK16                  !
C OUT ! IPOS   ! IS  ! POSITION DANS LK16 DE L'OCCURENCE 1 DE K16      !
C     ==================================================================
C 
C --- VARIABLES LOCALES ---
      LOGICAL  TROUVE,FINI 
      INTEGER  I
C 
C ====================== DEBUT DU PROGRAMME ============================
C
C-DBG WRITE(6,*)'======= UTFK16 : IN  ======'
      TROUVE = .FALSE.
      FINI   = .FALSE.
      I      = 1
100   CONTINUE               
      IF (.NOT. FINI) THEN   
         IF (LK16(I) .EQ. K16) THEN
            TROUVE = .TRUE.
            FINI   = .TRUE.
         ELSE 
            I = I + 1
            IF (I .GT. NBK16) THEN
               FINI = .TRUE.
            ENDIF
         ENDIF
         GOTO 100
      ENDIF
      IF (TROUVE) THEN
         IPOS = I
      ELSE
         IPOS = 0
      ENDIF
C-DBG WRITE(6,*)'======= UTFK16 : OUT ======'
C
      END
