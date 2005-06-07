      SUBROUTINE RBPH01 ( TRANGE, NBCHAM, TYPE, ITRESU, NFONCT, BASEMO,
     +                    TYPREF, TYPBAS, TOUSNO, MULTAP )
      IMPLICIT   NONE
      INTEGER             NBCHAM, ITRESU(*), NFONCT
      CHARACTER*8         BASEMO
      CHARACTER*16        TYPE(*), TYPBAS(*)
      CHARACTER*19        TRANGE, TYPREF(*)
      LOGICAL             TOUSNO, MULTAP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/97   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     OPERATEUR REST_BASE_PHYS
C               TRAITEMENT DES MOTS CLES "TOUT_CHAM" ET "NOM_CHAM"
C     ------------------------------------------------------------------
      INTEGER       N1, I, IRET
      CHARACTER*8   BLANC, MODE
      CHARACTER*16  CHAMP(8), NOMCMD
      CHARACTER*19  NOMCHA
C     ------------------------------------------------------------------
      DATA BLANC    /'        '/
C     ------------------------------------------------------------------
C
      NOMCMD = 'REST_BASE_PHYS'
      MODE = BASEMO
C
      CALL GETVTX ( ' ', 'TOUT_CHAM', 1,1,1, CHAMP, N1 )
C
      IF ( CHAMP(1) .EQ. 'OUI' ) THEN
         NBCHAM = 3
         TYPE(1) = 'DEPL            '
         TYPE(2) = 'VITE            '
         TYPE(3) = 'ACCE            '
         CALL JEEXIN ( TRANGE//'.DEPL' , IRET )
         IF ( IRET .EQ. 0 ) THEN
            CALL UTMESS('F',NOMCMD,'LE CHAMP DE DEPLACEMENT N''A '//
     +                             'PAS ETE CALCULE.')
         ELSE
            CALL JEVEUO ( TRANGE//'.DEPL', 'L', ITRESU(1) )
         ENDIF
         CALL JEEXIN ( TRANGE//'.VITE' , IRET )
         IF ( IRET .EQ. 0 ) THEN
            CALL UTMESS('F',NOMCMD,'LE CHAMP DE VITESSE N''A '//
     +                             'PAS ETE CALCULE.')
         ELSE
            CALL JEVEUO ( TRANGE//'.VITE', 'L', ITRESU(2) )
         ENDIF
         CALL JEEXIN ( TRANGE//'.ACCE' , IRET )
         IF ( IRET .EQ. 0 ) THEN
            CALL UTMESS('F',NOMCMD,'LE CHAMP D''ACCELERATION N''A '//
     +                             'PAS ETE CALCULE.')
         ELSE
            CALL JEVEUO ( TRANGE//'.ACCE', 'L', ITRESU(3) )
         ENDIF
         IF ( NFONCT .NE. 0 ) THEN
            NBCHAM = 4
            TYPE(4) = 'ACCE_ABSOLU     '
            ITRESU(4) = ITRESU(3)
         ENDIF
         IF ( MODE .EQ. BLANC ) THEN
            TYPREF(1) = ' '
            TYPREF(2) = ' '
            TYPREF(3) = ' '
            TYPREF(4) = ' '
         ELSE
            CALL RSEXCH ( BASEMO, 'DEPL', 1, NOMCHA, IRET )
            TYPREF(1) = NOMCHA
            TYPREF(2) = NOMCHA
            TYPREF(3) = NOMCHA
            TYPREF(4) = NOMCHA
         ENDIF
         TYPBAS(1) = 'DEPL'
         TYPBAS(2) = 'DEPL'
         TYPBAS(3) = 'DEPL'
         TYPBAS(4) = 'DEPL'
C
      ELSE
C
         CALL GETVTX ( ' ', 'NOM_CHAM', 1,1,0, CHAMP, N1 )
         NBCHAM = -N1
         CALL GETVTX ( ' ', 'NOM_CHAM', 1,1,NBCHAM, CHAMP, N1 )
C
         DO 10 I = 1 , NBCHAM
         IF ( CHAMP(I) .EQ. 'DEPL' ) THEN
            TYPE(I) = 'DEPL'
            CALL JEEXIN ( TRANGE//'.DEPL' , IRET )
            IF ( IRET .EQ. 0 ) THEN
               CALL UTMESS('F',NOMCMD,'LE CHAMP DE DEPLACEMENT N''A'//
     +                               ' PAS ETE CALCULE.')
            ELSE
               CALL JEVEUO ( TRANGE//'.DEPL', 'L', ITRESU(I) )
            ENDIF
            IF ( MODE .EQ. BLANC ) THEN
               TYPREF(I) = ' '
            ELSE
               CALL RSEXCH ( BASEMO, TYPE(I), 1, NOMCHA, IRET )
               TYPREF(I) = NOMCHA
            ENDIF
            TYPBAS(I) = 'DEPL'
C
         ELSEIF ( CHAMP(I) .EQ. 'VITE' ) THEN
            TYPE(I) = 'VITE'
            CALL JEEXIN ( TRANGE//'.VITE' , IRET )
            IF ( IRET .EQ. 0 ) THEN
               CALL UTMESS('F',NOMCMD,'LE CHAMP DE VITESSE N''A'//
     +                               ' PAS ETE CALCULE.')
            ELSE
               CALL JEVEUO ( TRANGE//'.VITE', 'L', ITRESU(I) )
            ENDIF
            IF ( MODE .EQ. BLANC ) THEN
               TYPREF(I) = ' '
            ELSE
               CALL RSEXCH ( BASEMO, 'DEPL', 1, NOMCHA, IRET )
               TYPREF(I) = NOMCHA
            ENDIF
            TYPBAS(I) = 'DEPL'
C
         ELSEIF ( CHAMP(I) .EQ. 'ACCE' ) THEN
            TYPE(I) = 'ACCE'
            CALL JEEXIN ( TRANGE//'.ACCE' , IRET )
            IF ( IRET .EQ. 0 ) THEN
               CALL UTMESS('F',NOMCMD,'LE CHAMP D''ACCELERATION N''A'//
     +                               ' PAS ETE CALCULE.')
            ELSE
               CALL JEVEUO ( TRANGE//'.ACCE', 'L', ITRESU(I) )
            ENDIF
            IF ( MODE .EQ. BLANC ) THEN
               TYPREF(I) = ' '
            ELSE
               CALL RSEXCH ( BASEMO, 'DEPL', 1, NOMCHA, IRET )
               TYPREF(I) = NOMCHA
            ENDIF
            TYPBAS(I) = 'DEPL'
C
         ELSEIF ( CHAMP(I) .EQ. 'ACCE_ABSOLU' ) THEN
            TYPE(I) = 'ACCE_ABSOLU'
            CALL JEEXIN ( TRANGE//'.ACCE' , IRET )
            IF ( IRET .EQ. 0 ) THEN
               CALL UTMESS('F',NOMCMD,'LE CHAMP D''ACCELERATION N''A'//
     +                               ' PAS ETE CALCULE.')
            ELSE
               CALL JEVEUO ( TRANGE//'.ACCE', 'L', ITRESU(I) )
            ENDIF
            IF ( MODE .EQ. BLANC ) THEN
               TYPREF(I) = ' '
            ELSE
               CALL RSEXCH ( BASEMO, 'DEPL', 1, NOMCHA, IRET )
               TYPREF(I) = NOMCHA
            ENDIF
            TYPBAS(I) = 'DEPL'
C
         ELSEIF ( CHAMP(I) .EQ. 'FORC_NODA' .OR.
     +            CHAMP(I) .EQ. 'REAC_NODA' ) THEN
            TYPE(I) = CHAMP(I)
            CALL JEEXIN ( TRANGE//'.DEPL' , IRET )
            IF ( IRET .EQ. 0 ) THEN
               CALL UTMESS('F',NOMCMD,'LE CHAMP DE DEPLACEMENT N''A'//
     +                               ' PAS ETE CALCULE.')
            ELSE
               CALL JEVEUO ( TRANGE//'.DEPL', 'L', ITRESU(I) )
            ENDIF
            IF ( MULTAP ) THEN 
               CALL UTMESS('F',NOMCMD,'DEVELOPPEMENT NON PREVU POUR '//
     +                                'LE MULT_APPUI OU CORR_STAT.')
            ENDIF
            IF ( MODE .EQ. BLANC ) THEN
               CALL UTMESS('F',NOMCMD,'DEVELOPPEMENT NON PREVU POUR '//
     +                                'LA SOUS-STRUCTURATION.')
            ELSE
               CALL RSEXCH ( BASEMO, TYPE(I), 1, NOMCHA, IRET )
               IF ( IRET .NE. 0 ) THEN
                  CALL UTMESS('F',NOMCMD,'LE CHAMP '//TYPE(I)//' N''A'//
     +                      ' PAS ETE CALCULE DANS LE MODE_MECA '//MODE)
               ENDIF
               TYPREF(I) = NOMCHA
            ENDIF
            TYPBAS(I) = TYPE(I)
C
         ELSE
            TYPE(I) = CHAMP(I)
            CALL JEEXIN ( TRANGE//'.DEPL' , IRET )
            IF ( IRET .EQ. 0 ) THEN
               CALL UTMESS('F',NOMCMD,'LE CHAMP DE DEPLACEMENT N''A'//
     +                               ' PAS ETE CALCULE.')
            ELSE
               CALL JEVEUO ( TRANGE//'.DEPL', 'L', ITRESU(I) )
            ENDIF
            IF ( .NOT. TOUSNO ) THEN 
               CALL UTMESS('F',NOMCMD,'L''OPTION '//TYPE(I)//
     +                            ' S''APLIQUE SUR TOUTE LA STRUCTURE')
            ENDIF
            IF ( MULTAP ) THEN 
               CALL UTMESS('F',NOMCMD,'DEVELOPPEMENT NON PREVU POUR '//
     +                                'LE MULT_APPUI OU CORR_STAT.')
            ENDIF
            IF ( MODE .EQ. BLANC ) THEN
               CALL UTMESS('F',NOMCMD,'DEVELOPPEMENT NON PREVU POUR '//
     +                                'LA SOUS-STRUCTURATION.')
            ELSE
               CALL RSEXCH ( BASEMO, TYPE(I), 1, NOMCHA, IRET )
               IF ( IRET .NE. 0 ) THEN
                  CALL UTMESS('F',NOMCMD,'LE CHAMP '//TYPE(I)//' N''A'//
     +                      ' PAS ETE CALCULE DANS LE MODE_MECA '//MODE)
               ENDIF
               TYPREF(I) = NOMCHA
            ENDIF
            TYPBAS(I) = TYPE(I)
C
         ENDIF
  10     CONTINUE
      ENDIF
C
      END
