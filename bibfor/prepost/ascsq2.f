      SUBROUTINE ASCSQ2 ( UN, NBSEP, TYPE, NLX, NLY)
      IMPLICIT    NONE
      INTEGER     UN, NBSEP, NLX(*), NLY(*)
      CHARACTER*8 TYPE(*)        
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/03/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C TOLE  CRP_20
C     MACR_ASCOUF_MAIL
C
C     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
C                                                "PLAQUE SOUS-EPAISSEUR"
C     DEUXIEME PARTIE ( APRES LES DONNEES UTILISATEUR )
C
C
C-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
C
C     NLX = NOMBRE TOTAL D'ELEMENTS CIRCONF. DE LA SOUS-EPAISSEUR K
C     NLY = NOMBRE TOTAL D'ELEMENTS LONGIT. DE LA SOUS-EPAISSEUR K
C 
C     ------------------------------------------------------------------
C 
      CHARACTER*4    CAR, CAR2
      CHARACTER*4    CAR3(8)
      INTEGER        J, K, IPOS1, IPOS2
C      
      DATA           CAR3/'fdro','exdr','extr','exga',
     &                    'fgau','inga','intr','indr'/
C     
      CALL ASPECR(UN,
     +'* DEBUT POINTS DE POST-TRAITEMENT')
      CALL ASPECR(UN,
     +'*')
C      
        DO 25 J=1,NBSEP
C        
          WRITE(UN,10)  '*'
          WRITE(UN,30) '* sous-epaisseur No',J
          WRITE(UN,10)  '*'
          CALL CODENT(J,'G',CAR)
          IPOS1 = INDEX(CAR,' ')-1
C
          IF (TYPE(J).EQ.'ELLI') THEN
            WRITE(UN,10) '*'
            WRITE(UN,10) '* plans circonf, longi et colonne centrale'
            WRITE(UN,10) '*'
            WRITE(UN,40) 'pcirc',CAR,' = circo . ',J,';'
            WRITE(UN,40) 'plong',CAR,' = longi . ',J,';'
            WRITE(UN,40) 'pcent',CAR,' = centr . ',J,';'
          END IF
          WRITE(UN,10) '*'
          WRITE(UN,10) '* ligaments tous les 45 degres'//
     &                ' a epaisseur minimale'
          WRITE(UN,10) '*'
          WRITE(UN,60) 'isep = ',J,';'
          DO 15 K = 1,8
            WRITE(UN,60) 'ilig = ',K,';'
            WRITE(UN,10)  'rlig = ilig/10. + isep;'
            WRITE(UN,10)  CAR3(k)//CAR(1:ipos1)//' = lig45 . rlig;'
C            WRITE(UN,10)  CAR3(k)//CAR(1:ipos1)//
C     &                    '_i = lig45 . rlig point initial;' 
C            WRITE(UN,10)  CAR3(k)//CAR(1:ipos1)//
C     &                    '_f = lig45 . rlig point final;'
 15       CONTINUE
C
          IF (TYPE(J).EQ.'ELLI') THEN
C
             WRITE(UN,10) '*'
             WRITE(UN,10) '* ligaments circonferentiels '//
     &                   'a l''epaisseur minimale'
             WRITE(UN,10) '*'
             WRITE(UN,60) 'isep = ',J,';'
             DO 20 K= 1,2*NLX(J)+1
               CALL CODENT(K,'G',CAR2)
               IPOS2 = INDEX(CAR2,' ')-1
               WRITE(UN,60) 'ilig = ',K,';'
               WRITE(UN,10)  'rlig = ilig/100. + isep;'
               WRITE(UN,10)  'cir'//CAR(1:IPOS1)//'_'//
     &                       CAR2(1:IPOS2)//' = ligcir . rlig;'
C               WRITE(UN,10)  'c'//CAR(1:IPOS1)//'_'//
C     &            CAR2(1:IPOS2)//'_i = ligcir . rlig point initial;'
C               WRITE(UN,10)  'c'//CAR(1:IPOS1)//'_'//
C     &            CAR2(1:IPOS2)//'_f = ligcir . rlig point final;'
 20          CONTINUE  
             WRITE(UN,10) '*'
             WRITE(UN,10) '* ligaments longitudinaux '//
     &                   'a l''epaisseur minimale'
             WRITE(UN,10) '*'
             DO 35 K= 1,2*NLY(J)+1
               CALL CODENT(K,'G',CAR2)
               IPOS2 = INDEX(CAR2,' ')-1
               WRITE(UN,60) 'ilig = ',K,';'
               WRITE(UN,10)  'rlig = ilig/100. + isep;'
               WRITE(UN,10)  'lon'//CAR(1:IPOS1)//'_'//
     &                       CAR2(1:IPOS2)//' = liglon . rlig;'
C               WRITE(UN,10)  'l'//CAR(1:IPOS1)//'_'//
C     &             CAR2(1:IPOS2)//'_i = liglon . rlig point initial;'
C               WRITE(UN,10)  'l'//CAR(1:IPOS1)//'_'//
C     &             CAR2(1:IPOS2)//'_f = liglon . rlig point final;'
 35          CONTINUE
C
          END IF
C
 25     CONTINUE
C        
      CALL ASPECR(UN,
     +'* FIN POINTS DE POST-TRAITEMENT')
      CALL ASPECR(UN,
     +'*')
      CALL ASPECR(UN,
     +'p1 = 0. 0. (-1.*lt1);')
      CALL ASPECR(UN,
     +'p2 = 0. 0. (coor 3 bou3);')
      CALL ASPECR(UN,
     +'ma = coude et p1 et p2;')
      CALL ASPECR(UN,
     +'opti sauv form ''fort.8'' ;')
      CALL ASPECR(UN,
     +'sort ma;')    
       CALL ASPECR(UN,
     +'sauv form ma;')
      CALL ASPECR(UN,
     +'fin;')
C
 10   FORMAT(T1,A)
 30   FORMAT(T1,A,I2,A) 
 40   FORMAT(T1,A5,T6,A3,T9,A,T20,I3,T23,A)
 60   FORMAT(T1,A,T8,I3,T11,A)
C 
      END
