      SUBROUTINE UTFLOA (FLOA,CH1,CH2)
      IMPLICIT   NONE
      LOGICAL            FLOA
      CHARACTER*24       CH1,CH2
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 01/02/2010   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C
C BUT : ECRIRE UN REEL AVEC EXPOSANT EN UN REEL SANS EXPOSANT 
C       OU ECRIT UN REEL AVEC EXPOSANT EN SUPPRIMANT LES ZEROS
C       INUTILES
C
C UTILISE POUR L'ECRITURE DANS TEST_RESU, TEST_TABLE, TEST_FONCTION :
C
C ARGUMENTS :
C
C   IN :
C     FLOA   : = TRUE  : SI ON DECIDE D'ECRIRE SANS EXPOSANT
C              = FALSE : SI ON CONSERVE L'ECRITURE AVEC EXPOSANT
C     CH1    : CHAINE DE CARACATERES REPRESENTANT UNE VALEUR REELLE
C              AVEC EXPOSANT
C
C   OUT :
C     CH2   : CHAINE DE CARACATERES REPRESENTANT LA VALEUR CH1 
C             AVEC OU SANS EXPOSANT
C
C
C EXEMPLES :
C
C      - FLOA = TRUE :
C           a)  CH1 = 5.77352E+04   => CH2 = 57735.2       
C           b)  CH1 = 1.547500E-02  => CH2 = 0.015475
C
C      - FLOA = FALSE :
C           a)  CH1 = 5.77352E+04   => CH2 = 5.77352E+04 
C           b)  CH1 = 1.547500E-02  => CH2 = 1.5475E-02 
C
C
C ----------------------------------------------------------------------
      INTEGER I,III,LCV,LXLGUT,NEXPO,IRET,IN,SIGN,II,NDECI,N
      INTEGER ITMP
      CHARACTER*2        CHTMP
      CHARACTER*24       CHV3,CHV1,CHV2,CHWK
C
      CALL JEMARQ()
C
C
C --- INITIALISATION
C
      CHV1=' '
      CHV2=' '
      CHV3=' '
      CHTMP=' '
      CHWK=' '
      IF(CH1(1:1).EQ.'-')THEN
          CHV1=CH1(2:)
          CH2(1:1)='-'
      ELSE
          CHV1=CH1
          CH2(1:1)=' '
      ENDIF
C
C --- POSITION DE L'EXPOSANT DANS LA CHAINE : III
C   
      LCV = LXLGUT(CHV1)
      II=0
      DO 10 I=1,LCV
         IF(CHV1(I:I).NE.'E')THEN
            II=II+1
            GOTO 10
         ENDIF
         GOTO 11
 10   CONTINUE
 11   CONTINUE 
C
C     SI LA CHAINE NE CONTINENT PAS DE 'E' : ON S'ARRETE
      IF(II.EQ.LCV)CALL ASSERT(.FALSE.)
      III=II+1
C
C     SIGNE : SIGN
      SIGN=0.D0
      IF(CHV1(III+1:III+1).EQ.'-')THEN 
         SIGN=-1
      ELSE IF(CHV1(III+1:III+1).EQ.'+')THEN 
         SIGN=1
      ENDIF
C
C     VALEUR DE L'EXPOSANT
      CALL LXLIIS(CHV1(III+2:III+3),NEXPO,IRET)
C
C
C --- POSITION DU POINT DANS LA CHAINE : II
C   
      II=0
      DO 20 I=1,LCV
         IF(CHV1(I:I).NE.'.')THEN
            II=II+1
            GOTO 20
         ENDIF
         GOTO 21
 20   CONTINUE
 21   CONTINUE
C
C     SI LA CHAINE NE CONTINENT PAS DE POINT : ON S'ARRETE
      IF(II.EQ.LCV)CALL ASSERT(.FALSE.)
C
C     POSITION DANS LA CHAINE DU '.' : II
      II=II+1
C
C
C --- REECRITURE (EVENTUELLE) DE CHV1 EN CHV3
C   
      IF(II.EQ.2)THEN
         CHV3=CHV1
         IN=SIGN*NEXPO
      ELSE
         CHV3(1:1)=CHV1(1:1)
         CHV3(2:2)='.'
         DO 30 I=1,II-2
            CHV3(I+2:I+2)=CHV1(I+1:I+1)
 30      CONTINUE
         DO 40 I=1,III-II-1
            CHV3(I+II:I+II)=CHV1(I+II:I+II)
 40      CONTINUE
         CHV3(III:III)='E'
         IN=II-2+SIGN*NEXPO
         IF(IN.GE.0)THEN
           CHV3(III+1:III+1)='+'
         ELSE
           CHV3(III+1:III+1)='-'
         ENDIF
         CALL CODENT(ABS(IN),'D0',CHTMP)
         CHV3=CHV3(1:III+1)//CHTMP(1:2)
         NEXPO=ABS(IN)
      ENDIF
C
C     NOMBRE DE DECIMALES : NDECI
      NDECI=III-3  
C
C     ===================================================
C     ====  SI ON EFFECTUE L'ECRITURE SANS EXPOSANT  ====
C     ===================================================
C
      IF(FLOA)THEN
C
C     --- EXPOSANT POSITIF ---
C
         IF(IN.GE.0)THEN
C
            IF(NDECI.GT.NEXPO)THEN
               CHV2(1:1)=CHV3(1:1)
               DO 50 I=1,NEXPO
                 CHV2(1+I:1+I)=CHV3(2+I:2+I)
 50            CONTINUE
               CHV2(2+NEXPO:2+NEXPO)='.'

                DO 60 I=1,NDECI-NEXPO
                  CHV2(I+2+NEXPO:I+2+NEXPO)=CHV3(2+NEXPO+I:2+NEXPO+I)
 60             CONTINUE
                N=LXLGUT(CHV2)
                ITMP=0
                DO 61 I=N,1,-1
                   IF(CHV2(I:I).EQ.'.')THEN
                       GOTO 66
                   ENDIF
                   IF(CHV2(I:I).EQ.'0')THEN
                       ITMP=ITMP+1
                   ELSE 
                       GOTO 66
                   ENDIF
 61             CONTINUE    
 66             CONTINUE  
                IF(ITMP.GT.0)THEN
                   CHWK=CHV2
                   CHV2=' '
                   CHV2=CHWK(1:N-ITMP)
                   IF(CHV2(N-ITMP:N-ITMP).EQ.'.')THEN
                        CHV2(N-ITMP+1:N-ITMP+1)='0'
                   ENDIF 
                ENDIF           

            ELSE
               CHV2(1:1)=CHV3(1:1)
               DO 150 I=1,NDECI
                 CHV2(1+I:1+I)=CHV3(2+I:2+I)
150            CONTINUE
               IF(NDECI.EQ.NEXPO)THEN
                  CHV2(2+NDECI:3+NDECI)='.0'
               ELSE
                  DO 151 I=1,NEXPO-NDECI
                    CHV2(1+NDECI+I:1+NDECI+I)='0'
151               CONTINUE
                  CHV2(2+NEXPO:3+NEXPO)='.0'
               ENDIF
            ENDIF
C
C     --- EXPOSANT NEGATIF --- 
C
         ELSEIF(IN.LT.0)THEN
C
            CHV2(1:2)='0.'
            DO 70 I=1,NEXPO-1
              CHV2(2+I:2+I)='0'
 70         CONTINUE
            CHV2(2+NEXPO:2+NEXPO)=CHV3(1:1)
            DO 80 I=1,NDECI
              CHV2(2+NEXPO+I:2+NEXPO+I)=CHV3(2+I:2+I)
 80         CONTINUE
C
         ENDIF
C
C
C     ===================================================
C     ====  SI ON CONSERVE L'ECRITURE AVEC EXPOSANT  ====
C     ===================================================
C
      ELSE
C
         N=LXLGUT(CHV3)
         ITMP=0
         DO 261 I=III-1,1,-1
            IF(CHV3(I:I).EQ.'.')THEN
                GOTO 266
            ENDIF
            IF(CHV3(I:I).EQ.'0')THEN
                ITMP=ITMP+1
            ELSE 
                GOTO 266
            ENDIF
 261     CONTINUE    
 266     CONTINUE  
         IF(ITMP.GT.0)THEN
             CHV2=CHV3(1:III-1-ITMP)
             IF(CHV2(III-1-ITMP:III-1-ITMP).EQ.'.')THEN
                CHV2(III-ITMP:III-ITMP)='0'
             ENDIF
             N=LXLGUT(CHV2)
             CHV2(N+1:N+4)=CHV3(III:III+3)
         ELSE
             CHV2=CHV3    
         ENDIF           




      ENDIF

C     
C
C
C --- FIN : ON RETOURNE CHV2 EN PRENANT EN COMPTE LE SIGNE
C     ----------------------------------------------------
C
      IF(CH2(1:1).EQ.'-')THEN
          CH2(2:)=CHV2
      ELSE
          CH2=CHV2
      ENDIF
C
      CALL JEDEMA()
C
      END
