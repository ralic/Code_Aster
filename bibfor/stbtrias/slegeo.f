      SUBROUTINE SLEGEO
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 27/05/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C
C  ================================================================
C  !                                                              !
C  !  FONCTION : LECTURE DANS LE FICHIER UNIVERSEL ISSU DE SUPER- !
C  !             TAB I-DEAS 4.0, 6.0 OU 7.0   DES NOEUDS ET       !
C  !             MAILLES RATTACHES AUX CURVES,MESHS AREA ET MESHS !
C  !             VOLUME PUIS ECRITURE DANS LE FICHIER MODELE.DANS !
C  !             UN SECOND TEMPS REGROUPEMENT DES CURVES,DES MESH !
C  !             ET DES MESH VOLUMES PUIS ECRITURE DANS LE FICHIER!
C  !             MODELE                                           !
C  !                                                              !
C  ================================================================
C  !                                                              !
C  !  ROUTINES APPELES : CODENT                                   !
C  !                          : IUNIFI (FONCTION)                 !
C  !                          : JJMMAA                            !
C  !                          : ECRCAV                            !
C  !                                                              !
C  !  ROUTINE APPELANTE : PRESUP                                  !
C  !                                                              !
C  ================================================================
C
C
C  --> DECLARATION DES VARIABLES LOCALES
C
      CHARACTER*2 PRFNOE,PRFMAI
      CHARACTER*80 CBUF
      CHARACTER*4 CT(3)
      CHARACTER*8 CHMAIL,CHNODE,NUOBJ(8)
      CHARACTER*10 NOMC,NOMA,NOMV
      CHARACTER*10 CHNOMC,CHNOMA,CHNOMV
      CHARACTER*12 CHENTI,AUT
      CHARACTER*13 CHLIGN,CHLIGE
      CHARACTER*80 CHFOGN
      CHARACTER*80 CHFOGM
      INTEGER IND,INDIC,INUM,ITYP,NBENTI,ITEST,NBLIGN
      INTEGER NBLIT,NBLIE,NBLIF
      INTEGER ICOMPC,ICOMPA,ICOMPV
      INTEGER NUMENT(8)
      INTEGER IUNV,IMES,IGRN,IGRM,IGRF,IMA
C
C  --> DECLARATION INDICE DE BOUCLES
C
      INTEGER I,J,K
C  ------------ FIN DECLARATION ---------------
C
C  --> N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
      IUNV=IUNIFI('IDEAS')
      IMES=IUNIFI('MESSAGE')
      IMOD=IUNIFI('FICHIER-MODELE')
C
C  --> COMPTEUR :NBRE DE CURVES,NBRE DE M.AREA,NBRE DE M.VOLUME
C
      ICOMPC=0
      ICOMPA=0
      ICOMPV=0
C
      PRFNOE='NO'
      PRFMAI='MA'
      CHFOGN='%FORMAT=(1*NOM_DE_NOEUD)'
      CHFOGM='%FORMAT=(1*NOM_DE_MAILLE)'
      CHNODE='        '
      CHMAIL='        '
      CHENTI='NBOBJ=      '
      CHLIGN='NBLIGT=      '
      CHLIGE='NBLIGE=      '
C
   1  CONTINUE
      READ(IUNV,'(A)')CBUF
      READ(CBUF,'(4X,I2)') IND
      NBLIGN=0
      ITEST=0
      IF (IND.EQ.-1) GOTO 1000
        READ(CBUF,'(4I10)') INDIC,INUM,ITYP,NBENTI
        IF(INDIC.EQ.3.OR.INDIC.EQ.5.OR.INDIC.EQ.6) THEN
C
          IF (INDIC.EQ.3.AND.ITYP.EQ.7) THEN
C         --> ON TROUVE UNE CURVE DECRITE PAR SES NOEUDS
            ICOMPC=ICOMPC+1
            NOMC(1:5)='CURVE'
            CALL CODENT(INUM,'G',NOMC(6:10))
C
          ELSE IF(INDIC.EQ.3.AND.ITYP.EQ.8) THEN
C         --> ON TROUVE UNE CURVE DECRITE PAR SES ELEMENTS
C
          ELSE IF (INDIC.EQ.5.AND.ITYP.EQ.7) THEN
C         --> ON TROUVE UNE MESH AREA DECRITE PAR SES NOEUDS
            ICOMPA=ICOMPA+1
            NOMA(1:6)='M_AREA'
            CALL CODENT(INUM,'G',NOMA(7:10))
C
          ELSE IF (INDIC.EQ.5.AND.ITYP.EQ.8) THEN
C         --> ON TROUVE UNE MESH AREA DECRITE PAR SES ELEMENTS
C
          ELSE IF(INDIC.EQ.6.AND.ITYP.EQ.7) THEN
C         --> ON TROUVE UN MESH VOLUME DECRIT PAR SES POINTS
            ICOMPV=ICOMPV+1
            NOMV(1:6)='M_VOLU'
            CALL CODENT(INUM,'G',NOMV(7:10))
C
          ELSE
C         --> ON TROUVE UN MESH VOLUME DECRIT PAR SES ELEMENTS
          ENDIF
C
          IDIV=INT(NBENTI/8)
          IREST=MOD(NBENTI,8)
C
          IF (IREST.NE.0) THEN
            ITEST=1
          ENDIF
C
            NBLIGN=IDIV+ITEST
            NBLIE=2
            NBLIF=1
            NBLIT=NBLIGN+NBLIE+NBLIF+1
C
            CALL CODENT(NBENTI,'G',CHENTI(7:12))
            CALL CODENT(NBLIT,'G',CHLIGN(8:13))
            CALL CODENT(NBLIE,'G',CHLIGE(8:13))
C
C   --> ECRITURE DE LA DATE(IBM & CRAY)
            CALL JJMMAA(CT,AUT)
C
            IF (INDIC.EQ.3.AND.ITYP.EQ.7) THEN
C
C    ---> ON TROUVE UNE CURVE DECRITE PAR SES NOEUDS
C
              ILONG=LXLGUT(NOMC)
              IF(ILONG.GT.8) CALL UTMESS('A','PRESUP',
     &        'GROUPE '//NOMC//' DE LONGUEUR SUPERIEURE A 8 '//
     &         '(TRONCATURE DU NOM)')
              WRITE(IMOD,'(A,4X,2A,2X,A,1X,A,1X,A)') 'GROUP_NO','NOM=',
     &                     NOMC(1:8),CHENTI,CHLIGE,CHLIGN
              WRITE(IMOD,'(12X,2A,10X,A,A2,A,A2,A,A4)')'AUTEUR=',AUT,
     &             'DATE=',CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
              WRITE(IMOD,'(A)') CHFOGN
            ELSE IF (INDIC.EQ.5.AND.ITYP.EQ.7) THEN
C
C    ---> ON TROUVE UNE MESH AREA DECRITE PAR SES NOEUDS
C
              ILONG=LXLGUT(NOMA)
              IF(ILONG.GT.8) CALL UTMESS('A','PRESUP',
     &        'GROUPE '//NOMA//' DE LONGUEUR SUPERIEURE A 8 '//
     &         '(TRONCATURE DU NOM)')
              WRITE(IMOD,'(A,4X,2A,2X,A,1X,A,1X,A)') 'GROUP_NO','NOM=',
     &                      NOMA(1:8),CHENTI,CHLIGE,CHLIGN
              WRITE(IMOD,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',AUT,
     &              'DATE=',CT(1),'/',CT(2),'/',CT(3)
              WRITE(IMOD,'(A)') CHFOGN
            ELSE IF (INDIC.EQ.6.AND.ITYP.EQ.7) THEN
C
C    ---> ON TROUVE UN MESH VOLUME DECRIT PAR SES NOEUDS
C
              ILONG=LXLGUT(NOMV)
              IF(ILONG.GT.8) CALL UTMESS('A','PRESUP',
     &        'GROUPE '//NOMV//' DE LONGUEUR SUPERIEURE A 8 '//
     &         '(TRONCATURE DU NOM)')
              WRITE(IMOD,'(A,4X,2A,2X,A,1X,A,1X,A)') 'GROUP_NO','NOM=',
     &                    NOMV(1:8),CHENTI,CHLIGE,CHLIGN
              WRITE(IMOD,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',AUT,
     &              'DATE=',CT(1),'/',CT(2),'/',CT(3)
              WRITE(IMOD,'(A)') CHFOGN
            ELSE IF (INDIC.EQ.3.AND.ITYP.EQ.8) THEN
C
C    ---> ON TROUVE UNE CURVE DECRITE PAR SES ELEMENTS
C
              ILONG=LXLGUT(NOMC)
              IF(ILONG.GT.8) CALL UTMESS('A','PRESUP',
     &        'GROUPE '//NOMC//' DE LONGUEUR SUPERIEURE A 8 '//
     &         '(TRONCATURE DU NOM)')
              WRITE(IMOD,'(A,4X,2A,2X,A,1X,A,1X,A)')'GROUP_MA','NOM=',
     &                     NOMC(1:8),CHENTI,CHLIGE,CHLIGN
              WRITE(IMOD,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',AUT,
     &             'DATE=',CT(1),'/',CT(2),'/',CT(3)
              WRITE(IMOD,'(A)') CHFOGM
            ELSE IF (INDIC.EQ.5.AND.ITYP.EQ.8) THEN
C
C    ---> ON TROUVE UNE MESH AREA DECRITE PAR SES ELEMENTS
C
              ILONG=LXLGUT(NOMA)
              IF(ILONG.GT.8) CALL UTMESS('A','PRESUP',
     &        'GROUPE '//NOMC//' DE LONGUEUR SUPERIEURE A 8 '//
     &         '(TRONCATURE DU NOM)')
              WRITE(IMOD,'(A,4X,2A,2X,A,1X,A,1X,A)')'GROUP_MA','NOM=',
     &                     NOMA(1:8),CHENTI,CHLIGE,CHLIGN
              WRITE(IMOD,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',AUT,
     &              'DATE:',CT(1),'/',CT(2),'/',CT(3)
              WRITE(IMOD,'(A)') CHFOGM
            ELSE
C
C    ---> ON TROUVE UN MESH VOLUME DECRIT PAR DES ELEMENTS
C
              ILONG=LXLGUT(NOMV)
              IF(ILONG.GT.8) CALL UTMESS('A','PRESUP',
     &        'GROUPE '//NOMV//' DE LONGUEUR SUPERIEURE A 8 '//
     &         '(TRONCATURE DU NOM)')
              WRITE(IMOD,'(A,4X,2A,2X,A,1X,A,1X,A)') 'GROUP_MA','NOM=',
     &                      NOMV(1:8),CHENTI,CHLIGE,CHLIGN
              WRITE(IMOD,'(12X,2A,10X,A,A2,A,A2,A,A2)') 'AUTEUR=',AUT,
     &              'DATE=',CT(1),'/',CT(2),'/',CT(3)
              WRITE(IMOD,'(A)') CHFOGM
            ENDIF
            IF(ITYP.EQ.7) THEN
              IF(IDIV.NE.0) THEN
                DO 20 I=1,IDIV
                  READ(IUNV,'(8I10)') (NUMENT(K),K=1,8)
                  DO 25 K=1,8
                    CALL CODNOP(CHNODE,PRFNOE,1,2)
                    CALL CODENT(NUMENT(K),'G',CHNODE(3:8))
                    NUOBJ(K)=CHNODE
  25              CONTINUE
                  WRITE(IMOD,'(8(2X,A))') (NUOBJ(J),J=1,8)
  20            CONTINUE
              ENDIF
C
              IF (IREST.NE.0) THEN
                READ (IUNV,'(8I10)') (NUMENT(I),I=1,8)
                DO 30 K=1,IREST
                  CALL CODNOP(CHNODE,PRFNOE,1,2)
                  CALL CODENT(NUMENT(K),'G',CHNODE(3:8))
                  NUOBJ(K)=CHNODE
  30            CONTINUE
                WRITE(IMOD,'(8(2X,A))') (NUOBJ(J),J=1,IREST)
              ENDIF
              WRITE(IMOD,'(A)') 'FINSF'
              WRITE(IMOD,'(A)') '%'
C
            ELSE
C
              IF(IDIV.NE.0) THEN
                DO 40 I=1,IDIV
                  READ(IUNV,'(8I10)') (NUMENT(K),K=1,8)
                  DO 45 K=1,8
                    CALL CODNOP(CHMAIL,PRFMAI,1,2)
                    CALL CODENT(NUMENT(K),'G',CHMAIL(3:8))
                    NUOBJ(K)=CHMAIL
  45              CONTINUE
                  WRITE(IMOD,'(8(2X,A))') (NUOBJ(J),J=1,8)
  40            CONTINUE
              ENDIF
C
              IF (IREST.NE.0) THEN
                READ (IUNV,'(8I10)') (NUMENT(I),I=1,8)
                DO 50 K=1,IREST
                  CALL CODNOP(CHMAIL,PRFMAI,1,2)
                  CALL CODENT(NUMENT(K),'G',CHMAIL(3:8))
                  NUOBJ(K)=CHMAIL
  50            CONTINUE
                WRITE(IMOD,'(8(2X,A))') (NUOBJ(J),J=1,IREST)
              ENDIF
              WRITE(IMOD,'(A)') 'FINSF'
              WRITE(IMOD,'(A)') '%'
            ENDIF
          ELSE
            WRITE(IMES,*) 'ON EST DANS 1 AUBERGE ESPAGNOLE'
            WRITE(IMES,*) 'JE N''Y COMPRENDS PLUS RIEN'
          ENDIF
          GO TO 1
 1000 CONTINUE
      WRITE(IMES,*) 'NOMBRE DE CURVES :',ICOMPC
      WRITE(IMES,*) 'NOMBRE DE M. AREAS:',ICOMPA
      WRITE(IMES,*) 'NOMBRE DE M. VOLUMES:',ICOMPV
C
      END
