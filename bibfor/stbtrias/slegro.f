      SUBROUTINE SLEGRO(DATSET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 11/05/2004   AUTEUR NICOLAS O.NICOLAS 
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
C     !  FONCTION: LECTURE SUR LE FICHIER UNIVERSEL ISSU DE SUPERTAB !
C     !            I-DEAS 4.0, 6.0 DOU 7.0  DES GROUPES DE NOEUDS    !
C     !            ET DE MAILLES PUIS ECRITURE SUR LE FICHIER MODELE !
C     !                                                              !
C     ================================================================
C     !                                                              !
C     !  ROUTINES APPELES : CODENT                                   !
C     !                          : IUNIFI (FONCTION)                 !
C     !                          : JJMMAA                            !
C     !                          : CODNOP                            !
C     !                                                              !
C     !  ROUTINE APPELANTE : PRESUP                                  !
C     !                                                              !
C     ================================================================
C
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER      ZI
      REAL*8       ZR
      COMPLEX*16   ZC
      LOGICAL      ZL,EXISDG
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM,JEXNOM,JEXATR
      CHARACTER*80 ZK80
C
C
C  --> DECLARATION VARIABLES LOCALES
C
      CHARACTER*4  CT(3)
      CHARACTER*2  PRFNOE,PRFMAI
      CHARACTER*80 CBUF
      CHARACTER*8  CHNODE,CHMAIL,CHNOD8(8),CHMAI8(8),CHGROU,NGRO8
      CHARACTER*8  TOTO
      CHARACTER*20 NOMGRO
      CHARACTER*12 CHENTI,CHNOMI,CHNOMA,AUT
      CHARACTER*13 CHLIGE,CHLIGN
      CHARACTER*80 CHFOGN,CVAL
      CHARACTER*80 CHFOGM
      INTEGER ENTCOD(4),NUMENT(4),NUMGRO,NBENTI,NBLIGN
      INTEGER NBNODE,NBMAIL,NBNOD8,NBMAI8,IGRN,IGRM,IUNV,IMOD
      INTEGER NBREST
      INTEGER NBLIT,NBLIE,NBLIF
      INTEGER IMI,IMA
      INTEGER NBMODU,NBTEST,DATSET
      INTEGER NBRLIG
      REAL*8  RVAL
      LOGICAL LWRIT
C
C  --> DECLARATION INDICES DE BOUCLES
C
      INTEGER I,J
C
C  ------------ FIN DECLARATION -------------
C
C  -->N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
      CALL JEMARQ()
      IUNV = IUNIFI('IDEAS')
      IMOD = IUNIFI('FICHIER-MODELE')
C
      PRFNOE='NO'
      PRFMAI='MA'
      CHFOGN='%FORMAT=(1*NOM_DE_NOEUD)'
      CHFOGM='%FORMAT=(1*NOM_DE_MAILLE)'
      CHNODE = '        '
      CHMAIL = '        '
      CHENTI='NBOBJ=      '
      CHLIGN='NBLIGT=      '
      CHLIGE='NBLIGE=      '
      CHNOMI='NUMIN=      '
      CHNOMA='NUMAX=      '
C
    1 CONTINUE
      LWRIT = .TRUE.
      READ (IUNV,'(A)') CBUF
      READ (CBUF,'(I6)') IND
      IF (IND.NE.-1) THEN
C
C  --> LECTURE SUR LE FICHIER UNIVERSEL DES GROUPES DE NOEUDS
C      ET/OU DE MAILLES
C
         IF (DATSET.EQ.752) THEN
           READ (CBUF,'(I10,40X,I10)') NUMGRO,NBENTI
           READ (IUNV,'(A)')    NOMGRO
           NBRLIG = 4
         ELSE IF (DATSET.EQ.2417) THEN
           READ (CBUF,'(I10,50X,I10)') NUMGRO,NBENTI
           READ (IUNV,'(A)')    NOMGRO
           NBRLIG = 4
         ELSE IF (DATSET.EQ.2429.OR.DATSET.EQ.2430.
     +      OR.DATSET.EQ.2432) THEN
           READ (CBUF,'(I10,60X,I10)') NUMGRO,NBENTI
           READ (IUNV,'(A)')    NOMGRO
           NBRLIG = 4
         ELSE IF ((DATSET.EQ.2435).OR.(DATSET.EQ.2467).
     &    OR.(DATSET.EQ.2452)) THEN
           READ (CBUF,'(I10,60X,I10)') NUMGRO,NBENTI
           READ (IUNV,'(A)')    NOMGRO
           NBRLIG = 2
         ENDIF
         ICOL = 1
         ILONG = LXLGUT (NOMGRO)
         IF(ILONG.GT.8) CALL UTMESS('A','PRESUP','GROUPE '//NOMGRO
     &      //' DE LONGUEUR SUPERIEURE A 8 (TRONCATURE DU NOM)')
         NGRO8 = NOMGRO
         CALL LXSCAN (NGRO8,ICOL,ICLASS,IVAL,RVAL,CVAL)
         IF (ICLASS.NE.3) THEN
            CALL UTMESS('A','PRESUP','LE NOM DU GROUPE EST INVALIDE'
     +         //': '//NGRO8//' : NON TRAITE')
            IF(NBENTI.EQ.0) THEN
              GO TO 1
            ELSE
              NBLIGN = INT(NBENTI/NBRLIG)
              DO 145 I=1,NBLIGN
                 READ(IUNV,'(I3)')
145           CONTINUE
              IF (NBENTI.GT.(NBRLIG*NBLIGN)) READ(IUNV,'(I3)')
              GO TO 1
            ENDIF
         ELSE IF (ILONG.NE.IVAL) THEN
            CALL UTMESS('A','PRESUP','LE NOM DU GROUPE '//
     +         NOMGRO//' EST TRONQUE '//': '//NGRO8(1:IVAL))
            TOTO = NGRO8(1:IVAL)
            NGRO8 = ' '
            NGRO8 = TOTO
         ENDIF
         IF(NGRO8(1:5).EQ.'COUL_') THEN
            CALL UTMESS('A','PRESUP','LE NOM DU GROUPE NE PEUT '
     +        //'COMMENCER PAR COUL_ : NON TRAITE')
            LWRIT = .FALSE.
         ENDIF
         IF (NBENTI.EQ.0) GO TO 1
         CALL WKVECT('&&PRESUP.GROU.NOEUD','V V K8',NBENTI,JGRN)
         CALL WKVECT('&&PRESUP.GROU.MAILLE','V V K8',NBENTI,JGRM)
C
         NBNODE=0
         NBMAIL=0
         NBTEST=0
C
         NBLIGN = INT(NBENTI/NBRLIG)
         NBMODU = MOD(NBENTI,NBRLIG)
C
         IF (NBMODU.NE.0) THEN
           NBTEST=1
         ENDIF
C
         DO 2 I = 1,NBLIGN
C
            IF(DATSET.EQ.752.OR.DATSET.EQ.2417.OR.DATSET.
     &        EQ.2429.OR.DATSET.EQ.2430.OR.DATSET.EQ.2432) THEN
              READ (IUNV,'(8I10)')
     &         (ENTCOD(J),NUMENT(J),J=1,NBRLIG)
            ELSEIF((DATSET.EQ.2435).OR.(DATSET.EQ.2467).
     &    OR.(DATSET.EQ.2452)) THEN
              READ (IUNV,'(2(I10,I10,20X))')
     &         (ENTCOD(J),NUMENT(J),J=1,NBRLIG)
            ENDIF
C
            DO 3 J = 1,NBRLIG
               IF (ENTCOD(J).EQ.7) THEN
                  CALL CODNOP(CHNODE,PRFNOE,1,2)
                  CALL CODENT(NUMENT(J),'G',CHNODE(3:8))
C --> RECHERCHE DU N MIN ET N MAX DANS UN GROUPE DE NOEUDS
C
                  IF (NBNODE.EQ.0) THEN
                    IMI=NUMENT(J)
                  ELSE
                    IMA=MAX(NUMENT(J),IMI)
                  ENDIF
C
                  NBNODE= NBNODE+ 1
                  ZK8(JGRN-1+NBNODE) = CHNODE
               ELSE IF (ENTCOD(J).EQ.8) THEN
                  CALL CODNOP(CHMAIL,PRFMAI,1,2)
                  CALL CODENT(NUMENT(J),'G',CHMAIL(3:8))
C --> RECHERCHE DU N MIN ET N MAX DANS UN GROUPE DE MAILLES
C
                  IF (NBMAIL.EQ.0) THEN
                    IMI=NUMENT(J)
                  ELSE
                    IMA=MAX(NUMENT(J),IMI)
                  ENDIF
C
                  NBMAIL= NBMAIL+ 1
                  ZK8(JGRM-1+NBMAIL) = CHMAIL
               END IF
    3       CONTINUE
C
    2    CONTINUE
C
         IF (NBENTI.GT. (NBRLIG*NBLIGN)) THEN
            IF(DATSET.EQ.752.OR.DATSET.EQ.2417.OR.DATSET.
     &        EQ.2429.OR.DATSET.EQ.2430.OR.DATSET.EQ.2432)  THEN
              READ (IUNV,'(8I10)')
     &         (ENTCOD(J),NUMENT(J),J=1,NBRLIG)
            ELSEIF ((DATSET.EQ.2435).OR.(DATSET.EQ.2467).
     &    OR.(DATSET.EQ.2452)) THEN
              READ (IUNV,'(2(I10,I10,20X))')
     &         (ENTCOD(J),NUMENT(J),J=1,NBRLIG)
            ENDIF
            DO 4 J = 1, (NBENTI-NBRLIG*NBLIGN)
               IF (ENTCOD(J).EQ.7) THEN
C
C --> ECRITURE DES NOEUDS (APPARTENANT A UN GROUPE) SUR
C     LE FICHIER BUFFER IGRN
C
                  CALL CODNOP(CHNODE,PRFNOE,1,2)
                  CALL CODENT(NUMENT(J),'G',CHNODE(3:8))
                  IF (NBLIGN.EQ.0.AND.J.EQ.1) THEN
                    IMI=NUMENT(J)
                  ENDIF
                  IMA=MAX(NUMENT(J),IMI)
                  NBNODE= NBNODE+ 1
                  ZK8(JGRN-1+NBNODE) = CHNODE
               ELSE IF (ENTCOD(J).EQ.8) THEN
C
C --> ECRITURE DES MAILLES (APPARTENANT A UN GROUPE) SUR
C     LE FICHIER BUFFER IGRM
C
                  CALL CODNOP(CHMAIL,PRFMAI,1,2)
                  CALL CODENT(NUMENT(J),'G',CHMAIL(3:8))
                  IF (NBLIGN.EQ.0.AND.J.EQ.1) THEN
                    IMI=NUMENT(J)
                  ENDIF
                  IMA=MAX(NUMENT(J),IMI)
                  NBMAIL= NBMAIL+ 1
                  ZK8(JGRM-1+NBMAIL) = CHMAIL
               END IF
    4       CONTINUE
         END IF
C
C --> ECRITURE SUR LE FICHIER NEUTRE DES GROUPES DE NOEUDS
C
         IF (NBNODE.NE.0) THEN
            CHGROU(1:4)='GRNO'
            CALL CODENT(NUMGRO,'G',CHGROU(5:8))
C
            NBLIE=3
            NBLIF=1
            NBNOD8=NBNODE/8
            NBLIT=NBNOD8+NBLIE+NBLIF+NBTEST+1
C
            CALL CODENT(NBLIT,'G',CHLIGN(8:13))
            CALL CODENT(NBNODE,'G',CHENTI(7:12))
            CALL CODENT(NBLIE,'G',CHLIGE(8:13))
            CALL CODENT(IMI,'G',CHNOMI(7:12))
            CALL CODENT(IMA,'G',CHNOMA(7:12))
C
C
C   --> ECRITURE DE LA DATE (IBM & CRAY)
            CALL JJMMAA(CT,AUT)
C
            IF(NOMGRO(1:9).EQ.'PERMANENT') THEN
              WRITE(IMOD,'(A,4X,2A,1X,A,1X,A,1X,A)')'GROUP_NO','NOM=',
     &                 CHGROU,CHENTI,CHLIGE,CHLIGN
            ELSE
              WRITE(IMOD,'(A,4X,2A,2X,A,1X,A,1X,A)')'GROUP_NO','NOM=',
     &                 NGRO8,CHENTI,CHLIGE,CHLIGN
            ENDIF
C
            WRITE(IMOD,'(12X,A,14X,A)') CHNOMI,CHNOMA
            WRITE(IMOD,'(12X,2A,7X,A,A2,A,A2,A,A4)') 'AUTEUR=',AUT,
     &            'DATE=',CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
            WRITE(IMOD,'(A)') CHFOGN
C
C --> ECRITURE DES NOEUDS
C
            WRITE (IMOD,'(8(2X,A))') (ZK8(JGRN-1+J),J=1,NBNODE)
C
C --> FIN ECRITURE DES NOEUDS
C
            WRITE (IMOD,'(A)') 'FINSF'
            WRITE (IMOD,'(A)') '%'
         END IF
C
C --> ECRITURE SUR LE FICHIER NEUTRE DES GROUPES DE MAILLES
C
         IF (NBMAIL.NE.0) THEN
            CHGROU(1:4)='GRMA'
            CALL CODENT(NUMGRO,'G',CHGROU(5:8))
C
            NBLIE=3
            NBLIF=1
            NBMAI8=NBMAIL/8
            NBLIT=NBMAI8+NBLIE+NBLIF+NBTEST+1
C
            CALL CODENT(NBMAIL,'G',CHENTI(7:12))
            CALL CODENT(NBLIT,'G',CHLIGN(8:13))
            CALL CODENT(NBLIE,'G',CHLIGE(8:13))
            CALL CODENT(IMI,'G',CHNOMI(7:12))
            CALL CODENT(IMA,'G',CHNOMA(7:12))
C
C   --> ECRITURE DE LA DATE (IBM & CRAY)
            CALL JJMMAA(CT,AUT)
C
            IF(NOMGRO(1:9).EQ.'PERMANENT'.AND.LWRIT) THEN
               WRITE(IMOD,'(A,4X,2A,1X,A,1X,A,1X,A)')'GROUP_MA','NOM=',
     &               CHGROU,CHENTI,CHLIGE,CHLIGN
            ELSE IF (LWRIT) THEN
               WRITE(IMOD,'(A,4X,2A,2X,A,1X,A,1X,A)')'GROUP_MA','NOM=',
     &               NGRO8,CHENTI,CHLIGE,CHLIGN
            ENDIF
            IF (LWRIT) WRITE(IMOD,'(12X,A,14X,A)') CHNOMI,CHNOMA
            IF (LWRIT) WRITE(IMOD,'(12X,2A,7X,A,A2,A,A2,A,A4)')
     &        'AUTEUR=',AUT,'DATE=',CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
            IF (LWRIT) WRITE(IMOD,'(A)') CHFOGM
C
C --> ECRITURE DES MAILLES
C
            IF (LWRIT) WRITE (IMOD,'(8(2X,A))')
     &           (ZK8(JGRM-1+J),J=1,NBMAIL)
C
C --> FIN ECRITURE DES MAILLES
C
            IF (LWRIT) WRITE (IMOD,'(A)') 'FINSF'
            IF (LWRIT) WRITE (IMOD,'(A)') '%'
         END IF
         CALL JEDETR('&&PRESUP.GROU.NOEUD')
         CALL JEDETR('&&PRESUP.GROU.MAILLE')
         GO TO 1
      END IF
      CALL JEDEMA()
      END
