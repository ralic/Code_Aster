      SUBROUTINE XRELL1(TABNOZ,NDIM,NAR,TABCO,PICKNO,NBPINO,FISS)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/05/2006   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT

      IMPLICIT NONE
      INTEGER        NDIM,NAR,TABNOZ(3,NAR),NBPINO,PICKNO(NBPINO)
      CHARACTER*8    FISS
      REAL*8         TABCO(NDIM,NAR)
C
C   CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT
C                    (VOIR BOOK VI 15/07/05) :
C           - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
C
C
C   IN
C       TABNOZ   : TABLEAU DES NOEUDS EXTREMIT� ET NOEUD MILIEU
C       NAR      : NOMBRE D'ARETES COUP�S
C       TABCO    : TABLEAU DES COORDONN�S DE NOEUDS MILIEU
C       PICKNO   : NUM�OS DES NOEUDS S�ECTIONN�
C       NBPINO   : NOMBRE DE NOEUDS S�ECTIONN�
C
C   OUT
C       FISS     : SD FISS_XFEM AVEC LISTE DES RELATIONS ENTRE LAGRANGE

      INTEGER      I,J,IN,DIMEQ,IA,EXT,LIBRE,K,EQ(100),TABNO(NAR,3),IE
      INTEGER      LISEQT(NAR,2),NRELEQ,JLIS1,PICKED,IEXT,NRELRL
      INTEGER      LISRLT(NAR,3),COEFI(2),JLIS2,JLIS3
      REAL*8       A(3),R8MAEM,DISMIN,DIST,COEFR(2),LISCOT(NAR,3),PADIST

C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------

      CALL JEMARQ()

      DO 10 I=1,NAR
        DO 11 J=1,3
          TABNO(I,J)=TABNOZ(J,I)
 11     CONTINUE
 10   CONTINUE


C     ------------------------------------------------------------------
C     CREATION DU TABLEAU TEMPORAIRE DES RELATION D'EGALITE : LISEQT
C     ------------------------------------------------------------------
      NRELEQ=0
      DO 100 I=1,NBPINO
        IN=PICKNO(I)
        DIMEQ=0
        DO 110 IA=1,NAR
          DO 111 J=1,2
C           ON CHERCHE LES ARETES EMANANTES
            IF (TABNO(IA,J).EQ.IN) THEN
              EXT=TABNO(IA,3-J)
C             ON REGARDE SI L'AUTRE EXTREMITE EST LIBRE
              LIBRE=1
              DO 112 K=1,NBPINO
                IF (EXT.EQ.PICKNO(K)) LIBRE=0
 112          CONTINUE
              IF (LIBRE.EQ.1) THEN
                DIMEQ=DIMEQ+1
                EQ(DIMEQ)=TABNO(IA,3)
              ENDIF
            ENDIF
 111      CONTINUE
 110    CONTINUE
        CALL ASSERT(DIMEQ-1.GE.0)
        DO 120 IE=1,DIMEQ-1
           NRELEQ=NRELEQ+1
           LISEQT(NRELEQ,1)=EQ(IE)
           LISEQT(NRELEQ,2)=EQ(IE+1)
 120    CONTINUE
 100  CONTINUE

C      WRITE(6,*)'NRELEQ ',NRELEQ

C     STOCKAGE DE LISEQT
      IF (NRELEQ.GT.0) THEN
        CALL WKVECT(FISS//'.CONTACT.LISEQ','G V I',NRELEQ*2,JLIS1)
        DO 130 IE=1,NRELEQ
          ZI(JLIS1-1+2*(IE-1)+1)=LISEQT(IE,1)
          ZI(JLIS1-1+2*(IE-1)+2)=LISEQT(IE,2)
C          WRITE(6,*)'LISEQ ',LISEQT(IE,1),LISEQT(IE,2)
 130    CONTINUE
      ENDIF

C     ------------------------------------------------------------------
C     CREATION DU TABLEAU TEMPORAIRE DES RELATION LINEAIRES      :LISRLT
C     ET DU TABLEAU TEMPORAIRE DES COEFFICIENTS DE CES RELATIONS :LISCOT
C     ------------------------------------------------------------------
      NRELRL=0
      DO 200 IA=1,NAR
        PICKED=0
        DO 210 J=1,2
          DO 211 K=1,NBPINO
            IF (TABNO(IA,J).EQ.PICKNO(K))   PICKED=PICKED+1
 211      CONTINUE
 210    CONTINUE

        IF (PICKED.EQ.2) THEN
          DO 220 IEXT=1,2
C           ON PARCOURT LES ARETES EMANANTES LIBRE
            DISMIN=R8MAEM()
            DO 221 I=1,NAR
              DO 222 J=1,2
                IF (TABNO(I,J).EQ.TABNO(IA,IEXT)) THEN
                  EXT=TABNO(I,3-J)
C                 ON VERIFIE SI L'AUTRE EXTREMITE EST LIBRE
                  LIBRE=1
                  DO 223 K=1,NBPINO
                     IF (EXT.EQ.PICKNO(K)) LIBRE=0
 223              CONTINUE
                  IF (LIBRE.EQ.1) THEN
                  
                  
C                    CALCUL DISTANCE ENTRE LAG �LIER ET LE LAG EXT
                     DIST=PADIST(NDIM,TABCO(1,I),TABCO(1,IA))
                     IF (DIST.LT.DISMIN) THEN
                        DISMIN=DIST
                        COEFI(IEXT)=TABNO(I,3)
                        COEFR(IEXT)=DIST
                  
                  
                     ENDIF
                  ENDIF
                ENDIF
 222          CONTINUE
 221        CONTINUE
 220      CONTINUE
          NRELRL=NRELRL+1
          LISRLT(NRELRL,1)=TABNO(IA,3)
          LISRLT(NRELRL,2)=COEFI(1)
          LISRLT(NRELRL,3)=COEFI(2)
          LISCOT(NRELRL,1)=1.D0
          LISCOT(NRELRL,2)=-COEFR(2)/(COEFR(1)+COEFR(2))
          LISCOT(NRELRL,3)=-COEFR(1)/(COEFR(1)+COEFR(2))
        ENDIF

 200  CONTINUE

C      WRITE(6,*)'NRELRL ',NRELRL

C     STOCKAGE DE LISRLT ET LISCOT
      IF (NRELRL.GT.0) THEN
        CALL WKVECT(FISS//'.CONTACT.LISRL','G V I',NRELRL*3,JLIS2)
        CALL WKVECT(FISS//'.CONTACT.LISCO','G V R',NRELRL*3,JLIS3)
        DO 230 IE=1,NRELRL
          DO 231 J=1,3
            ZI(JLIS2-1+3*(IE-1)+J)=LISRLT(IE,J)
            ZR(JLIS3-1+3*(IE-1)+J)=LISCOT(IE,J)
C            WRITE(6,*)'LISRL ',LISRLT(IE,J),LISCOT(IE,J)
 231      CONTINUE
 230    CONTINUE
      ENDIF

      CALL JEDEMA()
      END
