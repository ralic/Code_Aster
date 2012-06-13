      SUBROUTINE FONNO3 (NOMA,TABLEV,NDIM,NA,NB,NOE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8         NOMA
      INTEGER             TABLEV(2),NDIM,NA,NB,NOE(4,4)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C      RECUP DES FACES CONNECTEES AU FOND 
C          POUR CHACUNE DES 2 MAILLES
C       ----------------------------------------------------
C    ENTREES
C       NOMA   : NOM DU MAILLAGE
C       TABLEV : VECTEUR CONTNANT LES NUMEROS DES DEUX MAILLES 
C                CONNECTEES AU NOEUD SOMMET COURANT ET AUX LEVRES
C       NDIM   : DIMENSION DU MODELE
C       NA     : NUMERO DU NOEUD SOMMET COURANT
C       NB     : NUMERO DU NOEUD SOMMET SUIVANT
C    SORTIE
C       NOE    : NOEUDS DES FACES CONTENANT NA et NB ET APPARTENANT AUX
C                MAILLES CONNECTEES AU NOEUD SOMMET COURANT 
C                ET AUX LEVRES
C
      INTEGER      IATYMA,IRET,IAMASE,ITYP
      INTEGER      I,J,JF,NUMERT(12,3),NBFT,NUMERO(6,4),NBF
      INTEGER      COMPTE,IMA,NN,INP,COMPT(2),COMPF
      INTEGER      NUMERF(4,2) 
      CHARACTER*8  K8B,TYPE

C     -----------------------------------------------------------------
C           
      CALL JEMARQ() 

C
C     RECUPERATION DE L'ADRESSE DES TYPFON DE MAILLES
      CALL JEVEUO ( NOMA//'.TYPMAIL','L',IATYMA)

      COMPTE=0
      DO 10 I=1,4
        DO 11 J=1,4
          NOE(I,J)=0
  11    CONTINUE
  10  CONTINUE
      DO 130 IMA=1,2
        ITYP = IATYMA-1+TABLEV(IMA)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
        CALL DISMOI('F','NBNO_TYPMAIL',TYPE,'TYPE_MAILLE',
     &   NN,K8B,IRET)
        CALL JEVEUO(JEXNUM( NOMA//'.CONNEX',
     &      TABLEV(IMA)),'L',IAMASE)
C
C       EN 3D
C
        IF (NDIM.EQ.3) THEN
          CALL CONFAC(TYPE,NUMERT,NBFT,NUMERO,NBF)
C         RECHERCHE DES INDICES LOCAUX
          I = 1
          DO 131 INP=1,NN  
            IF ((ZI(IAMASE-1+INP).EQ.NA).OR.
     &                (ZI(IAMASE-1+INP).EQ.NB))   THEN
              COMPT(I) = INP
              I = I+1
            ENDIF
 131      CONTINUE
C         RECHERCHE DE LA FACE
          DO 132 INP=1,NBF
            COMPF = 0
            DO 133 I =1,4
              IF((NUMERO(INP,I).EQ.COMPT(1))
     &         .OR.(NUMERO(INP,I).EQ.COMPT(2))) THEN
                           COMPF = COMPF + 1
              ENDIF
 133        CONTINUE
            IF (COMPF.EQ.2) THEN
C             RECUPERATION DES NOEUDS SOMMETS DE LA FACE INP
              COMPTE = COMPTE + 1
              DO 134 JF=1,4
                IF (NUMERO(INP,JF).NE.0) THEN
                   NOE(COMPTE,JF) = ZI(IAMASE-1+NUMERO(INP,JF))
                ELSE
                   NOE(COMPTE,JF) = 0
                ENDIF
 134          CONTINUE
            ENDIF
 132      CONTINUE

C
C       EN 2D
C
        ELSEIF (NDIM.EQ.2) THEN
          IF (TYPE(1:4).EQ.'QUAD') THEN
            NUMERF(1,1)=1
            NUMERF(1,2)=2
            NUMERF(2,1)=2
            NUMERF(2,2)=3
            NUMERF(3,1)=3
            NUMERF(3,2)=4
            NUMERF(4,1)=4
            NUMERF(4,2)=1
          ELSE
            NUMERF(1,1)=1
            NUMERF(1,2)=2
            NUMERF(2,1)=2
            NUMERF(2,2)=3
            NUMERF(3,1)=3
            NUMERF(3,2)=1
            NUMERF(4,1)=0
            NUMERF(4,2)=0
          ENDIF
C         RECHERCHE DES INDICES LOCAUX
          I = 1
          DO 135 INP=1,NN
            IF (ZI(IAMASE-1+INP).EQ.NA) THEN
              COMPT(I) = INP
              I = I+1
            ENDIF
 135      CONTINUE
C         RECHERCHE DE LA FACE OU ARETE
          DO 136 INP=1,4
            COMPF = 0
            DO 137 I =1,2
              IF(NUMERF(INP,I).EQ.COMPT(1)) THEN
               COMPF = COMPF + 1
              ENDIF
 137        CONTINUE
            IF (COMPF.EQ.1) THEN
C             RECUPERATION DES NOEUDS SOMMETS DE LA FACE INP
              COMPTE = COMPTE + 1
              DO 138 JF=1,2
                NOE(COMPTE,JF) = ZI(IAMASE-1+NUMERF(INP,JF))
 138          CONTINUE
            ENDIF
 136      CONTINUE
        ENDIF
 130  CONTINUE
 
      CALL JEDEMA()
      END
