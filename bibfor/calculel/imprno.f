      SUBROUTINE IMPRNO ( PRCHNO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 27/01/98   AUTEUR VABHHTS J.PELLET 
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
C----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C     HISTOR : VARIABLE SUR 6 CARACTERES + PRCHNO EN K*(*)
C----------------------------------------------------------------------
      CHARACTER*(*) PRCHNO
C----------------------------------------------------------------------
C OBJET :
C      IMPRESSION DES OBJETS JEVEUX COMPOSANT UNE STRUCTURE DE DONNEES
C      DE TYPE PROF_CHNO SUR LE FICHIER RESULTAT
C----------------------------------------------------------------------
C IN  PRCHNO  K*19  : NOM D'UN PROF_CHNO
C----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      CHARACTER*19 PROFNO
      CHARACTER*1 K1BID
C
C   STRUCTURE DE DONNEES PROF_CHNO
C
        PROFNO = PRCHNO
        CALL JEIMPO('RESULTAT',PROFNO//'.LILI',' ',
     & 'REPERTOIRE DES NOMS DE LIGREL')
        CALL JEIMPA('RESULTAT',PROFNO//'.LILI',
     & 'REPERTOIRE DES NOMS DE LIGREL')
C
C
        CALL JEIMPO('RESULTAT',PROFNO//'.DEEQ',' ',
     & 'NUMERO DU NOEUD ET DU DDL')
        CALL JEIMPA('RESULTAT',PROFNO//'.DEEQ',
     & 'NUMERO DU NOEUD ET DU DDL')
C
        CALL JELIRA(PROFNO//'.PRNO','NMAXOC',NOBJ,K1BID)
C
       DO 1 I=1,NOBJ
        CALL JEIMPO('RESULTAT',JEXNUM(PROFNO//'.PRNO',I),' ',
     & 'PROFIL NOEUD')
        CALL JEIMPA('RESULTAT',JEXNUM(PROFNO//'.PRNO',I),
     & 'PROFIL NOEUD')
1      CONTINUE
C
      END
