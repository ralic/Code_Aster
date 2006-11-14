      SUBROUTINE GCOU2D ( BASE, RESU, NOMA, NOMNO, NOEUD, COOR, RINF,
     &                    RSUP, MODULE, DIR )
      IMPLICIT   NONE
      REAL*8              RINF, RSUP, MODULE, DIR(3), COOR(*)
      CHARACTER*1         BASE
      CHARACTER*8               NOMA,        NOEUD
      CHARACTER*24        RESU,       NOMNO
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/11/2006   AUTEUR SALMONA L.SALMONA 
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
C FONCTION REALISEE:
C
C     OPTION COURONNE EN 2D
C     ---------------------
C
C 1.  POUR LE NOEUD DU FOND DE FISSURE ON RECUPERE
C     LE TRIPLET ( MODULE(THETA), RINF, RSUP )
C
C 2.  ENSUITE ON CALCUL THETA SUR TOUT LES NOEUDS DU MAILLAGE
C
C     ------------------------------------------------------------------
C ENTREE:
C        RESU   : NOM DU CONCEPT DE TYPE CHAM_NO
C        NOMA   : NOM DU MAILLAGE
C        NOMNO  : NOM DE L'OBJET CONTENANT LES NOEUDS DU MAILLAGE
C        NOEUD  : NOM DU NOEUD DU FOND DE FISSURE
C        COOR   : COORDONNEES DES NOEUDS
C        RINF   : RAYON INFERIEURE DE LA COURONNE
C        RSUP   : RAYON SUPERIEURE DE LA COURONNE
C        MODULE : MODULE(THETA)
C        DIR    : DIRECTION DU CHAMPS THETA
C
C SORTIE:
C        DIR    : DIRECTION DU CHAMPS THETA NORMALISEE
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
      CHARACTER*32       JEXNUM , JEXNOM
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER       ITHETA, I, IREFE, IDESC, NUM, IERD, NBEL, NUMA
      INTEGER       NEC,IBID
      REAL*8        XM, YM, XI, YI, EPS, D, NORME, ALPHA, VALX, VALY
      CHARACTER*8   K8B
      CHARACTER*24  CHAMNO
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      EPS = 1.D-06
C
C     --- LA DIRECTION DE THETA EST DONNEE, ON LA NORME ---
C
      NORME = 0.D0
      DO 1 I = 1,2
         NORME =  NORME + DIR(I)*DIR(I)
1     CONTINUE
      NORME = SQRT(NORME)
      DIR(1) = DIR(1)/NORME
      DIR(2) = DIR(2)/NORME
C
C     --- RECUPERATION DU NUMERO DE NOEUD DU FOND DE FISSURE ---
C
      CALL JENONU ( JEXNOM(NOMNO,NOEUD), NUM )
C
C  .DESC
      CHAMNO = RESU(1:19)//'.DESC'
      CALL DISMOI('F','NB_EC','DEPL_R','GRANDEUR',NEC,K8B,IBID)
      CALL WKVECT ( CHAMNO, BASE//' V I', 2+NEC, IDESC )
C
      CALL JEECRA ( CHAMNO, 'DOCU', 0, 'CHNO' )
      CALL JENONU ( JEXNOM('&CATA.GD.NOMGD','DEPL_R'), NUMA )
      ZI(IDESC+1-1) = NUMA
      ZI(IDESC+2-1) = -2
      ZI(IDESC+3-1) = 6
C
C  .REFE
      CHAMNO = RESU(1:19)//'.REFE'
      CALL WKVECT ( CHAMNO, BASE//' V K24', 2, IREFE )
      ZK24(IREFE+1-1) = NOMA//'                '
C
C  .VALE
      CHAMNO = RESU(1:19)//'.VALE'
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBEL,K8B,IERD)
      CALL WKVECT ( CHAMNO, BASE//' V R', 2*NBEL, ITHETA )
C
C     --- CALCUL DE THETA ---
C
C     ---NOEUD DU FOND DE FISSURE
C
      ZR(ITHETA + (NUM-1)*2 + 1 - 1) = MODULE*DIR(1)
      ZR(ITHETA + (NUM-1)*2 + 2 - 1) = MODULE*DIR(2)
C
C BOUCLE SUR LES AUTRES NOEUDS COURANTS DU MAILLAGE
C
      DO 500 I=1,NBEL
         IF ( I .NE. NUM ) THEN
            XM = COOR((I-1)*3+1)
            YM = COOR((I-1)*3+2)
            XI = COOR((NUM-1)*3+1)
            YI = COOR((NUM-1)*3+2)
            D  = SQRT((XI-XM)*(XI-XM)+(YI-YM)*(YI-YM))
            VALX = MODULE*DIR(1)
            VALY = MODULE*DIR(2)
            ALPHA = (D-RINF)/(RSUP-RINF)
            IF ((ABS(ALPHA).LE.EPS).OR.(ALPHA.LT.0)) THEN
               ZR(ITHETA+(I-1)*2+1-1) = VALX
               ZR(ITHETA+(I-1)*2+2-1) = VALY
            ELSE IF((ABS(ALPHA-1).LE.EPS).OR.((ALPHA-1).GT.0)) THEN
               ZR(ITHETA+(I-1)*2+1-1) = 0.D0
               ZR(ITHETA+(I-1)*2+2-1) = 0.D0
            ELSE
               ZR(ITHETA+(I-1)*2+1-1) = (1-ALPHA)*VALX
               ZR(ITHETA+(I-1)*2+2-1) = (1-ALPHA)*VALY
            ENDIF
         ENDIF
500   CONTINUE
C
      CALL JEDEMA()
      END
