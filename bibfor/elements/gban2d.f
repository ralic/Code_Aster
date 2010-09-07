      SUBROUTINE GBAN2D ( RESU, NOMA, COOR, RINF, RSUP, MODULE )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/09/2010   AUTEUR DESOZA T.DESOZA 
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
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C     OPTION BANDE EN 2D
C     ------------------
C
C 1.  RECUPERATION DU TRIPLET
C     ( MODULE(THETA), RINF, RSUP )
C
C 2.  ENSUITE ON CALCUL THETA SUR TOUT LES NOEUDS DU MAILLAGE
C
C     ------------------------------------------------------------------
C ENTREE:
C        RESU   : NOM DU CONCEPT DE TYPE CHAM_NO
C        NOMA   : NOM DU MAILLAGE
C        COOR   : COORDONNEES DES NOEUDS
C        RINF   : RAYON INFERIEURE DE LA BANDE
C        RSUP   : RAYON SUPERIEURE DE LA BANDE
C        MODULE : MODULE(THETA)
C
C SORTIE:
C        RESU   : CHAMP THETA
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       JEVEUX: JEMARQ, WKVECT, JEECRA, JENONU, JEDEMA.
C       ELEMENT_FINI: DISMOI.
C
C     FONCTIONS INTRINSEQUES:
C       AUCUNE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C      31/08/00 (OB): TOILETTAGE FORTRAN. CALCUL D'UN CHAMP THETA PLUS
C                     REGULIER (DE CLASSE C2 SUR )RINF,RE( ).
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT   NONE
C
C DECLARATION PARAMETRES D'APPELS
      REAL*8              RINF, RSUP, MODULE, COOR(*)
      CHARACTER*8               NOMA
      CHARACTER*24        RESU

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------

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
      CHARACTER*32       JEXNOM
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES
      INTEGER       ITHETA,I,IREFE,IDESC,IERD,NBNO,NUMA,NEC,IBID
      REAL*8        XM, EPS, ALPHA
      REAL*8        RMINI, RMAXI, RMEAN, RDIFF
      CHARACTER*8   K8B
      CHARACTER*24  CHAMNO
C
      CALL JEMARQ()
C
C --- RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE :
C     ============================================
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IERD)
C
C --- CREATION DU CHAMNO DE DEPL_R THETA :
C     ==================================
C ---  .DESC DU CHAMNO :
C       --------------
      CHAMNO = RESU(1:19)//'.DESC'
      CALL DISMOI('F','NB_EC','DEPL_R','GRANDEUR',NEC,K8B,IBID)
      CALL WKVECT ( CHAMNO, 'G V I',2+NEC, IDESC )
C
      CALL JEECRA ( CHAMNO, 'DOCU', 0, 'CHNO' )
      CALL JENONU ( JEXNOM('&CATA.GD.NOMGD','DEPL_R'), NUMA )
      ZI(IDESC+1-1) = NUMA
      ZI(IDESC+2-1) = -2
      ZI(IDESC+3-1) = 6
C
C ---  .REFE DU CHAMNO :
C       --------------
      CHAMNO = RESU(1:19)//'.REFE'
      CALL WKVECT ( CHAMNO, 'G V K24', 4, IREFE )
      ZK24(IREFE+1-1) = NOMA//'                '
C
C ---  .VALE DU CHAMNO :
C       --------------
      CHAMNO = RESU(1:19)//'.VALE'
      CALL WKVECT ( CHAMNO, 'G V R', 2*NBNO, ITHETA )
C
C --- AFFECTATION DU CHAMNO DE DEPL_R THETA DE LA MANIERE SUIVANTE :
C --- THETA(I,1) = MODULE                      SI R =< RINF
C --- THETA(I,1) = MODULE*ALPHA                SI RINF <= R =< RSUP
C --- THETA(I,1) = 0.                          SI R > RSUP
C     =======================================================
C
C     LE COEFFICIENT ALPHA EST CHOISI DE TELLE SORTE QUE ALPHA SOIT :
C     . DE CLASSE C-INFINI ENTRE RINF ET RSUP ==> POLYNOME EN X
C     . DE CLASSE C2 AVEC SON PROLONGEMENT EN RINF ET RSUP. CES
C       PROLONGEMENTS SONT DES CONSTANTES? DONC ON S'ARRANGE POUR QUE
C       LES DERIVEES PREMIERES ET SECONDES SOIENT NULLES
C     ON SE BASE SUR UN POLYNOME EN X AU CUBE
C
C     ENTRE RINF ET (RINF+RSUP)/2. :
C       ALPHA     =  -4. * (X - RI)**3 / (RS - RI)**3 + 1
C       ALPHA '   = -12. * (X - RI)**2 / (RS - RI)**3
C       ALPHA ''  = -24. * (X - RI)    / (RS - RI)**3
C       ALPHA ''' = -24.               / (RS - RI)**3
C       ON VERIFIE QUE ALPHA(RI) = 1., ALPHA'(RI) = 0., ALPHA''(RI) = 0.
C
C     ENTRE (RINF+RSUP)/2. ET RSUP :
C       ALPHA     =  -4. * (X - RS)**3 / (RS - RI)**3
C       ALPHA '   = -12. * (X - RS)**2 / (RS - RI)**3
C       ALPHA ''  = -24. * (X - RS)    / (RS - RI)**3
C       ALPHA ''' = -24.               / (RS - RI)**3
C       ON VERIFIE QUE ALPHA(RS) = 0., ALPHA'(RS) = 0., ALPHA''(RS) = 0.
C
C     AU MILIEU IL Y A BIEN CONTINUITE C-INFINI :
C     ALPHA(RM) (GAUCHE) = -4.*(0.5*RS - 0.5*RI)**3 / (RS - RI)**3 + 1
C               (DROIT)  = -4.*(0.5*RI - 0.5*RS)**3 / (RS - RI)**3
C
C     ALPHA'(RM)   = 12. * (0.5*RI - 0.5*RS))**2 / (RS - RI)**3
C     ALPHA''(RM)  = 12. * (RI-RS) / (RS - RI)**3
C     ALPHA'''(RM) = 24. / (RS - RI)**3
C     LES DERIVEES SONT NULLES AU-DELA
C
C     =======================================================

      EPS  = 1.0D-06
      RMINI = RINF + EPS
      RMAXI = RSUP - EPS
      RMEAN = ( RINF + RSUP ) / 2.D0
      RDIFF = RSUP - RINF
C
C --- BOUCLE SUR LES NOEUDS DU MAILLAGE :
C     ---------------------------------
      DO 10 , I=1,NBNO
C
        XM = COOR((I-1)*3+1)
C
        IF ( XM.LT.RMINI ) THEN
          ALPHA = 1.D0
        ELSEIF ( XM.GT.RMAXI ) THEN
          ALPHA = 0.D0
        ELSEIF ( XM.LE.RMEAN ) THEN
          ALPHA = -4.D0 * ((XM - RINF)/RDIFF)**3 + 1
        ELSE
          ALPHA = -4.D0 * ((XM - RSUP)/RDIFF)**3
        ENDIF
C
        ZR(ITHETA+(I-1)*2+1-1) = ALPHA * MODULE
C
 10   CONTINUE
C
      CALL JEDEMA()
C
      END
