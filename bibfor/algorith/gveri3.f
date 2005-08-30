      SUBROUTINE GVERI3(CHFOND,LNOFF,THLAGR,NDEG,TRAV1,TRAV2,TRAV3)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/08/2005   AUTEUR GALENNE E.GALENNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE CIBHHLV L.VIVAN

      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
C FONCTION REALISEE:     DANS LE CADRE DE X-FEM
C
C     - METHODE THETA_LAGRANGE
C
C         POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
C         LE DOUBLET (RINF, RSUP )
C
C     - METHODE THETA_LEGENDRE
C
C         POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
C         LE TRIPLET ( DEGRE DES POLYNOMES DE LEGENDRE, RINF, RSUP )
C
C     ------------------------------------------------------------------
C ENTREE:
C        CHFOND : NOMS DES NOEUDS
C        LNOFF  : NOMBRE DE NOEUD DE GAMM0
C        THLAGR : SI THETA_LAGRANGE  THLAGR = .TRUE.
C        NDEG   : DEGRE DES POLYNOMES DE LEGENDRE
C
C SORTIE:
C        RINF          ( OBJET TRAV1 )
C        RSUP          ( OBJET TRAV2 )
C        MODULE(THETA) ( OBJET TRAV3 )
C     ------------------------------------------------------------------
C
      CHARACTER*24      TRAV0,TRAV1,TRAV2,TRAV3,CHFOND,ABSGAM
      CHARACTER*8       NOMPAR(1),THETF,RINFF,RSUPF
C
      INTEGER           LNOFF,NDEG,NBRE,NR,NRF,NBPAR,I,J
      INTEGER           IADRT0,IADRT1,IADRT2,IADRT3,IFON,IADABS,IER
C
      REAL*8            RINF,RSUP,THET,XL,VALPAR(1),VALRES
C
      LOGICAL           THLAGR
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON/IVARJE/ZI(1)
      COMMON/RVARJE/ZR(1)
      COMMON/CVARJE/ZC(1)
      COMMON/LVARJE/ZL(1)
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNOM,JEXNUM
      CHARACTER*80 ZK80
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
      CALL JEMARQ()
C
      IF(THLAGR) THEN
        NBRE   = LNOFF - 1
      ELSE
        NBRE = NDEG
        IF(NDEG.GT.7) THEN
          CALL UTMESS('F','GVERI3','LE DEGRE DU POLYNOME DOIT ETRE
     &                      AU PLUS EGAL A 7 ')
        ENDIF
      ENDIF
C
C ALLOCATION DE 3 OBJETS DE TRAVAIL
C
      TRAV0 = '&&VERIFG.GAM0'//'           '
      TRAV1 = '&&VERIFG.RINF'//'           '
      TRAV2 = '&&VERIFG.RSUP'//'           '
      TRAV3 = '&&VERIFG.THET'//'           '
      CALL WKVECT(TRAV0,'V V K8',LNOFF,IADRT0)
      CALL WKVECT(TRAV1,'V V R',LNOFF,IADRT1)
      CALL WKVECT(TRAV2,'V V R',LNOFF,IADRT2)
      CALL WKVECT(TRAV3,'V V R',(NBRE+1)*LNOFF,IADRT3)
C
      CALL GETVR8 (' ', 'R_INF', 0, 1, 1,RINF, NR)
      CALL GETVR8 (' ', 'R_SUP', 0, 1, 1,RSUP, NR)
      CALL GETVID (' ', 'R_INF_FO', 0, 1, 1,RINFF, NRF)
      CALL GETVID (' ', 'R_SUP_FO', 0, 1, 1,RSUPF, NRF)
C

      CALL JEVEUO(CHFOND,'L',IFON)
      ABSGAM='&&GVERI3.TEMP     .ABSCU'
      CALL WKVECT(ABSGAM,'V V R',LNOFF,IADABS)
      DO 10 I=1,LNOFF
        ZR(IADABS-1+(I-1)+1)=ZR(IFON-1+4*(I-1)+4)
 10   CONTINUE
      XL=ZR(IADABS-1+(LNOFF-1)+1)

      IF(.NOT.THLAGR) THEN
C
C METHODE THETA_LEGENDRE
C
         DO 50 J=1,LNOFF
              ZK8(IADRT0 + J - 1) = 'PTFONFIS'
              IF (NR.NE.0) THEN
                ZR(IADRT1 + J - 1) = RINF
                ZR(IADRT2 + J - 1) = RSUP
              ELSE IF (NRF.NE.0) THEN
                 NBPAR = 1
                 NOMPAR(1) = 'ABSC'
                 VALPAR(1) = ZR(IADABS + J - 1)
                 CALL FOINTE('FM',RINFF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                 ZR(IADRT1 + J - 1) = VALRES
                 CALL FOINTE('FM',RSUPF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                 ZR(IADRT2 + J - 1) = VALRES
             ELSE
                CALL UTMESS('F','GVERI3','PROBLEME DANS RINF ET RSUP ')
             ENDIF
50       CONTINUE
C
         CALL GLEGEN(NBRE,LNOFF,XL,ABSGAM,ZR(IADRT3))
         
      ELSE IF(THLAGR) THEN
C
C METHODE THETA_LAGRANGE
C
         DO 60 J=1,LNOFF
              ZK8(IADRT0 + J - 1) = 'PTFONFIS'
              IF (NR.NE.0) THEN
                ZR(IADRT1 + J - 1) = RINF
                ZR(IADRT2 + J - 1) = RSUP
              ELSE IF (NRF.NE.0) THEN
                 NBPAR = 1
                 NOMPAR(1) = 'ABSC'
                 VALPAR(1) = ZR(IADABS + J - 1)
                 CALL FOINTE('FM',RINFF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                 ZR(IADRT1 + J - 1) = VALRES
                 CALL FOINTE('FM',RSUPF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                 ZR(IADRT2 + J - 1) = VALRES
             ELSE
                CALL UTMESS('F','GVERI3','PROBLEME DANS RINF ET RSUP ')
             ENDIF
60       CONTINUE
C
      ENDIF
C
      CALL JEDETR (ABSGAM)
      CALL JEDETR (TRAV0)
C
      CALL JEDEMA()
      END
