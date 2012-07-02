      SUBROUTINE RVPSTE ( DIM, LIEU, SSCH19, NOMSD, TYPAFF )
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      CHARACTER*24 NOMSD,LIEU
      CHARACTER*19 SSCH19
      CHARACTER*2  DIM
      CHARACTER*1  TYPAFF
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATION EXTRACTION DU POST-TRAITEMENT DE L' OCCURENCE COURANTE
C     ------------------------------------------------------------------
C IN  DIM    : K : '1D' OU '2D' OU '3D'(RELIQUAR --> MESSAGE)
C IN  LIEU   : K : OJB S V K24 NOMS DE LIEU DE POST-TRAITEMENT
C IN  SSCH19 : K : SS_CHAM_19 DE L' EXTRACTION DES CMP NECESSAIRES
C OUT NOMSD  : K : OJB S V K24  NOM DE SD D' EVALUATION SUR LES LIEUX
C     ------------------------------------------------------------------
C     LIEU DE POST-TRAITEMENT ::= RECORD
C        .ABSC : XD V R
C        .REFE : S E K8
C         CHOIX DOCU(.REFE) PARMI
C         -----             -----
C          'SGTD' : DESC = (XA,YA,XB,YB)       REFE =  NOM_COURBE
C          'SGT3' : DESC = (XA,YA,ZA,XB,YB,ZB) REFE =  NOM_COURBE
C          'ARCC' : DESC = (XC,YC,R,A1,A2)     REFE =  NOM_COURBE
C          'CHMM' : DESC = L_NOMS_NOEUDS       REFE =  NOM_COURBE
C          'LSTN' : DESC = L_NOMS_NOEUDS       REFE =  NOM_MAILLAGE
C         FIN_CHOIX
C         ---------
C
C     SD_EVAL
C        . UNE SD_EVAL PAR LIEU DE POST
C        . TYPE : SOUS_CHAM_GD
C        . LE LIEU JOUE LE ROLE D' UN MAILLAGE
C             NOEUD  : POINT DE POST
C             MAILLE : TYPE SEG2 DANS LES CAS 'SGTD','ARCC',CHMM'
C                      TYPE POI1 DANS LES CAS 'LSTN'
C
C    REPRESENTATION SOUS_CHAM_GD :
C        'CHNO' : STANDARD
C        'CHLM' : SEG2 --> NB_CMP PAR POINT, 2 POINTS
C                 POI1 --> NB_CMP PAR POINT, N POINTS
C                          N EST LE NOMBRE DE MAILLES SIGNIFICATIVES
C                          DU MAILLAGE INITIAL
C     ------------------------------------------------------------------
C
C
      INTEGER      ALIEU,IBID,L,NBL,ANOMSD,I,ANBNDF,ACLOCF,ADESCM
      CHARACTER*24 NREFE,DESCM
      CHARACTER*19 SDLIEU,SDEVAL
      CHARACTER*4  DOCU
      CHARACTER*1 K1BID
C
C==================== CORPS DE LA ROUTINE =============================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEMARQ()
      DESCM = '&&RVPSTE.PTR.DESC.TYP.MA'
      CALL JELIRA(LIEU,'LONMAX',NBL,K1BID)
      CALL JEVEUO(LIEU,'L',ALIEU)
      CALL WKVECT(NOMSD,'V V K24',NBL,ANOMSD)
      CALL I3CRDM(DESCM)
      CALL JEVEUO(DESCM,'L',ADESCM)
      CALL WKVECT('&&RVPSTE.NB.ND.FACE.TYPE','V V I',18,ANBNDF)
      CALL WKVECT('&&RVPSTE.CNC.LOC.FA.TYPE','V V I',72,ACLOCF)
      DO 5, I = 1, 3, 1
         IBID = ZI(ADESCM + I-1)
         DO 6, L = 1, 6, 1
            ZI(ANBNDF + 6*(I-1) + L-1) = ZI(IBID + L+1)
6        CONTINUE
         DO 7, L = 1, 24, 1
            ZI(ACLOCF + 24*(I-1) + L-1) = ZI(IBID + L+7)
7        CONTINUE
5     CONTINUE
      DO 10, L = 1, NBL, 1
         SDLIEU             = ZK24(ALIEU + L-1)(1:19)
         SDEVAL             = SDLIEU
         SDEVAL(1:8)        = '&&RVPSTE'
         ZK24(ANOMSD + L-1) = SDEVAL//'     '
         NREFE              = SDLIEU//'.REFE'
         CALL JELIRA(NREFE,'DOCU',IBID,DOCU)
         IF ( DOCU .EQ. 'SGTD' ) THEN
            CALL RVECHC(DIM,SSCH19,SDLIEU,SDEVAL,ZI(ANBNDF),ZI(ACLOCF))
         ELSE IF ( DOCU .EQ. 'ARCC') THEN
            CALL RVECHC(DIM,SSCH19,SDLIEU,SDEVAL,ZI(ANBNDF),ZI(ACLOCF))
         ELSE IF ( DOCU .EQ. 'SGT3') THEN
            CALL RVECHC(DIM,SSCH19,SDLIEU,SDEVAL,ZI(ANBNDF),ZI(ACLOCF))
         ELSE IF ( DOCU .EQ. 'CHMM') THEN
            CALL RVECHM(SSCH19,SDLIEU,SDEVAL)
         ELSE IF ( DOCU .EQ. 'LSTN') THEN
            IF ( TYPAFF .EQ. 'N' ) THEN
               CALL RVECHN(SSCH19,SDLIEU,SDEVAL)
            ELSE
               CALL RVECHE(SSCH19,SDLIEU,SDEVAL)
            ENDIF
         ELSE
C           AUTRE LIEU DE POST-TRAITEMENT
         ENDIF
10    CONTINUE
      CALL JEDETR('&&RVPSTE.NB.ND.FACE.TYPE')
      CALL JEDETR('&&RVPSTE.CNC.LOC.FA.TYPE')
      CALL I3DRDM(DESCM)
      CALL JEDEMA()
      END
