      SUBROUTINE EXTBSP ( MCF, IOCC, NOMF )
      IMPLICIT   NONE
      INTEGER             IOCC
      CHARACTER*(*)       MCF, NOMF
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/04/99   AUTEUR CIBHHPD P.DAVID 
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
C     RECUPERATION DU NOM D'UNE FONCTION DANS UN CONCEPT "TABL_INTSP"
C     ----------------------------------------------------------------
C
      INTEGER       IBID, IVAL(3), IVIT, NUMI, NUMJ, N1, N2, N3,
     +              N4, N5, N6, N7, N8, IRET, NBPAR, NBPARK
      REAL*8        R8B
      COMPLEX*16    C16B
      CHARACTER*8   K8B, INTESP, NOEUI, NOEUJ, KVAL(4), CMPI, CMPJ
      CHARACTER*16  NOPAR(5)
      CHARACTER*19  NOMFON
C     ----------------------------------------------------------------
C
      CALL GETVID ( MCF, 'INTE_SPEC'     , IOCC,1,1, INTESP, N1 )
      CALL GETVIS ( MCF, 'NUME_VITE_FLUI', IOCC,1,1, IVIT  , N2 )
      CALL GETVIS ( MCF, 'NUME_ORDRE_I'  , IOCC,1,1, NUMI  , N3 )
      CALL GETVIS ( MCF, 'NUME_ORDRE_J'  , IOCC,1,1, NUMJ  , N4 )
      CALL GETVID ( MCF, 'NOEUD_I'       , IOCC,1,1, NOEUI , N5 )
      CALL GETVID ( MCF, 'NOEUD_J'       , IOCC,1,1, NOEUJ , N6 )
      CALL GETVTX ( MCF, 'NOM_CMP_I'     , IOCC,1,1, CMPI  , N7 )
      CALL GETVTX ( MCF, 'NOM_CMP_J'     , IOCC,1,1, CMPJ  , N8 )
C
      NBPAR = 0
      NBPARK = 0
C
      IF ( N2 .NE. 0 ) THEN
         NBPAR = NBPAR + 1
         IVAL(NBPAR) = IVIT
         NOPAR(NBPAR) = 'NUME_VITE_FLUI'
      ENDIF
C
      IF ( N3 .NE. 0 ) THEN
         IF ( NUMI .GT. NUMJ ) THEN
            CALL UTMESS ('A','EXTBSP',
     +        'LA MATRICE EST TRIANGULAIRE SUPERIEUR-INVERSION INDICE')
            IBID  = NUMI
            NUMI  = NUMJ
            NUMJ  = IBID
         ENDIF
         NBPAR = NBPAR + 1
         IVAL(NBPAR) = NUMI
         NOPAR(NBPAR) = 'NUME_ORDRE_I'
         NBPAR = NBPAR + 1
         IVAL(NBPAR) = NUMJ
         NOPAR(NBPAR) = 'NUME_ORDRE_J'
      ENDIF
C
      IF ( N5 .NE. 0 ) THEN
         NBPARK = NBPARK + 1
         KVAL(NBPARK) = NOEUI
         NBPAR = NBPAR + 1
         NOPAR(NBPAR) = 'NOEUD_I'
         NBPARK = NBPARK + 1
         KVAL(NBPARK) = CMPI
         NBPAR = NBPAR + 1
         NOPAR(NBPAR) = 'NOM_CMP_I'
         NBPARK = NBPARK + 1
         KVAL(NBPARK) = NOEUJ
         NBPAR = NBPAR + 1
         NOPAR(NBPAR) = 'NOEUD_J'
         NBPARK = NBPARK + 1
         KVAL(NBPARK) = CMPJ
         NBPAR = NBPAR + 1
         NOPAR(NBPAR) = 'NOM_CMP_J'
      ENDIF
C
      CALL TBLIVA ( INTESP, NBPAR, NOPAR, IVAL, R8B, C16B, KVAL, K8B,
     +             R8B, 'FONCTION', K8B, IBID, R8B, C16B, NOMFON, IRET )
      IF ( IRET .NE. 0 ) CALL UTMESS('F','EXTBSP','Y A UN BUG' )
C
      NOMF = NOMFON
C
      END
